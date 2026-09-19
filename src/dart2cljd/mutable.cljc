(ns dart2cljd.mutable
  "Deterministic syntactic mutability analysis (research item R1).

  A Dart local that is assigned after its declaration (plain assignment,
  compound assignment, `++`/`--`) is emitted as a mutable box:
  `volatile!` normally, `atom` when the local is referenced from a nested
  function (closure capture). Reads become `@x`, writes become `vswap!` /
  `vreset!` (or `swap!` / `reset!` for atoms).

  The analysis is purely syntactic and intentionally over-approximates:
  name shadowing in disjoint scopes is not tracked (the result is still
  correct, just occasionally boxes a local that did not need it), and
  references inside nested functions only feed the `:captured` set.

  No type information is needed, so results are stable and reproducible."
  (:require [clojure.set :as set]))

(declare walk)

(def ^:private skip-keys #{:ir :src :annotations})

(defn- walk-children [node depth acc]
  (reduce-kv (fn [acc k v]
               (if (contains? skip-keys k)
                 acc
                 (walk v depth acc)))
             acc
             node))

(defn- declare-name [acc n]
  (cond-> acc (string? n) (update :declared conj n)))

(defn- assign-name [acc n]
  (cond-> acc (string? n) (update :assigned conj n)))

(defn- ref-name [acc n]
  (cond-> acc (string? n) (update :free conj n)))

(defn- pattern-declares
  "Record every variable bound by a Dart pattern as declared."
  [acc p]
  (if (or (nil? p) (not (map? p)))
    acc
    (case (:ir p)
      :var-pattern (declare-name acc (:name p))
      :guarded-pattern (pattern-declares acc (:pattern p))
      (:and-pattern :or-pattern)
      (-> acc
          (pattern-declares (:lhs p))
          (pattern-declares (:rhs p)))
      (:cast-pattern :null-assert-pattern :null-check-pattern :paren-pattern)
      (pattern-declares acc (:pattern p))
      :list-pattern (reduce pattern-declares acc (:elements p))
      :map-pattern (reduce pattern-declares acc (map :value (:entries p)))
      :record-pattern (reduce pattern-declares acc (:fields p))
      :object-pattern (reduce pattern-declares acc (:fields p))
      :pattern-field (pattern-declares acc (:pattern p))
      :rest-pattern (pattern-declares acc (:pattern p))
      acc)))

(defn- pattern-assigns
  "Record assignment to every variable target of a Dart pattern."
  [acc p]
  (if (or (nil? p) (not (map? p)))
    acc
    (case (:ir p)
      :var-pattern (assign-name acc (:name p))
      (:and-pattern :or-pattern)
      (-> acc
          (pattern-assigns (:lhs p))
          (pattern-assigns (:rhs p)))
      (:cast-pattern :null-assert-pattern :null-check-pattern :paren-pattern)
      (pattern-assigns acc (:pattern p))
      :list-pattern (reduce pattern-assigns acc (:elements p))
      :map-pattern (reduce pattern-assigns acc (map :value (:entries p)))
      :record-pattern (reduce pattern-assigns acc (:fields p))
      :object-pattern (reduce pattern-assigns acc (:fields p))
      :pattern-field (pattern-assigns acc (:pattern p))
      :rest-pattern (pattern-assigns acc (:pattern p))
      acc)))

(defn- walk
  [node depth acc]
  (cond
    (map? node)
    (case (:ir node)
      :id (if (pos? depth) (ref-name acc (:name node)) acc)

      ;; nested functions: only references inside count (as captures);
      ;; their own declarations/assignments belong to their own analysis.
      :fn (walk (:body node) (inc depth) acc)
      :fn-decl (let [acc (if (zero? depth)
                           (declare-name acc (:name node))
                           acc)]
                 (walk (:body node) (inc depth) acc))

      :var-decl
      (reduce (fn [acc v]
                (-> acc
                    (declare-name (:name v))
                    (as-> a (walk (:init v) depth a))))
              acc (:vars node))

      :declared-ident (declare-name acc (:name node))

      :assign
      (let [acc (if (and (zero? depth) (= :id (:ir (:lhs node))))
                  (assign-name acc (:name (:lhs node)))
                  acc)
            acc (walk (:lhs node) depth acc)]
        (walk (:rhs node) depth acc))

      :postfix
      (let [acc (if (and (zero? depth)
                         (contains? #{"++" "--"} (:op node))
                         (= :id (:ir (:operand node))))
                  (assign-name acc (:name (:operand node)))
                  acc)]
        (walk (:operand node) depth acc))

      :prefix
      (let [acc (if (and (zero? depth)
                         (contains? #{"++" "--"} (:op node))
                         (= :id (:ir (:operand node))))
                  (assign-name acc (:name (:operand node)))
                  acc)]
        (walk (:operand node) depth acc))

      :for-in
      (let [acc (-> acc
                    (declare-name (:name (:var node)))
                    (declare-name (:name (:pattern node)))
                    (pattern-declares (:pattern node))
                    (as-> a (walk (:iterable node) depth a)))]
        (walk (:body node) depth acc))

      :for-parts
      (let [init (:init node)
            acc (if (and init (= :var-decl (:ir init)))
                  (reduce declare-name acc (map :name (:vars init)))
                  acc)
            acc (-> acc
                    (pattern-declares (:pattern node))
                    (as-> a (walk init depth a))
                    (as-> a (walk (:cond node) depth a))
                    (as-> a (walk (:updaters node) depth a)))]
        (walk (:body node) depth acc))

      :for-element
      (let [loop (:loop node)
            acc (-> acc
                    (declare-name (:name (:var loop)))
                    (pattern-declares (:pattern loop))
                    (as-> a (walk loop depth a)))]
        (walk (:body node) depth acc))

      :pattern-var-decl
      (let [acc (-> acc
                    (pattern-declares (:pattern node))
                    (as-> a (walk (:expr node) depth a)))]
        acc)

      :pattern-assign
      (let [acc (if (zero? depth)
                  (pattern-assigns acc (:pattern node))
                  acc)
            acc (walk (:pattern node) depth acc)]
        (walk (:expr node) depth acc))

      :catch
      (let [acc (-> acc
                    (declare-name (:exception-var node))
                    (declare-name (:stack-var node)))]
        (walk (:body node) depth acc))

      (walk-children node depth acc))

    (vector? node) (reduce (fn [acc x] (walk x depth acc)) acc node)
    (seq? node) (reduce (fn [acc x] (walk x depth acc)) acc node)
    :else acc))

(defn analyze
  "Analyze a function/method body block (IR) and its parameter names.

  Options:
    :params   collection of parameter names (declared, may be assigned)
    :exclude  names that are not mutable locals (e.g. instance fields)

  Returns {:mutable #{name} :captured #{name} :declared #{name}}.

  `:exclude` only removes names from `:mutable`/`:captured` (a field cannot be
  boxed); `:declared` keeps them, because a parameter or local that *shadows*
  a field is a real local: Dart scoping is innermost-first, so the emitter
  must resolve it as one (`(error x)` calls the parameter, `(.error this x)`
  the field)."
  [block {:keys [params exclude]}]
  (let [base {:declared (set params) :assigned #{} :free #{}}
        acc (walk block 0 base)
        mutable (-> (set/intersection (:declared acc) (:assigned acc))
                    (set/difference (set exclude)))]
    {:mutable mutable
     :captured (set/intersection mutable (:free acc))
     :declared (:declared acc)}))

(defn assigns?
  "True when `node` contains a depth-0 assignment to the local `name`
  (plain/compound assignment or ++/--). Used to reject mutation of loop
  bindings, which cannot be boxed."
  [node name]
  (let [found? (atom false)]
    (letfn [(scan [n depth]
              (when-not @found?
                (when (map? n)
                  (case (:ir n)
                    (:fn :fn-decl) nil
                    :assign
                    (do
                      (when (and (zero? depth)
                                 (= :id (:ir (:lhs n)))
                                 (= name (:name (:lhs n))))
                        (reset! found? true))
                      (doseq [v (vals (dissoc n :ir :src :annotations))]
                        (scan v depth)))
                    (:postfix :prefix)
                    (do
                      (when (and (zero? depth)
                                 (contains? #{"++" "--"} (:op n))
                                 (= :id (:ir (:operand n)))
                                 (= name (:name (:operand n))))
                        (reset! found? true))
                      (doseq [v (vals (dissoc n :ir :src :annotations))]
                        (scan v depth)))
                    (doseq [v (vals (dissoc n :ir :src :annotations))]
                      (scan v depth))))
                (when (coll? n)
                  (doseq [v n] (scan v depth)))))]
      (scan node 0)
      @found?)))
