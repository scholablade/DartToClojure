(ns dart2cljd.pattern
  "Dart 3 pattern -> deterministic cljd `cond` tests + destructuring bindings
  (research item R4).

  `alternatives` compiles a pattern against a subject *form* (usually a temp
  symbol) and returns a vector of alternatives:
    {:tests [form ...] :binds [[sym form] ...]}
  Each alternative matches when every test is truthy; on a match the binds
  destructure the subject into the pattern's variables. Or-patterns produce
  several alternatives (the caller duplicates the case body); and-patterns
  are merged into one.

  Type tests use `dart/is?`; object/record field access uses `.-field`;
  list/map/record structural tests use `count`/`contains?`. Getter paths may
  be evaluated more than once (documented divergence)."
  (:require [dart2cljd.emit.core :as core]))

(declare alternatives)

(defn- fail! [node msg]
  (core/fail! node msg))

(defn- alt [tests binds]
  {:tests (vec tests) :binds (vec binds)})

(def ^:private no-alt [(alt [] [])])

(defn- cross [as bs]
  (vec (for [a as b bs]
         (alt (concat (:tests a) (:tests b))
              (concat (:binds a) (:binds b))))))

(defn- type-test [node ty ctx]
  (when ty
    (list 'dart/is? node (core/type-symbol ty ctx))))

(defn- list-elements [p subject ctx opts]
  (let [structural? (get opts :structural-tests? true)]
    (loop [els (seq (:elements p)) i 0 tests [] binds [] rest? false]
      (if-not els
        (alt (concat (when structural?
                       (concat
                        (when-let [t (type-test subject
                                                {:ir :named-type :prefix nil :name "List"
                                                 :type-args [] :nullable false :src (:src p)}
                                                ctx)]
                          [t])
                        (if rest?
                          [(list '<= i (list 'count subject))]
                          [(list '= i (list 'count subject))])))
                     tests)
             binds)
      (let [e (first els)]
        (if (= :rest-pattern (:ir e))
          (let [inner (:pattern e)
                rest-subject (list 'into [] (list 'drop i subject))
                {sub-tests :tests sub-binds :binds}
                (first (alternatives inner rest-subject ctx opts))]
            (recur (next els) i (into tests sub-tests) (into binds sub-binds) true))
          (let [{sub-tests :tests sub-binds :binds}
                (first (alternatives e (list 'get subject i) ctx opts))]
            (recur (next els) (inc i) (into tests sub-tests) (into binds sub-binds) rest?))))))))

(defn- record-field-subject [subject pfield pos]
  (if-let [n (:name pfield)]
    (list (symbol (str ".-" n)) subject)
    (list (symbol (str ".-$" pos)) subject)))

(defn- map-pattern-alt [p subject ctx opts]
  (let [base (when (get opts :structural-tests? true)
               (when-let [t (type-test subject
                                       {:ir :named-type :prefix nil :name "Map"
                                        :type-args [] :nullable false :src (:src p)}
                                       ctx)]
                 [t]))]
    (loop [es (seq (:entries p)) tests base binds []]
      (if-not es
        (alt tests binds)
        (let [e (first es)
              key (core/emit-expr (:key e) ctx)
              sub (list 'get subject key)
              {sub-tests :tests sub-binds :binds}
              (first (alternatives (:value e) sub ctx opts))]
          (recur (next es)
                 (into tests (cons (list 'contains? subject key) sub-tests))
                 (into binds sub-binds)))))))

(defn- record-pattern-alt [p subject ctx opts]
  (let [base (when (get opts :structural-tests? true)
               (when-let [t (type-test subject
                                       {:ir :named-type :prefix nil :name "Record"
                                        :type-args [] :nullable false :src (:src p)}
                                       ctx)]
                 [t]))]
    (loop [fs (seq (:fields p)) pos 1 tests base binds []]
      (if-not fs
        (alt tests binds)
        (let [f (first fs)
              named? (some? (:name f))
              sub (record-field-subject subject f pos)
              {sub-tests :tests sub-binds :binds}
              (first (alternatives (:pattern f) sub ctx opts))]
          (recur (next fs)
                 (if named? pos (inc pos))
                 (into tests sub-tests)
                 (into binds sub-binds)))))))

(defn- object-pattern-alt [p subject ctx opts]
  (let [base (when-let [t (type-test subject (:type p) ctx)] [t])]
    (loop [fs (seq (:fields p)) tests base binds []]
      (if-not fs
        (alt tests binds)
        (let [f (first fs)
              sub (list (symbol (str ".-" (:name f))) subject)
              {sub-tests :tests sub-binds :binds}
              (first (alternatives (:pattern f) sub ctx opts))]
          (recur (next fs) (into tests sub-tests) (into binds sub-binds)))))))

(def ^:private rel-ops
  {"<" '< ">" '> "<=" '<= ">=" '>= "==" '= "!=" 'not=})

(defn alternatives
  "Compile pattern `p` against subject form `subject`.

  Options:
    :structural-tests?  when false (declarations/assignments), implicit
                        List/Map/Record/count shape tests are omitted."
  ([p subject ctx] (alternatives p subject ctx {}))
  ([p subject ctx opts]
  (if (or (nil? p) (not (map? p)))
    no-alt
    (case (:ir p)
      :const-pattern
      [(alt [(list '= subject (core/emit-expr (:expr p) ctx))] [])]

      :var-pattern
      [(alt (cond-> []
              (:type p) (conj (list 'dart/is? subject (core/type-symbol (:type p) ctx))))
            [[(symbol (:name p)) subject]])]

      :wildcard-pattern
      [(alt (cond-> []
              (:type p) (conj (list 'dart/is? subject (core/type-symbol (:type p) ctx))))
            [])]

      :rel-pattern
      (let [op (get rel-ops (:op p))]
        (when-not op (fail! p (str "unsupported relational pattern operator " (:op p))))
        [(alt [(list op subject (core/emit-expr (:operand p) ctx))] [])])

      :paren-pattern (alternatives (:pattern p) subject ctx opts)

      :cast-pattern
      (cross [(alt [(list 'dart/is? subject (core/type-symbol (:type p) ctx))] [])]
             (alternatives (:pattern p) subject ctx opts))

      (:null-check-pattern :null-assert-pattern)
      (cross [(alt [(list 'some? subject)] [])]
             (alternatives (:pattern p) subject ctx opts))

      :list-pattern [(list-elements p subject ctx opts)]

      :map-pattern [(map-pattern-alt p subject ctx opts)]

      :record-pattern [(record-pattern-alt p subject ctx opts)]

      :object-pattern [(object-pattern-alt p subject ctx opts)]

      :and-pattern (cross (alternatives (:lhs p) subject ctx opts)
                          (alternatives (:rhs p) subject ctx opts))

      :or-pattern (into (alternatives (:lhs p) subject ctx opts)
                        (alternatives (:rhs p) subject ctx opts))

      :guarded-pattern (alternatives (:pattern p) subject ctx opts)

      (fail! p (str "unsupported pattern " (:ir p)))))))

(defn guard-expr
  "Guard expression of a possibly guarded pattern (evaluated after binds)."
  [p]
  (when (and (map? p) (= :guarded-pattern (:ir p)))
    (:when p)))
