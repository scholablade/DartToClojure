(ns dart2cljd.idiomatic
  "M4 idiomatic mode: Flutter widget class analysis and emission.

  A Dart `StatelessWidget` subclass becomes a plain builder fn; a
  `StatefulWidget` + `State` pair becomes a fn whose State instance fields
  are atoms watched by `f/widget :watch`, whose State methods become local
  fns (`letfn`) and whose `build` method becomes the widget body.

  Everything here is a pure function of the unit IR. Classes whose shape is
  not recognized (unrecognized lifecycle overrides, getters/setters, statics,
  generics, named constructors, mixins, heritage from other unit classes, ...)
  are left to the faithful emitter.

  Widget identity and rebuilds:
    * a constructor `key` parameter becomes a `.key` parameter of the builder
      fn, and the emitted widget is wrapped in `(m/KeyedSubtree .key key …)`
      (`f/widget :key` would double-wrap through `ValueKey`);
    * every `setState` call additionally bumps a hidden `d2c$tick` atom which
      the widget watches, so a callback that writes no state atom (or mutates
      the object held by one) still rebuilds. The extra atom swap is
      redundant when the callback does write state atoms; Flutter coalesces
      the resulting rebuild scheduling.

  Lifecycle resources:
    * `initState` is recognized when it only calls `super.initState()` and
      assigns declared fields that have no initializer; those fields become
      `f/widget :managed` resources with `:refresh-on :never` (created once,
      like initState);
    * `dispose` is recognized when it only calls `super.dispose()` and
      disposes exactly those fields (`field.dispose()`/`.cancel()`/`.close()`
      map to the `:dispose` option);
    * `vsync: this` in a managed expression maps to the `:vsync` directive
      binding (`d2c$vsync`);
    * managed fields may be read from `build` (including closures) but not
      from other State methods, which are emitted outside the widget body and
      cannot see the managed bindings; anything else falls back.

  Documented divergence (deterministic): state fields are one atom each;
  in-place mutation of the object held by an atom notifies watchers only
  through the `setState` tick, not by itself."
  (:refer-clojure :exclude [methods])
  (:require [dart2cljd.alias :as alias]
            [dart2cljd.emit.core :as core]))

;; ---------------------------------------------------------------------------
;; IR shape helpers
;; ---------------------------------------------------------------------------

(def ^:private lifecycle-methods
  "State lifecycle members that have no deterministic f/widget mapping."
  #{"initState" "dispose" "didUpdateWidget" "didChangeDependencies"
    "reassemble" "activate" "deactivate" "setState"})

(defn- named-type-name [t]
  (when (= :named-type (:ir t)) (str (:name t))))

(defn- super-name [d]
  (named-type-name (:extends d)))

(defn- state-type-arg
  "For `class S extends State<W>`, the name \"W\"."
  [d]
  (let [e (:extends d)]
    (when (and (= :named-type (:ir e)) (= "State" (str (:name e))))
      (some-> (first (:type-args e)) :name str))))

(defn- heritage-targets [d]
  (concat (when-let [n (super-name d)] [n])
          (keep named-type-name (:with d))
          (keep named-type-name (:implements d))))

(defn- methods [d] (filter #(= :method (:ir %)) (:members d)))
(defn- ctors [d] (filter #(= :ctor (:ir %)) (:members d)))

(defn- field-specs
  "Instance field declarations of a class:
  {:name :type :init :final :src}."
  [d]
  (vec (for [m (:members d)
             :when (and (= :field-decl (:ir m)) (not (:static m)))
             v (get-in m [:fields :vars])]
         {:name (:name v)
          :type (get-in m [:fields :type])
          :init (:init v)
          :final (boolean (or (get-in m [:fields :final])
                              (get-in m [:fields :const])))
          :src (:src v)})))

(defn- has-static-member? [d]
  (boolean (some #(and (contains? #{:field-decl :method} (:ir %)) (:static %))
                 (:members d))))

(defn- class-shape-ok?
  "True when a class has no heritage/generic shape that the widget mapping
  cannot represent."
  [d]
  (and (not (:abstract d))
       (nil? (:modifier d))
       (empty? (:type-params d))
       (empty? (:with d))
       (empty? (:implements d))
       (not (has-static-member? d))))

(defn- regular-methods
  "Plain instance methods of a class, or nil when any method is a getter,
  setter, operator, static, abstract, external or generic."
  [d]
  (let [ms (methods d)]
    (when (every? #(and (= :method (:kind %))
                        (not (:static %))
                        (not (:abstract %))
                        (not (:external %))
                        (not (:operator %))
                        (empty? (:type-params %)))
                  ms)
      ms)))

(defn- widget-ctor
  "The unnamed non-factory constructor of a widget class, a synthetic
  no-arg one when the class declares none, or nil when unsupported."
  [d]
  (let [cs (ctors d)]
    (cond
      (empty? cs) {:ir :ctor :name nil :params [] :initializers [] :src (:src d)}
      (and (= 1 (count cs))
           (nil? (:name (first cs)))
           (not (:factory (first cs)))
           (not (:external (first cs)))
           (empty? (:initializers (first cs))))
      (first cs)
      :else nil)))

(defn- keep-key-param
  "The builder-fn form of a constructor `key` parameter: a `super.key` loses
  its `:super` flag, and an optional parameter gets an explicit nil default
  (Dart named params are implicitly nullable; cljd named params are required
  without a default)."
  [p]
  (let [p (dissoc p :super)]
    (if (and (not (:required p)) (nil? (:default p)))
      (assoc p :default {:ir :null-lit})
      p)))

(defn- clean-params
  "Constructor parameters without framework params (`super.*`), keeping `key`
  as a plain parameter so widget identity survives (see the ns docstring).

  Returns {:params [...] :drop-args #{...} :key? bool} or nil when a dropped
  framework parameter cannot be dropped safely at call sites (i.e. it is not
  named)."
  [ctor]
  (loop [ps (seq (:params ctor)) out [] drops #{} key? false]
    (if-not ps
      {:params (vec out) :drop-args drops :key? key?}
      (let [p (first ps)
            n (:name p)
            named? (#{:named} (:kind p))]
        (cond
          (= "key" n)
          (recur (next ps) (conj out (keep-key-param p)) drops true)

          (:super p)
          (if named?
            (recur (next ps) out (cond-> drops n (conj n)) key?)
            nil)

          :else (recur (next ps) (conj out p) drops key?))))))

(defn- find-method [d name]
  (some #(when (= name (:name %)) %) (methods d)))

(defn- build-param
  "The single required positional `BuildContext` parameter of a build
  method, or nil."
  [m]
  (let [ps (:params m)]
    (when (and (= 1 (count ps))
               (= :required-positional (:kind (first ps))))
      (first ps))))

;; ---------------------------------------------------------------------------
;; state-class rewriting (this.x / widget.x / setState)
;; ---------------------------------------------------------------------------

(defn- this-node? [t] (= :this (:ir t)))

(defn- widget-base?
  "True for parse shapes of `widget` (bare id or `widget.x` prefix)."
  [t]
  (or (and (= :id (:ir t)) (= "widget" (:name t)))
      (and (= :prefixed-id (:ir t)) (= "widget" (:prefix t)))))

(defn- walk-fields [n walk]
  (reduce-kv (fn [m k v]
               (assoc m k (if (contains? #{:ir :src :annotations} k)
                            v (walk v))))
             {} n))

(defn- rewrite-state
  "Rewrite `this.x`, `this.m(...)` and `widget.x` accesses in a State class
  member. Returns [node ok?]; unsupported accesses mark `ok?` false."
  [node {:keys [fields methods widget-fields context-name]}]
  (let [bad (atom nil)
        field? #(contains? fields %)
        method? #(contains? methods %)
        fail! (fn [n] (reset! bad n) n)]
    (letfn [(walk [n]
              (cond
                (map? n)
                (case (:ir n)
                  :prefixed-id
                  (let [p (:prefix n) nm (:name n)]
                    (cond
                      (= "widget" p)
                      (if (contains? widget-fields nm)
                        {:ir :id :name nm :src (:src n)}
                        (fail! n))

                      (= "this" p)
                      (if (or (field? nm) (method? nm))
                        {:ir :id :name nm :src (:src n)}
                        (fail! n))

                      :else (walk-fields n walk)))

                  :prop
                  (let [t (:target n) nm (:name n)]
                    (cond
                      (this-node? t)
                      (if (or (field? nm) (method? nm))
                        {:ir :id :name nm :src (:src n)}
                        (fail! n))

                      (widget-base? t)
                      (if (contains? widget-fields nm)
                        (assoc n :target {:ir :id :name nm :src (:src t)})
                        ;; widget.a.b: rewrite `widget.a` to `a`, keep `.b`
                        (if (and (= :prefixed-id (:ir t))
                                 (contains? widget-fields (:name t)))
                          (assoc (walk-fields n walk)
                                 :target {:ir :id :name (:name t) :src (:src t)})
                          (fail! n)))

                      :else (walk-fields n walk)))

                  :call
                  (let [t (:target n) nm (:name n)]
                    (cond
                      (this-node? t)
                      (if (or (method? nm) (= "setState" nm))
                        (-> n (assoc :target nil) (walk-fields walk))
                        (fail! n))

                      (widget-base? t)
                      (fail! n)

                      :else (walk-fields n walk)))

                  :id
                  (cond
                    (contains? #{"widget" "mounted"} (:name n)) (fail! n)
                    (and (= "context" (:name n)) (not= "context" context-name))
                    (fail! n)
                    :else n)

                  :this (fail! n)
                  :super (fail! n)
                  (walk-fields n walk))

                (vector? n) (mapv walk n)
                :else n))]
      (let [out (walk node)]
        [out (nil? @bad)]))))

(defn- references-name? [node n]
  (cond
    (map? node)
    (or (some #(= n %) [(:name node) (:prefix node) (:id node)])
        (boolean (some #(references-name? % n)
                       (vals (dissoc node :ir :src :annotations)))))
    (coll? node) (boolean (some #(references-name? % n) node))
    :else false))

(defn- uses-set-state?
  "True when the IR contains a target-less `setState(...)` call (the shape
  `rewrite-state` leaves for `this.setState(...)`)."
  [node]
  (cond
    (map? node)
    (or (and (= :call (:ir node))
             (nil? (:target node))
             (= "setState" (:name node)))
        (boolean (some uses-set-state?
                       (vals (dissoc node :ir :src :annotations)))))
    (coll? node) (boolean (some uses-set-state? node))
    :else false))

(defn- mutates-name?
  "True when the IR assigns or increments the identifier `n`."
  [node n]
  (cond
    (map? node)
    (or (and (contains? #{:assign :postfix :prefix} (:ir node))
             (let [t (or (:lhs node) (:operand node))]
               (and (map? t) (= :id (:ir t)) (= n (:name t)))))
        (boolean (some #(mutates-name? % n)
                       (vals (dissoc node :ir :src :annotations)))))
    (coll? node) (boolean (some #(mutates-name? % n) node))
    :else false))

(defn- strip-paren [node]
  (if (= :paren (:ir node)) (recur (:expr node)) node))

(defn- this-member-name
  "Name when `t` accesses a member of `names` through an implicit or
  explicit `this` (`f`, `this.f`)."
  [t names]
  (let [t (strip-paren t)]
    (cond
      (and (map? t) (= :id (:ir t)) (contains? names (:name t))) (:name t)
      (and (map? t) (= :prefixed-id (:ir t)) (= "this" (:prefix t))
           (contains? names (:name t))) (:name t)
      (and (map? t) (= :prop (:ir t)) (this-node? (:target t))
           (contains? names (:name t))) (:name t)
      :else nil)))

(defn- super-call? [e name]
  (and (= :call (:ir e))
       (= :super (:ir (:target e)))
       (= name (:name e))))

(defn- stmt-expr [st]
  (when (= :expr-stmt (:ir st)) (:expr st)))

(def ^:private dispose-forms
  {"dispose" '.dispose "cancel" '.cancel "close" '.close})

(defn- parse-init-state
  "Recognized `initState` body: `super.initState()` plus `field = expr`
  assignments to declared fields without initializers. Returns
  {:assignments [{:name f :expr e}]} or nil when the body has anything else."
  [m field-map]
  (when (= :block-body (:ir (:body m)))
    (let [names (set (keys field-map))]
      (reduce
       (fn [acc st]
         (let [e (stmt-expr st)
               f (when (and e (= :assign (:ir e)) (= "=" (:op e)))
                   (this-member-name (:lhs e) names))]
           (cond
             (and e (super-call? e "initState")) acc
             (and f (nil? (:init (get field-map f))))
             (if (some #(= f (:name %)) (:assignments acc))
               (reduced nil)
               (update acc :assignments conj {:name f :expr (:rhs e)}))
             :else (reduced nil))))
       {:assignments []}
       (get-in m [:body :block :stmts])))))

(defn- parse-dispose
  "Recognized `dispose` body: `super.dispose()` plus `field.dispose()` /
  `field.cancel()` / `field.close()` for the given managed fields. Returns
  {:disposes {field form}} or nil when the body has anything else."
  [m managed-names]
  (when (= :block-body (:ir (:body m)))
    (reduce
     (fn [acc st]
       (let [e (stmt-expr st)
             f (when (and e (= :call (:ir e)))
                 (this-member-name (:target e) managed-names))
             form (when f (get dispose-forms (:name e)))]
         (cond
           (and e (super-call? e "dispose")) acc
           (and f form) (update acc :disposes assoc f form)
           :else (reduced nil))))
     {:disposes {}}
     (get-in m [:body :block :stmts]))))

(defn- substitute-vsync
  "Replace `this` used as the value of a `vsync:` named argument with the
  `:vsync` directive binding `d2c$vsync`. Returns [expr vsync?] or nil when
  `this` appears anywhere else in the expression."
  [node]
  (let [stray? (volatile! false)
        vsync? (volatile! false)]
    (letfn [(walk [n]
              (cond
                (map? n)
                (cond
                  (= :this (:ir n)) (do (vreset! stray? true) n)

                  (and (= :arg (:ir n)) (= "vsync" (:name n)))
                  (if (= :this (:ir (:expr n)))
                    (do (vreset! vsync? true)
                        (assoc n :expr {:ir :id :name "d2c$vsync" :src (:src n)}))
                    (walk-fields n walk))

                  :else (walk-fields n walk))

                (vector? n) (mapv walk n)
                :else n))]
      (let [out (walk node)]
        (when-not @stray?
          [out @vsync?])))))

;; ---------------------------------------------------------------------------
;; plan
;; ---------------------------------------------------------------------------

(defn- widget-fields-ok
  "Field specs of a widget class: every field must be immutable and either
  parameter-backed or initialized in its declaration."
  [d param-names]
  (let [specs (field-specs d)]
    (when (every? (fn [f]
                    (and (:final f)
                         (or (contains? param-names (:name f)) (:init f))))
                  specs)
      specs)))

(defn- fill-param-types
  "Field-formal parameters (`this.x`) carry no type in the Dart AST; copy the
  declared field type onto them so the builder fn parameter keeps its scalar
  `^Type` hint (int literals reaching `double` slots)."
  [params fields]
  (let [types (into {} (keep (fn [f] (when (:type f) [(:name f) (:type f)])))
                    fields)]
    (mapv (fn [p]
            (if (and (:this p) (nil? (:type p)) (contains? types (:name p)))
              (assoc p :type (get types (:name p)))
              p))
          params)))

(defn- stateless-info [d]
  (when (class-shape-ok? d)
    (when-let [ctor (widget-ctor d)]
      (when-let [{:keys [params drop-args key?]} (clean-params ctor)]
        (when-let [ms (regular-methods d)]
          (let [build (find-method d "build")
                others (remove #(= "build" (:name %)) ms)
                param-names (set (keep :name params))]
            (when build
              (when-let [ctx-param (build-param build)]
                (when-let [fields (widget-fields-ok d param-names)]
                  (let [env {:fields (set (map :name fields))
                             :methods (set (map :name others))
                             :widget-fields #{}
                             :context-name nil}
                        rw-methods (mapv #(rewrite-state % env) others)
                        rw-build (rewrite-state build
                                                (assoc env :context-name
                                                       (:name ctx-param)))]
                    (when (and (every? second rw-methods) (second rw-build))
                      {:kind :stateless
                       :name (:name d)
                       :params (fill-param-types params fields)
                       :drop-args drop-args
                       :key? key?
                       :field-inits (vec (filter :init fields))
                       :methods (mapv first rw-methods)
                       :build (first rw-build)
                       :context-name (:name ctx-param)
                       :state-fields []})))))))))))

(defn- state-info
  "Analyse a `State<W>` class; `widget-field-names` are the fields of the
  paired `StatefulWidget`, readable through `widget.<field>`.

  `initState`/`dispose` are recognized when the class initializes fields that
  have no initializer (they become `:managed` resources, created once and
  disposed) and disposes exactly those fields. Managed fields may be read
  from `build` (including closures) but not from other methods, which are
  emitted outside the widget body and cannot see the managed bindings.
  Anything else keeps the faithful fallback."
  [s widget-field-names]
  (when (class-shape-ok? s)
    (when-let [ms (regular-methods s)]
      (let [build (find-method s "build")
            init (find-method s "initState")
            dispose (find-method s "dispose")
            others (remove #(contains? #{"build" "initState" "dispose"} (:name %)) ms)]
        (when (and build
                   (build-param build)
                   (not-any? #(contains? lifecycle-methods (:name %)) others))
          (let [fields (field-specs s)
                field-map (into {} (map (juxt :name identity)) fields)
                parsed-init (if init (parse-init-state init field-map)
                                {:assignments []})
                managed-names (set (map :name (:assignments parsed-init)))
                parsed-dispose (if dispose (parse-dispose dispose managed-names)
                                   {:disposes {}})
                managed (when (and parsed-init parsed-dispose)
                          (mapv (fn [a]
                                  (assoc a :dispose
                                         (get (:disposes parsed-dispose) (:name a))))
                                (:assignments parsed-init)))
                uninitialized (remove #(or (:init %)
                                           (contains? managed-names (:name %)))
                                      fields)
                method-names (set (map :name others))
                env {:fields (set (map :name fields))
                     :methods method-names
                     :widget-fields widget-field-names
                     :context-name (:name (build-param build))}
                rewritten (map (fn [m] (rewrite-state m env)) (concat others [build]))
                ok? (every? second rewritten)
                [others' build'] (if ok?
                                   [(mapv first (butlast rewritten))
                                    (first (last rewritten))]
                                   [nil nil])
                field-rewrites (map #(rewrite-state (:init %) env) fields)
                fields-ok? (every? second field-rewrites)
                managed-ok? (and parsed-init
                                 parsed-dispose
                                 (empty? uninitialized)
                                 (every? :dispose managed)
                                 (every? (fn [m]
                                           (not-any? #(references-name? % (:name m))
                                                     others'))
                                         managed))
                rw (mapv (fn [m]
                           (when-let [[e vsync?] (substitute-vsync (:expr m))]
                             (let [[e ok] (rewrite-state e env)]
                               (when ok (assoc m :init e :vsync? vsync?)))))
                         managed)
                managed-rw (when (every? some? rw) rw)
                field-init (into {} (map (fn [f [init _]] [(:name f) init])
                                         fields field-rewrites))]
            (when (and ok? fields-ok? managed-ok? managed-rw
                       (not-any? #(mutates-name? build' (:name %)) managed))
              {:kind :stateful
               :name (:name s)
               :fields (mapv (fn [f] {:name (:name f) :init (field-init (:name f))})
                             (remove #(contains? managed-names (:name %)) fields))
               :managed managed-rw
               :vsync? (boolean (some :vsync? managed-rw))
               :methods others'
               :build build'
               :context-name (:name (build-param build))})))))))

(defn- created-state-name
  "Name of the State class returned by a `createState` method, or nil.
  A no-argument `Foo()` is parsed as a target-less call in parse-only mode,
  so that shape is accepted when `Foo` names a class of the unit."
  [create by-name]
  (when create
    (let [e (some-> create :body :expr)
          e (if (= :paren (:ir e)) (:expr e) e)]
      (case (:ir e)
        :new (get-in e [:ctor :type :name])
        :call (when (and (nil? (:target e)) (empty? (:args e))
                         (contains? by-name (str (:name e))))
                (str (:name e)))
        nil))))

(defn- stateful-info [d by-name]
  (when (class-shape-ok? d)
    (when-let [ctor (widget-ctor d)]
      (when-let [{:keys [params drop-args key?]} (clean-params ctor)]
        (when-let [ms (regular-methods d)]
          (let [create (find-method d "createState")
                others (remove #(= "createState" (:name %)) ms)
                state-name (created-state-name create by-name)
                s (get by-name state-name)
                param-names (set (keep :name params))]
            (when (and create
                       (empty? others)
                       (not (find-method d "build"))
                       state-name
                       s
                       (= (:name d) (state-type-arg s)))
              (when-let [fields (widget-fields-ok d param-names)]
                (when-let [st (state-info s (set (map :name fields)))]
                  {:kind :stateful
                   :name (:name d)
                   :state-name (:name s)
                   :params (fill-param-types params fields)
                   :drop-args drop-args
                   :key? key?
                   :field-inits (vec (filter :init fields))
                   :methods (:methods st)
                   :build (:build st)
                   :context-name (:context-name st)
                   :state-fields (:fields st)
                   :managed (:managed st)
                   :vsync? (:vsync? st)})))))))))

(defn plan
  "Analyze a unit for idiomatic widget classes.

  Returns {:widgets {class-name info} :consumed #{state-class-name}} where
  `:widgets` maps a widget class to its emission info and `:consumed` names
  classes that must not be emitted separately (their State class)."
  [unit]
  (let [classes (filter #(= :class (:ir %)) (:decls unit))
        by-name (into {} (map (juxt :name identity)) classes)
        heritage (into {} (map (fn [d] [(:name d) (set (heritage-targets d))]))
                       classes)
        extended-by-other? (fn [n]
                             (boolean
                              (some (fn [d]
                                      (and (not= (:name d) n)
                                           (contains? (get heritage (:name d)) n)))
                                    classes)))
        referenced? (fn [n except]
                      (boolean
                       (some (fn [d]
                               (and (not (contains? except (:name d)))
                                    (references-name? d n)))
                             (:decls unit))))
        candidates
        (into {}
              (keep (fn [d]
                      (when-not (extended-by-other? (:name d))
                        (case (super-name d)
                          "StatelessWidget" (when-let [info (stateless-info d)]
                                              [(:name d) info])
                          "StatefulWidget"
                          (when-let [info (stateful-info d by-name)]
                            (let [sname (:state-name info)]
                              (when (and sname
                                         (not (extended-by-other? sname))
                                         (not (referenced? sname
                                                           #{(:name d) sname})))
                                [(:name d) info])))
                          nil))))
              classes)]
    {:widgets candidates
     :consumed (set (keep :state-name (vals candidates)))}))

;; ---------------------------------------------------------------------------
;; emission
;; ---------------------------------------------------------------------------

(defn- wrap-let [bindings form]
  (if (seq bindings) (list 'let bindings form) form))

(defn- wrap-letfn [fns form]
  (if (seq fns) (list 'letfn (vec fns) form) form))

(defn- wrap-widget
  "Add `directives` (:watch/:context) to an emitted widget body. When the
  body already is an `f/widget` threading form, the directives are merged
  into it instead of nesting a second widget."
  [ctx directives body]
  (let [w (symbol (str (or (:flutter-alias ctx) "f") "/widget"))]
    (cond
      (and (empty? directives) (= 1 (count body)))
      (first body)

      (and (= 1 (count body))
           (seq? (first body))
           (= w (first (first body))))
      (list* w (concat directives (rest (first body))))

      :else (list* w (concat directives body)))))

(defn- emit-method-fn [m ctx]
  (let [{:keys [params body async]} (core/emit-callable m ctx)]
    (with-meta (list* (symbol (:name m)) params body)
      (when async {:async true}))))

(defn emit-widget
  "Emission info from `plan` -> vector of top-level forms (one defn)."
  [{:keys [name params field-inits methods build context-name state-fields
           key? managed vsync?]}
   ctx]
  (let [param-names (set (keep :name params))
        method-names (set (map :name methods))
        init-field-names (set (map :name field-inits))
        managed-names (set (map :name managed))
        tick? (or (some uses-set-state? methods)
                  (uses-set-state? build))
        atom-fields (into {} (for [f state-fields]
                               [(:name f) (symbol (:name f))]))
        state-ctx (assoc ctx
                         :atom-fields atom-fields
                         :state-ctx true
                         :tick-atom (when tick? 'd2c$tick))
        locals (cond-> (into param-names (concat method-names init-field-names
                                                 managed-names))
                 context-name (conj context-name))
        init-ctx (assoc state-ctx :locals param-names)
        init-bindings (vec
                       (concat
                        (mapcat (fn [f]
                                  [(symbol (:name f))
                                   (core/emit-expr (:init f) init-ctx)])
                                field-inits)
                        (mapcat (fn [f]
                                  [(symbol (:name f))
                                   (list 'atom
                                         (core/emit-expr (:init f) init-ctx))])
                                state-fields)
                        (when tick?
                          ['d2c$tick (list 'atom 0)])))
        managed-ctx (assoc state-ctx :locals locals)
        method-ctx (assoc state-ctx :locals locals)
        method-fns (mapv #(emit-method-fn % method-ctx) methods)
        build-ctx (assoc state-ctx :locals locals)
        build-body (:body (core/emit-callable
                           {:ir :method :params [] :body (:body build)}
                           build-ctx))
        directives (vec
                    (concat
                     (when vsync? [:vsync 'd2c$vsync])
                     (when (seq managed)
                       [:managed (vec (mapcat
                                       (fn [m]
                                         [(symbol (:name m))
                                          (core/emit-expr (:init m) managed-ctx)
                                          :refresh-on :never
                                          :dispose (:dispose m)])
                                       managed))])
                     (when (or (seq state-fields) tick?)
                       [:watch (vec (concat (mapcat (fn [f] ['_ (symbol (:name f))])
                                                    state-fields)
                                            (when tick? ['_ 'd2c$tick])))])
                     (when context-name
                       [:context (symbol context-name)])))
        widget (wrap-let init-bindings
                         (wrap-letfn method-fns
                                     (wrap-widget ctx directives build-body)))
        body (if key?
               (list (alias/qualify "KeyedSubtree" (:aliases ctx))
                     '.key 'key '.child widget)
               widget)]
    [(list* 'defn (symbol name)
            (core/emit-params params {} state-ctx)
            [body])]))
