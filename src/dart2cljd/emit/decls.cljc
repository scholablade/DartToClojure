(ns dart2cljd.emit.decls
  "IR declarations -> top-level cljd forms: ns/imports, defs, defns, classes.

  Class mapping (research items R3/R5/R7):

    * instance fields become `deftype` fields (`^:mutable` for Dart `var`),
      with declaration/initializer-list/field-formal initial values computed
      in the constructor make-fn;
    * every Dart constructor becomes a ns-level make-fn: `Foo-new` for the
      unnamed constructor, `Foo-named` for named ones, so call sites never
      depend on the field order of the implicit deftype constructor;
    * statics become ns-level `def`/`defn` named `Foo-member`;
    * getters/setters use `^:getter`/`^:setter`;
    * abstract classes without fields use `^:abstract`; abstract classes with
      fields are emitted concrete with throwing stubs (ClojureDart does not
      generate constructors for abstract types, so they could not be
      subclassed with field arguments);
    * enums become a deftype plus one ns-level `def` per constant and a
      `values` vector;
    * typedefs are expanded at use sites and emitted as comments;
    * annotations other than `@override` become `:dart/annotations` metadata
      (`@Deprecated` additionally gets `^:deprecated`).

  Unsupported Dart declarations (extensions, extension types, class aliases,
  mixins with fields, generators, `operator []=`) fail loudly with a source
  position."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dart2cljd.alias :as alias]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.statements :as st]
            [dart2cljd.idiomatic :as idiomatic]
            [dart2cljd.typeinfo :as typeinfo]))

(defn ctx-for
  "Build an emission context for a unit (options may carry :aliases)."
  [aliases {:keys [mode] :or {mode :faithful} :as opts}]
  (core/ctx aliases
            (assoc (select-keys opts [:unit-info :class-name :type-vars :mixins
                                      :fn-typedefs])
                   :mode mode
                   :flutter-alias (alias/flutter-alias aliases)
                   :emit-body st/emit-body
                   :emit-stmt st/emit-stmt
                   :emit-switch-expr st/emit-switch-expr)))

(defn unit-aliases
  "Alias context for a unit's imports, honoring :default-alias, local type
  names and (batch mode) :project-types / :project-values / :wildcard-alias /
  :resolver."
  [unit {:keys [default-alias local-names project-types project-values
                wildcard-alias resolver] :as opts}]
  (let [imports (filter #(= :import (:ir %)) (:directives unit))]
    (alias/context imports
                   {:default-alias (if (contains? opts :default-alias)
                                     default-alias
                                     "m")
                    :local-names (set local-names)
                    :project-types project-types
                    :project-values project-values
                    :wildcard-alias wildcard-alias
                    :resolver resolver})))

;; ---------------------------------------------------------------------------
;; annotations
;; ---------------------------------------------------------------------------

(defn- dart-expr-str
  "Best-effort deterministic Dart rendering of a constant expression, used
  only inside annotation comments/metadata."
  [node]
  (case (:ir node)
    :id (:name node)
    :prefixed-id (str (:prefix node) "." (:name node))
    :string-lit (pr-str (:value node))
    :int-lit (str (:value node))
    :double-lit (str (:value node))
    :bool-lit (str (:value node))
    :null-lit "null"
    :list-lit (str "[" (str/join ", " (map dart-expr-str (:elements node))) "]")
    :set-lit (str "{" (str/join ", " (map dart-expr-str (:elements node))) "}")
    :map-lit (str "{"
                  (str/join ", " (map (fn [e]
                                        (str (dart-expr-str (:key e)) ": "
                                             (dart-expr-str (:value e))))
                                      (:elements node)))
                  "}")
    :prop (str (dart-expr-str (:target node)) "." (:name node))
    :call (str (when-let [t (:target node)] (str (dart-expr-str t) "."))
               (:name node) "("
               (str/join ", " (map (fn [a]
                                     (str (when (:name a) (str (:name a) ": "))
                                          (dart-expr-str (:expr a))))
                                   (:args node)))
               ")")
    :new (str (get-in node [:ctor :type :name]) "("
              (str/join ", " (map (fn [a]
                                    (str (when (:name a) (str (:name a) ": "))
                                         (dart-expr-str (:expr a))))
                                  (:args node)))
              ")")
    (str node)))

(defn- annotation-str [ann]
  (str "@" (dart-expr-str (:name ann))
       (when (:ctor ann) (str "." (:ctor ann)))
       (when (seq (:args ann))
         (str "("
              (str/join ", " (map (fn [a]
                                    (str (when (:name a) (str (:name a) ": "))
                                         (dart-expr-str (:expr a))))
                                  (:args ann)))
              ")"))))

(defn- annotation-meta
  "Metadata map for a node's annotations: @override is dropped, @Deprecated
  becomes ^:deprecated, everything else is preserved verbatim under
  :dart/annotations."
  [node]
  (let [anns (remove #(= "override" (dart-expr-str (:name %))) (:annotations node))
        strs (mapv annotation-str anns)
        deprecated? (some #(= "Deprecated" (dart-expr-str (:name %))) anns)]
    (cond-> {}
      deprecated? (assoc :deprecated true)
      (seq strs) (assoc :dart/annotations strs))))

(defn- with-annotations [form node]
  (let [m (annotation-meta node)]
    (if (seq m) (vary-meta form merge m) form)))

(defn- with-async [sym async?]
  (if async? (vary-meta sym assoc :async true) sym))

;; ---------------------------------------------------------------------------
;; fields and class shape
;; ---------------------------------------------------------------------------

(defn- field-decl-specs [m]
  (let [vl (:fields m)
        base {:type (:type vl)
              :late (:late vl)
              ;; `late final` fields are assigned after construction (ctor
              ;; bodies, `initState`); ClojureDart has no late fields, so the
              ;; generated make-fn passes a placeholder and the assignment
              ;; needs `^:mutable` (Dart still enforces single assignment at
              ;; the source level because the Dart input compiled).
              :mutable (not (or (:const vl)
                                (and (:final vl) (not (:late vl)))))
              :annotations (:annotations m)}]
    (for [v (:vars vl)]
      (merge base {:name (:name v) :init (:init v) :src (:src v)}))))

(defn- instance-field-specs
  ([d] (instance-field-specs d []))
  ([d extra-fields]
   (let [declared (vec (mapcat field-decl-specs
                               (filter #(and (= :field-decl (:ir %))
                                             (not (:static %)))
                                       (:members d))))
         declared-names (set (map :name declared))
         implicit (for [m (:members d)
                        :when (= :ctor (:ir m))
                        p (:params m)
                        :when (and (:this p)
                                   (not (contains? declared-names (:name p))))]
                    {:name (:name p) :type (:type p) :mutable false
                     :init nil :annotations [] :src (:src p)})]
     (vec (concat declared implicit extra-fields)))))

(def ^:private scalar-late-defaults
  "Placeholder a make-fn passes for a `late` non-nullable scalar field (the
  value is assigned after construction: a constructor body, or a late
  initializer moved after it). Dart would throw LateInitializationError on a
  read before assignment; the generated code returns the type's zero value
  instead (documented divergence). Non-scalar late fields stay dynamic with a
  nil placeholder."
  {"int" 0 "num" 0 "double" 0.0 "bool" false "String" ""})

(defn- late-placeholder
  "Placeholder for a `late` field, or nil when the field must stay dynamic
  (non-nullable reference types have no value to fabricate)."
  [f]
  (when (and (:late f) (not (:nullable (:type f))))
    (get scalar-late-defaults (str (:name (:type f))))))

(defn- field-type-hint
  "cljd type hint for a field, or nil to emit it dynamic.

  A `late` field is assigned after the make-fn receives the field values
  (Dart assigns it later), so it gets a placeholder: non-nullable scalars keep
  their declared type plus a zero-value placeholder; other non-nullables would
  need nil, which a non-nullable hint rejects, so they stay dynamic; nullable
  types keep the `?` hint and the nil placeholder."
  [f ctx]
  (when (:type f)
    (if (and (:late f) (not (:nullable (:type f)))
             (nil? (late-placeholder f)))
      nil
      (core/type-symbol (:type f) ctx))))

(defn- field-symbol [f ctx]
  (let [tag (field-type-hint f ctx)
        m (cond-> {}
            (:mutable f) (assoc :mutable true)
            tag (assoc :tag tag))]
    (with-meta (symbol (:name f)) m)))

(defn- class-static-members
  "Static members of a class as `name -> {kind sym}`, where kind is one of
  :field :getter :method :setter. Fields/getters/methods use the generated
  `Class-member` name; setters the generated `set-member!` fn. Bare
  references inside the class must distinguish kinds (a getter read is a
  call, a setter write is a call, a field write is `set!`)."
  [d]
  (let [class (:name d)]
    (reduce (fn [m [name kind]]
              (update m name assoc kind
                      (if (= :setter kind)
                        (symbol (str "set-" name "!"))
                        (core/member-symbol class name))))
            {}
            (concat
             (for [m (:members d)
                   :when (and (= :field-decl (:ir m)) (:static m))
                   v (get-in m [:fields :vars])]
               [(:name v) :field])
             (for [m (:members d)
                   :when (and (= :method (:ir m)) (:static m))]
               [(:name m) (case (:kind m)
                            :getter :getter
                            :setter :setter
                            :method)])))))

(defn- instance-member-sets [d]
  (let [ms (filter #(and (= :method (:ir %)) (not (:static %))) (:members d))]
    {:instance-methods (set (for [m ms
                                  :when (not= :getter (:kind m))
                                  :when (not= :setter (:kind m))]
                              (:name m)))
     :instance-getters (set (for [m ms :when (= :getter (:kind m))] (:name m)))
     :instance-setters (set (for [m ms :when (= :setter (:kind m))] (:name m)))}))

(defn- member-ctx
  "Emission context for a class body: fields excluded from mutation analysis,
  bare static members rewritten, implicit-this members resolved."
  [ctx class-name members fields statics type-vars]
  (merge (assoc ctx
                :class-name class-name
                :fields (set (map :name fields))
                :scalar-fields (set (for [f fields
                                          :when (typeinfo/scalar-type? (:type f))]
                                      (:name f)))
                :static-members statics
                :type-vars (set type-vars)
                :this-sym 'this)
         (instance-member-sets {:members members})))

;; ---------------------------------------------------------------------------
;; project mixins (M8): inline members into appliers
;; ---------------------------------------------------------------------------

(defn- mixin-decl
  "IR declaration of an applied project mixin: declared in the current unit or
  (batch mode) in another converted file. Nil for external mixins, which keep
  the Dart `with` clause."
  [m ctx]
  (get (:mixins ctx) (str (:name m))))

(defn- ir-mentions?
  "True when an IR node contains a node satisfying `pred`. Walks maps and
  vectors, skipping :src and :annotations (positions and annotation bodies
  never affect the emitted member semantics)."
  [pred node]
  (cond
    (map? node) (or (and (not (contains? #{:src :annotations} (:ir node)))
                         (pred node))
                    (some (fn [[k v]]
                            (when-not (contains? #{:src :annotations} k)
                              (ir-mentions? pred v)))
                          node))
    (vector? node) (boolean (some #(ir-mentions? pred %) node))
    (seq? node) (boolean (some #(ir-mentions? pred (second %)) (partition 2 node)))
    :else false))

(defn- named-type? [name node]
  (and (map? node)
       (= :named-type (:ir node))
       (= name (str (:name node)))))

(defn- check-mixin-inlinable!
  "Fail loudly when a mixin cannot be represented by inlining or by the
  generated mixin declaration."
  [m]
  (let [tps (set (keep :name (:type-params m)))]
    (doseq [member (:members m)]
      (case (:ir member)
        (:field-decl :method) nil
        (core/fail! member (str "unsupported mixin member " (:ir member)))))
    ;; Type parameters are dropped: ClojureDart has no bounds support, and
    ;; `on Base<T>` / `implements I<T>` are emitted raw (type arguments are
    ;; erased). Members that actually use a type parameter cannot be
    ;; inlined, because the applier's context has no such binding.
    (when (seq tps)
      (doseq [member (:members m)
              :when (some #(ir-mentions? (partial named-type? %) member) tps)]
        (core/fail! member
                    (str "generic mixin " (:name m) " uses type parameter "
                         "in a member; ClojureDart cannot emit bounded type "
                         "parameters, so it cannot be inlined"))))
    (when (> (count (:on m)) 1)
      (core/fail! m "mixins with multiple `on` types are not supported"))
    (when (some :static (:members m))
      (core/fail! m "mixins with static members are not supported"))
    (doseq [member (:members m)
            :when (and (= :method (:ir member))
                       (ir-mentions? #(= :super (:ir %)) member))]
      (core/fail! member
                  (str "mixin " (:name m) " calls `super` in a method; "
                       "inlining cannot preserve mixin linearization")))))

(defn- mixin-field-specs
  "Inlined field specs of `mixins`, in declaration order."
  [mixins]
  (vec (for [m mixins
             member (:members m)
             :when (and (= :field-decl (:ir member)) (not (:static member)))
             f (field-decl-specs member)]
         f)))

(defn- mixin-methods
  "Concrete (non-abstract, non-static) method IR nodes of `mixins`, in
  declaration order. Abstract methods are handled by `implements`."
  [mixins]
  (vec (for [m mixins
             member (:members m)
             :when (and (= :method (:ir member))
                        (not (:static member))
                        (not (:abstract member)))]
         member)))

(defn- check-mixin-collisions!
  "Dart rejects a class member that collides with an applied mixin's member;
  inlining must too. Compare fields and methods by name (a Dart field also
  declares a getter/setter), including collisions between two mixins."
  [d own-fields mixins]
  (let [seen (volatile! (into (set (map :name own-fields))
                              (for [m (:members d)
                                    :when (and (= :method (:ir m))
                                               (not (:static m)))]
                                (:name m))))]
    (doseq [m mixins
            member (:members m)
            :when (not (:static member))
            :let [names (case (:ir member)
                          :field-decl (map :name (get-in member [:fields :vars]))
                          :method [(:name member)]
                          [])]
            name names]
      (when (contains? @seen name)
        (core/fail! member (str "member " name " collides with a member of the "
                                "applier or of another applied mixin")))
      (vswap! seen conj name))))

;; ---------------------------------------------------------------------------
;; method specs
;; ---------------------------------------------------------------------------

(def ^:private operator-symbols
  #{"+" "-" "*" "/" "%" "<" ">" "<=" ">=" "==" "&" "|" "<<" ">>" ">>>"})

(def ^:private operator-strings
  #{"^" "~" "~/" "[]"})

(defn- method-name
  "cljd method name for a Dart method/operator. Operators that Clojure cannot
  read as symbols are emitted as strings (ClojureDart supports string member
  names). `operator []=` is rejected: ClojureDart cannot emit its void return
  type for user types."
  [m]
  (let [n (:name m)]
    (if (:operator m)
      (cond
        (= n "[]=") (core/fail! m "operator []= is not supported by ClojureDart")
        (contains? operator-symbols n) (symbol n)
        (contains? operator-strings n) n
        :else (core/fail! m (str "unsupported operator " n)))
      (symbol n))))

(defn- method-symbol [m ctx]
  (let [n (method-name m)
        ret (when (and (:return-type m) (symbol? n))
              (core/type-symbol (:return-type m) ctx))]
    (cond-> (if (symbol? n) n (symbol n))
      (= :getter (:kind m)) (vary-meta assoc :getter true)
      (= :setter (:kind m)) (vary-meta assoc :setter true)
      ret (vary-meta assoc :tag ret))))

(defn- abstract-stub []
  (list 'throw (list 'UnsupportedError. "abstract method")))

(defn- void-return?
  "True when a method's Dart return type is `void` (setters included)."
  [m]
  (= "void" (str (:name (:return-type m)))))

(defn- emit-method [m ctx {:keys [concrete-stubs?]}]
  (let [{:keys [params body]} (core/emit-callable
                               m (assoc ctx :params (:params m)))
        body (cond
               (:abstract m) (if concrete-stubs? [(abstract-stub)] [])
               (= :setter (:kind m)) (conj (vec body) nil)
               ;; ClojureDart writes the last body form as `return <form>;`;
               ;; a Dart void method must not return a value (C4).
               (void-return? m) (conj (vec body) nil)
               :else body)]
    (with-annotations
     (list* (method-symbol m ctx) (into ['this] params) body)
     m)))

(defn- emit-static-member [m ctx]
  (let [class (:class-name ctx)
        sym (core/member-symbol class (:name m))]
    (case (:ir m)
      :field-decl
      (mapv (fn [v]
              (let [sym (core/member-symbol class (:name v))]
                (with-annotations
                 (list 'def
                       (if (get-in m [:fields :const])
                         (with-meta sym {:const true})
                         sym)
                       (when (:init v) (core/emit-expr (:init v) ctx)))
                 m)))
            (get-in m [:fields :vars]))

      :method
      (let [{:keys [params body async]} (core/emit-callable
                                         m (assoc ctx :params (:params m)))
            name-sym (with-async (if (= :setter (:kind m))
                                   (symbol (str "set-" (:name m) "!"))
                                   sym)
                                 async)]
        [(with-annotations (list* 'defn name-sym params body) m)])
      (core/fail! m (str "unsupported static member " (:ir m))))))

;; ---------------------------------------------------------------------------
;; constructors
;; ---------------------------------------------------------------------------

(defn- super-shape
  "Common super-constructor invocation shape across constructors:
  {:name <named-super-ctor-or-nil> :keys [[:pos i]|[:named n] ...]}.
  Returns nil when no constructor calls super."
  [ctors d]
  (let [shapes (keep (fn [c]
                       (some #(when (= :super-init (:ir %)) %) (:initializers c)))
                     ctors)
        shape-of (fn [si]
                   {:name (:name si)
                    :keys (vec (map-indexed
                                (fn [i a] (if-let [n (:name a)]
                                            [:named n]
                                            [:pos i]))
                                (:args si)))})]
    (when (seq shapes)
      (let [shapes (map shape-of shapes)
            first-shape (first shapes)]
        (when-not (every? #(= first-shape %) shapes)
          (core/fail! d "constructors must invoke the same super constructor with the same argument shape"))
        first-shape))))

(defn- super-field-name [k]
  (if (= :pos (first k))
    (symbol (str "d2c$super" (second k)))
    (symbol (str "d2c$super$" (second k)))))

(defn- super-init-value [ctor k ctx]
  (let [si (some #(when (= :super-init (:ir %)) %) (:initializers ctor))]
    (when si
      (let [args (:args si)
            arg (case (first k)
                  :pos (nth args (second k) nil)
                  :named (some #(when (= (second k) (:name %)) %) args))]
        (when arg (core/emit-expr (:expr arg) ctx))))))

(defn- field-init-values [ctor fields ctx]
  (let [by-decl (into {} (for [f fields
                               :when (:init f)]
                           [(:name f) (core/emit-expr (:init f) ctx)]))
        by-list (into {} (for [i (:initializers ctor)
                               :when (= :field-init (:ir i))]
                           [(:field i) (core/emit-expr (:expr i) ctx)]))
        by-param (into {} (for [p (:params ctor)
                                :when (:this p)]
                            [(:name p) (symbol (:name p))]))]
    (merge by-decl by-list by-param)))

(defn- ctor-defn-name
  "Ns-level make-fn name for a constructor, tagged with the class as return
  type when the class is not generic (so locals bound to constructor calls
  are statically typed, which makes Dart convert int literals for `double`
  fields)."
  [d cname async]
  (let [sym (core/ctor-symbol-for (:name d) cname)
        sym (if (seq (keep :name (:type-params d)))
              sym
              (vary-meta sym assoc :tag (symbol (:name d))))]
    (with-async sym async)))

(defn- fill-field-formal-types
  "Field-formal parameters (`this.x`) carry no type in the Dart AST; copy the
  declared field type onto them so the make-fn parameter gets its `^Type`
  hint."
  [ctor fields]
  (let [field-types (into {} (keep (fn [f] (when (:type f) [(:name f) (:type f)])))
                          fields)]
    (mapv (fn [p]
            (if (and (:this p) (nil? (:type p)) (contains? field-types (:name p)))
              (assoc p :type (get field-types (:name p)))
              p))
          (:params ctor))))

(defn- emit-ctor [d ctor fields ctx]
  (let [class (:name d)
        cname (:name ctor)
        params (fill-field-formal-types ctor fields)
        ctor' (assoc ctor :params params)
        ctx (update ctx :locals set/union (set (keep :name params)))
        shape (:super-shape d)
        super-values (when shape
                       (into {} (for [k (:keys shape)]
                                  [(super-field-name k)
                                   (super-init-value ctor k ctx)])))
        field-names (map :name fields)
        init-values (field-init-values ctor fields ctx)
        assert-inits (keep #(when (= :assert-init (:ir %)) %) (:initializers ctor))
        redirect (some #(when (= :redirect-init (:ir %)) %) (:initializers ctor))]
    (cond
      redirect
      (let [{:keys [params]} (core/emit-callable ctor' ctx)
            target (core/ctor-symbol-for class (:name redirect))]
        [(with-annotations
          (list 'defn (ctor-defn-name d cname nil) params
                (list* target (core/emit-args (:args redirect) ctx)))
          ctor)])

      (:factory ctor)
      (let [{:keys [params body async]} (core/emit-callable ctor' ctx)]
        [(with-annotations
          (list* 'defn (ctor-defn-name d cname async)
                 params body)
          ctor)])

      :else
      ;; The make-fn binds field values as locals before calling the
      ;; constructor; inside the Dart constructor body bare field names refer
      ;; to the fields, so reads/writes go through `this` (M8).
      ;;
      ;; `late` field initializers are the exception: Dart evaluates them
      ;; lazily and they may reference `this` (`vsync: this`), which does not
      ;; exist while the make-fn's let bindings are computed. They are
      ;; assigned through `this` right after construction, before the Dart
      ;; constructor body (eager instead of lazy; documented divergence).
      (let [{:keys [params body async]} (core/emit-callable ctor' (assoc ctx :ctor-body? true))
            this (symbol "this")
            all-fields (concat field-names
                               (when shape (map super-field-name (:keys shape))))
            body-ctx (assoc ctx :ctor-body? true)
            late-inits (for [f fields
                             :when (and (:late f) (:init f))]
                         {:name (:name f)
                          :expr (core/emit-expr (:init f) body-ctx)})
            late-init-names (set (map :name late-inits))
            init-values (apply dissoc init-values late-init-names)
            late-defaults (into {} (keep (fn [f]
                                           (when-let [v (late-placeholder f)]
                                             [(:name f) v])))
                                fields)
            field-bindings (vec (mapcat (fn [f]
                                          (let [sym (if (string? f) (symbol f) f)
                                                v (get super-values f
                                                       (get init-values f
                                                            (get late-defaults f)))]
                                            (if (= sym v) [] [sym v])))
                                        all-fields))
            ctor-call (list* (symbol class)
                             (map #(if (string? %) (symbol %) %) all-fields))
            late-assigns (map (fn [{:keys [name expr]}]
                                (list (symbol (str ".-" name "!")) this expr))
                              late-inits)
            body (if (and (empty? (remove nil? body))
                          (= :empty-body (:ir (:body ctor))))
                   []
                   body)]
        [(with-annotations
          (list 'defn (ctor-defn-name d cname async)
                params
                (list* 'let (into field-bindings [this ctor-call])
                       (concat (map #(list 'assert
                                           (core/emit-expr (:cond %) ctx)
                                           (when (:message %)
                                             (core/emit-expr (:message %) ctx)))
                                    assert-inits)
                               body
                               late-assigns
                               [this])))
          ctor)]))))

;; ---------------------------------------------------------------------------
;; classes
;; ---------------------------------------------------------------------------

(defn- type-params-meta [d]
  (when-let [tps (seq (keep :name (:type-params d)))]
    {:type-params (vec (map symbol tps))}))

(defn- type-name-symbol [d]
  (let [sym (symbol (:name d))
        m (cond-> (type-params-meta d)
            (:abstract d) (assoc :abstract true)
            (:mixin? d) (assoc :mixin true))]
    (if (seq m) (with-meta sym m) sym)))

(defn- heritage-forms [d ctx]
  (let [shape (:super-shape d)
        super (when-let [e (:extends d)]
                (let [sym (core/type-ref e ctx)]
                  (if (seq (:keys shape))
                    (let [arg-forms (mapcat (fn [k]
                                              (if (= :named (first k))
                                                [(symbol (str "." (second k)))
                                                 (super-field-name k)]
                                                [(super-field-name k)]))
                                            (:keys shape))]
                      (if-let [m (:name shape)]
                        (list* '. sym (symbol m) arg-forms)
                        (list* sym arg-forms)))
                    sym)))]
    (concat
     (when super [:extends super])
     (for [i (:implements d)] (core/type-ref i ctx))
     ;; Project mixins (declaration known to the converter): their members
     ;; are inlined into the class, the type becomes an `implements` clause
     ;; (keeps `is` checks and interface satisfaction). Type arguments are
     ;; dropped because generated mixins are never generic (M8).
     (for [m (:with d) :when (mixin-decl m ctx)]
       (core/type-symbol m ctx))
     (for [m (:with d) :when (not (mixin-decl m ctx))]
       (with-meta (core/type-ref m ctx) {:mixin true})))))

(defn- emit-class-members [d ctx opts]
  (mapcat
   (fn [m]
     (case (:ir m)
       :field-decl (if (:static m) [] [])
       :method (if (:static m)
                 []
                 [(emit-method m ctx opts)])
       :ctor []
       (core/fail! m (str "unsupported class member " (:ir m)))))
   (:members d)))

(defn- emit-static-members [d ctx]
  (mapcat (fn [m]
            (case (:ir m)
              :field-decl (if (:static m) (emit-static-member m ctx) [])
              :method (if (:static m) (emit-static-member m ctx) [])
              []))
          (:members d)))

(defn- emit-class [d ctx]
  (when (:native d)
    (core/fail! d "native classes are not supported"))
  (let [mixins (vec (keep #(mixin-decl % ctx) (:with d)))
        _ (doseq [m mixins] (check-mixin-inlinable! m))
        mfields (mixin-field-specs mixins)
        mmethods (mixin-methods mixins)
        fields (instance-field-specs d mfields)
        _ (check-mixin-collisions! d (instance-field-specs d) mixins)
        ctors (filter #(= :ctor (:ir %)) (:members d))
        ctors (if (and (empty? ctors) (not (:abstract d)))
                [{:ir :ctor :name nil :params [] :initializers []
                  :body {:ir :empty-body :src (:src d)} :src (:src d)}]
                ctors)
        shape (super-shape ctors d)
        d' (assoc d :super-shape shape)
        super-fields (mapv (fn [k]
                             {:name (name (super-field-name k))
                              :type nil :mutable false :init nil
                              :annotations [] :src (:src d)})
                           (:keys shape))
        all-fields (into fields super-fields)
        concrete-stubs? (and (:abstract d) (seq fields))
        class-ctx (member-ctx ctx (:name d) (concat (:members d) mmethods) all-fields
                              (class-static-members d)
                              (keep :name (:type-params d)))
        type-sym (type-name-symbol d)
        type-sym (if concrete-stubs?
                   (vary-meta type-sym dissoc :abstract)
                   type-sym)
        deftype-form (list* 'deftype type-sym (mapv #(field-symbol % class-ctx) all-fields)
                            (concat [:type-only true]
                                    (heritage-forms d' class-ctx)
                                    [(symbol (:name d))]
                                    (emit-class-members d class-ctx
                                                        {:concrete-stubs? concrete-stubs?})
                                    (map #(emit-method % class-ctx {:concrete-stubs? false})
                                         mmethods)))
        make-fns (mapcat #(emit-ctor d' % fields class-ctx) ctors)
        static-forms (emit-static-members d class-ctx)]
    (into [deftype-form] (concat make-fns static-forms))))

(defn- emit-mixin [d ctx]
  (check-mixin-inlinable! d)
  (let [fields (instance-field-specs d)
        class-ctx (member-ctx ctx (:name d) (:members d) fields {}
                              ;; type parameters are dropped: ClojureDart has no
                              ;; bounds, and `check-mixin-inlinable!` rejects
                              ;; members that reference them.
                              [])
        on (:on d)
        extends (when-let [b (first on)] (core/type-symbol b class-ctx))
        deftype-form (list* 'deftype
                            (type-name-symbol (assoc d :mixin? true :type-params []))
                            (mapv #(field-symbol % class-ctx) fields)
                            (concat
                             [:type-only true]
                             (when extends [:extends extends])
                             (for [i (:implements d)]
                               (core/type-symbol i class-ctx))
                             [(symbol (:name d))]
                             (emit-class-members d class-ctx {})))]
    [deftype-form]))

;; ---------------------------------------------------------------------------
;; enums
;; ---------------------------------------------------------------------------

(defn- named-type [name src]
  {:ir :named-type :prefix nil :name name :type-args [] :nullable false :src src})

(defn- ctor-arg-values
  "Bind a constructor's parameters to the constant's arguments."
  [ctor args ctx]
  (let [pos-args (vec (remove :name args))
        named (into {} (keep (fn [a]
                               (when-let [n (:name a)] [n a]))
                             args))
        param-value (fn [p i]
                      (if (= :named (:kind p))
                        (some-> (get named (:name p)) :expr (core/emit-expr ctx))
                        (some-> (nth pos-args i nil) :expr (core/emit-expr ctx))))]
    (vec (mapcat (fn [[i p]] [(symbol (:name p)) (param-value p i)])
                 (map-indexed vector (:params ctor))))))

(defn- emit-enum-constant [d c i class-ctx fields ctor implicit]
  (let [init-values (if ctor (field-init-values ctor fields class-ctx) {})
        bindings (if ctor (ctor-arg-values ctor (:args c) class-ctx) [])
        field-values (mapcat (fn [f]
                               [(or (get init-values (:name f))
                                    (when (and ctor (:this (some #(when (= (:name %)
                                                                            (:name f)) %)
                                                                (:params ctor))))
                                      (symbol (:name f)))
                                    nil)])
                             fields)
        implicit-values (mapcat (fn [f]
                                  (case (:name f)
                                    "name" [(:name c)]
                                    "index" [i]
                                    []))
                                implicit)
        values (into (vec implicit-values) field-values)]
    (with-annotations
     (list 'def (core/member-symbol (:name d) (:name c))
           (if (seq bindings)
             (list 'let bindings (list* (symbol (:name d)) values))
             (list* (symbol (:name d)) values)))
     c)))

(defn- emit-enum [d ctx]
  (let [class (:name d)
        fields (instance-field-specs d)
        declared (set (map :name fields))
        ;; Dart enums have implicit `name`/`index`; a declared field with the
        ;; same name overrides it (a user-declared field), so the
        ;; synthesized field is only added when not shadowed.
        implicit (cond-> []
                   (not (contains? declared "name"))
                   (conj {:name "name" :type (named-type "String" (:src d))
                          :mutable false :init nil :annotations []})
                   (not (contains? declared "index"))
                   (conj {:name "index" :type (named-type "int" (:src d))
                          :mutable false :init nil :annotations []}))
        all-fields (into implicit fields)
        class-ctx (member-ctx ctx class (:members d) fields {}
                              (keep :name (:type-params d)))
        ctor (some #(when (and (= :ctor (:ir %)) (nil? (:name %))) %) (:members d))
        deftype-form (list* 'deftype (type-name-symbol d)
                            (mapv #(field-symbol % class-ctx) all-fields)
                            (concat [:type-only true]
                                    (for [i (:implements d)]
                                      (core/type-ref i class-ctx))
                                    [(symbol class)]
                                    (emit-class-members d class-ctx {})))
        constants (map-indexed #(emit-enum-constant d %2 %1 class-ctx fields ctor implicit)
                               (:constants d))]
    (into [deftype-form]
          (concat constants
                  [(list 'def (symbol (str class "-values"))
                         (mapv #(core/member-symbol class (:name %)) (:constants d)))]))))

;; ---------------------------------------------------------------------------
;; top-level declarations
;; ---------------------------------------------------------------------------

(defn- library-name [unit]
  (some #(when (= :library (:ir %)) (:name %)) (:directives unit)))

(defn- path->ns [path]
  (when path
    (let [base (-> path (str/split #"/") last (str/replace #"\.dart$" ""))]
      (when (seq base) (symbol base)))))

(defn- emit-top-var [d ctx]
  (let [vl (:vars d)
        const? (:const vl)]
    (mapv (fn [v]
            (with-annotations
             (list 'def (if const?
                          (with-meta (symbol (:name v)) {:const true})
                          (symbol (:name v)))
                   (when (:init v) (core/emit-expr (:init v) ctx)))
             d))
          (:vars vl))))

(defn- emit-top-fn [d ctx]
  (let [{:keys [params body async]} (core/emit-callable d (assoc ctx :params (:params d)))]
    (case (:kind d)
      :fn
      [(with-annotations
        (list* 'defn (with-async (symbol (:name d)) async) params body) d)]

      :getter
      [(with-annotations
        (list* 'defn (with-async (symbol (:name d)) async) [] body) d)]

      :setter
      (let [v (symbol (or (:name (first (:params d))) "v"))]
        [(with-annotations
          (list* 'defn (symbol (str "set-" (:name d) "!")) [v] body) d)])

      (core/fail! d (str "unsupported top-level function kind " (:kind d))))))

(defn emit-decl
  "Emit one top-level declaration as a vector of forms."
  [d ctx]
  (case (:ir d)
    :fn-decl (emit-top-fn d ctx)
    :top-var-decl (emit-top-var d ctx)
    :class (emit-class d ctx)
    :mixin (emit-mixin d ctx)
    :enum (emit-enum d ctx)
    :type-alias
    [(list 'comment (str "typedef " (:name d) " (expanded at use sites)"))]
    :class-alias
    (core/fail! d "class aliases (typedef C = Base with M) are not supported")
    :extension
    (core/fail! d "Dart extensions are not supported by ClojureDart")
    :extension-type
    (core/fail! d "extension types are not supported by ClojureDart")
    (core/fail! d (str "unsupported top-level declaration " (:ir d)))))

;; ---------------------------------------------------------------------------
;; unit info
;; ---------------------------------------------------------------------------

(defn- class-unit-info [d]
  (if (= :enum (:ir d))
    {:ctors #{}
     :static-fields (into #{"values"}
                          (map :name (:constants d)))
     :static-getters #{}
     :static-setters #{}
     :static-methods #{}}
    {:ctors (let [ctors (filter #(= :ctor (:ir %)) (:members d))]
              (if (seq ctors)
                (set (map :name ctors))
                (when-not (:abstract d) #{nil})))
     :static-fields (set (for [m (:members d)
                               :when (and (= :field-decl (:ir m))
                                          (:static m))
                               v (get-in m [:fields :vars])]
                           (:name v)))
     :static-getters (set (for [m (:members d)
                                :when (and (= :method (:ir m))
                                           (:static m)
                                           (= :getter (:kind m)))]
                            (:name m)))
     :static-setters (set (for [m (:members d)
                                :when (and (= :method (:ir m))
                                           (:static m)
                                           (= :setter (:kind m)))]
                            (:name m)))
     :static-methods (set (for [m (:members d)
                                :when (and (= :method (:ir m))
                                           (:static m)
                                           (= :method (:kind m)))]
                            (:name m)))}))

(defn- own-operators [d]
  (for [m (:members d)
        :when (and (= :method (:ir m)) (:operator m))]
    [(str (:name m)) (count (:params m))]))

(defn- operator-classes
  "Map [op arity] -> vector of unit class names declaring (or inheriting)
  that operator, in declaration order. Used for runtime `dart/is?` dispatch
  when an operator receiver's type is not statically known."
  [unit]
  (let [classes (filter #(contains? #{:class :enum} (:ir %)) (:decls unit))
        by-name (into {} (map (juxt :name identity)) classes)
        with-inherited (fn with-inherited
                         ([d] (with-inherited d #{}))
                         ([d seen]
                          (when-not (contains? seen (:name d))
                            (let [sup (some-> d :extends :name str (get by-name))]
                              (concat (own-operators d)
                                      (when sup
                                        (with-inherited sup (conj seen (:name d)))))))))]
    (reduce (fn [acc d]
              (reduce (fn [acc [op arity]]
                        (update acc [op arity] (fnil conj []) (:name d)))
                      acc
                      (distinct (with-inherited d))))
            {}
            classes)))

(defn unit-info
  "Whole-unit resolution table (classes/statics/typedefs/top-level names).
  Public for batch mode, which builds the cross-file project index from it." [unit]
  {:classes (into {}
                  (for [d (:decls unit)
                        :when (contains? #{:class :enum} (:ir d))]
                    [(:name d) (class-unit-info d)]))
   :operator-classes (operator-classes unit)
   :typedefs (into {}
                   (for [d (:decls unit)
                         :when (= :type-alias (:ir d))]
                     [(:name d) (:type d)]))
   :top-fns (set (for [d (:decls unit)
                       :when (and (= :fn-decl (:ir d)) (= :fn (:kind d)))]
                   (:name d)))
   :top-getters (set (for [d (:decls unit)
                           :when (and (= :fn-decl (:ir d)) (= :getter (:kind d)))]
                       (:name d)))
   :top-setters (set (for [d (:decls unit)
                           :when (and (= :fn-decl (:ir d)) (= :setter (:kind d)))]
                       (:name d)))
   :top-vars (into {}
                   (for [d (:decls unit)
                         :when (= :top-var-decl (:ir d))
                         v (get-in d [:vars :vars])]
                     [(:name v) {:const (boolean (get-in d [:vars :const]))
                                 :final (boolean (get-in d [:vars :final]))}]))})

(defn emit-unit
  "Normalized :unit IR -> vector of top-level forms, ns first.

  In :idiomatic mode, recognized Flutter widget classes are emitted as
  builder fns and `cljd.flutter` is added to the requires."
  [unit {:keys [ns path aliases mode] :as opts}]
  (let [info (unit-info unit)
        idiomatic? (= :idiomatic mode)
        plan (if idiomatic?
               (idiomatic/plan unit)
               {:widgets {} :consumed #{}})
        info (assoc info :widget-fns (:widgets plan))
        local-mixins (into {}
                           (for [d (:decls unit) :when (= :mixin (:ir d))]
                             [(:name d) d]))
        local-names (into (set (keys (:classes info))) (keys local-mixins))
        ;; project mixins (batch mode) come from the index; same-unit
        ;; declarations win (identical content).
        mixins (merge (:project-mixins opts) local-mixins)
        aliases (or aliases (unit-aliases unit (assoc opts :local-names local-names)))
        base-ctx (ctx-for aliases (assoc opts :unit-info info :mixins mixins))
        ns-name (or ns (some-> (library-name unit) symbol) (path->ns path) 'main)
        requires (alias/require-clauses
                  (filter #(= :import (:ir %)) (:directives unit))
                  (:resolver opts)
                  (select-keys opts [:exports-uri :wildcard-alias]))
        ;; cljd.flutter is a cljd namespace, not a Dart library: it must be
        ;; required as a symbol (string requires are resolved by the Dart
        ;; analyzer and would fail with "Can't find Dart lib").
        requires (if (and idiomatic?
                          (not-any? #(= "cljd.flutter" (str (first %))) requires))
                   (conj requires
                         [(symbol "cljd.flutter")
                          :as (symbol (alias/flutter-alias aliases))])
                   requires)
        ;; A barrel file (external `export`s) needs the exports shim itself;
        ;; for importers the shim is added by `require-clauses` through the
        ;; re-exporting import.
        requires (if (and (:wildcard-alias opts)
                          (:exports-uri opts)
                          (not-any? #(= (:exports-uri opts) (str (first %)))
                                    requires))
                   (conj requires
                         [(:exports-uri opts)
                          :as (symbol (:wildcard-alias opts))])
                   requires)
        ns-form (list* 'ns ns-name
                       (when (seq requires)
                         [(list* :require requires)]))]
    (into [ns-form]
          (mapcat (fn [d]
                    (if-let [w (get-in plan [:widgets (:name d)])]
                      (idiomatic/emit-widget w base-ctx)
                      (when-not (contains? (:consumed plan) (:name d))
                        (emit-decl d base-ctx))))
                  (:decls unit)))))
