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
            (assoc (select-keys opts [:unit-info :class-name :type-vars])
                   :mode mode
                   :flutter-alias (alias/flutter-alias aliases)
                   :emit-body st/emit-body
                   :emit-stmt st/emit-stmt
                   :emit-switch-expr st/emit-switch-expr)))

(defn unit-aliases
  "Alias context for a unit's imports, honoring :default-alias and local
  type names."
  [unit {:keys [default-alias local-names] :as opts}]
  (let [imports (filter #(= :import (:ir %)) (:directives unit))]
    (alias/context imports
                   {:default-alias (if (contains? opts :default-alias)
                                     default-alias
                                     "m")
                    :local-names (set local-names)})))

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
              :mutable (not (or (:final vl) (:const vl)))
              :annotations (:annotations m)}]
    (for [v (:vars vl)]
      (merge base {:name (:name v) :init (:init v) :src (:src v)}))))

(defn- instance-field-specs [d]
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
    (vec (concat declared implicit))))

(defn- field-symbol [f ctx]
  (let [m (cond-> {}
            (:mutable f) (assoc :mutable true)
            (:type f) (assoc :tag (core/type-symbol (:type f) ctx)))]
    (with-meta (symbol (:name f)) m)))

(defn- class-static-names [d]
  (into {}
        (concat
         (for [m (:members d)
               :when (and (= :field-decl (:ir m)) (:static m))
               v (get-in m [:fields :vars])]
           [(:name v) (core/member-symbol (:name d) (:name v))])
         (for [m (:members d)
               :when (and (= :method (:ir m)) (:static m))]
           [(:name m) (core/member-symbol (:name d) (:name m))]))))

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
  bare static names rewritten, implicit-this members resolved."
  [ctx class-name members fields statics type-vars]
  (merge (assoc ctx
                :class-name class-name
                :fields (set (map :name fields))
                :scalar-fields (set (for [f fields
                                          :when (typeinfo/scalar-type? (:type f))]
                                      (:name f)))
                :static-names statics
                :type-vars (set type-vars)
                :this-sym 'this)
         (instance-member-sets {:members members})))

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

(defn- emit-method [m ctx {:keys [concrete-stubs?]}]
  (let [{:keys [params body]} (core/emit-callable
                               m (assoc ctx :params (:params m)))
        body (cond
               (:abstract m) (if concrete-stubs? [(abstract-stub)] [])
               (= :setter (:kind m)) (conj (vec body) nil)
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
      (let [{:keys [params body async]} (core/emit-callable ctor' ctx)
            this (symbol "this")
            all-fields (concat field-names
                               (when shape (map super-field-name (:keys shape))))
            field-bindings (vec (mapcat (fn [f]
                                          (let [sym (if (string? f) (symbol f) f)
                                                v (get super-values f
                                                       (get init-values f))]
                                            (if (= sym v) [] [sym v])))
                                        all-fields))
            ctor-call (list* (symbol class)
                             (map #(if (string? %) (symbol %) %) all-fields))
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
     (for [m (:with d)] (with-meta (core/type-ref m ctx) {:mixin true})))))

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
  (let [fields (instance-field-specs d)
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
        class-ctx (member-ctx ctx (:name d) (:members d) all-fields
                              (class-static-names d)
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
                                                        {:concrete-stubs? concrete-stubs?})))
        make-fns (mapcat #(emit-ctor d' % fields class-ctx) ctors)
        static-forms (emit-static-members d class-ctx)]
    (into [deftype-form] (concat make-fns static-forms))))

(defn- emit-mixin [d ctx]
  (let [fields (instance-field-specs d)]
    (when (seq fields)
      (core/fail! d "mixins with fields are not supported by ClojureDart"))
    (let [class-ctx (member-ctx ctx (:name d) (:members d) [] {}
                                (keep :name (:type-params d)))
          deftype-form (list* 'deftype (type-name-symbol (assoc d :mixin? true)) []
                              (concat [:type-only true]
                                      (for [i (:implements d)]
                                        (core/type-ref i class-ctx))
                                      [(symbol (:name d))]
                                      (emit-class-members d class-ctx {})))]
      [deftype-form])))

;; ---------------------------------------------------------------------------
;; enums
;; ---------------------------------------------------------------------------

(defn- named-type [name src]
  {:ir :named-type :prefix nil :name name :type-args [] :nullable false :src src})

(defn- ctor-arg-values
  "Bind a constructor's parameters to the constant's arguments."
  [ctor args ctx]
  (let [pos-args (vec (remove :name args))
        named (into {} (filter :name) args)
        param-value (fn [p i]
                      (if (= :named (:kind p))
                        (some-> (get named (:name p)) :expr (core/emit-expr ctx))
                        (some-> (nth pos-args i nil) :expr (core/emit-expr ctx))))]
    (vec (mapcat (fn [[i p]] [(symbol (:name p)) (param-value p i)])
                 (map-indexed vector (:params ctor))))))

(defn- emit-enum-constant [d c i class-ctx fields ctor]
  (let [init-values (if ctor (field-init-values ctor fields class-ctx) {})
        bindings (if ctor (ctor-arg-values ctor (:args c) class-ctx) [])
        values (into [(:name c) i]
                     (mapcat (fn [f]
                               [(or (get init-values (:name f))
                                    (when (and ctor (:this (some #(when (= (:name %)
                                                                            (:name f)) %)
                                                                (:params ctor))))
                                      (symbol (:name f)))
                                    nil)])
                             fields))]
    (with-annotations
     (list 'def (core/member-symbol (:name d) (:name c))
           (if (seq bindings)
             (list 'let bindings (list* (symbol (:name d)) values))
             (list* (symbol (:name d)) values)))
     c)))

(defn- emit-enum [d ctx]
  (let [class (:name d)
        fields (instance-field-specs d)
        name-field {:name "name" :type (named-type "String" (:src d))
                    :mutable false :init nil :annotations []}
        index-field {:name "index" :type (named-type "int" (:src d))
                     :mutable false :init nil :annotations []}
        all-fields (into [name-field index-field] fields)
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
        constants (map-indexed #(emit-enum-constant d %2 %1 class-ctx fields ctor)
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

(defn- unit-info [unit]
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
                       (:name d)))})

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
        local-names (set (keys (:classes info)))
        aliases (or aliases (unit-aliases unit (assoc opts :local-names local-names)))
        base-ctx (ctx-for aliases (assoc opts :unit-info info))
        ns-name (or ns (some-> (library-name unit) symbol) (path->ns path) 'main)
        requires (alias/require-clauses
                  (filter #(= :import (:ir %)) (:directives unit)))
        ;; cljd.flutter is a cljd namespace, not a Dart library: it must be
        ;; required as a symbol (string requires are resolved by the Dart
        ;; analyzer and would fail with "Can't find Dart lib").
        requires (if (and idiomatic?
                          (not-any? #(= "cljd.flutter" (str (first %))) requires))
                   (conj requires
                         [(symbol "cljd.flutter")
                          :as (symbol (alias/flutter-alias aliases))])
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
