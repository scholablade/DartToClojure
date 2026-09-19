(ns dart2cljd.emit.core
  "IR expressions -> cljd forms (faithful mode).

  Emission is a pure function of (IR, ctx). `ctx` carries:
    :aliases     an alias context from dart2cljd.alias
    :mode        :faithful (default) or :idiomatic
    :emit-body   fn [stmts ctx] -> vector of forms (injected by the facade to
                 avoid a cyclic dependency with dart2cljd.emit.statements)
    :emit-stmt   fn [stmt ctx] -> form
    :mutable     set of local names that are reassigned (R1); reads emit
                 `(deref x)`, writes `vswap!`/`reset!`
    :captured    subset of :mutable referenced from nested functions (boxed
                 with `atom` instead of `volatile!`)
    :fields      instance field names in scope (never treated as mutable
                 locals, never dereferenced)
    :static-members map of bare class-static member name -> {kind symbol},
                 for method bodies that reference statics without qualification
                 (kinds: :field :getter :method :setter)
    :class-name  current class (for bare static references in methods)
    :unit-info   whole-unit class/typedef/getter table built by emit.decls
    :type-vars   type parameters in scope (kept bare in type positions)
    :local-types {:classes map of local name -> unit class name,
                  :scalars set of names known to be Dart scalars}, from
                 dart2cljd.typeinfo (unit-info :classes feeds it)
    :mixins      project mixin declarations by name (same unit and, in batch
                 mode, cross-file). Appliers inline their members and use
                 `implements` instead of Dart `with` (M8)

  Anything not representable fails loudly via `fail!` with the source
  position; no placeholder markers are emitted."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dart2cljd.alias :as alias]
            [dart2cljd.form :as form]
            [dart2cljd.mutable :as mutable]
            [dart2cljd.typeinfo :as typeinfo]))

(declare emit-expr emit-increment type-symbol assign-slot)

(defn fail!
  "Raise a loud, positioned emission error."
  [node msg]
  (throw (ex-info (str "dart2cljd: " msg
                       (when-let [{:keys [line col]} (:src node)]
                         (str " at " line ":" col)))
                  {:dart2cljd/kind :emit :node node})))

(defn ctx
  "Build an emission context."
  [aliases opts]
  (merge {:aliases aliases
          :mode :faithful
          :mutable #{}
          :captured #{}
          :fields #{}
          :locals #{}
          :local-types {}
          :type-vars #{}}
         (select-keys opts [:mode :emit-body :emit-stmt :emit-switch-expr
                            :mutable :captured :fields :locals :static-members
                            :class-name :unit-info :type-vars :this-sym
                            :local-types :instance-methods :instance-getters
                            :instance-setters :atom-fields :state-ctx
                            :flutter-alias :thread-props :mixins
                            :fn-typedefs])))

;; ---------------------------------------------------------------------------
;; unit info helpers (local classes, statics, typedefs)
;; ---------------------------------------------------------------------------

(defn class-info
  "Info map for a class declared in the current unit, or nil."
  [name ctx]
  (get-in ctx [:unit-info :classes (str name)]))

(defn local-class? [name ctx]
  (some? (class-info name ctx)))

(defn typedef-type
  "Target type node of a typedef declared in the current unit, or nil."
  [name ctx]
  (get-in ctx [:unit-info :typedefs (str name)]))

(defn member-symbol
  "Deterministic ns-level symbol for a static member / named constructor:
  `Foo.named` -> `Foo-named` (munged to `Foo_named` in Dart)."
  [class member]
  (symbol (str class "-" member)))

(defn ctor-symbol-for
  "Ns-level make-fn symbol for a constructor: unnamed -> `Foo-new`."
  [class ctor-name]
  (member-symbol class (or ctor-name "new")))

(defn- known-ctor? [class ctor-name ctx]
  (contains? (:ctors (class-info class ctx)) ctor-name))

(defn- static-member-kind
  "One of :field :getter :setter :method :ctor, or nil."
  [class member ctx]
  (let [info (class-info class ctx)]
    (cond
      (nil? info) nil
      (contains? (:ctors info) member) :ctor
      (contains? (:static-fields info) member) :field
      (contains? (:static-getters info) member) :getter
      (contains? (:static-setters info) member) :setter
      (contains? (:static-methods info) member) :method
      :else nil)))

;; ---------------------------------------------------------------------------
;; batch-mode project index (types declared in other files, M6)
;; ---------------------------------------------------------------------------

(defn- project-type-info
  "Batch-mode index info for a type declared in another file of the same
  project, or nil. Index built by dart2cljd.batch."
  [name ctx]
  (get (get-in ctx [:aliases :project-types]) (str name)))

(defn- project-value-info
  "Batch-mode index info for a top-level value declared in another file of
  the same project, or nil."
  [name ctx]
  (get (get-in ctx [:aliases :project-values]) (str name)))

(defn- project-value-symbol [info name]
  (symbol (str (:ns info) "/" name)))

(defn- own-instance-member?
  "True when the current class declares an instance member with this name.
  Dart scoping gives instance members priority over cross-file top-level
  values; members inherited from outside the unit are not visible to this
  syntactic pass (documented approximation)."
  [name ctx]
  (let [n (str name)]
    (or (contains? (:fields ctx) n)
        (contains? (:instance-methods ctx) n)
        (contains? (:instance-getters ctx) n)
        (contains? (:instance-setters ctx) n))))

(defn- static-member-info
  "Kind map `{kind symbol}` for a bare static member of the current class, or
  nil."
  [name ctx]
  (get (:static-members ctx) (str name)))

(defn- project-ctor-symbol
  "Ns-level make-fn symbol for a constructor of a project type, or nil when
  the index does not know that constructor."
  [info class ctor-name]
  (when (contains? (:ctors info) ctor-name)
    (symbol (str (:ns info) "/" (member-symbol class (or ctor-name "new"))))))

(defn- drop-args
  "Remove named arguments whose names are in `drops`."
  [drops args]
  (if (seq drops)
    (vec (remove #(and (:name %) (contains? drops (:name %))) args))
    args))

;; ---------------------------------------------------------------------------
;; types, constructors, arguments
;; ---------------------------------------------------------------------------

(def ^:private bare-types
  #{"dynamic" "void" "Object" "Never" "Null"})

(defn type-ref
  "Type annotation for a heritage clause (extends/implements/with): like
  `type-symbol` but preserves type arguments as `:type-params` metadata so
  ClojureDart can emit `Comparable<Circle>` instead of a raw type."
  [node ctx]
  (let [sym (type-symbol node ctx)]
    (if-let [args (seq (:type-args node))]
      (with-meta sym {:type-params (mapv #(type-symbol % ctx) args)})
      sym)))

(def ^:private never-nullable-types
  "Dart types that must not carry the `?` nullability suffix."
  #{"dynamic" "void" "Never" "Null"})

(defn type-symbol
  "IR type annotation -> cljd type symbol.

  Typedefs declared in the current unit are expanded at use sites; type
  parameters in scope stay bare; type arguments are dropped (ClojureDart
  resolves Dart generics from the declaration site). Nullable Dart types get
  the ClojureDart `?` suffix (`^String?`), so nil defaults and nulls keep
  type-checking like the Dart original.

  External *function* typedefs (`VoidCallback`, `ValueChanged<T>`, ...) map to
  `Function`: ClojureDart's analyzer helper crashes on function-typedef
  elements, and parse-only mode cannot tell them from classes, so batch mode
  resolves the names first (`:fn-typedefs`, RESEARCH M8.10)."
  [node ctx]
  (when node
    (let [sym (case (:ir node)
                :named-type
                (let [{:keys [prefix name]} node
                      name (str name)
                      ptype (project-type-info name ctx)
                      fn-typedef? (contains? (:fn-typedefs ctx) name)]
                  (cond
                    prefix (if fn-typedef? 'Function
                               (symbol (str prefix "/" name)))
                    (contains? (:type-vars ctx) name) (symbol name)
                    (typedef-type name ctx) (type-symbol (typedef-type name ctx) ctx)
                    (:typedef-target ptype) (:typedef-target ptype)
                    (= :typedef (:kind ptype))
                    (fail! node (str "cannot expand typedef " name " (defined in "
                                     (:file ptype) "); use a concrete type instead"))
                    (contains? bare-types name) (symbol name)
                    fn-typedef? 'Function
                    :else (alias/qualify name (:aliases ctx))))

                :fn-type 'Function
                :record-type 'Record
                (fail! node (str "unsupported type annotation " (:ir node))))]
      (if (and (:nullable node) (not (contains? never-nullable-types (str sym))))
        (symbol (str sym "?"))
        sym))))

(defn- ctor-symbol [ctor ctx]
  (let [t (:type ctor)
        ;; `const Foo.named(...)` parses in parse-only mode as a NamedType
        ;; whose :prefix is the class and whose :name is the constructor,
        ;; e.g. {:prefix "EdgeInsets" :name "all"}. Library prefixes are
        ;; known to the alias table, so they are not mistaken for classes.
        qualified-parse? (and (nil? (:name ctor))
                              (:prefix t)
                              (not (alias/import-prefix? (:prefix t) (:aliases ctx)))
                              (alias/class-name? (:prefix t)))
        base-name (if qualified-parse? (:prefix t) (:name t))
        info (when base-name (project-type-info base-name ctx))
        n (or (:name ctor) (when qualified-parse? (:name t)))]
    (cond
      (and info (:widget? info) (nil? n))
      (symbol (str (:ns info) "/" base-name))

      (and info (:widget? info))
      (fail! ctor (str base-name " is translated as a widget builder fn; "
                       "constructor " n " is not available in idiomatic mode"))

      info
      (or (project-ctor-symbol info base-name n)
          (fail! ctor (str base-name " has no constructor " (or n "new"))))

      :else
      (let [base (if qualified-parse?
                   (alias/qualify (:prefix t) (:aliases ctx))
                   (type-symbol t ctx))]
        (if n
          (symbol (str base "." n))
          base)))))

(defn- void-callback-literal?
  "True for an anonymous zero-parameter fn. A closure *literal* whose body
  returns a value is rejected in a Dart `void Function()` context
  (`setState`, listeners, timers: `return_of_invalid_type_from_closure`); an
  indirect reference is accepted."
  [node]
  (and (= :fn (:ir node)) (empty? (:params node))))

(defn- lift-closure-arg
  "Wrap a zero-parameter closure *positional* argument in a `let` so it is
  not a Dart closure literal in the argument position; ClojureDart hoists the
  binding into a local function declaration, which Dart accepts in void
  contexts. Named arguments are left alone: they are usually typed callbacks
  (`onRefresh:`, `onPressed:`), where the hoisted local's `dynamic Function()`
  type is not assignable to the parameter type (RESEARCH M8.18)."
  [expr node named? idx]
  (if (and (not named?) (void-callback-literal? node))
    (let [sym (symbol (str "d2c$cb" idx))]
      (list 'let [sym expr] sym))
    expr))

(defn emit-args
  "Flatten IR arguments into a cljd argument sequence: positional values
  interleaved with `.name value` pairs for named arguments.

  Zero-parameter closure positional arguments are lifted out of the argument
  position (see `lift-closure-arg`)."
  [args ctx]
  (mapcat (fn [idx a]
            (let [node (:expr a)
                  e (lift-closure-arg (emit-expr node ctx) node (some? (:name a)) idx)]
              (if-let [n (:name a)]
                [(symbol (str "." n)) e]
                [e])))
          (range)
          args))

;; ---------------------------------------------------------------------------
;; parameters
;; ---------------------------------------------------------------------------

(def ^:private cljd-reserved-locals
  "cljd names the emitter emits as syntax or as bare core calls. A Dart local
  with one of these names shadows the emitted form — ClojureDart resolves
  locals before macros/special forms, so `(when …)` under a local `when`
  compiles as a call on the local (probe: RESEARCH M8.16) — therefore such
  locals are renamed."
  #{"let" "when" "and" "or" "not" "deref" "do" "defn" "recur" "fn" "def"
    "cond" "into" "str" "quot" "mod" "count" "concat" "vec" "first" "drop"
    "doto" "doseq" "case" "loop" "inc" "dec" "get" "next" "seq" "merge"
    "mapcat" "atom" "assert" "ns" "deftype" "comment" "letfn"})

(defn local-renames
  "`name -> name$local` for bound names that collide with a cljd name the
  emitter emits; see `cljd-reserved-locals`."
  [names]
  (into {}
        (for [n names
              :let [s (str n)]
              :when (contains? cljd-reserved-locals s)]
          [s (str s "$local")])))

(defn local-symbol
  "Binding symbol for local `name`: renamed when `ctx` carries a rename for it
  (readers use the same helper, so binding and references stay in sync)."
  [name ctx]
  (symbol (get (:local-renames ctx) (str name) (str name))))

(def ^:private scalar-type-names
  #{"int" "double" "num" "bool" "String"})

(defn- strip-paren [node]
  (if (= :paren (:ir node)) (recur (:expr node)) node))

(defn- literal-tag
  "cljd tag symbol for a scalar literal expression, or nil."
  [node]
  (let [n (strip-paren node)]
    (case (:ir n)
      :int-lit 'int
      :double-lit 'double
      :string-lit 'String
      :bool-lit 'bool
      :prefix (when (= "-" (:op n)) (literal-tag (:operand n)))
      nil)))

(defn loop-var-symbol
  "Binding symbol for a C-style loop variable.

  ClojureDart types `loop` locals from the *binding symbol's* tag, not from the
  initializer (compiler `emit-loop*`), so an unhinted `[i 0]` becomes
  `dc.dynamic i$1 = 0` and later numeric interop inserts `(i as num)` casts —
  which promote `i` to `num` in Dart and break int member reads (`i.isEven`).
  Scalar-typed or scalar-literal-initialized loop variables get a `^Type`
  hint, matching Dart's inference for `var i = 0`."
  [v ctx]
  (let [s (local-symbol (:name v) ctx)
        ty (:type v)]
    (cond
      (and (map? ty) (= :named-type (:ir ty)) (nil? (:prefix ty))
           (not (:nullable ty))
           (contains? scalar-type-names (str (:name ty))))
      (vary-meta s assoc :tag (symbol (str (:name ty))))

      (and (nil? ty) (:init v) (literal-tag (:init v)))
      (vary-meta s assoc :tag (literal-tag (:init v)))

      :else s)))

(def ^:private param-hint-types
  "Dart scalar types emitted as `^Type` metadata on cljd fn parameters.
  They are required so that Dart int literals reaching `double` slots are
  converted at the static call boundary, matching Dart's implicit
  int-literal-to-double conversion (dynamic calls would fail at runtime)."
  #{"int" "double" "num" "bool" "String"})

(defn param-symbol
  "cljd binding symbol for a Dart formal parameter (after renaming, when the
  parameter is a reassigned local).

  Scalar Dart types are always emitted as `^Type` hints (int literals reaching
  `double` slots need them). Non-scalar types — local classes, batch project
  types, type variables — are hinted only in `@override` positions
  (`:override-params?`): a hint becomes part of the emitted Dart method
  signature, and ClojureDart cannot map a *generated* library's own class back
  to a cljd symbol at a cross-file call site (`Can't resolve type
  <generated-class>`). Overrides are called by the Flutter framework, never from
  another generated namespace, and their hint is what makes the parameter's
  members resolve (`didUpdateWidget(covariant <WidgetType> oldWidget)`).
  External types are left unhinted (an unresolvable cljd hint would break
  compilation), except that the batch exports shim resolves them."
  [p rename ctx]
  (let [s (symbol (get rename (:name p) (:name p)))
        ty (:type p)
        n (when (and (= :named-type (:ir ty)) (nil? (:prefix ty)))
            (str (:name ty)))
        hint? (and n
                   (not (:nullable ty))
                   (or (contains? param-hint-types n)
                       (and (:override-params? ctx)
                            (or (some? (project-type-info n ctx))
                                (contains? (get-in ctx [:unit-info :classes]) n)
                                (contains? (:type-vars ctx) n)))))
        ;; `type-symbol` expands typedefs and qualifies project types; the
        ;; scalar set maps to itself.
        tag (when hint? (type-symbol ty ctx))]
    (if tag (vary-meta s assoc :tag tag) s)))

(defn- dart-const-default?
  "True when `form` can be written into a Dart parameter default.

  Dart requires compile-time constants and ClojureDart writes the emitted form
  verbatim into the parameter list: a collection or call would inline
  construction statements there (`dc.dynamic options = final lcoc_core...;
  ...`), which is not valid Dart. Only scalar literals and nil are safe."
  [form]
  (or (nil? form) (boolean? form) (number? form) (string? form)))

(defn emit-params
  "Dart formal parameter list ->
  `{:params [...] :rebinds [[sym default] ...]}`.

  Required positional params first, then Dart optional positional params after
  a `...` delimiter, then Dart named params as `.name default` pairs.

  A default that is not a Dart constant (collection, call, interpolation, ...)
  cannot appear in the Dart parameter list: the parameter gets a `nil` default
  and the real default is bound at the top of the body (`:rebinds`).
  Divergence: an argument explicitly passed as nil also takes the default
  (Dart would keep the nil)."
  [params rename ctx]
  (let [handled (mapv (fn [p]
                        (let [s (param-symbol p rename ctx)
                              has-default? (and (:default p) (not (:required p)))
                              d (when has-default? (emit-expr (:default p) ctx))
                              const? (dart-const-default? d)]
                          {:kind (:kind p)
                           :name (:name p)
                           :sym s
                           :has-default? has-default?
                           :default (if const? d nil)
                           :rebind (when (and has-default? (not const?))
                                     [s (list 'if (list 'nil? s) d s)])}))
                      params)
        reqs (filter #(= :required-positional (:kind %)) handled)
        opts (filter #(= :optional-positional (:kind %)) handled)
        nms (filter #(= :named (:kind %)) handled)
        params-vec
        (vec
         (concat
          (map :sym reqs)
          (when (seq opts)
            (cons '...
                  (mapcat (fn [{:keys [sym default]}] [sym default]) opts)))
          (mapcat (fn [{:keys [name sym default has-default?]}]
                    (let [nsym (symbol (str "." name))]
                      (if has-default?
                        [(vary-meta nsym merge (meta sym)) default]
                        [(vary-meta nsym merge (meta sym))])))
                  nms)))]
    {:params params-vec
     :rebinds (vec (mapcat :rebind handled))}))

(defn- box-form [captured? init]
  (list (if captured? 'atom 'volatile!) init))

(defn wrap-rebinds
  "Wrap `body` forms in the parameter-default rebinds from `emit-params`
  (non-constant Dart defaults become a nil default plus a body binding).
  Constructor emitters place them *before* the field bindings/constructor
  call, so the default reaches the field values."
  [rebinds body]
  (if (seq rebinds)
    [(list* 'let rebinds (if (seq body) body [nil]))]
    body))

(defn emit-callable
  "Emit a function-like node (`:params` + `:body`) into
  `{:params [...] :rebinds [...] :body [...] :async bool}`.

  Block bodies get a mutability analysis (R1); reassigned parameters are
  renamed and re-bound to a box so mutation compiles. `:rebinds` are not
  applied to `:body` (callers decide where; see `wrap-rebinds`).

  The enclosing function's control-flow context (early-return guards, loop
  break/continue flags) is cleared: a nested function has its own control
  flow and must not skip its statements on the outer function's state."
  [node ctx]
  (let [ctx (dissoc ctx :guards :result :done :break-syms :continue-syms)
        params (:params node)
        body (:body node)
        async (:async body)
        generator? (and async (re-find #"\*" (str async)))]
    (when generator?
      (fail! body "generators (async*/sync*) are not supported by ClojureDart"))
    (case (:ir body)
      :expr-body
      (let [renames (local-renames (keep :name params))
            ctx' (-> ctx
                     (update :locals set/union (set (keep :name params)))
                     (update :local-renames merge renames))
            {:keys [params rebinds]} (emit-params params renames ctx')]
        {:params params
         :rebinds rebinds
         :body [(emit-expr (:expr body) ctx')]
         :async (some? async)})

      :block-body
      (let [stmts (:stmts (:block body))
            param-names (keep :name params)
            info (mutable/analyze (:block body)
                                  {:params param-names
                                   :exclude (:fields ctx)})
            mutated (set/intersection (:mutable info) (set param-names))
            ;; Reserved names are renamed first (`when` -> `when$local`), then
            ;; reassigned parameters get the `$param` suffix so the box and the
            ;; parameter binding stay distinct.
            base-renames (local-renames (concat param-names (:declared info)))
            rename (into {}
                         (for [n param-names
                               :let [base (get base-renames n n)]
                               :when (or (contains? mutated n) (not= base n))]
                           [n (str base (when (contains? mutated n) "$param"))]))
            ctx' (assoc ctx
                        :mutable (:mutable info)
                        :captured (:captured info)
                        :local-types (typeinfo/local-types
                                      (:block body)
                                      (get-in ctx [:unit-info :classes])
                                      params)
                        :locals (set/union (:locals ctx) (:declared info))
                        :local-renames (merge (:local-renames ctx) base-renames))
            body-forms ((:emit-body ctx') stmts (assoc ctx' :fn-body? true))
            bindings (vec (mapcat (fn [n]
                                    [(local-symbol n ctx')
                                     (box-form (contains? (:captured info) n)
                                               (symbol (get rename n n)))])
                                  (sort mutated)))
            body-forms (if (seq mutated)
                         [(list* 'let bindings (if (seq body-forms) body-forms [nil]))]
                         body-forms)
            {:keys [params rebinds]} (emit-params params rename ctx')]
        {:params params
         :rebinds rebinds
         :body body-forms
         :async (some? async)})

      :empty-body
      (let [{:keys [params rebinds]} (emit-params params
                                                  (local-renames (keep :name params))
                                                  ctx)]
        {:params params
         :rebinds rebinds
         :body [nil]
         :async (some? async)})

      :native-body (fail! body "native function bodies are not supported")
      (fail! body (str "unsupported function body " (:ir body))))))

;; ---------------------------------------------------------------------------
;; member-access chains
;; ---------------------------------------------------------------------------

(defn- link? [n]
  (and (map? n) (contains? #{:prop :call :prefixed-id} (:ir n))))

(defn- class-like? [name ctx]
  (or (local-class? name ctx)
      (alias/class-name? name)))

(defn- static-base? [n ctx]
  (and (= :id (:ir n))
       (class-like? (:name n) ctx)
       (not (alias/import-prefix? (:name n) (:aliases ctx)))))

(defn- id-node [name src]
  {:ir :id :name name :src src})

(defn- qualified-node [sym src]
  {:ir :qualified :symbol sym :src src})

(defn- super-base [src]
  {:ir :qualified :symbol (form/hint 'super 'this) :src src})

(defn- import-prefix-sym?
  "True when `name` is an import alias *and* not shadowed by a local or field
  of the same name (Dart scoping: a local declaration shadows a library
  prefix, so `item.display` with a field `item` is property access, not a
  library-qualified reference)."
  [name ctx]
  (and (alias/import-prefix? name (:aliases ctx))
       (not (contains? (:locals ctx) (str name)))
       (not (contains? (:fields ctx) (str name)))))

(defn- unchain
  "Decompose a property/call/prefixed-identifier chain into [base links],
  links ordered from the base outwards. Handles Dart's syntactic
  `PrefixedIdentifier` forms:
    - known import prefix  -> library-qualified symbol (m/Text)
    - UpperCamelCase prefix -> static member access on a class
    - anything else         -> instance property access (.-field obj)"
  [node ctx]
  (loop [n node links []]
    (case (:ir n)
      :prefixed-id
      (let [{:keys [prefix name src]} n]
        (if (import-prefix-sym? prefix ctx)
          [(qualified-node (symbol (str prefix "/" name)) src) links]
          [(id-node prefix src) (cons {:kind :get :name name :null-aware false} links)]))

      (:prop :call)
      (let [t (:target n)
            link (if (= :call (:ir n))
                   {:kind :call :name (:name n) :args (:args n) :null-aware (:null-aware n)}
                   {:kind :get :name (:name n) :null-aware (:null-aware n)})]
        (cond
          ;; a target-less call/property is itself the chain base
          (nil? t) [n links]

          ;; library-qualified call: math/max(1, 2), m/Text("x")
          (and (= :call (:ir n))
               (= :id (:ir t))
               (import-prefix-sym? (:name t) ctx))
          [(assoc (qualified-node (symbol (str (:name t) "/" (:name n))) (:src n))
                  :args (:args n))
           links]

          (= :super (:ir t))
          [(super-base (:src n)) (cons link links)]

          (link? t) (recur t (cons link links))
          :else [t (cons link links)]))

      [n links])))

(defn- link-step [l ctx]
  (case (:kind l)
    :get (symbol (str ".-" (:name l)))
    :call (list* (symbol (str "." (:name l))) (emit-args (:args l) ctx))))

(defn- emit-base [base ctx]
  (if (= :qualified (:ir base))
    (if (contains? base :args)
      (list* (:symbol base) (emit-args (:args base) ctx))
      (:symbol base))
    (emit-expr base ctx)))

(defn- thread
  "Thread `init` through `steps`, using some-> when any link was null-aware."
  [init steps null-aware?]
  (cond
    (empty? steps) init
    (= 1 (count steps))
    (let [s (first steps)
          direct (if (symbol? s) (list s init) (list* (first s) init (rest s)))]
      (if null-aware? (list 'some-> init s) direct))
    :else (list* (if null-aware? 'some-> '->) init steps)))

(defn- emit-local-static
  "Emit a static member access on a class declared in this unit, threading
  any further links from the static value."
  [base links ctx node]
  (let [class (:name base)
        f (first links)
        member (:name f)
        more (rest links)
        kind (static-member-kind class member ctx)
        base-form (case (:kind f)
                    :get (case kind
                           :field (member-symbol class member)
                           :getter (list (member-symbol class member))
                           :method (member-symbol class member) ; tear-off
                           :ctor (ctor-symbol-for class member)
                           nil (fail! node (str "unknown static member "
                                                class "." member)))
                    :call (case kind
                            :method (list* (member-symbol class member)
                                           (emit-args (:args f) ctx))
                            :ctor (list* (ctor-symbol-for class member)
                                         (emit-args (:args f) ctx))
                            :getter (fail! node (str class "." member
                                                     " is a getter, not a method"))
                            :field (fail! node (str class "." member
                                                    " is a field, not a method"))
                            nil (fail! node (str "unknown static member "
                                                 class "." member))))]
    (thread base-form (map #(link-step % ctx) more)
            (boolean (some :null-aware links)))))

(defn- emit-project-static
  "Static member access on a project class declared in another file of the
  same batch conversion. Its statics are emitted as ns-level names
  (`ns/Class-member`), mirroring `emit-local-static`."
  [base links ctx node]
  (let [class (:name base)
        info (project-type-info class ctx)
        f (first links)
        member (:name f)
        more (rest links)
        kinds (get (:statics info) member)
        sym (symbol (str (:ns info) "/" class "-" member))
        base-form (case (:kind f)
                    :get (cond
                           (contains? kinds :field) sym
                           (contains? kinds :getter) (list sym)
                           (contains? kinds :method) sym ; tear-off
                           (contains? kinds :ctor) sym   ; constructor tear-off
                           :else (fail! node (str "unknown static member "
                                                  class "." member)))
                    :call (cond
                            (contains? kinds :method) (list* sym (emit-args (:args f) ctx))
                            (contains? kinds :ctor) (list* sym (emit-args (:args f) ctx))
                            (contains? kinds :getter) (fail! node (str class "." member
                                                                       " is a getter, not a method"))
                            (contains? kinds :field) (fail! node (str class "." member
                                                                      " is a field, not a method"))
                            :else (fail! node (str "unknown static member "
                                                   class "." member))))]
    (when (:widget? info)
      (fail! node (str class " is translated as a widget builder fn; static "
                       "member access is not available in idiomatic mode")))
    (thread base-form (map #(link-step % ctx) more)
            (boolean (some :null-aware links)))))

(defn- emit-chain [node ctx]
  (let [[base links] (unchain node ctx)
        null-aware? (boolean (some :null-aware links))]
    (when (nil? base)
      (fail! node "member access without a target"))
    (if (and (static-base? base ctx) (local-class? (:name base) ctx))
      (emit-local-static base links ctx node)
      (if (and (static-base? base ctx) (project-type-info (:name base) ctx))
        (emit-project-static base links ctx node)
        (if (static-base? base ctx)
          (let [[f & more] links]
            (if (= :call (:kind f))
              ;; static method call, then optional further steps
              (thread (list* (alias/qualify-member (:name base) (:name f) (:aliases ctx))
                             (emit-args (:args f) ctx))
                      (map #(link-step % ctx) more)
                      (boolean (some :null-aware links)))
              ;; static field/getter path -> dotted symbol, then optional
              ;; further steps (`WidgetsBinding.instance.addObserver(this)`,
              ;; `Colors.black.withAlpha(12)`).
              (let [getters (vec (take-while #(= :get (:kind %)) links))
                    more (drop (count getters) links)]
                (when (empty? getters)
                  (fail! node "unsupported static member chain"))
                (thread (symbol (str (alias/qualify (:name base) (:aliases ctx))
                                     (apply str (map #(str "." (:name %)) getters))))
                        (map #(link-step % ctx) more)
                        (boolean (some :null-aware links))))))
          (thread (emit-base base ctx)
                  (map #(link-step % ctx) links)
                  null-aware?))))))

(defn- known-bare?
  "True when a bare name is definitely not an implicit-this member access:
  a local, parameter, top-level function/variable, dart:core name, or
  referred name."
  [n ctx]
  (or (contains? (:locals ctx) n)
      (contains? (get-in ctx [:unit-info :top-fns]) n)
      (contains? (get-in ctx [:unit-info :top-getters]) n)
      (contains? (get-in ctx [:unit-info :top-setters]) n)
      (contains? (get-in ctx [:unit-info :top-vars]) n)
      (contains? (get-in ctx [:unit-info :widget-fns]) n)
      (contains? alias/core-symbols n)
      (contains? (:referred (:aliases ctx)) n)
      (alias/import-prefix? n (:aliases ctx))))

(defn- widget-fn-info
  "Emission info for a class emitted as an idiomatic widget builder fn."
  [ctx class]
  (get-in ctx [:unit-info :widget-fns (str class)]))

(defn- drop-widget-args
  "Remove named constructor arguments this widget builder does not accept
  (currently `key`)."
  [ctx class args]
  (drop-args (:drop-args (widget-fn-info ctx class)) args))

(defn- emit-fn-call
  "Unqualified call: `foo(...)` (top-level function, constructor, static
  member, cross-file project value, or implicit-this method)."
  [node ctx]
  (let [n (:name node)]
    (cond
      (contains? (get-in ctx [:unit-info :widget-fns]) (str n))
      (list* (symbol (str n)) (emit-args (drop-widget-args ctx n (:args node)) ctx))

      (and (local-class? n ctx) (known-ctor? n nil ctx))
      (list* (ctor-symbol-for n nil) (emit-args (:args node) ctx))

      (local-class? n ctx)
      (fail! node (str n " has no unnamed constructor"))

      (static-member-info n ctx)
      (let [sm (static-member-info n ctx)]
        (cond
          (:method sm) (list* (:method sm) (emit-args (:args node) ctx))
          (:field sm) (fail! node (str n " is a static field, not a method"))
          (:getter sm) (fail! node (str n " is a static getter; read it as a property"))
          :else (fail! node (str n " is a static setter; assign to it"))))

      (and (project-value-info n ctx) (not (own-instance-member? n ctx)))
      (let [{:keys [kinds] :as info} (project-value-info n ctx)]
        (cond
          (or (contains? kinds :fn) (contains? kinds :var))
          (list* (project-value-symbol info n) (emit-args (:args node) ctx))

          (contains? kinds :getter)
          (fail! node (str n " is a getter; read it as a property"))

          :else (fail! node (str n " is a setter; assign to it"))))

      (project-type-info n ctx)
      (let [info (project-type-info n ctx)]
        (if (:widget? info)
          (list* (symbol (str (:ns info) "/" n))
                 (emit-args (drop-args (:drop-args info) (:args node)) ctx))
          (if-let [sym (project-ctor-symbol info n nil)]
            (list* sym (emit-args (:args node) ctx))
            (fail! node (str n " has no unnamed constructor")))))

      (class-like? n ctx)
      (list* (alias/qualify n (:aliases ctx)) (emit-args (:args node) ctx))

      (and (:this-sym ctx)
           (not (known-bare? n ctx)))
      (list* (symbol (str "." n)) (:this-sym ctx) (emit-args (:args node) ctx))

      :else
      (list* (if (and (:wildcard-alias (:aliases ctx)) (not (known-bare? n ctx)))
               (alias/qualify n (:aliases ctx))
               (local-symbol n ctx))
             (emit-args (:args node) ctx)))))

;; ---------------------------------------------------------------------------
;; operators
;; ---------------------------------------------------------------------------

(def ^:private binary-ops
  {"==" '= "!=" 'not= "<" '< ">" '> "<=" '<= ">=" '>=
   "+" '+ "-" '- "*" '* "/" '/ "~/" 'quot
   "&&" 'and "||" 'or "??" 'or
   "&" 'bit-and "|" 'bit-or "^" 'bit-xor
   "<<" 'bit-shift-left ">>" 'bit-shift-right ">>>" 'unsigned-bit-shift-right})

(def ^:private non-variadic-ops
  "Dart operators whose cljd function takes exactly two arguments: a
  same-operator chain must not be flattened into one call (`(quot a b c)`)."
  #{"~/" "<<" ">>" ">>>"})

(defn compound-fn
  "Function form implementing a Dart compound assignment operator. Accepts
  either the compound spelling (`+=`) or the bare operator (`+`)."
  [op]
  (let [op (if (and (string? op) (str/ends-with? op "=")) (subs op 0 (dec (count op))) op)]
    (case op
      "+" '+ "-" '- "*" '* "/" '/ "~/" 'quot
      "%" '(fn [a b] (. a % b))
      "&" 'bit-and "|" 'bit-or "^" 'bit-xor
      "<<" 'bit-shift-left ">>" 'bit-shift-right ">>>" 'unsigned-bit-shift-right
      (fail! {:src nil} (str "unsupported compound assignment operator " op)))))

(def ^:private operator-call-names
  "Dart binary operators that can be user-defined, as cljd method names.
  `^`, `~` and `~/` are emitted as string member names because they are not
  readable as Clojure symbols in that position."
  {"==" "==" "!=" "!=" "<" "<" ">" ">" "<=" "<=" ">=" ">="
   "+" "+" "-" "-" "*" "*" "/" "/" "%" "%" "~/" "~/"
   "&" "&" "|" "|" "^" "^" "<<" "<<" ">>" ">>" ">>>" ">>>"})

(def ^:private string-member-ops #{"^" "~" "~/" ">>>"})

(defn- operator-method-symbol [m]
  (if (contains? string-member-ops m) m (symbol m)))

(defn- bare-operator [op]
  (if (and (string? op) (str/ends-with? op "=")) (subs op 0 (dec (count op))) op))

(defn- user-op-form
  "`(. receiver op argument)` when `op` (possibly compound) names a
  user-overloadable operator, else nil."
  [receiver op argument]
  (when-let [m (get operator-call-names (bare-operator op))]
    (list '. receiver (operator-method-symbol m) argument)))

(defn- local-class-name
  "Class name when `node` is an identifier known to hold an instance of a
  class declared in this unit (see dart2cljd.typeinfo)."
  [node ctx]
  (let [n (strip-paren node)]
    (when (= :id (:ir n))
      (get-in (:local-types ctx) [:classes (:name n)]))))

(defn- local-class-receiver [node ctx]
  (local-class-name node ctx))

(defn- scalar-receiver?
  "True when `node` is an identifier known to be a Dart scalar: a scalar
  local/parameter or an implicit-this field of scalar type."
  [node ctx]
  (let [n (strip-paren node)]
    (and (= :id (:ir n))
         (or (contains? (get-in (:local-types ctx) [:scalars]) (:name n))
             (contains? (:scalar-fields ctx) (:name n))))))

(defn- emit-user-operator [node ctx]
  (let [m (get operator-call-names (:op node))]
    (list '. (emit-expr (:lhs node) ctx) (operator-method-symbol m)
          (emit-expr (:rhs node) ctx))))

(def ^:private guarded-binary-ops
  "User-overloadable operators whose fallback is a plain cljd/numeric form,
  so an unknown receiver needs a runtime `dart/is?` dispatch guard. `==`/`!=`
  are excluded: cljd `=` already falls back to Dart `==` (IEquiv fallback)."
  #{"+" "-" "*" "/" "~/" "%" "&" "|" "^" "<<" ">>" ">>>"})

(def ^:private guarded-prefix-ops #{"-" "~"})

(defn- operator-candidates
  "Unit classes declaring operator `op` with `arity` (0 unary, 1 binary), in
  declaration order (from dart2cljd.emit.decls' unit-info)."
  [ctx op arity]
  (get-in ctx [:unit-info :operator-classes [op arity]]))

(defn- type-symbol-for [class ctx]
  (type-symbol {:ir :named-type :prefix nil :name class
                :type-args [] :nullable false}
               ctx))

(defn- plain-binary-form
  "Two-argument fallback for an overloadable operator. No same-operator
  flattening here: guarded operands are pre-bound locals."
  [op ctx a b]
  (cond
    (= op "%") (if (= :idiomatic (:mode ctx))
                 (list 'mod a b)
                 (list '. a (symbol "%") b))
    ;; `cljd.core/quot` is declared `^num` although Dart's `num.~/` returns
    ;; `int`; the interop form keeps the int static type, which int contexts
    ;; need (`int f() => (a ~/ 2).clamp(0, n)`). The member name must be the
    ;; string `"~/"`: the bare symbol `~/` reads as `unquote`.
    (= op "~/") (list '. a "~/" b)
    :else (list (binary-ops op) a b)))

(defn- unknown-id-receiver?
  "True when `node` is an identifier whose type is not known (not a unit
  class, not a scalar). Guards are restricted to identifiers: guarding
  arbitrary expressions would add `dart/is?` checks to ordinary numeric
  arithmetic (e.g. `(+ (fib (- n 1)) …)`)."
  [node ctx]
  (let [n (strip-paren node)]
    (and (= :id (:ir n))
         (not (local-class-receiver n ctx))
         (not (scalar-receiver? n ctx)))))

(defn- guard-binary? [node ctx]
  (and (contains? guarded-binary-ops (:op node))
       (seq (operator-candidates ctx (:op node) 1))
       (unknown-id-receiver? (:lhs node) ctx)))

(defn- emit-guarded-binary
  "Runtime dispatch for an operator whose receiver type is unknown:
  `(let [d2c$a A d2c$b B] (if (and (dart/is? d2c$a C1) (dart/is? d2c$b C1))
  (. d2c$a + d2c$b) … fallback))`.

  Both operands are narrowed: Dart promotes them inside the branch, which is
  what makes the call type-check against a hinted operator parameter
  (`Point.+ [this ^Point o]`) — the source would only reach it with a `C1`
  right-hand side anyway."
  [node ctx]
  (let [a 'd2c$a b 'd2c$b
        method (operator-method-symbol (get operator-call-names (:op node)))
        fallback (plain-binary-form (:op node) ctx a b)
        chain (reduce (fn [else class]
                        (let [t (type-symbol-for class ctx)]
                          (list 'if
                                (list 'and
                                      (list 'dart/is? a t)
                                      (list 'dart/is? b t))
                                (list '. a method b)
                                else)))
                      fallback
                      (reverse (operator-candidates ctx (:op node) 1)))]
    (list 'let [a (emit-expr (:lhs node) ctx)
                b (emit-expr (:rhs node) ctx)]
          chain)))

(defn- guard-prefix? [node ctx]
  (and (contains? guarded-prefix-ops (:op node))
       (seq (operator-candidates ctx (:op node) 0))
       (unknown-id-receiver? (:operand node) ctx)))

(defn- emit-guarded-prefix [node ctx]
  (let [a 'd2c$a
        unary? (= "-" (:op node))
        user-form (if unary? (list '.unary- a) (list '. a "~"))
        fallback (if unary? (list '- a) (list 'bit-not a))
        chain (reduce (fn [else class]
                        (list 'if
                              (list 'dart/is? a (type-symbol-for class ctx))
                              user-form
                              else))
                      fallback
                      (reverse (operator-candidates ctx (:op node) 0)))]
    (list 'let [a (emit-expr (:operand node) ctx)] chain)))

(defn- flatten-same [op node ctx]
  (let [n (strip-paren node)]
    (if (and (= :binary (:ir n)) (= op (:op n)))
      (into (flatten-same op (:lhs n) ctx) [(emit-expr (:rhs n) ctx)])
      [(emit-expr node ctx)])))

(defn- emit-binary [node ctx]
  (let [op (:op node)]
    (cond
      (and (local-class-receiver (:lhs node) ctx)
           (contains? operator-call-names op))
      (emit-user-operator node ctx)

      (guard-binary? node ctx)
      (emit-guarded-binary node ctx)

      (= op "%")
      (if (= :idiomatic (:mode ctx))
        (list 'mod (emit-expr (:lhs node) ctx) (emit-expr (:rhs node) ctx))
        (list '. (emit-expr (:lhs node) ctx) (symbol "%") (emit-expr (:rhs node) ctx)))

      ;; `cljd.core/quot` is declared `^num` although Dart's `num.~/` returns
      ;; `int`; the interop form keeps the int static type, which int contexts
      ;; need (`int f() => (a ~/ 2).clamp(0, n)`). The member name must be the
      ;; string `"~/"`: the bare symbol `~/` reads as `unquote`.
      (= op "~/")
      (list '. (emit-expr (:lhs node) ctx) "~/" (emit-expr (:rhs node) ctx))

      (contains? binary-ops op)
      (let [args (if (contains? non-variadic-ops op)
                   [(emit-expr (:lhs node) ctx) (emit-expr (:rhs node) ctx)]
                   (flatten-same op node ctx))]
        (list* (binary-ops op) args))

      :else (fail! node (str "unsupported binary operator " op)))))

(defn- emit-prefix [node ctx]
  (case (:op node)
    "!" (list 'not (emit-expr (:operand node) ctx))
    "-" (cond
          (local-class-receiver (:operand node) ctx)
          (list (symbol ".unary-") (emit-expr (:operand node) ctx))
          (guard-prefix? node ctx)
          (emit-guarded-prefix node ctx)
          :else (list '- (emit-expr (:operand node) ctx)))
    "~" (cond
          (local-class-receiver (:operand node) ctx)
          (list '. (emit-expr (:operand node) ctx) "~")
          (guard-prefix? node ctx)
          (emit-guarded-prefix node ctx)
          :else (list 'bit-not (emit-expr (:operand node) ctx)))
    ("++" "--") (emit-increment node ctx true)
    (fail! node (str "unsupported prefix operator " (:op node)))))

(defn- emit-postfix [node ctx]
  (case (:op node)
    "!" (emit-expr (:operand node) ctx)
    ("++" "--") (emit-increment node ctx false)
    (fail! node (str "unsupported postfix operator " (:op node)))))

;; ---------------------------------------------------------------------------
;; mutation (R1)
;; ---------------------------------------------------------------------------

(defn- simple-target? [node]
  (contains? #{:id :this :super} (:ir node)))

(defn- increment-form
  "Build the cljd form for `place++`/`++place` (and --) given the read and
  write forms of the place. Dart evaluates the place once, so the old value
  is bound before the write."
  [read-form write-form op prefix?]
  (if prefix?
    (write-form (list op read-form))
    (list 'let [(symbol "d2c$old") read-form]
          (write-form (list op (symbol "d2c$old")))
          (symbol "d2c$old"))))

(defn- static-write-target
  "When `lhs` names a static member of a class (declared in this unit or in
  another file of the batch), return a place descriptor
  `{:read <form-or-nil> :write <fn>}`; nil when `lhs` is not a static
  access. Reads are nil for write-only setters; members that cannot be
  assigned fail loudly."
  [lhs ctx]
  (let [obj (:target lhs)
        member (:name lhs)]
    (when (and (= :id (:ir obj))
               (not (alias/import-prefix? (:name obj) (:aliases ctx))))
      (let [class (:name obj)
            linfo (class-info class ctx)
            pinfo (project-type-info class ctx)]
        (cond
          linfo
          (let [field? (contains? (:static-fields linfo) member)
                setter? (contains? (:static-setters linfo) member)
                getter? (contains? (:static-getters linfo) member)
                method? (contains? (:static-methods linfo) member)
                ctor? (contains? (:ctors linfo) member)
                field-sym (member-symbol class member)
                setter-sym (symbol (str "set-" member "!"))]
            (cond
              field? {:read field-sym :write (fn [v] (list 'set! field-sym v))}
              setter? {:read (when getter? (list field-sym))
                       :write (fn [v] (list setter-sym v))}
              getter? (fail! lhs (str class "." member " is a getter, not assignable"))
              method? (fail! lhs (str class "." member " is a method, not assignable"))
              ctor? (fail! lhs (str class "." member " is a constructor, not assignable"))
              :else (fail! lhs (str "unknown static member " class "." member))))

          pinfo
          (let [kinds (get (:statics pinfo) member)
                field-sym (symbol (str (:ns pinfo) "/" class "-" member))
                setter-sym (symbol (str (:ns pinfo) "/set-" member "!"))]
            (cond
              (contains? kinds :field)
              {:read field-sym :write (fn [v] (list 'set! field-sym v))}

              (contains? kinds :setter)
              {:read (when (contains? kinds :getter) (list field-sym))
               :write (fn [v] (list setter-sym v))}

              (contains? kinds :getter)
              (fail! lhs (str class "." member " is a getter, not assignable"))

              (contains? kinds :method)
              (fail! lhs (str class "." member " is a method, not assignable"))

              (contains? kinds :ctor)
              (fail! lhs (str class "." member " is a constructor, not assignable"))

              :else (fail! lhs (str "unknown static member " class "." member)))))))))

(defn- assign-static
  "Assign to a place descriptor from `static-write-target`."
  [st op rhs lhs]
  (when (and (nil? (:read st)) (not (contains? #{nil "="} op)))
    (fail! lhs "compound assignment needs a matching getter"))
  (assign-slot (:read st) (:write st) (if (nil? op) "=" op) rhs))

(defn- emit-increment-place
  "`x++` on a property/index target, incl. static members of classes."
  [target node ctx op prefix?]
  (if-let [st (and (= :prop (:ir target)) (static-write-target target ctx))]
    (do
      (when (nil? (:read st))
        (fail! node (str (:name target) " has no getter; cannot modify it")))
      (increment-form (:read st) (:write st) op prefix?))
    (let [read (emit-expr target ctx)
          setter (if (= :index (:ir target))
                   (fn [v] (list '. (emit-expr (:target target) ctx)
                                 "[]=" (emit-expr (:index target) ctx) v))
                   (fn [v] (list (symbol (str ".-" (:name target) "!"))
                                 (emit-expr (:target target) ctx) v)))]
      (if (or prefix? (simple-target? (:target target)))
        (setter (list op read))
        (list 'let [(symbol "d2c$old") read]
              (setter (list op (symbol "d2c$old")))
              (symbol "d2c$old"))))))

(defn- emit-increment
  "`x++` / `++x` (and --) preserving Dart value semantics."
  [node ctx prefix?]
  (let [op (if (= "++" (:op node)) 'inc 'dec)
        target (:operand node)]
    (case (:ir target)
      :id
      (let [n (:name target)
            sm (static-member-info n ctx)]
        (cond
          (contains? (get-in (:local-types ctx) [:classes]) n)
          (do
            (when (contains? (:mutable ctx) n)
              (fail! node (str "increment/decrement of reassigned class-typed local "
                               n " is not supported")))
            (let [form (user-op-form (local-symbol n ctx) (:op node) 1)]
              (if prefix?
                (list 'set! (local-symbol n ctx) form)
                (list 'let [(symbol "d2c$old") (local-symbol n ctx)]
                      (list 'set! (local-symbol n ctx) form)
                      (symbol "d2c$old")))))

          (contains? (:mutable ctx) n)
          (let [write (list (if (contains? (:captured ctx) n) 'swap! 'vswap!)
                            (local-symbol n ctx) op)]
            (if prefix?
              write
              (list 'let [(symbol "d2c$old") (list 'deref (local-symbol n ctx))]
                    write (symbol "d2c$old"))))

          (and (or (:ctor-body? ctx) (:in-fn ctx))
               (:this-sym ctx)
               (contains? (:fields ctx) n))
          (increment-form (list (symbol (str ".-" n)) (:this-sym ctx))
                          (fn [v] (list (symbol (str ".-" n "!")) (:this-sym ctx) v))
                          op prefix?)

          (contains? (:atom-fields ctx) n)
          (let [a (get (:atom-fields ctx) n)
                write (list 'swap! a op)]
            (if prefix?
              write
              (list 'let [(symbol "d2c$old") (list 'deref a)]
                    write (symbol "d2c$old"))))

          sm
          (let [{:keys [field getter setter]} sm]
            (cond
              field (increment-form field (fn [v] (list 'set! field v)) op prefix?)
              setter (if getter
                       (increment-form (list getter)
                                       (fn [v] (list setter v))
                                       op prefix?)
                       (fail! node (str n " has no static getter; cannot modify it")))
              :else (fail! node (str n " is not modifiable"))))

          (and (project-value-info n ctx) (not (own-instance-member? n ctx)))
          (let [{:keys [kinds] :as info} (project-value-info n ctx)
                sym (project-value-symbol info n)]
            (cond
              (contains? kinds :var)
              (if (:final? info)
                (fail! node (str n " is final; incrementing is invalid Dart"))
                (increment-form sym (fn [v] (list 'set! sym v)) op prefix?))

              (contains? kinds :setter)
              (if (contains? kinds :getter)
                (increment-form (list sym)
                                (fn [v] (list (symbol (str (:ns info) "/set-" n "!")) v))
                                op prefix?)
                (fail! node (str n " has no getter; cannot modify it")))

              :else (fail! node (str n " is not modifiable"))))

          :else
          (let [write (list 'set! (local-symbol n ctx)
                            (list op (local-symbol n ctx)))]
            (if prefix?
              write
              (list 'let [(symbol "d2c$old") (local-symbol n ctx)]
                    write (symbol "d2c$old"))))))

      :prop (emit-increment-place target node ctx op prefix?)
      :prefixed-id (emit-increment-place (assoc target :ir :prop
                                                :target (id-node (:prefix target)
                                                                 (:src target)))
                                         node ctx op prefix?)
      :index (emit-increment-place target node ctx op prefix?)

      (fail! node "unsupported increment/decrement target"))))

(defn- assign-id [lhs op rhs ctx]
  (let [n (:name lhs)
        op (if (= "=" op) nil op)
        sm (static-member-info n ctx)]
    (cond
      (contains? (:mutable ctx) n)
      (let [setter (if (contains? (:captured ctx) n) 'swap! 'vswap!)
            resetter (if (contains? (:captured ctx) n) 'reset! 'vreset!)
            l (local-symbol n ctx)
            d (list 'deref l)
            m (when (contains? (get-in (:local-types ctx) [:classes]) n)
                (get operator-call-names (bare-operator op)))]
        (case op
          nil (list resetter l rhs)
          "??" (list 'if (list 'nil? d) (list resetter l rhs) d)
          "&&" (list 'if d (list resetter l rhs) d)
          "||" (list 'if d d (list resetter l rhs))
          (if m
            (list setter l
                  (list 'fn ['d2c$a 'd2c$b]
                        (list '. 'd2c$a (operator-method-symbol m) 'd2c$b))
                  rhs)
            (list setter l (compound-fn op) rhs))))

      (contains? (:atom-fields ctx) n)
      (let [a (get (:atom-fields ctx) n)
            d (list 'deref a)]
        (case op
          nil (list 'reset! a rhs)
          "??" (list 'if (list 'nil? d) (list 'reset! a rhs) d)
          "&&" (list 'if d (list 'reset! a rhs) d)
          "||" (list 'if d d (list 'reset! a rhs))
          (list 'swap! a (compound-fn op) rhs)))

      (and (or (:ctor-body? ctx) (:in-fn ctx))
           (:this-sym ctx)
           (contains? (:fields ctx) n))
      (assign-slot (list (symbol (str ".-" n)) (:this-sym ctx))
                   (fn [v] (list (symbol (str ".-" n "!")) (:this-sym ctx) v))
                   (if (nil? op) "=" op)
                   rhs)

      (and (contains? (get-in (:local-types ctx) [:classes]) n)
           (contains? operator-call-names (bare-operator op)))
      (list 'set! (local-symbol n ctx)
            (user-op-form (local-symbol n ctx) op rhs))

      (contains? (get-in ctx [:unit-info :top-setters]) n)
      (list (symbol (str "set-" n "!")) rhs)

      (and (:this-sym ctx)
           (contains? (:instance-setters ctx) n))
      (list (symbol (str ".-" n "!")) (:this-sym ctx) rhs)

      (and sm (not (contains? (:locals ctx) n)))
      (let [{:keys [field getter setter]} sm]
        (cond
          field (assign-static {:read field
                                :write (fn [v] (list 'set! field v))}
                               op rhs lhs)
          setter (assign-static {:read (when getter (list getter))
                                 :write (fn [v] (list setter v))}
                                op rhs lhs)
          :else (fail! lhs (str n " is not assignable"))))

      (and (project-value-info n ctx)
           (not (own-instance-member? n ctx))
           (not (contains? (:locals ctx) n)))
      (let [{:keys [kinds final?] :as info} (project-value-info n ctx)
            sym (project-value-symbol info n)
            setter-sym (symbol (str (:ns info) "/set-" n "!"))]
        (cond
          (contains? kinds :setter)
          (assign-static {:read (when (contains? kinds :getter) (list sym))
                          :write (fn [v] (list setter-sym v))}
                         op rhs lhs)

          (contains? kinds :var)
          (if final?
            (fail! lhs (str n " is final; assignment is invalid Dart"))
            (assign-static {:read sym :write (fn [v] (list 'set! sym v))}
                           op rhs lhs))

          :else (fail! lhs (str n " is not assignable"))))

      :else
      (let [l (local-symbol n ctx)]
        (case op
          nil (list 'set! l rhs)
          "??" (list 'if (list 'nil? l)
                     (list 'set! l rhs)
                     l)
          "&&" (list 'if l
                     (list 'set! l rhs)
                     l)
          "||" (list 'if l
                     l
                     (list 'set! l rhs))
          (list 'set! l (list (compound-fn op) l rhs)))))))

(defn- assign-slot [read write op rhs]
  (case op
    "=" (write rhs)
    "??" (list 'if (list 'nil? read) (write rhs) read)
    "&&" (list 'if read (write rhs) read)
    "||" (list 'if read read (write rhs))
    (write (list (compound-fn op) read rhs))))

(defn- assign-prop [lhs op rhs ctx]
  (let [obj (:target lhs)
        name (:name lhs)
        read (list (symbol (str ".-" name)) (emit-expr obj ctx))
        write (fn [v] (list (symbol (str ".-" name "!")) (emit-expr obj ctx) v))]
    (assign-slot read write op rhs)))

(defn- assign-index [lhs op rhs ctx]
  (let [obj (:target lhs)
        idx (:index lhs)
        read (list 'get (emit-expr obj ctx) (emit-expr idx ctx))
        write (fn [v] (list '. (emit-expr obj ctx) "[]=" (emit-expr idx ctx) v))]
    (assign-slot read write op rhs)))

(defn assign-write
  "Emit the write half of an assignment (value semantics not preserved)."
  [lhs op rhs ctx]
  (case (:ir lhs)
    :id (assign-id lhs op rhs ctx)
    :prop (if-let [st (static-write-target lhs ctx)]
            (assign-static st op rhs lhs)
            (assign-prop lhs op rhs ctx))
    :prefixed-id (let [p (assoc lhs :ir :prop
                                :target (id-node (:prefix lhs) (:src lhs)))]
                   (if-let [st (static-write-target p ctx)]
                     (assign-static st op rhs p)
                     (assign-prop p op rhs ctx)))
    :index (assign-index lhs op rhs ctx)
    (fail! lhs "unsupported assignment target")))

(defn- emit-assign [node ctx]
  (assign-write (:lhs node) (:op node) (emit-expr (:rhs node) ctx) ctx))

(defn emit-expr-stmt
  "Emit an expression in statement position: increments drop their
  value-preserving temporary."
  [node ctx]
  (if (and (contains? #{:postfix :prefix} (:ir node))
           (contains? #{"++" "--"} (:op node)))
    (emit-increment node ctx true)
    (emit-expr node ctx)))

(defn- emit-is [node ctx]
  (let [test (list 'dart/is? (emit-expr (:expr node) ctx) (type-symbol (:type node) ctx))]
    (if (:negated node) (list 'not test) test)))

;; ---------------------------------------------------------------------------
;; strings
;; ---------------------------------------------------------------------------

(defn- emit-interp [node ctx]
  (let [parts (->> (:parts node)
                   (map #(emit-expr % ctx))
                   (remove #(and (string? %) (empty? %)))
                   vec)]
    (cond
      (empty? parts) ""
      (and (= 1 (count parts)) (string? (first parts))) (first parts)
      :else (list* 'str parts))))

;; ---------------------------------------------------------------------------
;; collections
;; ---------------------------------------------------------------------------

(declare emit-elem emit-elem-seg elem-segments emit-for-parts-element
         emit-list)

(defn- spread? [x] (= :spread (:ir x)))

(defn- simple-elem?
  "An element that yields exactly one collection element. Collection-for
  elements and spreads splice; an `if` element splices only when one of its
  branches does."
  [e]
  (case (:ir e)
    :for-element false
    :spread false
    :null-aware-elem false
    :if-element (and (simple-elem? (:then e))
                     (or (nil? (:else e)) (simple-elem? (:else e))))
    true))

(defn- emit-elem-seg
  "Emit a collection element as a form evaluating to a *sequence* of elements
  (a segment). Dart splices collection-for results and spreads into the
  literal; segments let `concat` reproduce that (M8)."
  [node ctx]
  (case (:ir node)
    :if-element
    (list 'if (emit-expr (:cond node) ctx)
          (emit-elem-seg (:then node) ctx)
          (when (:else node) (emit-elem-seg (:else node) ctx)))

    :for-element
    (let [loop (:loop node)]
      (case (:ir loop)
        :for-in (let [v (local-symbol (:name (:var loop)) ctx)
                      coll (emit-expr (:iterable loop) ctx)
                      body (:body node)]
                  (if (simple-elem? body)
                    (list 'for [v coll] (emit-elem body ctx))
                    (list 'mapcat (list 'fn [v] (emit-elem-seg body ctx)) coll)))
        :for-parts (emit-for-parts-element node ctx)
        (fail! node (str "unsupported collection-for " (:ir loop)))))

    :spread (if (= :list-lit (:ir (:expr node)))
              (emit-list (:expr node) ctx)
              (emit-expr (:expr node) ctx))

    :null-aware-elem (let [v (emit-expr (:expr node) ctx)]
                       (list 'if (list 'nil? v) [] (vector v)))

    (vector (emit-expr node ctx))))

(defn- elem-segments
  "Segments of an element list: consecutive single elements are coalesced
  into a vector literal, splicing elements stay sequence-valued forms."
  [elems ctx]
  (mapcat (fn [group]
            (if (every? simple-elem? group)
              [(mapv #(emit-elem % ctx) group)]
              (map #(emit-elem-seg % ctx) group)))
          (partition-by simple-elem? elems)))

(defn- emit-list [node ctx]
  (let [elems (:elements node)]
    (if (every? simple-elem? elems)
      (mapv #(emit-elem % ctx) elems)
      (list 'vec (list* 'concat (elem-segments elems ctx))))))

(defn- emit-set [node ctx]
  (let [elems (:elements node)]
    (if (every? simple-elem? elems)
      (apply form/set-lit (map #(emit-elem % ctx) elems))
      (list 'into (form/set-lit)
            (list* 'concat (elem-segments elems ctx))))))

(defn- emit-map [node ctx]
  (let [elems (:elements node)
        entries (remove spread? elems)
        spreads (filter spread? elems)]
    (if (seq spreads)
      (list* 'merge
             (form/map-lit* (map (fn [e] [(emit-elem (:key e) ctx)
                                          (emit-elem (:value e) ctx)])
                                 entries))
             (map #(if (:null-aware %)
                     (list 'or (emit-expr (:expr %) ctx) (form/map-lit))
                     (emit-expr (:expr %) ctx))
                  spreads))
      (form/map-lit* (map (fn [e] [(emit-elem (:key e) ctx)
                                   (emit-elem (:value e) ctx)])
                          entries)))))

(defn- for-element-new-value
  "New-value form for one C-style collection-for updater on a loop variable
  (`i++`, `--i`, `i += e`, `i = e`), or nil when the updater is unsupported."
  [u names ctx]
  (case (:ir u)
    (:postfix :prefix)
    (let [n (:name (:operand u))
          op (:op u)]
      (when (and (contains? names n) (contains? #{"++" "--"} op))
        (list (if (= "++" op) 'inc 'dec) (local-symbol n ctx))))

    :assign
    (let [lhs (:lhs u)]
      (when (and (= :id (:ir lhs)) (contains? names (:name lhs)))
        (if (= "=" (:op u))
          (emit-expr (:rhs u) ctx)
          (list (compound-fn (:op u)) (local-symbol (:name lhs) ctx)
                (emit-expr (:rhs u) ctx)))))

    nil))

(defn- emit-for-parts-element
  "C-style collection-for element: `[for (init; cond; updates) body]`.

  Lowered to an eager accumulator loop (Dart collection literals are eager):
  the element form is bound before the recur so Dart's body-before-updates
  evaluation order is preserved."
  [node ctx]
  (let [loop (:loop node)
        init (:init loop)
        decl-vars (when (and init (= :var-decl (:ir init))) (:vars init))]
    (when (and init (not= :var-decl (:ir init)))
      (fail! init "collection-for initializer must be a declaration"))
    (when (empty? decl-vars)
      (fail! node "collection-for requires declared loop variables"))
    (let [names (set (map :name decl-vars))]
      (doseq [v decl-vars]
        (when (mutable/assigns? (:body node) (:name v))
          (fail! (:body node)
                 (str "assignment to collection-for variable " (:name v)
                      " is not supported"))))
      (let [ctx (-> ctx
                    (update :mutable #(apply disj % names))
                    (update :captured #(apply disj % names)))
            bindings (vec (mapcat (fn [v]
                                    [(loop-var-symbol v ctx)
                                     (when (:init v) (emit-expr (:init v) ctx))])
                                  decl-vars))
            new-by-var (reduce (fn [m u]
                                 (let [lhs (case (:ir u)
                                             (:postfix :prefix) (:name (:operand u))
                                             :assign (:name (:lhs u))
                                             nil)
                                       form (for-element-new-value u names ctx)]
                                   (when (nil? form)
                                     (fail! u "unsupported collection-for update"))
                                   (assoc m lhs form)))
                               {}
                               (:updaters loop))
            new-vals (mapv (fn [v]
                             (get new-by-var (:name v) (local-symbol (:name v) ctx)))
                           decl-vars)
            cond (when (:cond loop) (emit-expr (:cond loop) ctx))
            seg (if (simple-elem? (:body node))
                  (vector (emit-elem (:body node) ctx))
                  (emit-elem-seg (:body node) ctx))
            acc (symbol "d2c$acc")
            seg-sym (symbol "d2c$seg")]
        (list 'loop (conj bindings acc [])
              (list 'if (if (some? cond) cond true)
                    (list 'let [seg-sym seg]
                          (list* 'recur
                                 (conj new-vals (list 'into acc seg-sym))))
                    acc))))))

(defn- emit-elem [node ctx]
  (case (:ir node)
    :if-element
    (list 'if (emit-expr (:cond node) ctx)
          (emit-elem (:then node) ctx)
          (when (:else node) (emit-elem (:else node) ctx)))

    :for-element (fail! node "collection-for in a single-element position")
    :null-aware-elem (fail! node "null-aware element in a single-element position")
    :spread (fail! node "spread outside a collection literal")
    (emit-expr node ctx)))

;; ---------------------------------------------------------------------------
;; functions
;; ---------------------------------------------------------------------------

(defn- emit-fn [node ctx]
  ;; Inside a nested function (Dart closure) ClojureDart captures deftype
  ;; fields by value and forbids `set!` on them; accessing them through
  ;; `this` preserves Dart's read-at-call-time/mutable-field semantics.
  (let [{:keys [params body async rebinds]} (emit-callable node (assoc ctx :in-fn true))]
    (with-meta (list* 'fn params (wrap-rebinds rebinds body))
      (when async {:async true}))))

;; ---------------------------------------------------------------------------
;; cascade
;; ---------------------------------------------------------------------------

(defn- emit-cascade-section [s ctx]
  (case (:ir s)
    :call (list* (symbol (str "." (:name s))) (emit-args (:args s) ctx))
    :prop (list (symbol (str ".-" (:name s))))
    :assign (let [{:keys [lhs op rhs]} s
                  value (emit-expr rhs ctx)]
              (when-not (= "=" op)
                (fail! s (str "unsupported cascade assignment operator " op)))
              (case (:ir lhs)
                :id (list (symbol (str ".-" (:name lhs) "!")) value)
                :prop (list (symbol (str ".-" (:name lhs) "!")) value)
                (fail! s "unsupported cascade assignment target")))
    (fail! s (str "unsupported cascade section " (:ir s)))))

(defn- emit-cascade [node ctx]
  (list* 'doto (emit-expr (:target node) ctx)
         (map #(emit-cascade-section % ctx) (:sections node))))

;; ---------------------------------------------------------------------------
;; dispatch
;; ---------------------------------------------------------------------------

(defn emit-id
  "Emit a bare identifier: mutable box -> deref, locals/fields, static and
  class members, top getters, cross-file project values, implicit-this
  members, otherwise alias resolution. Dart scoping is innermost-first:
  locals shadow class members, class members shadow top-level names."
  [name ctx]
  (let [n (str name)
        sm (static-member-info n ctx)]
    (cond
      (contains? (:mutable ctx) n) (list 'deref (local-symbol n ctx))
      (contains? (:locals ctx) n) (local-symbol n ctx)
      ;; In a constructor body the deftype field is not a local of the
      ;; generated make-fn (its initial value was passed to the constructor
      ;; call), and in a nested function ClojureDart snapshots field locals
      ;; by value; read through `this` in both cases.
      (and (or (:ctor-body? ctx) (:in-fn ctx))
           (:this-sym ctx)
           (contains? (:fields ctx) n))
      (list (symbol (str ".-" n)) (:this-sym ctx))
      (contains? (:fields ctx) n) (symbol n)
      (contains? (:atom-fields ctx) n) (list 'deref (get (:atom-fields ctx) n))
      sm (cond
           (:field sm) (:field sm)
           (:getter sm) (list (:getter sm))
           (:method sm) (:method sm) ; tear-off
           (:setter sm) (fail! {:src nil}
                               (str n " is a write-only static setter")))
      (contains? (get-in ctx [:unit-info :top-getters]) n) (list (symbol n))
      (contains? (get-in ctx [:unit-info :top-vars]) n) (symbol n)
      (contains? (get-in ctx [:unit-info :widget-fns]) n) (symbol n)
      (and (project-value-info n ctx) (not (own-instance-member? n ctx)))
      (let [{:keys [kinds] :as info} (project-value-info n ctx)]
        (cond
          (contains? kinds :var) (project-value-symbol info n)
          (contains? kinds :getter) (list (project-value-symbol info n))
          (contains? kinds :fn) (project-value-symbol info n)
          :else (fail! {:src nil} (str n " is a write-only setter"))))
      (and (:this-sym ctx)
           (not (known-bare? n ctx)))
      (list (symbol (str ".-" n)) (:this-sym ctx))
      :else (alias/qualify n (:aliases ctx)))))

(defn- emit-expr-literal [node ctx]
  (case (:ir node)
    :id (emit-id (:name node) ctx)
    :prefixed-id (emit-chain node ctx)
    :int-lit (:value node)
    :double-lit (if (number? (:value node))
                  (:value node)
                  (fail! node (str "unsupported double literal " (:raw node))))
    :bool-lit (:value node)
    :null-lit nil
    :string-lit (:value node)
    :interp-string (:value node)
    :interp-expr (emit-expr (:expr node) ctx)
    :string-interp (emit-interp node ctx)
    :adjacent-strings (list* 'str (map #(emit-expr % ctx) (:parts node)))
    :this 'this
    :super 'super))

;; ---------------------------------------------------------------------------
;; idiomatic widget threading (M4)
;; ---------------------------------------------------------------------------

(def ^:private default-thread-props
  "Named Dart arguments that f/widget can thread a builder chain through."
  #{"child" "body" "home"})

(declare thread-arg)

(defn- ctor-node?
  "A constructor call node: an InstanceCreationExpression, or — in
  parse-only mode, where imported types are unresolved — a target-less call
  whose name is class-like."
  [node ctx]
  (case (:ir node)
    :new true
    :call (and (nil? (:target node))
               (class-like? (:name node) ctx)
               (not (alias/import-prefix? (:name node) (:aliases ctx)))
               (not (contains? (get-in ctx [:unit-info :top-fns])
                               (str (:name node))))
               (not (contains? (get-in ctx [:unit-info :top-getters])
                               (str (:name node)))))
    false))

(defn- chain-forms
  "Number of forms in the widget chain rooted at a constructor node."
  [node ctx]
  (if-let [{:keys [value]} (thread-arg node ctx)]
    (inc (chain-forms value ctx))
    1))

(defn- thread-arg
  "The named argument to continue a widget chain through: the threadable
  argument (whose value is a constructor node) with the longest chain; ties
  go to the last such argument. nil when there is none."
  [node ctx]
  (let [props (or (:thread-props ctx) default-thread-props)
        cands (keep-indexed
               (fn [i a]
                 (when (and (:name a) (contains? props (:name a)))
                   (let [v (strip-paren (:expr a))]
                     (when (ctor-node? v ctx)
                       {:index i :arg a :value v}))))
               (:args node))]
    (when (seq cands)
      (apply max-key (fn [{:keys [index value]}]
                       [(chain-forms value ctx) index])
             cands))))

(defn- ctor-node-name
  "Class name a constructor node constructs."
  [node]
  (case (:ir node)
    :new (get-in node [:ctor :type :name])
    :call (:name node)))

(defn- emit-ctor-head [node ctx]
  (let [raw (ctor-node-name node)]
    (cond
      (contains? (get-in ctx [:unit-info :widget-fns]) (str raw)) (symbol (str raw))

      (local-class? raw ctx)
      (if (known-ctor? raw (:name (:ctor node)) ctx)
        (ctor-symbol-for raw (:name (:ctor node)))
        (fail! node (str raw " has no constructor "
                         (or (:name (:ctor node)) "new"))))

      (project-type-info raw ctx)
      (let [info (project-type-info raw ctx)]
        (cond
          (:widget? info) (symbol (str (:ns info) "/" raw))
          :else (or (project-ctor-symbol info raw (:name (:ctor node)))
                    (fail! node (str raw " has no constructor "
                                     (or (:name (:ctor node)) "new"))))))

      :else (case (:ir node)
              :new (ctor-symbol (:ctor node) ctx)
              :call (alias/qualify raw (:aliases ctx))))))

(defn- emit-ctor-args [node ctx drop-index]
  (let [raw (ctor-node-name node)
        args (:args node)
        args (if (some? drop-index)
               (vec (concat (take drop-index args) (drop (inc drop-index) args)))
               args)]
    (emit-args (drop-widget-args ctx raw args) ctx)))

(defn- emit-ctor-form
  "Emit a constructor call, optionally removing the argument at `drop-index`
  and/or rendering a zero-argument intermediate chain form bare."
  ([node ctx] (emit-ctor-form node ctx {}))
  ([node ctx {:keys [drop-index bare?]}]
   (let [head (emit-ctor-head node ctx)
         args (emit-ctor-args node ctx drop-index)]
     (if (and bare? (empty? args))
       head
       (list* head args)))))

(defn- emit-chain-tail [node ctx]
  (let [{:keys [index value arg]} (thread-arg node ctx)]
    (if value
      (into [(emit-ctor-form node ctx {:drop-index index :bare? true})
             (symbol (str "." (:name arg)))]
            (emit-chain-tail value ctx))
      [(emit-ctor-form node ctx)])))

(defn- emit-widget-thread
  "`(f/widget A .body B .child C)` for a chain rooted at `node`."
  [node ctx]
  (let [child-prop (symbol ".child")
        forms (remove #(= child-prop %) (emit-chain-tail node ctx))]
    (list* (symbol (str (or (:flutter-alias ctx) "f") "/widget")) forms)))

(defn- thread-widget?
  "True when `node` should be emitted as an f/widget threading form."
  [node ctx]
  (and (= :idiomatic (:mode ctx))
       (ctor-node? node ctx)
       (>= (chain-forms node ctx) 3)))

(defn- emit-set-state
  "Evaluate a `setState` callback inline; state writes swap the state atoms.

  When the widget watches a tick atom (idiomatic state emission), every
  `setState` additionally bumps it so callbacks that write no state atom still
  rebuild (RESEARCH M4.4/M5; Flutter coalesces the extra rebuild)."
  [node ctx]
  (let [args (:args node)
        f (some-> args first :expr)
        body (when (and (= :fn (:ir f)) (empty? (:params f))) (:body f))
        tick (when-some [t (:tick-atom ctx)] (list 'swap! t 'inc))]
    (when-not body
      (fail! node "setState requires a no-argument function literal"))
    (let [form (case (:ir body)
                 :expr-body
                 (let [e (:expr body)]
                   (if (and (contains? #{:postfix :prefix} (:ir e))
                            (contains? #{"++" "--"} (:op e)))
                     (emit-increment e ctx true)
                     (emit-expr e ctx)))

                 :block-body
                 ;; Not a value tail: the form is inlined where the tick (or
                 ;; the enclosing expression) follows.
                 (let [forms ((:emit-body ctx) (:stmts (:block body))
                              (assoc ctx :fn-body? false :value-tail? false))]
                   (cond
                     (empty? forms) nil
                     (= 1 (count forms)) (first forms)
                     :else (list* 'do forms)))

                 (fail! node "unsupported setState body"))]
      (cond
        (nil? tick) form
        (nil? form) tick
        :else (list 'do form tick)))))

(defn- emit-expr-call [node ctx]
  (case (:ir node)
    :new (if (thread-widget? node ctx)
           (emit-widget-thread node ctx)
           (emit-ctor-form node ctx))
    :call (cond
            (and (:state-ctx ctx)
                 (nil? (:target node))
                 (= "setState" (:name node)))
            (emit-set-state node ctx)

            (:target node) (emit-chain node ctx)

            (thread-widget? node ctx) (emit-widget-thread node ctx)

            :else (emit-fn-call node ctx))
    :prop (emit-chain node ctx)
    :call-fn (list* (emit-expr (:fn node) ctx) (emit-args (:args node) ctx))
    :index (let [target (emit-expr (:target node) ctx)
                 index (emit-expr (:index node) ctx)]
             (if (:null-aware node)
               (list 'some-> target (list 'get index))
               (list 'get target index)))
    :cascade (emit-cascade node ctx)))

(def ^:private literal-ir?
  "True for IR nodes that are always compile-time literals."
  #(contains? #{:int-lit :double-lit :string-lit :bool-lit :null-lit} (:ir %)))

(defn- const-record?
  "A record literal whose fields are all literals is `:dart/const` in
  ClojureDart (bound records are inlined, so the upstream record-type
  rendering bug is not hit). Records with computed fields are wrapped in
  `cljd.core/identity` because ClojureDart's `write-type` crashes on the
  positional/named field types it itself builds for non-const records."
  [node]
  (every? #(literal-ir? (:expr %)) (:fields node)))

(defn- emit-expr-coll [node ctx]
  (case (:ir node)
    :record-lit
    (let [rec (form/record-lit
               (map (fn [f]
                      (case (:ir f)
                        :record-pos-field [nil (emit-elem (:expr f) ctx)]
                        :record-named-field [(:name f) (emit-elem (:expr f) ctx)]))
                    (:fields node)))]
      (if (const-record? node) rec (list 'cljd.core/identity rec)))
    :list-lit (emit-list node ctx)
    :set-lit (emit-set node ctx)
    :map-lit (emit-map node ctx)
    :if-element (emit-elem node ctx)
    :for-element (emit-elem node ctx)
    :null-aware-elem (emit-elem node ctx)))

(defn- emit-expr-op [node ctx]
  (case (:ir node)
    :binary (emit-binary node ctx)
    :prefix (emit-prefix node ctx)
    :postfix (emit-postfix node ctx)
    :assign (emit-assign node ctx)
    :ternary (list 'if
                   (emit-expr (:cond node) ctx)
                   (emit-expr (:then node) ctx)
                   (emit-expr (:else node) ctx))
    :is (emit-is node ctx)
    :as (form/hint (type-symbol (:type node) ctx) (emit-expr (:expr node) ctx))
    :await (list 'await (emit-expr (:expr node) ctx))
    :throw (list 'throw (emit-expr (:expr node) ctx))
    :rethrow (if-let [e (:catch-exception ctx)]
               (list 'throw e)
               (fail! node "rethrow outside of a catch clause"))
    :paren (emit-expr (:expr node) ctx)
    :fn (emit-fn node ctx)
    :switch-expr (if-let [f (:emit-switch-expr ctx)]
                   (f node ctx)
                   (fail! node "switch expressions require the statements facade"))))

(defn emit-expr
  "Emit one IR expression node as a cljd form."
  [node ctx]
  (when node
    (case (:ir node)
      (:id :prefixed-id :int-lit :double-lit :bool-lit :null-lit :string-lit
           :interp-string :interp-expr :string-interp :adjacent-strings
           :this :super)
      (emit-expr-literal node ctx)

      (:new :call :prop :call-fn :index :cascade)
      (emit-expr-call node ctx)

      (:record-lit :list-lit :set-lit :map-lit :if-element :for-element
                   :null-aware-elem)
      (emit-expr-coll node ctx)

      (:binary :prefix :postfix :assign :ternary :is :as :await :throw
               :rethrow :paren :fn :switch-expr)
      (emit-expr-op node ctx)

      (fail! node (str "unsupported expression " (:ir node))))))
