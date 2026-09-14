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
    :static-names map of bare class-static names -> emitted symbols, for
                 method bodies that reference statics without qualification
    :class-name  current class (for bare static references in methods)
    :unit-info   whole-unit class/typedef/getter table built by emit.decls
    :type-vars   type parameters in scope (kept bare in type positions)

  Anything not representable fails loudly via `fail!` with the source
  position; no placeholder markers are emitted."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dart2cljd.alias :as alias]
            [dart2cljd.form :as form]
            [dart2cljd.mutable :as mutable]))

(declare emit-expr emit-increment type-symbol)

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
          :type-vars #{}}
         (select-keys opts [:mode :emit-body :emit-stmt :emit-switch-expr
                            :mutable :captured :fields :locals :static-names
                            :class-name :unit-info :type-vars :this-sym
                            :instance-methods :instance-getters
                            :instance-setters])))

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

(defn type-symbol
  "IR type annotation -> cljd type symbol.

  Typedefs declared in the current unit are expanded at use sites; type
  parameters in scope stay bare; type arguments are dropped (ClojureDart
  resolves Dart generics from the declaration site)."
  [node ctx]
  (when node
    (case (:ir node)
      :named-type
      (let [{:keys [prefix name]} node
            name (str name)]
        (cond
          prefix (symbol (str prefix "/" name))
          (contains? (:type-vars ctx) name) (symbol name)
          (typedef-type name ctx) (type-symbol (typedef-type name ctx) ctx)
          (contains? bare-types name) (symbol name)
          :else (alias/qualify name (:aliases ctx))))

      :fn-type 'Function
      :record-type 'Record
      (fail! node (str "unsupported type annotation " (:ir node))))))

(defn- ctor-symbol [ctor ctx]
  (let [base (type-symbol (:type ctor) ctx)]
    (if-let [n (:name ctor)]
      (symbol (str base "." n))
      base)))

(defn emit-args
  "Flatten IR arguments into a cljd argument sequence: positional values
  interleaved with `.name value` pairs for named arguments."
  [args ctx]
  (mapcat (fn [a]
            (let [e (emit-expr (:expr a) ctx)]
              (if-let [n (:name a)]
                [(symbol (str "." n)) e]
                [e])))
          args))

;; ---------------------------------------------------------------------------
;; parameters
;; ---------------------------------------------------------------------------

(defn param-symbol
  "cljd binding symbol for a Dart formal parameter (after renaming, when the
  parameter is a reassigned local). Type annotations are not emitted: the
  generated code relies on ClojureDart's dynamic member resolution, and
  hints would break when a Dart type has no cljd counterpart."
  [p rename _ctx]
  (symbol (get rename (:name p) (:name p))))

(defn emit-params
  "Dart formal parameter list -> cljd fn parameter vector.

  Required positional params first, then Dart optional positional params
  after a `...` delimiter, then Dart named params as `.name default` pairs."
  [params rename ctx]
  (let [required (filter #(= :required-positional (:kind %)) params)
        optional (filter #(= :optional-positional (:kind %)) params)
        named (filter #(= :named (:kind %)) params)]
    (vec
     (concat
      (map #(param-symbol % rename ctx) required)
      (when (seq optional)
        (cons '...
              (mapcat (fn [p]
                        [(param-symbol p rename ctx)
                         (if (:default p)
                           (emit-expr (:default p) ctx)
                           nil)])
                      optional)))
      (mapcat (fn [p]
                (let [s (param-symbol p rename ctx)
                      nsym (symbol (str "." (:name p)))]
                  (if (and (:default p) (not (:required p)))
                    [(vary-meta nsym merge (meta s))
                     (emit-expr (:default p) ctx)]
                    [(vary-meta nsym merge (meta s))])))
              named)))))

(defn- box-form [captured? init]
  (list (if captured? 'atom 'volatile!) init))

(defn emit-callable
  "Emit a function-like node (`:params` + `:body`) into
  `{:params [...] :body [...] :async bool}`.

  Block bodies get a mutability analysis (R1); reassigned parameters are
  renamed and re-bound to a box so mutation compiles."
  [node ctx]
  (let [params (:params node)
        body (:body node)
        async (:async body)
        generator? (and async (re-find #"\*" (str async)))]
    (when generator?
      (fail! body "generators (async*/sync*) are not supported by ClojureDart"))
    (case (:ir body)
      :expr-body
      (let [ctx' (update ctx :locals set/union (set (keep :name params)))]
        {:params (emit-params params {} ctx')
         :body [(emit-expr (:expr body) ctx')]
         :async (some? async)})

      :block-body
      (let [stmts (:stmts (:block body))
            param-names (keep :name params)
            info (mutable/analyze (:block body)
                                  {:params param-names
                                   :exclude (:fields ctx)})
            mutated (set/intersection (:mutable info) (set param-names))
            rename (into {} (map (fn [n] [n (str n "$param")])) mutated)
            ctx' (assoc ctx
                        :mutable (:mutable info)
                        :captured (:captured info)
                        :locals (set/union (:locals ctx) (:declared info)))
            body-forms ((:emit-body ctx') stmts (assoc ctx' :fn-body? true))
            bindings (vec (mapcat (fn [n]
                                    [(symbol n)
                                     (box-form (contains? (:captured info) n)
                                               (symbol (str n "$param")))])
                                  (sort mutated)))
            body-forms (if (seq mutated)
                         [(list* 'let bindings (if (seq body-forms) body-forms [nil]))]
                         body-forms)]
        {:params (emit-params params rename ctx')
         :body body-forms
         :async (some? async)})

      :empty-body
      {:params (emit-params params {} ctx) :body [nil] :async (some? async)}

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
        (if (alias/import-prefix? prefix (:aliases ctx))
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
               (alias/import-prefix? (:name t) (:aliases ctx)))
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

(defn- emit-chain [node ctx]
  (let [[base links] (unchain node ctx)
        null-aware? (boolean (some :null-aware links))]
    (when (nil? base)
      (fail! node "member access without a target"))
    (if (and (static-base? base ctx) (local-class? (:name base) ctx))
      (emit-local-static base links ctx node)
      (if (static-base? base ctx)
        (if (every? #(= :get (:kind %)) links)
          ;; static field/property path -> dotted symbol (m/Colors.grey.shade300)
          (symbol (str (alias/qualify (:name base) (:aliases ctx))
                       (apply str (map #(str "." (:name %)) links))))
          ;; static method call, then optional further steps
          (let [[f & more] links]
            (when-not (= :call (:kind f))
              (fail! node "unsupported static member chain"))
            (thread (list* (alias/qualify-member (:name base) (:name f) (:aliases ctx))
                           (emit-args (:args f) ctx))
                    (map #(link-step % ctx) more)
                    (boolean (some :null-aware links)))))
        (thread (emit-base base ctx)
                (map #(link-step % ctx) links)
                null-aware?)))))

(defn- known-bare?
  "True when a bare name is definitely not an implicit-this member access:
  a local, parameter, top-level function, dart:core name, or referred name."
  [n ctx]
  (or (contains? (:locals ctx) n)
      (contains? (get-in ctx [:unit-info :top-fns]) n)
      (contains? (get-in ctx [:unit-info :top-getters]) n)
      (contains? (get-in ctx [:unit-info :top-setters]) n)
      (contains? alias/core-symbols n)
      (contains? (:referred (:aliases ctx)) n)
      (alias/import-prefix? n (:aliases ctx))))

(defn- emit-fn-call
  "Unqualified call: `foo(...)` (top-level function, constructor, static
  member, or implicit-this method)."
  [node ctx]
  (let [n (:name node)]
    (cond
      (and (local-class? n ctx) (known-ctor? n nil ctx))
      (list* (ctor-symbol-for n nil) (emit-args (:args node) ctx))

      (local-class? n ctx)
      (fail! node (str n " has no unnamed constructor"))

      (class-like? n ctx)
      (list* (alias/qualify n (:aliases ctx)) (emit-args (:args node) ctx))

      (contains? (:static-names ctx) n)
      (list* (get (:static-names ctx) n) (emit-args (:args node) ctx))

      (and (:this-sym ctx)
           (not (known-bare? n ctx)))
      (list* (symbol (str "." n)) (:this-sym ctx) (emit-args (:args node) ctx))

      :else
      (list* (symbol n) (emit-args (:args node) ctx)))))

;; ---------------------------------------------------------------------------
;; operators
;; ---------------------------------------------------------------------------

(def ^:private binary-ops
  {"==" '= "!=" 'not= "<" '< ">" '> "<=" '<= ">=" '>=
   "+" '+ "-" '- "*" '* "/" '/ "~/" 'quot
   "&&" 'and "||" 'or "??" 'or
   "&" 'bit-and "|" 'bit-or "^" 'bit-xor
   "<<" 'bit-shift-left ">>" 'bit-shift-right ">>>" 'unsigned-bit-shift-right})

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

(defn- strip-paren [node]
  (if (= :paren (:ir node)) (recur (:expr node)) node))

(defn- flatten-same [op node ctx]
  (let [n (strip-paren node)]
    (if (and (= :binary (:ir n)) (= op (:op n)))
      (into (flatten-same op (:lhs n) ctx) [(emit-expr (:rhs n) ctx)])
      [(emit-expr node ctx)])))

(defn- emit-binary [node ctx]
  (let [op (:op node)]
    (cond
      (= op "%")
      (list '. (emit-expr (:lhs node) ctx) (symbol "%") (emit-expr (:rhs node) ctx))

      (contains? binary-ops op)
      (list* (binary-ops op) (flatten-same op node ctx))

      :else (fail! node (str "unsupported binary operator " op)))))

(defn- emit-prefix [node ctx]
  (case (:op node)
    "!" (list 'not (emit-expr (:operand node) ctx))
    "-" (list '- (emit-expr (:operand node) ctx))
    "~" (list 'bit-not (emit-expr (:operand node) ctx))
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

(defn- emit-increment
  "`x++` / `++x` (and --) preserving Dart value semantics."
  [node ctx prefix?]
  (let [op (if (= "++" (:op node)) 'inc 'dec)
        target (:operand node)]
    (case (:ir target)
      :id
      (let [n (:name target)]
        (cond
          (contains? (:mutable ctx) n)
          (let [write (list (if (contains? (:captured ctx) n) 'swap! 'vswap!)
                            (symbol n) op)]
            (if prefix?
              write
              (list 'let [(symbol "d2c$old") (list 'deref (symbol n))]
                    write (symbol "d2c$old"))))

          :else
          (let [write (list 'set! (symbol n) (list op (symbol n)))]
            (if prefix?
              write
              (list 'let [(symbol "d2c$old") (symbol n)] write (symbol "d2c$old"))))))

      (:prop :index)
      (let [read (emit-expr target ctx)
            setter (if (= :index (:ir target))
                     (fn [v] (list '. (emit-expr (:target target) ctx)
                                   "[]=" (emit-expr (:index target) ctx) v))
                     (fn [v] (list (symbol (str ".-" (:name target) "!"))
                                   (emit-expr (:target target) ctx) v)))
            write (setter (list op read))]
        (if (or prefix? (simple-target? (:target target)))
          write
          (list 'let [(symbol "d2c$old") read]
                (setter (list op (symbol "d2c$old")))
                (symbol "d2c$old"))))

      (fail! node "unsupported increment/decrement target"))))

(defn- assign-id [lhs op rhs ctx]
  (let [n (:name lhs)
        op (if (= "=" op) nil op)]
    (cond
      (contains? (:mutable ctx) n)
      (let [setter (if (contains? (:captured ctx) n) 'swap! 'vswap!)
            resetter (if (contains? (:captured ctx) n) 'reset! 'vreset!)
            d (list 'deref (symbol n))]
        (case op
          nil (list resetter (symbol n) rhs)
          "??" (list 'if (list 'nil? d) (list resetter (symbol n) rhs) d)
          "&&" (list 'if d (list resetter (symbol n) rhs) d)
          "||" (list 'if d d (list resetter (symbol n) rhs))
          (list setter (symbol n) (compound-fn op) rhs)))

      (contains? (get-in ctx [:unit-info :top-setters]) n)
      (list (symbol (str "set-" n "!")) rhs)

      (and (:this-sym ctx)
           (contains? (:instance-setters ctx) n))
      (list (symbol (str ".-" n "!")) (:this-sym ctx) rhs)

      :else
      (case op
        nil (list 'set! (symbol n) rhs)
        "??" (list 'if (list 'nil? (symbol n))
                   (list 'set! (symbol n) rhs)
                   (symbol n))
        "&&" (list 'if (symbol n)
                   (list 'set! (symbol n) rhs)
                   (symbol n))
        "||" (list 'if (symbol n)
                   (symbol n)
                   (list 'set! (symbol n) rhs))
        (list 'set! (symbol n) (list (compound-fn op) (symbol n) rhs))))))

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
    :prop (assign-prop lhs op rhs ctx)
    :prefixed-id (assign-prop (assoc lhs :ir :prop
                                     :target (id-node (:prefix lhs) (:src lhs)))
                              op rhs ctx)
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

(declare emit-elem)

(defn- spread? [x] (= :spread (:ir x)))

(defn- emit-list [node ctx]
  (let [elems (:elements node)]
    (if (some #(#{:spread :null-aware-elem} (:ir %)) elems)
      (let [plain (remove #(#{:spread :null-aware-elem} (:ir %)) elems)
            extras (keep (fn [e]
                           (case (:ir e)
                             :spread (emit-expr (:expr e) ctx)
                             :null-aware-elem (let [v (emit-expr (:expr e) ctx)]
                                                (list 'if (list 'nil? v)
                                                      []
                                                      (vector v)))
                             nil))
                         elems)]
        (list 'into (mapv #(emit-elem % ctx) plain)
              (list* 'concat extras)))
      (mapv #(emit-elem % ctx) elems))))

(defn- emit-set [node ctx]
  (let [elems (:elements node)]
    (if (some spread? elems)
      (list 'into (apply form/set-lit (map #(emit-elem % ctx) (remove spread? elems)))
            (list* 'concat (map #(emit-expr (:expr %) ctx)
                                (filter spread? elems))))
      (apply form/set-lit (map #(emit-elem % ctx) elems)))))

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

(defn- emit-elem [node ctx]
  (case (:ir node)
    :if-element
    (list 'if (emit-expr (:cond node) ctx)
          (emit-elem (:then node) ctx)
          (when (:else node) (emit-elem (:else node) ctx)))

    :for-element
    (let [loop (:loop node)]
      (if (= :for-in (:ir loop))
        (list 'for [(symbol (:name (:var loop)))
                    (emit-expr (:iterable loop) ctx)]
              (emit-elem (:body node) ctx))
        (fail! node "only for-in elements are supported yet")))

    :null-aware-elem (emit-expr (:expr node) ctx)
    :spread (fail! node "spread outside a collection literal")
    (emit-expr node ctx)))

;; ---------------------------------------------------------------------------
;; functions
;; ---------------------------------------------------------------------------

(defn- emit-fn [node ctx]
  (let [{:keys [params body async]} (emit-callable node ctx)]
    (with-meta (list* 'fn params body)
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
  "Emit a bare identifier: mutable box -> deref, static/class members, top
  getters, implicit-this members, otherwise alias resolution."
  [name ctx]
  (let [n (str name)]
    (cond
      (contains? (:mutable ctx) n) (list 'deref (symbol n))
      (contains? (:static-names ctx) n) (get (:static-names ctx) n)
      (contains? (get-in ctx [:unit-info :top-getters]) n) (list (symbol n))
      (contains? (:locals ctx) n) (symbol n)
      (contains? (:fields ctx) n) (symbol n)
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

(defn- emit-expr-call [node ctx]
  (case (:ir node)
    :new (let [ctor (:ctor node)
               raw (get-in ctor [:type :name])
               cname (:name ctor)]
           (if (local-class? raw ctx)
             (if (known-ctor? raw cname ctx)
               (list* (ctor-symbol-for raw cname) (emit-args (:args node) ctx))
               (fail! node (str raw " has no constructor " (or cname "new"))))
             (list* (ctor-symbol ctor ctx) (emit-args (:args node) ctx))))
    :call (if (:target node)
            (emit-chain node ctx)
            (emit-fn-call node ctx))
    :prop (emit-chain node ctx)
    :call-fn (list* (emit-expr (:fn node) ctx) (emit-args (:args node) ctx))
    :index (let [target (emit-expr (:target node) ctx)
                 index (emit-expr (:index node) ctx)]
             (if (:null-aware node)
               (list 'some-> target (list 'get index))
               (list 'get target index)))
    :cascade (emit-cascade node ctx)))

(defn- emit-expr-coll [node ctx]
  (case (:ir node)
    :record-lit (form/record-lit
                 (map (fn [f]
                        (case (:ir f)
                          :record-pos-field [nil (emit-elem (:expr f) ctx)]
                          :record-named-field [(:name f) (emit-elem (:expr f) ctx)]))
                      (:fields node)))
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
