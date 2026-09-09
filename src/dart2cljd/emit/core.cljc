(ns dart2cljd.emit.core
  "IR expressions -> cljd forms (faithful mode).

  Emission is a pure function of (IR, ctx). `ctx` carries:
    :aliases    an alias context from dart2cljd.alias
    :mode       :faithful (default) or :idiomatic
    :emit-body  fn [stmts ctx] -> vector of forms (injected by the facade to
                avoid a cyclic dependency with dart2cljd.emit.statements)
    :emit-stmt  fn [stmt ctx] -> form

  Anything not yet representable fails loudly via `fail!` with the source
  position; no placeholder markers are emitted."
  (:require [dart2cljd.alias :as alias]
            [dart2cljd.form :as form]))

(declare emit-expr)

(defn fail!
  "Raise a loud, positioned emission error."
  [node msg]
  (throw (ex-info (str "dart2cljd: " msg
                       (when-let [{:keys [line col]} (:src node)]
                         (str " at " line ":" col)))
                  {:dart2cljd/kind :emit :node node})))

(defn ctx
  ([aliases] (ctx aliases nil))
  ([aliases {:keys [mode emit-body emit-stmt]
             :or {mode :faithful}}]
   {:aliases aliases
    :mode mode
    :emit-body emit-body
    :emit-stmt emit-stmt}))

;; ---------------------------------------------------------------------------
;; types, constructors, arguments
;; ---------------------------------------------------------------------------

(defn type-symbol
  "IR type annotation -> cljd type symbol (type arguments are dropped;
  ClojureDart resolves Dart generics from the declaration site)."
  [node ctx]
  (case (:ir node)
    :named-type (let [{:keys [prefix name]} node]
                  (if prefix
                    (symbol (str prefix "/" name))
                    (alias/qualify name (:aliases ctx))))
    :fn-type 'Function
    (fail! node (str "unsupported type annotation " (:ir node)))))

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
;; member-access chains
;; ---------------------------------------------------------------------------

(defn- link? [n]
  (and (map? n) (contains? #{:prop :call :prefixed-id} (:ir n))))

(defn- static-base? [n ctx]
  (and (= :id (:ir n))
       (alias/class-name? (:name n))
       (not (contains? (:local-names (:aliases ctx)) (:name n)))))

(defn- id-node [name src]
  {:ir :id :name name :src src})

(defn- qualified-node [sym src]
  {:ir :qualified :symbol sym :src src})

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

(defn- emit-chain [node ctx]
  (let [[base links] (unchain node ctx)
        null-aware? (boolean (some :null-aware links))]
    (when (nil? base)
      (fail! node "member access without a target"))
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
              null-aware?))))

(defn- emit-fn-call
  "Unqualified call: `foo(...)` (top-level function or implicit this)."
  [node ctx]
  (let [n (:name node)
        f (if (alias/class-name? n)
            (alias/qualify n (:aliases ctx))
            (symbol n))]
    (list* f (emit-args (:args node) ctx))))

;; ---------------------------------------------------------------------------
;; operators
;; ---------------------------------------------------------------------------

(def ^:private binary-ops
  {"==" '= "!=" 'not= "<" '< ">" '> "<=" '<= ">=" '>=
   "+" '+ "-" '- "*" '* "/" '/ "~/" 'quot
   "&&" 'and "||" 'or "??" 'or
   "&" 'bit-and "|" 'bit-or "^" 'bit-xor
   "<<" 'bit-shift-left ">>" 'bit-shift-right ">>>" 'unsigned-bit-shift-right})

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
  (let [x (emit-expr (:operand node) ctx)]
    (case (:op node)
      "!" (list 'not x)
      "-" (list '- x)
      "~" (list 'bit-not x)
      (fail! node (str "unsupported prefix operator " (:op node))))))

(defn- emit-postfix [node ctx]
  (case (:op node)
    "!" (emit-expr (:operand node) ctx)
    (fail! node (str "unsupported postfix operator " (:op node)
                     " (mutation is handled by the mutable-local pass)"))))

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

(defn param-symbol [p _ctx]
  (when-not (= :required-positional (:kind p))
    (fail! p "only required positional parameters are supported yet"))
  (symbol (:name p)))

(defn emit-fn-body [body ctx]
  (case (:ir body)
    :expr-body [(emit-expr (:expr body) ctx)]
    :block-body (let [f (:emit-body ctx)]
                  (if f
                    (f (:stmts (:block body)) ctx)
                    (fail! body "internal: ctx is missing :emit-body")))
    :empty-body [nil]
    (fail! body (str "unsupported function body " (:ir body)))))

(defn- emit-fn [node ctx]
  (let [params (mapv #(param-symbol % ctx) (:params node))]
    (list* 'fn params (emit-fn-body (:body node) ctx))))

;; ---------------------------------------------------------------------------
;; cascade
;; ---------------------------------------------------------------------------

(defn- emit-cascade-section [s ctx]
  (case (:ir s)
    :call (list* (symbol (str "." (:name s))) (emit-args (:args s) ctx))
    :prop (list (symbol (str ".-" (:name s))))
    :assign (let [{:keys [lhs op rhs]} s]
              (when-not (= "=" op)
                (fail! s (str "unsupported cascade assignment operator " op)))
              (case (:ir lhs)
                (:id :prop) (list (symbol (str ".-" (:name lhs) "!"))
                                  (emit-expr rhs ctx))
                (fail! s "unsupported cascade assignment target")))
    (fail! s (str "unsupported cascade section " (:ir s)))))

(defn- emit-cascade [node ctx]
  (list* 'doto (emit-expr (:target node) ctx)
         (map #(emit-cascade-section % ctx) (:sections node))))

;; ---------------------------------------------------------------------------
;; dispatch
;; ---------------------------------------------------------------------------

(defn emit-expr
  "Emit one IR expression node as a cljd form."
  [node ctx]
  (when node
    (case (:ir node)
      :id (alias/qualify (:name node) (:aliases ctx))
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
      :binary (emit-binary node ctx)
      :prefix (emit-prefix node ctx)
      :postfix (emit-postfix node ctx)
      :ternary (list 'if
                     (emit-expr (:cond node) ctx)
                     (emit-expr (:then node) ctx)
                     (emit-expr (:else node) ctx))
      :is (emit-is node ctx)
      :as (form/hint (type-symbol (:type node) ctx) (emit-expr (:expr node) ctx))
      :await (list 'await (emit-expr (:expr node) ctx))
      :throw (list 'throw (emit-expr (:expr node) ctx))
      :paren (emit-expr (:expr node) ctx)
      :fn (emit-fn node ctx)
      :new (list* (ctor-symbol (:ctor node) ctx) (emit-args (:args node) ctx))
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
      :cascade (emit-cascade node ctx)
      :list-lit (emit-list node ctx)
      :set-lit (emit-set node ctx)
      :map-lit (emit-map node ctx)
      :if-element (emit-elem node ctx)
      :for-element (emit-elem node ctx)
      :null-aware-elem (emit-elem node ctx)
      :this 'this
      :super 'super
      (fail! node (str "unsupported expression " (:ir node))))))
