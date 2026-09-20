(ns dart2cljd.ast
  "Normalization from astdump EDN (schema :ast-version 1) to the dart2cljd IR.

  `normalize-node` turns one analyzer node into one IR node; `normalize-doc`
  turns a whole astdump document into `{:ast-version :mode :path :errors
  :resolved :unit}` with `:unit` normalized.

  Failure policy: anything the translator cannot faithfully represent raises
  `ex-info` with `:dart2cljd/kind` (:input, :ast-version, :parse,
  :generic-ast, :unsupported, :schema) and, where available, the offending
  `:src` position. No placeholder markers are ever produced."
  (:require [clojure.string :as str]
            [dart2cljd.ir :as ir]))

;; ---------------------------------------------------------------------------
;; errors
;; ---------------------------------------------------------------------------

(defn- at
  "Render a :src position for diagnostics."
  [{:keys [line col]}]
  (if (and line col) (str " at " line ":" col) ""))

(defn- fail!
  [kind msg data]
  (throw (ex-info (str "dart2cljd: " msg)
                  (assoc data :dart2cljd/kind kind))))

;; ---------------------------------------------------------------------------
;; field helpers (mutually recursive with normalize-node)
;; ---------------------------------------------------------------------------

(declare normalize-node)

(defn- normalize-params
  "Flatten a FormalParameterList node (or nil) into a vector of :param nodes."
  [param-list]
  (mapv normalize-node (or (:params param-list) [])))

(defn- normalize-args
  "Flatten an ArgumentList node (or nil) into a vector of :arg nodes.
  Positional arguments become {:ir :arg :name nil ...}, named arguments keep
  their name."
  [arg-list]
  (mapv (fn [arg]
          (if (= :NamedArgument (:ast arg))
            (normalize-node arg)
            {:ir :arg
             :name nil
             :expr (normalize-node arg)
             :src (:src arg)}))
        (or (:args arg-list) [])))

(defn- normalize-fn-suffix
  "Normalize the raw function-typed parameter suffix map emitted by astdump."
  [v src]
  (when v
    {:ir :fn-suffix
     :type-params (mapv normalize-node (or (:type-params v) []))
     :params (normalize-params (:params v))
     :nullable (:nullable v)
     :src src}))

(defn- normalize-record-fields
  "Normalize RecordLiteral fields: positional expressions are wrapped in
  :record-pos-field so every element of :fields is an IR node."
  [fields]
  (mapv (fn [f]
          (if (= :RecordNamedField (:ast f))
            (normalize-node f)
            {:ir :record-pos-field
             :expr (normalize-node f)
             :src (:src f)}))
        (or fields [])))

(defn- normalize-value
  [raw k mode node-src]
  (let [v (get raw k)]
    (case mode
      :scalar v
      :child (normalize-node v)
      :children (mapv normalize-node (or v []))
      :dotted (when v (apply str v))
      :args (normalize-args v)
      :params (normalize-params v)
      :fn-suffix (normalize-fn-suffix v node-src)
      :record-fields (normalize-record-fields v)
      (fail! :schema
             (str "unknown field mode " mode " for key " k)
             {:mode mode :key k}))))

(defn- normalize-fields
  [raw fields node-src]
  (reduce (fn [m [k mode raw-k]]
            (assoc m k (normalize-value raw (or raw-k k) mode node-src)))
          {}
          fields))

;; ---------------------------------------------------------------------------
;; overrides: tags whose IR is not a straight key-by-key copy
;; ---------------------------------------------------------------------------

(def ^:private overrides
  {;; statements that wrap a declaration: unwrap to the declaration's IR
   :VariableDeclarationStatement
   (fn [raw]
     (assoc (normalize-node (:vars raw)) :src (:src raw)))

   :FunctionDeclarationStatement
   (fn [raw]
     (assoc (normalize-node (:fn raw)) :src (:src raw)))

   ;; old-style typedef Foo(...) -> same :type-alias shape as GenericTypeAlias
   :FunctionTypeAlias
   (fn [raw]
     {:name (:name raw)
      :type-params (mapv normalize-node (or (:type-params raw) []))
      :type {:ir :fn-type
             :src (:src raw)
             :return-type (normalize-node (:return-type raw))
             :type-params (mapv normalize-node (or (:type-params raw) []))
             :params (normalize-params (:params raw))
             :nullable false}})

   ;; set vs map is decided by astdump's syntactic kind inference
   :SetOrMapLiteral
   (fn [raw]
     (case (:kind raw)
       :map {:ir :map-lit
             :const (:const raw)
             :type-args (mapv normalize-node (or (:type-args raw) []))
             :elements (mapv normalize-node (or (:elements raw) []))}
       :set {:ir :set-lit
             :const (:const raw)
             :type-args (mapv normalize-node (or (:type-args raw) []))
             :elements (mapv normalize-node (or (:elements raw) []))}
       (fail! :unsupported
              (str "cannot determine whether this literal is a map or a set"
                   (at (:src raw)) " without type resolution")
              {:tag :SetOrMapLiteral :src (:src raw)})))

   ;; assigned patterns have no keyword/type; keep the :var-pattern shape
   :AssignedVariablePattern
   (fn [raw]
     {:name (:name raw)
      :keyword nil
      :type nil})

   ;; unify the three for-in loop part variants
   :ForEachPartsWithDeclaration
   (fn [raw]
     (let [v (normalize-node (:var raw))]
       {:var v
        :pattern nil
        :final (:final v)
        :iterable (normalize-node (:iterable raw))}))

   :ForEachPartsWithIdentifier
   (fn [raw]
     {:var {:ir :declared-ident
            :final false
            :const false
            :type nil
            :name (:var raw)
            :src (:src raw)}
      :pattern nil
      :final false
      :iterable (normalize-node (:iterable raw))})

   :ForEachPartsWithPattern
   (fn [raw]
     {:var nil
      :pattern (normalize-node (:pattern raw))
      :final (:final raw)
      :iterable (normalize-node (:iterable raw))})

   ;; unify the three C-style for part variants
   :ForPartsWithDeclarations
   (fn [raw]
     {:init (normalize-node (:init raw))
      :pattern nil
      :cond (normalize-node (:cond raw))
      :updaters (mapv normalize-node (or (:updaters raw) []))})

   :ForPartsWithExpression
   (fn [raw]
     {:init (normalize-node (:init raw))
      :pattern nil
      :cond (normalize-node (:cond raw))
      :updaters (mapv normalize-node (or (:updaters raw) []))})

   :ForPartsWithPattern
   (fn [raw]
     {:init nil
      :pattern (normalize-node (:pattern raw))
      :cond (normalize-node (:cond raw))
      :updaters (mapv normalize-node (or (:updaters raw) []))})})

;; ---------------------------------------------------------------------------
;; public API
;; ---------------------------------------------------------------------------

(defn normalize-node
  "Normalize one astdump EDN node into an IR node. Returns nil for nil.

  Throws ex-info for :generic astdump nodes, unknown :ast tags, and malformed
  input. All fields declared by the tag's spec are present on the result."
  [raw]
  (when (some? raw)
    (when-not (map? raw)
      (fail! :input (str "expected an AST node map, got " (pr-str raw))
             {:value raw}))
    (let [tag (:ast raw)
          src (:src raw)]
      (when (:generic raw)
        (fail! :generic-ast
               (str "astdump emitted a :generic node for " tag (at src)
                    "; extend bin/astdump.dart to cover it")
               {:tag tag :src src}))
      (when-not tag
        (fail! :input (str "AST node without :ast tag: " (pr-str raw))
               {:node raw}))
      (let [spec (get ir/node-specs tag)]
        (when-not spec
          (fail! :unsupported
                 (str "unsupported AST node " tag (at src)
                      "; add a spec to dart2cljd.ir/node-specs")
                 {:tag tag :src src}))
        (let [base (cond-> {:ir (:ir spec) :src src}
                     (contains? raw :annotations)
                     (assoc :annotations
                            (mapv normalize-node (:annotations raw)))
                     ;; `--types` resolution facts (a rendered analyzer type,
                     ;; e.g. "String", "Vec", "int?"); parse-only dumps have none.
                     (contains? raw :static-type)
                     (assoc :dart-type (:static-type raw))
                     ;; `--types` resolution facts: what a name refers to
                     ;; (`:local`, `:param`, instance/static/top-level members,
                     ;; `:class`, `:ctor`, ...); see ir.cljc for the table.
                     (contains? raw :element)
                     (assoc :dart-element (:element raw))
                     ;; `--types` resolution facts: extension members are
                     ;; static sugar in Dart (`180.w`, `s.startWith(v)`); the
                     ;; emitter needs the extension's name and library to emit
                     ;; ClojureDart's explicit application form.
                     (contains? raw :extension)
                     (assoc :dart-extension (:extension raw))
                     (contains? raw :extension-uri)
                     (assoc :dart-extension-uri (:extension-uri raw))
                     ;; `--types` resolution facts: the instantiated type
                     ;; arguments of a constructor call, which Dart would infer
                     ;; from the arguments (the generated code loses the
                     ;; argument's static type, so the emitter restores them).
                     (contains? raw :resolved-type-args)
                     (assoc :dart-type-args (:resolved-type-args raw))
                     ;; `--types` write facts on assignment/increment nodes:
                     ;; the element being written and, for fields, the class
                     ;; declaring it (ClojureDart munges field names).
                     (contains? raw :write-element)
                     (assoc :dart-write-element (:write-element raw))
                     (contains? raw :write-field-class)
                     (assoc :dart-write-field-class (:write-field-class raw)))
              fields (if-let [ov (get overrides tag)]
                       (ov raw)
                       (normalize-fields raw (:fields spec) src))]
          (merge base fields))))))

(defn normalize-doc
  "Normalize a whole astdump document.

  Options:
    :fail-on-errors?  when true (default), throw if the Dart parse produced
                      any :error-severity diagnostic.

  Returns {:ast-version 1 :mode ... :path ... :errors [...] :resolved ...
           :unit <IR>}."
  ([doc] (normalize-doc doc nil))
  ([doc {:keys [fail-on-errors?] :or {fail-on-errors? true}}]
   (when-not (map? doc)
     (fail! :input "expected an astdump EDN map" {:value doc}))
   (let [v (:ast-version doc)]
     (when-not (= v ir/ast-version)
       (fail! :ast-version
              (str "unsupported astdump schema version " (pr-str v)
                   " (expected " ir/ast-version ")")
              {:got v :expected ir/ast-version})))
   (let [errors (filterv #(= :error (:severity %)) (:errors doc))]
     (when (and fail-on-errors? (seq errors))
       (fail! :parse
              (str "Dart parse errors: "
                   (str/join "; " (map #(str (:message %) (at %)) errors)))
              {:errors errors})))
   (when-not (contains? doc :unit)
     (fail! :input "astdump output has no :unit" {}))
   (-> doc
       (assoc :unit (normalize-node (:unit doc)))
       (select-keys [:ast-version :mode :path :errors :resolved
                     :fn-typedefs :unit]))))
