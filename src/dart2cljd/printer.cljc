(ns dart2cljd.printer
  "Deterministic pretty-printer for emitted cljd forms.

  The printer never consults the environment: given the same form and
  `*width*` it always produces the same bytes. Layout is driven by a fixed
  set of rules (one per special form) with a generic fallback; there is no
  randomness, no map iteration order dependence (MapLit/SetLit preserve
  order), and no timestamps.

  Public entry points: `flat`, `print-form`, `print-forms`."
  (:require [clojure.string :as str]
            [dart2cljd.form :as form]))

(def ^:dynamic *width*
  "Maximum line width used to decide between flat and multi-line layout."
  100)

;; ---------------------------------------------------------------------------
;; atoms, metadata, flat rendering
;; ---------------------------------------------------------------------------

(defn- pad [n] (apply str (repeat n " ")))

(declare atom-str)

(def ^:private printable-meta
  "Metadata keys the printer renders. Reader :line/:column and other
  incidental keys are ignored."
  #{:tag :const :async :mutable :getter :setter :abstract :mixin :type-only
    :private :dynamic :macro-support :builder :override :deprecated})

(defn- meta-prefix
  "Render whitelisted metadata as ^tag / ^:flag prefixes (sorted for
  determinism)."
  [x]
  (when-let [m (meta x)]
    (apply str
           (for [[k v] (sort-by (comp str key) m)
                 :when v]
             (cond
               (= k :tag) (str "^" v " ")
               (= k :type-params)
               (str "^{:type-params ["
                    (str/join " " (map atom-str v)) "]} ")
               (= k :dart/annotations)
               (str "^{:dart/annotations ["
                    (str/join " " (map pr-str v)) "]} ")
               (printable-meta k) (str "^:" (name k) " ")
               :else nil)))))

(defn atom-str
  "Deterministic rendering of an atom (with metadata prefix for symbols)."
  [x]
  (cond
    (string? x) (pr-str x)
    (nil? x) "nil"
    (symbol? x) (str (meta-prefix x) x)
    :else (str x)))

(defn flat
  "Render `x` on a single line (strings are escaped, so never multi-line)."
  [x]
  (cond
    (form/map-lit? x)
    (str "{" (str/join " " (mapcat (fn [[k v]] [(flat k) (flat v)]) (:pairs x))) "}")

    (form/set-lit? x)
    (str "#{" (str/join " " (map flat (:elems x))) "}")

    (form/hint? x)
    (str "^" (atom-str (:tag x)) " " (flat (:form x)))

    (form/record-lit? x)
    (str "#dart ("
         (str/join " " (map (fn [[n v]]
                              (if n (str "." n " " (flat v)) (flat v)))
                            (:fields x)))
         ")")

    (seq? x)
    (str (meta-prefix x) "(" (str/join " " (map flat x)) ")")

    (vector? x)
    (str (meta-prefix x) "[" (str/join " " (map flat x)) "]")

    (map? x)
    (str (meta-prefix x) "{" (str/join " " (mapcat (fn [[k v]] [(flat k) (flat v)]) x)) "}")

    :else (atom-str x)))

(defn- fits? [x indent]
  (let [s (flat x)]
    (and (not (str/includes? s "\n"))
         (<= (+ indent (count s)) *width*))))

;; ---------------------------------------------------------------------------
;; generic layout helpers
;; ---------------------------------------------------------------------------

(declare pp)

(defn- hang
  "Lay out `open elems close` with the first element hanging on the opening
  line and the rest one per line at `elem-indent`."
  [open elems close indent elem-indent]
  (if (empty? elems)
    (str (pad indent) open close)
    (let [ls (map #(pp % elem-indent) elems)]
      (str (pad indent) open (str/triml (first ls))
           (str/join "" (map #(str "\n" %) (rest ls)))
           close))))

(defn- pair-line
  "Render a key/value pair as `k v` (or `k` newline `v` when too wide) at
  `indent`."
  [k v indent]
  (let [k-str (flat k)]
    (if (fits? v (+ indent (count k-str) 1))
      (str (pad indent) k-str " " (flat v))
      (str (pad indent) k-str "\n" (pp v (+ indent 2))))))

(defn- directive? [x]
  (or (keyword? x)
      (and (symbol? x)
           (str/starts-with? (name x) ".")
           (not (str/starts-with? (name x) ".-")))))

(defn- pp-elems
  "Render call arguments at `indent`, pairing `.named`/`:directive` markers
  with their following value."
  [elems indent]
  (loop [elems (seq elems) out []]
    (if-not elems
      out
      (let [e (first elems)]
        (if (and (directive? e) (next elems))
          (recur (nnext elems) (conj out (pair-line e (second elems) indent)))
          (recur (next elems) (conj out (pp e indent))))))))

(defn- pp-map [pairs indent]
  (if (empty? pairs)
    (str (pad indent) "{}")
    (let [ls (map (fn [[k v]] (pair-line k v (inc indent))) pairs)]
      (str (pad indent) "{" (str/triml (first ls))
           (str/join "" (map #(str "\n" %) (rest ls)))
           "}"))))

;; ---------------------------------------------------------------------------
;; special forms
;; ---------------------------------------------------------------------------

(defn- pp-defn [x indent]
  (let [[head name & more] x
        [doc more] (if (string? (first more))
                     [(first more) (rest more)]
                     [nil more])
        [args & body] more
        header (str "(" head " " (flat name) (when doc (str " " (pr-str doc))) " " (flat args))]
    (if (seq body)
      (str (pad indent) header "\n"
           (str/join "\n" (map #(pp % (+ indent 2)) body))
           ")")
      (str (pad indent) header ")"))))

(defn- pp-def [x indent]
  (let [[head name & [value]] x
        name-str (flat name)]
    (if (nil? value)
      (str (pad indent) "(" head " " name-str ")")
      (let [header (str "(" head " " name-str)]
        (if (fits? value (+ indent (count header) 1))
          (str (pad indent) header " " (flat value) ")")
          (str (pad indent) header "\n" (pp value (+ indent 2)) ")"))))))

(defn- pp-let [x indent]
  (let [[head & more] x
        [bindings & body] more
        b-indent (+ indent (count (str "(" head " [")))
        pairs (partition 2 bindings)
        bind-str (str/join "\n" (map (fn [[k v]] (pair-line k v b-indent)) pairs))]
    (str (pad indent) "(" head " ["
         (str/triml bind-str)
         "]\n"
         (str/join "\n" (map #(pp % (+ indent 2)) body))
         ")")))

(defn- pp-fn [x indent]
  (let [[head & more] x
        [fname more] (if (symbol? (first more))
                       [(first more) (rest more)]
                       [nil more])
        [args & body] more]
    (str (pad indent) "(" head (when fname (str " " fname)) " " (flat args)
         (if (seq body)
           (str "\n" (str/join "\n" (map #(pp % (+ indent 2)) body)) ")")
           ")"))))

(defn- pp-if [x indent]
  (let [[head cond then else] x]
    (str (pad indent) "(" head " " (flat cond) "\n"
         (pp then (+ indent 2)) "\n"
         (pp else (+ indent 2)) ")")))

(defn- pp-when [x indent]
  (let [[head cond & body] x]
    (str (pad indent) "(" head " " (flat cond) "\n"
         (str/join "\n" (map #(pp % (+ indent 2)) body)) ")")))

(defn- pp-do [x indent]
  (str (pad indent) "(do\n"
       (str/join "\n" (map #(pp % (+ indent 2)) (rest x))) ")"))

(defn- pp-cond [x indent]
  (let [[head & clauses] x
        lines (loop [cs clauses out []]
                (if (empty? cs)
                  out
                  (if (next cs)
                    (recur (nnext cs) (conj out (pair-line (first cs)
                                                           (second cs)
                                                           (+ indent 2))))
                    (recur nil (conj out (pp (first cs) (+ indent 2)))))))]
    (str (pad indent) "(" head
         (str/join "" (map #(str "\n" %) lines))
         ")")))

(defn- pp-case [x indent]
  (let [[head expr & clauses] x
        pairs (partition 2 clauses)
        default (when (odd? (count clauses)) (last clauses))]
    (str (pad indent) "(" head " " (flat expr)
         (str/join "" (map (fn [[c r]] (str "\n" (pair-line c r (+ indent 2))))
                           pairs))
         (when default (str "\n" (pp default (+ indent 2))))
         ")")))

(defn- pp-catch [x indent]
  (let [[head & more] x
        binds (vec (take 2 (take-while symbol? more)))
        body (drop (count binds) more)]
    (str (pad indent) "(" head " " (str/join " " (map flat binds))
         (str/join "" (map #(str "\n" (pp % (+ indent 2))) body))
         ")")))

(defn- pp-try [x indent]
  (let [[head & body] x
        handler? (fn [f]
                   (and (seq? f)
                        (contains? #{"catch" "finally"}
                                   (some-> f first name))))
        [stmts handlers] (split-with (complement handler?) body)]
    (str (pad indent) "(" head
         (str/join "" (map #(str "\n" (pp % (+ indent 2))) stmts))
         (str/join "" (map #(str "\n" (pp % (+ indent 2))) handlers))
         ")")))

(defn- pp-thread [x indent]
  (let [[head expr & steps] x]
    (str (pad indent) "(" head " " (flat expr)
         (str/join "" (map #(str "\n" (pp % (+ indent 4))) steps))
         ")")))

(defn- pp-bind-body [x indent]
  (let [[head bindings & body] x]
    (str (pad indent) "(" head " " (flat bindings) "\n"
         (str/join "\n" (map #(pp % (+ indent 2)) body)) ")")))

(defn- pp-ns-clause [c indent]
  (let [[k & reqs] c]
    (str (pad indent) "(" k
         (str/join "" (map #(str "\n" (pp % (+ indent 1))) reqs))
         ")")))

(defn- pp-ns [x indent]
  (let [[head name & clauses] x]
    (str (pad indent) "(" head " " (flat name)
         (str/join "" (map #(str "\n" (pp-ns-clause % (+ indent 2))) clauses))
         ")")))

(defn- pp-generic [x indent]
  (let [head (first x)
        args (rest x)
        head-str (flat head)]
    (if (empty? args)
      (str (pad indent) "(" head-str ")")
      (let [arg-indent (+ indent 2)
            lines (pp-elems args arg-indent)
            ls (concat (butlast lines) [(str (last lines) ")")])]
        (str (pad indent) "(" head-str "\n"
             (str/join "\n" ls))))))

(defn- pp-deftype [x indent]
  (let [[head name fields & body] x
        lines (loop [es (seq body) out []]
                (if-not es
                  out
                  (let [e (first es)]
                    (if (and (keyword? e) (next es))
                      (recur (nnext es)
                             (conj out (pair-line e (second es) (+ indent 2))))
                      (recur (next es) (conj out (pp e (+ indent 2))))))))]
    (str (pad indent) "(" head " " (flat name) "\n"
         (pp fields (+ indent 2))
         (str/join "" (map #(str "\n" %) lines))
         ")")))

(defn- pp-seq [x indent]
  (let [h (when (symbol? (first x)) (name (first x)))
        prefix (meta-prefix x)
        body (case h
               ("defn" "defn-" "defmacro" "defmethod") (pp-defn x indent)
               "def" (pp-def x indent)
               ("let" "letfn" "loop" "when-let" "when-some" "if-let" "if-some")
               (pp-let x indent)
               ("fn" "fn*") (pp-fn x indent)
               ("if" "if-not") (pp-if x indent)
               ("when" "when-not") (pp-when x indent)
               "do" (pp-do x indent)
               "cond" (pp-cond x indent)
               "case" (pp-case x indent)
               "deftype" (pp-deftype x indent)
               "try" (pp-try x indent)
               "catch" (pp-catch x indent)
               ("->" "->>" "some->" "some->>" "doto") (pp-thread x indent)
               ("doseq" "for") (pp-bind-body x indent)
               "ns" (pp-ns x indent)
               (pp-generic x indent))]
    (if (str/blank? prefix)
      body
      (str (pad indent) prefix (str/triml body)))))

(defn- pp [x indent]
  (cond
    ;; these always use their multi-line layout, even when short
    (and (seq? x)
         (symbol? (first x))
         (contains? #{"ns" "defn" "defn-" "defmacro" "deftype"} (name (first x))))
    (pp-seq x indent)

    (fits? x indent) (str (pad indent) (flat x))
    (form/hint? x) (let [tag (atom-str (:tag x))]
                     (str (pad indent) "^" tag " "
                          (str/triml (pp (:form x) (+ indent 1 (count tag) 1)))))
    (form/record-lit? x) (str (pad indent) (flat x))
    (seq? x) (pp-seq x indent)
    (vector? x) (hang "[" (seq x) "]" indent (inc indent))
    (form/set-lit? x) (hang "#{" (:elems x) "}" indent (+ indent 2))
    (form/map-lit? x) (pp-map (:pairs x) indent)
    (map? x) (pp-map (seq x) indent)
    :else (str (pad indent) (flat x))))

(defn print-form
  "Render one form as deterministic cljd source (no trailing newline)."
  ([x] (print-form x nil))
  ([x {:keys [width] :or {width 100}}]
   (binding [*width* width]
     (pp x 0))))

(defn print-forms
  "Render top-level forms separated by a blank line."
  ([forms] (print-forms forms nil))
  ([forms {:keys [width] :or {width 100}}]
   (binding [*width* width]
     (str/join "\n\n" (map #(pp % 0) forms)))))
