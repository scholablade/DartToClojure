(ns dart2cljd.batch
  "M6: batch directory conversion (`--dir`).

  Translates a whole Dart source tree into a ClojureDart tree:

    * discovery walks `:dir` deterministically (relative paths, sorted) and
      applies the default excludes (`*.g.dart`, `*.freezed.dart`,
      `*.mocks.dart`) plus user `:exclude` globs;
    * every discovered file is translated independently; failures are
      collected per file (the run never aborts on the first one) and reported
      with source positions;
    * `:out` receives one `.cljd` per converted input, preserving the tree
      structure; `:ns-prefix` (optional) is prepended to the generated
      namespaces.

  File -> namespace mapping is the inverse of ClojureDart's `ns-to-paths`
  (`foo/bar_baz.dart` -> `foo.bar-baz`), so the generated tree compiles as a
  ClojureDart source tree.

  Directive policy: `part`, `part of` and `export` are not representable in
  ClojureDart and fail loudly per file; `library` names are ignored (the
  generated ns comes from the output path).

  Public entry point: `convert-dir`. The map it returns is pure data (the CLI
  prints it); nothing here calls System/exit."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dart2cljd.alias :as alias]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.frontend :as frontend]
            [dart2cljd.idiomatic :as idiomatic]
            [dart2cljd.printer :as printer])
  (:import (java.nio.file FileSystems PathMatcher Paths)))

;; ---------------------------------------------------------------------------
;; discovery
;; ---------------------------------------------------------------------------

(def default-excludes
  "Generated Dart files skipped by default (file-name globs)."
  ["*.g.dart" "*.freezed.dart" "*.mocks.dart"])

(defn- path-matcher ^PathMatcher [pattern]
  (.getPathMatcher (FileSystems/getDefault) (str "glob:" pattern)))

(defn- matcher-matches?
  [^PathMatcher m ^String path]
  (.matches m (Paths/get path (into-array String []))))

(defn- rel-path
  [^java.io.File base ^java.io.File f]
  (-> (.relativize (.toPath base) (.toPath f))
      str
      (str/replace (str java.io.File/separatorChar) "/")))

(defn discover
  "Deterministic discovery of the .dart files under `:dir`.

  Returns `{:files [rel ...] :skipped [{:file rel :reason :excluded} ...]}`,
  both sorted by relative path. `:exclude` globs are matched against the
  relative path; a pattern without `/` also matches the file name (so
  `--exclude '*.g.dart'` works at any depth)."
  [{:keys [dir exclude]}]
  (let [base (java.io.File. (str dir))]
    (when-not (.isDirectory base)
      (throw (ex-info (str "dart2cljd: --dir is not a directory: " dir)
                      {:dart2cljd/kind :batch :dir (str dir)})))
    (let [name-matches (into (mapv path-matcher default-excludes)
                             (map path-matcher
                                  (remove #(str/includes? % "/") exclude)))
          path-matches (mapv path-matcher exclude)
          all (->> (file-seq base)
                   (filter #(.isFile ^java.io.File %))
                   (filter #(str/ends-with? (.getName ^java.io.File %) ".dart"))
                   (map #(rel-path base %))
                   sort)
          excluded? (fn [rel]
                      (let [name (last (str/split rel #"/"))]
                        (or (some #(matcher-matches? % name) name-matches)
                            (some #(matcher-matches? % rel) path-matches))))]
      {:files (vec (remove excluded? all))
       :skipped (vec (for [rel all :when (excluded? rel)]
                       {:file rel :reason :excluded}))})))

;; ---------------------------------------------------------------------------
;; paths and directives
;; ---------------------------------------------------------------------------

(defn path->ns
  "Relative Dart path -> generated cljd namespace symbol.

  `foo/bar_baz.dart` -> `foo.bar-baz`, the inverse of ClojureDart's
  `ns-to-paths` (`_` becomes `-` in every segment). `:ns-prefix` is
  prepended. Fails loudly for segments containing `-` or `:` (such paths
  cannot round-trip through `ns-to-paths`)."
  [rel {:keys [ns-prefix]}]
  (let [segments (str/split (str/replace rel #"\.dart$" "") #"/")]
    (doseq [s segments]
      (when (or (str/includes? s "-") (str/includes? s ":"))
        (throw (ex-info (str "dart2cljd: cannot map path segment " (pr-str s)
                             " in " rel " to a namespace (characters `-`/`:`)")
                        {:dart2cljd/kind :batch :file rel :segment s}))))
    (symbol (str (when ns-prefix (str ns-prefix "."))
                 (str/join "." (map #(str/replace % "_" "-") segments))))))

(def ^:private unsupported-directives
  {:part "`part` directives are not supported by ClojureDart; merge the part files by hand"
   :part-of "`part of` files are not supported by ClojureDart; merge the part files by hand"
   :export "`export` directives are not supported by ClojureDart"})

(defn check-directives!
  "Fail loudly when a unit carries a directive the translator cannot
  represent (`part`, `part of`, `export`)."
  [unit rel]
  (doseq [d (:directives unit)
          :let [kind (:ir d)]
          :when (contains? unsupported-directives kind)]
    (let [{:keys [line col]} (:src d)]
      (throw (ex-info (str "dart2cljd: " (get unsupported-directives kind)
                           (when (and line col) (str " at " line ":" col)))
                      {:dart2cljd/kind :batch
                       :file rel
                       :directive kind
                       :node d})))))

;; ---------------------------------------------------------------------------
;; package name (for own-`package:` import resolution)
;; ---------------------------------------------------------------------------

(defn pubspec-package
  "Read `name:` from the nearest pubspec.yaml at or above `dir` (nil when
  there is none)."
  [dir]
  (loop [d (.getCanonicalFile (java.io.File. (str dir)))]
    (when d
      (let [p (java.io.File. d "pubspec.yaml")]
        (if (.isFile p)
          (some-> (re-find #"(?m)^name:\s*([A-Za-z0-9_-]+)" (slurp p)) second)
          (recur (.getParentFile d)))))))

;; ---------------------------------------------------------------------------
;; project index (cross-file correctness, M6 phase 2)
;; ---------------------------------------------------------------------------

(defn- parent-dir [rel]
  (str/join "/" (butlast (str/split rel #"/"))))

(defn resolve-relative
  "Resolve a relative Dart import URI against the importing file's directory
  (both relative to `:dir`). Returns the normalized relative path, or
  ::missing when the URI escapes the converted tree."
  [file uri]
  (let [dir (parent-dir file)
        parts (concat (when (seq dir) (str/split dir #"/"))
                      (str/split uri #"/"))]
    (loop [ps parts out []]
      (cond
        (empty? ps) (str/join "/" out)
        (= (first ps) ".") (recur (rest ps) out)
        (= (first ps) "..") (if (seq out)
                              (recur (rest ps) (pop out))
                              ::missing)
        :else (recur (rest ps) (conj out (first ps)))))))

(defn project-target
  "Generated ns for an import URI that points into the converted tree, nil
  for external libraries (`dart:*`, other packages), or ::missing for a
  relative / own-`package:` URI that does not resolve to a converted file."
  [index file uri]
  (cond
    (nil? uri) nil
    (str/starts-with? uri "dart:") nil
    (str/starts-with? uri "package:")
    (let [[pkg rest] (str/split (subs uri (count "package:")) #"/" 2)]
      (if (and rest (= pkg (:package index)))
        (get (:file->ns index) rest ::missing)
        nil))
    (str/starts-with? uri "/") nil
    :else
    (get (:file->ns index) (resolve-relative file uri) ::missing)))

(defn- declared-types
  "Type declarations of a unit relevant to cross-file references."
  [unit]
  (for [d (:decls unit)
        :when (contains? #{:class :enum :mixin :type-alias} (:ir d))]
    [(:name d) (:ir d)]))

(defn- statics-map
  "Generated ns-level static members of a class: member name -> set of kinds
  (:field :getter :method :setter; a getter/setter pair shares a name)."
  [ci]
  (reduce (fn [m [name kind]]
            (update m name (fnil conj #{}) kind))
          {}
          (concat
           (for [n (:static-fields ci)] [n :field])
           (for [n (:static-getters ci)] [n :getter])
           (for [n (:static-setters ci)] [n :setter])
           (for [n (:static-methods ci)] [n :method])
           (for [c (:ctors ci) :when c] [c :ctor]))))

(defn- type-info
  [name ns kind widget-plan ui]
  (let [ci (get (:classes ui) name)
        kind (if (= :type-alias kind) :typedef kind)]
    (cond-> {:ns ns :kind kind}
      ci (assoc :ctors (or (:ctors ci) #{})
                :statics (statics-map ci))
      widget-plan (assoc :widget? true :drop-args (:drop-args widget-plan))
      (nil? ci) (assoc :ctors #{} :statics {}))))

(defn- values-map
  "Top-level values of a unit as `name -> {:ns ns :kinds #{...} :final? bool}`.

  Kinds are :fn :getter :setter :var; a getter and a setter with the same
  name merge into one entry (Dart allows the pair). `:final?` marks const and
  final vars, which must not be assigned."
  [ns ui]
  (reduce (fn [m [name kind extra]]
            (update m name
                    (fn [e]
                      (cond-> (-> e
                                  (assoc :ns ns)
                                  (update :kinds (fnil conj #{}) kind))
                        extra (assoc :final? true)))))
          {}
          (concat
           (for [n (:top-fns ui)] [n :fn nil])
           (for [n (:top-getters ui)] [n :getter nil])
           (for [n (:top-setters ui)] [n :setter nil])
           (for [[n m] (:top-vars ui)]
             [n :var (or (:const m) (:final m))]))))

(defn- add-index-entries
  "Add `entries` (in sorted file order) under `key`, first-wins with a
  duplicate warning that names both files."
  [acc key label entries]
  (reduce (fn [acc {:keys [file name info]}]
            (if-let [prev (get-in acc [key name])]
              (update acc :warnings conj
                      (str "duplicate " label " " name " in " (:file prev)
                           " and " file "; " (:file prev) " wins"))
              (assoc-in acc [key name] (assoc info :file file))))
          acc
          entries))

(defn- typedef-target-symbol
  "Resolve a typedef's target type to the symbol the use site should emit,
  using the *declaring* file's emission context (its imports, local names and
  the current project type map). Throws when the target is a bare type
  parameter (unrepresentable at use sites)."
  [d unit types]
  (let [ui (decls/unit-info unit)
        local-names (set (map first (declared-types unit)))
        aliases (decls/unit-aliases unit {:project-types types
                                          :local-names local-names})
        ctx (core/ctx aliases {:unit-info ui
                               :type-vars (set (keep :name (:type-params d)))})
        sym (core/type-symbol (:type d) ctx)]
    (when (contains? (set (keep :name (:type-params d))) (str sym))
      (throw (ex-info (str "typedef " (:name d) " resolves to type parameter "
                           sym)
                      {:dart2cljd/kind :batch :typedef (:name d)})))
    sym))

(defn- resolve-typedefs
  "Iteratively expand project typedef targets until a fixpoint, so typedef
  chains (also across files) resolve. Typedefs that stay unresolved (cycles,
  type-parameter targets) keep no target and fail loudly at their use sites."
  [types parsed]
  (loop [types types passes 0]
    (if (>= passes 10)
      types
      (let [progress (volatile! false)
            types' (reduce (fn [types {:keys [unit]}]
                             (reduce (fn [types d]
                                       (if (nil? (:typedef-target (get types (:name d))))
                                         (if-let [sym (try
                                                        (typedef-target-symbol d unit types)
                                                        (catch Exception _ nil))]
                                           (do (vswap! progress (constantly true))
                                               (assoc-in types [(:name d) :typedef-target] sym))
                                           types)
                                         types))
                                     types
                                     (filter #(= :type-alias (:ir %)) (:decls unit))))
                           types
                           parsed)]
        (if @progress (recur types' (inc passes)) types')))))

(defn build-index
  "Pass 1 of batch conversion: build the cross-file index from parsed files
  (`[{:file rel :unit unit} ...]`, sorted by path).

  Returns `{:file->ns ... :package ... :project-types ... :project-values ...
  :warnings [...]}`. Type and value names are global to the generated
  project; duplicate declarations are a warning and first-wins by sorted
  path (deterministic)."
  [parsed opts]
  (let [file->ns (into {} (map (fn [{:keys [file]}] [file (path->ns file opts)]))
                       parsed)
        entries (for [{:keys [file unit]} parsed
                      :let [ui (decls/unit-info unit)
                            widgets (when (= :idiomatic (:mode opts))
                                      (:widgets (idiomatic/plan unit)))
                            ns (get file->ns file)]
                      [name kind] (declared-types unit)]
                  {:file file :name name
                   :info (type-info name ns kind (get widgets name) ui)})
        value-entries (for [{:keys [file unit]} parsed
                            :let [ns (get file->ns file)
                                  ui (decls/unit-info unit)]
                            [name info] (values-map ns ui)]
                        {:file file :name name :info info})
        base (-> {:project-types {} :project-values {} :warnings []}
                 (add-index-entries :project-types "type" entries)
                 (add-index-entries :project-values "value" value-entries))
        base (update base :project-types #(resolve-typedefs % parsed))]
    (assoc base :file->ns file->ns :package (:package opts))))

;; ---------------------------------------------------------------------------
;; import resolution (per file)
;; ---------------------------------------------------------------------------

(def reserved-aliases
  "Names that cannot be used as a cljd `:as` alias (special forms)."
  #{"if" "when" "when-not" "let" "letfn" "fn" "def" "defn" "do" "loop"
    "recur" "case" "cond" "try" "catch" "finally" "throw" "new" "set!"
    "quote" "var" "for" "doseq" "dotimes" "and" "or" "not" "nil" "true"
    "false" "this" "super" "ns" "require" "import" "deftype" "reify"})

(defn- alias-candidate
  "Deterministic alias for a generated project ns: its last segment, made
  safe for `:as` (reserved words get a `-lib` suffix)."
  [ns]
  (let [base (last (str/split (name ns) #"\."))]
    (if (contains? reserved-aliases base) (str base "-lib") base)))

(defn- fresh-alias
  "First free alias from `base`, `base-2`, `base-3`, ..."
  [base taken]
  (loop [cand base n 2]
    (if (contains? taken cand)
      (recur (str base "-" n) (inc n))
      cand)))

(defn file-resolver
  "Per-file import resolver: Dart import URI -> `{:ns ... :alias ...}` for
  imports that resolve into the converted tree (symbol requires).

  `dart:*` and external packages return nil (string requires, unchanged).
  A relative or own-`package:` URI that does not resolve to a converted file
  is a loud per-file failure: the generated tree would import a library that
  does not exist. Auto-suggested aliases are unique within the file
  (deterministically: last segment, then `-2`, `-3`, ...)."
  [index file unit]
  (let [imports (filter #(= :import (:ir %)) (:directives unit))
        classified (mapv (fn [imp]
                           (let [uri (get-in imp [:uri :value])]
                             {:imp imp :uri uri
                              :target (project-target index file uri)}))
                         imports)
        fixed (into #{}
                    (concat (keep :as imports)
                            (keep (fn [{:keys [uri target]}]
                                    (when (nil? target)
                                      (alias/suggest-alias uri)))
                                  classified)))
        taken (volatile! fixed)
        entries (reduce
                 (fn [m {:keys [imp uri target]}]
                   (case target
                     nil m
                     ::missing
                     (let [{:keys [line col]} (get-in imp [:uri :src])]
                       (throw (ex-info (str "dart2cljd: cannot resolve import "
                                            (pr-str uri)
                                            (when (and line col)
                                              (str " at " line ":" col))
                                            ": no converted file matches it")
                                       {:dart2cljd/kind :batch
                                        :file file :uri uri :node imp})))
                     (let [a (or (:as imp)
                                 (fresh-alias (alias-candidate target) @taken))]
                       (vswap! taken conj a)
                       (assoc m uri {:ns target :alias a}))))
                 {}
                 classified)]
    (fn [uri] (get entries uri))))

;; ---------------------------------------------------------------------------
;; per-file conversion
;; ---------------------------------------------------------------------------

(defn parse-unit
  "Parse a Dart file (path relative to `:dir`) and return its normalized unit."
  [rel opts]
  (let [path (java.io.File. (str (:dir opts)) rel)]
    (:unit (ast/normalize-doc (frontend/astdump-file (.getPath path) opts)))))

(defn- emit-source
  "Emit one unit for batch output (ns from the path, project index attached,
  mode honored)."
  [unit rel ns {:keys [mode resolver project-types project-values]}]
  (printer/print-forms
   (decls/emit-unit unit
                    (cond-> {:ns ns :path rel
                             :resolver resolver
                             :project-types project-types
                             :project-values project-values}
                      (= :idiomatic mode) (assoc :mode :idiomatic)))))

(defn- file-project-values
  "Project values declared in *other* files: same-file values stay bare
  (namespace-level references need no qualification)."
  [index rel]
  (let [ns (get (:file->ns index) rel)]
    (into {} (remove (fn [[_ info]] (= ns (:ns info))))
          (:project-values index))))

(defn convert-one
  "Translate one parsed unit and write `<out>/<rel>.cljd`.

  Returns `{:file rel :out path :ns ns}`."
  [rel unit index {:keys [out] :as opts}]
  (check-directives! unit rel)
  (let [ns (path->ns rel opts)
        resolver (file-resolver index rel unit)
        source (str (emit-source unit rel ns (assoc opts
                                                    :resolver resolver
                                                    :project-types (:project-types index)
                                                    :project-values (file-project-values index rel)))
                    "\n")
        out-file (java.io.File. (str out) (str/replace rel #"\.dart$" ".cljd"))]
    (io/make-parents out-file)
    (spit out-file source)
    {:file rel :out (.getPath out-file) :ns ns}))

;; ---------------------------------------------------------------------------
;; batch runner
;; ---------------------------------------------------------------------------

(defn summary
  "Deterministic one-line report: `converted N, skipped M, failed K`."
  [{:keys [converted skipped failed]}]
  (str "converted " (count converted)
       ", skipped " (count skipped)
       ", failed " (count failed)))

(defn convert-dir
  "Translate every Dart file under `:dir` into `:out`.

  Options:
    :dir         source directory (required)
    :out         output directory (required)
    :ns-prefix   namespace prefix for generated files (optional)
    :exclude     additional exclusion globs (vector)
    :package     Dart package name for own-`package:` imports (optional;
                 defaults to the nearest pubspec.yaml `name:`)
    :mode        :faithful (default) | :idiomatic
    :root/:dart  astdump frontend options (dart2cljd.frontend)

  Pass 1 parses every file and builds the cross-file project index (imports,
  type -> namespace, constructors/statics); pass 2 emits with that index, so
  cross-file imports become symbol requires and cross-file type references
  resolve to the generated namespaces.

  Returns `{:converted [...] :skipped [...] :failed [...] :warnings [...]
  :package ... :exit 0|1}`: the run never aborts on a per-file failure, it
  collects them all (each failure carries the exception, whose message has
  file/line/column)."
  [opts]
  (when-not (:dir opts)
    (throw (ex-info "dart2cljd: convert-dir requires :dir"
                    {:dart2cljd/kind :batch :options opts})))
  (when-not (:out opts)
    (throw (ex-info "dart2cljd: convert-dir requires :out"
                    {:dart2cljd/kind :batch :options opts})))
  (let [{:keys [files skipped]} (discover opts)
        opts (assoc opts :package (or (:package opts) (pubspec-package (:dir opts))))
        parsed (mapv (fn [rel]
                       (try
                         (let [unit (parse-unit rel opts)]
                           (check-directives! unit rel)
                           {:file rel :unit unit})
                         (catch Exception e {:file rel :error e})))
                     files)
        ok (into [] (filter :unit) parsed)
        parse-failed (into [] (remove :unit) parsed)
        index (build-index ok opts)
        emitted (mapv (fn [{:keys [file unit]}]
                        (try
                          {:ok (convert-one file unit index opts)}
                          (catch Exception e {:file file :error e})))
                      ok)
        converted (into [] (keep :ok) emitted)
        failed (->> (concat parse-failed (remove :ok emitted))
                    (sort-by :file)
                    vec)]
    {:converted converted
     :skipped skipped
     :failed failed
     :warnings (:warnings index)
     :package (:package index)
     :exit (if (empty? failed) 0 1)}))
