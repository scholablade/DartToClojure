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

  Directive policy: `part`/`part of` libraries are assembled before emission
  (M9: a library's part declarations are merged into its namespace and part
  files produce no output of their own); `export` is supported through the
  generated exports shim; `library` names are ignored (the generated ns comes
  from the output path).

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
            [dart2cljd.library :as library]
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

(defn resolve-relative
  "Resolve a relative Dart URI against the declaring file's directory (both
  relative to `:dir`). Returns the normalized relative path, or
  `:dart2cljd.batch/missing` when the URI escapes the converted tree."
  [file uri]
  (let [rel (library/resolve-relative file uri)]
    (if (= rel library/missing)
      ::missing
      rel)))

(defn library-units
  "Group parsed files into libraries with their `part` files merged (M9).

  See `dart2cljd.library/group`; `opts` may carry `:excluded` and
  `:parse-failed` sets. Returns `{:libraries :parts :errors}`."
  [parsed opts]
  (library/group parsed opts))

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

(defn- private-name?
  "Dart private names (`_x`) are library-scoped: they can never be referenced
  from another converted file, so the project index leaves them out (no
  spurious duplicate warnings when two libraries each declare `_helper`)."
  [n]
  (and (string? n) (str/starts-with? n "_")))

;; ---------------------------------------------------------------------------
;; exports (barrel files)
;; ---------------------------------------------------------------------------

(defn export-target
  "Resolve an `export` URI: `{:ns ns}` for a converted project file,
  `{:external uri}` for `dart:*` / other packages / unconverted relative
  URIs, `::missing` for a relative or own-`package:` URI that does not match a
  converted file."
  [index file uri]
  (cond
    (nil? uri) nil
    (str/starts-with? uri "dart:") {:external uri}
    (str/starts-with? uri "package:")
    (let [[pkg rest] (str/split (subs uri (count "package:")) #"/" 2)]
      (if (and rest (= pkg (:package index)))
        (let [ns (get (:file->ns index) rest ::missing)]
          (if (= ::missing ns) ::missing {:ns ns}))
        {:external uri}))
    (str/starts-with? uri "/") {:external uri}
    :else
    (let [rel (resolve-relative file uri)]
      (if (= ::missing rel)
        ::missing
        (let [ns (get (:file->ns index) rel ::missing)]
          (if (= ::missing ns) ::missing {:ns ns}))))))

(defn- direct-exports
  "Direct exports of a file: `{:project #{ns} :external #{uri} :missing
  [uri]}`."
  [index file unit]
  (reduce (fn [acc d]
            (let [uri (get-in d [:uri :value])
                  t (export-target index file uri)]
              (cond
                (= ::missing t) (update acc :missing conj uri)
                (contains? t :ns) (update acc :project conj (:ns t))
                (contains? t :external) (update acc :external conj (:external t))
                :else acc)))
          {:project #{} :external #{} :missing []}
          (filter #(= :export (:ir %)) (:directives unit))))

(defn- export-closures
  "Transitive export closures per file: `{file {:project #{ns} :external
  #{uri} :missing [uri]}}`, expanding project exports through their own
  closures until a fixpoint."
  [direct file->ns]
  (let [ns->file (into {} (map (fn [[f ns]] [ns f])) file->ns)]
    (loop [closures direct passes 0]
      (if (>= passes 10)
        closures
        (let [progress (volatile! false)
              next (into {}
                         (for [[file {:keys [project external missing]}] closures]
                           (let [project (into project
                                               (mapcat (fn [ns]
                                                         (get-in closures
                                                                 [(ns->file ns) :project]
                                                                 #{})))
                                               project)
                                 external (into external
                                                (mapcat (fn [ns]
                                                          (get-in closures
                                                                  [(ns->file ns) :external]
                                                                  #{})))
                                                project)]
                             [file {:project project :external external
                                    :missing missing}])))]
          (when (not= next closures) (vreset! progress true))
          (if @progress (recur next (inc passes)) next))))))

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
                      [name kind] (declared-types unit)
                      :when (not (private-name? name))]
                  {:file file :name name
                   :info (type-info name ns kind (get widgets name) ui)})
        value-entries (for [{:keys [file unit]} parsed
                            :let [ns (get file->ns file)
                                  ui (decls/unit-info unit)]
                            [name info] (values-map ns ui)
                            :when (not (private-name? name))]
                        {:file file :name name :info info})
        mixin-entries (for [{:keys [file unit]} parsed
                            d (:decls unit)
                            :when (and (= :mixin (:ir d))
                                       (not (private-name? (:name d))))]
                        {:file file :name (:name d)
                         :info {:decl d :kind :mixin
                                :ns (get file->ns file)}})
        base (-> {:project-types {} :project-values {} :project-mixins {}
                  :warnings []}
                 (add-index-entries :project-types "type" entries)
                 (add-index-entries :project-values "value" value-entries)
                 (add-index-entries :project-mixins "mixin" mixin-entries))
        base (update base :project-types #(resolve-typedefs % parsed))
        project-index (assoc base :file->ns file->ns :package (:package opts))
        direct (into {} (for [{:keys [file unit]} parsed]
                          [file (direct-exports project-index file unit)]))
        ;; Every external URI imported anywhere: parse-only mode cannot say
        ;; which library an otherwise-unresolved name comes from, so the
        ;; generated shim re-exports these too and unresolved names are
        ;; routed through it (M8).
        external-imports (into #{}
                                (for [{:keys [file unit]} parsed
                                      d (:directives unit)
                                      :when (= :import (:ir d))
                                      :let [uri (get-in d [:uri :value])]
                                      :when (and uri
                                                 (not (contains? alias/implicit-uris uri))
                                                 (nil? (project-target project-index
                                                                       file uri)))]
                                  uri))]
    (-> base
        (assoc :file->ns file->ns
               :package (:package opts)
               :external-imports external-imports
               :exports (export-closures direct file->ns)))))

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
  "Per-file import resolution.

  Returns `{:resolver fn :wildcard? bool :external-uris #{}}`:

    * `resolver` maps a Dart import URI to `{:ns ... :alias ...}`, plus the
      transitive project exports of the target (`:project-exports`) and
      whether it re-exports external packages (`:external?`);
    * `wildcard?` is true when an imported file re-exports external packages:
      the emitter must then require the exports shim and route otherwise
      unresolved names through it;
    * `external-uris` is the union of external export URIs reachable through
      the file's imports (used for diagnostics).

  `dart:*` and external packages return nil (string requires, unchanged).
  A relative or own-`package:` URI that does not resolve to a converted file
  is a loud per-file failure: the generated tree would import a library that
  does not exist. Auto-suggested aliases are unique within the file
  (deterministically: last segment, then `-2`, `-3`, ...)."
  [index file unit]
  (let [imports (filter #(= :import (:ir %)) (:directives unit))
          exports (:exports index)
          ns->file (into {} (map (fn [[f ns]] [ns f])) (:file->ns index))
        classified (mapv (fn [imp]
                           (let [uri (get-in imp [:uri :value])
                                 target (project-target index file uri)
                                 target-file (when (symbol? target)
                                               (get ns->file target))
                                 closure (when target-file
                                           (get exports target-file
                                                {:project #{} :external #{}}))]
                             {:imp imp :uri uri :target target
                              :exports closure}))
                         imports)
        fixed (into #{}
                    (concat (keep :as imports)
                            (keep (fn [{:keys [uri target]}]
                                    (when (nil? target)
                                      (alias/suggest-alias uri)))
                                  classified)))
        taken (volatile! fixed)
        entries (reduce
                 (fn [m {:keys [imp uri target exports]}]
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
                                 (fresh-alias (alias-candidate target) @taken))
                           project-exports (-> (:project exports)
                                               (disj target)
                                               sort
                                               vec)]
                       (vswap! taken conj a)
                       (assoc m uri {:ns target :alias a
                                     :project-exports project-exports
                                     :external? (boolean (seq (:external exports)))}))))
                 {}
                 classified)
        external-uris (into #{} (mapcat #(get-in % [:exports :external]))
                            classified)
        ;; A barrel file re-exports external packages itself: its own
        ;; otherwise-unresolved names (and those of its importers) can only be
        ;; resolved through the generated exports shim.
        own-external (get-in index [:exports file :external])
        ;; Direct external imports of this file: parse-only mode cannot tell
        ;; which library an unresolved name comes from, so with the shim
        ;; enabled they are routed through it as well (M8).
        external-imports (into #{} (keep (fn [{:keys [uri target]}]
                                           (when (nil? target) uri)))
                               classified)]
    {:resolver (fn [uri] (get entries uri))
     :wildcard? (boolean (or (seq external-uris) (seq own-external)))
     :aliases (into #{} (map :alias (vals entries)))
     :external-uris (into external-uris own-external)
     :external-imports external-imports}))

;; ---------------------------------------------------------------------------
;; per-file conversion
;; ---------------------------------------------------------------------------

(defn parse-unit
  "Parse a Dart file (path relative to `:dir`) and return its normalized unit.

  With `:types` the frontend dumps the analyzer-resolved unit (expression
  `:static-type` facts); when the file cannot be resolved the conversion falls
  back to the syntactic passes and warns, so a missing `dart pub get` in the
  source project is visible."
  [rel opts]
  (let [path (java.io.File. (str (:dir opts)) rel)
        doc (frontend/astdump-file (.getPath path) opts)]
    (when (and (:types opts) (false? (:resolved doc)))
      (println (str "dart2cljd: warning: --types could not resolve " rel
                    " (run `dart pub get` in the source project); using the "
                    "syntactic passes for it")))
    (:unit (ast/normalize-doc doc))))

(defn- emit-source
  "Emit one unit for batch output (ns from the path, project index attached,
  mode and exports shim honored)."
  [unit rel ns {:keys [mode resolver project-types project-values
                       project-mixins exports-uri wildcard-alias fn-typedefs]}]
  (printer/print-forms
   (decls/emit-unit unit
                    (cond-> {:ns ns :path rel
                             :resolver resolver
                             :project-types project-types
                             :project-values project-values
                             :project-mixins project-mixins
                             :exports-uri exports-uri
                             :wildcard-alias wildcard-alias
                             :fn-typedefs fn-typedefs}
                      (= :idiomatic mode) (assoc :mode :idiomatic)))))

(defn- file-project-values
  "Project values declared in *other* files: same-file values stay bare
  (namespace-level references need no qualification)."
  [index rel]
  (let [ns (get (:file->ns index) rel)]
    (into {} (remove (fn [[_ info]] (= ns (:ns info))))
          (:project-values index))))

(defn- file-project-mixins
  "Mixin declarations of *other* files, for member inlining into appliers
  (same-file declarations are resolved from the unit itself)."
  [index rel]
  (let [ns (get (:file->ns index) rel)]
    (into {}
          (comp (remove (fn [[_ info]] (= ns (:ns info))))
                (map (fn [[name info]] [name (:decl info)])))
          (:project-mixins index))))

(def ^:private wildcard-aliases
  "Deterministic candidates for the exports-shim require alias."
  ["exports" "d2c-exports" "wildcard"])

(defn convert-one
  "Translate one parsed unit and write `<out>/<rel>.cljd`.

  Returns `{:file rel :out path :ns ns}`."
  [rel unit index {:keys [out exports-uri] :as opts}]
  (let [missing (get-in index [:exports rel :missing])]
    (when (seq missing)
      (let [uri (first missing)]
        (throw (ex-info (str "dart2cljd: cannot resolve export " (pr-str uri)
                             ": no converted file matches it")
                        {:dart2cljd/kind :batch :file rel :uri uri})))))
  (let [{:keys [resolver wildcard? aliases external-imports]}
        (file-resolver index rel unit)
        _ (when (and wildcard? (nil? exports-uri))
            (throw (ex-info (str "dart2cljd: " rel " imports a file that re-exports "
                                 "external packages; pass --exports-file and "
                                 "--exports-uri to generate the exports shim")
                            {:dart2cljd/kind :batch :file rel})))
        ;; With the shim enabled, unresolved names from any external import
        ;; are routed through it (the shim re-exports every external URI the
        ;; tree imports). Without the flags the M6 default-alias fallback is
        ;; kept.
        shim? (or wildcard? (and exports-uri (seq external-imports)))
        wildcard-alias (when (and shim? exports-uri)
                         (or (first (remove aliases wildcard-aliases))
                             (fresh-alias (first wildcard-aliases) aliases)))
        ns (path->ns rel opts)
        source (str (emit-source unit rel ns
                                 (assoc opts
                                        :resolver resolver
                                        :project-types (:project-types index)
                                        :project-values (file-project-values index rel)
                                        :project-mixins (file-project-mixins index rel)
                                        :fn-typedefs (:fn-typedefs index)
                                        :wildcard-alias wildcard-alias))
                    "\n")
        out-file (java.io.File. (str out) (str/replace rel #"\.dart$" ".cljd"))]
    (io/make-parents out-file)
    (spit out-file source)
    {:file rel :out (.getPath out-file) :ns ns}))

(defn- write-exports-shim!
  "Write the generated Dart shim re-exporting every external URI the
  converted tree imports or re-exports. Returns the written path, or nil when
  there is nothing to write (or no `:exports-file`)."
  [{:keys [exports-file] :as _opts} index]
  (let [uris (->> (concat (mapcat :external (vals (:exports index)))
                          (:external-imports index))
                  set
                  sort)]
    (when (and (seq uris) exports-file)
      (let [file (java.io.File. (str exports-file))]
        (io/make-parents file)
        (spit file (str "// Generated by dart2cljd: re-exports for Dart `export`"
                        " directives and imports.\n"
                        "// Do not edit; regenerate with the converter.\n"
                        (str/join "\n" (map #(str "export '" % "';") uris))
                        "\n"))
        (.getPath file)))))

(defn- file-imports
  "Converted files this unit imports (relative paths)."
  [index rel unit]
  (let [ns->file (into {} (map (fn [[f ns]] [ns f])) (:file->ns index))]
    (into #{}
          (keep (fn [imp]
                  (let [t (project-target index rel (get-in imp [:uri :value]))]
                    (when (symbol? t) (get ns->file t)))))
          (filter #(= :import (:ir %)) (:directives unit)))))

(defn- propagate-import-failures
  "Mark files that (transitively) import a failed file as failed too and
  delete their output: their requires point at a namespace that was never
  generated, so they must not look converted."
  [index units converted failed]
  (loop [converted (vec converted) failed (vec failed)]
    (let [failed-files (set (map :file failed))
          failed-dep (fn [{:keys [file]}]
                       (some #(when (contains? failed-files %) %)
                             (file-imports index file (get units file))))
          bad (filter failed-dep converted)
          good (remove failed-dep converted)]
      (if (empty? bad)
        [good failed]
        (do
          (doseq [{:keys [out]} bad]
            (when out (.delete (io/file out))))
          (recur (vec good)
                 (into failed
                       (map (fn [{:keys [file]}]
                              {:file file
                               :error (ex-info (str "dart2cljd: imports failed file "
                                                    (failed-dep {:file file}))
                                               {:dart2cljd/kind :batch :file file})})
                            bad))))))))

;; ---------------------------------------------------------------------------
;; batch runner
;; ---------------------------------------------------------------------------

(defn summary
  "Deterministic one-line report: `converted N, merged P, skipped M,
  failed K` (`merged` appears only when part files were merged)."
  [{:keys [converted skipped failed merged]}]
  (str "converted " (count converted)
       (when (seq merged) (str ", merged " (count merged)))
       ", skipped " (count skipped)
       ", failed " (count failed)))

(defn- resolve-fn-typedefs
  "Union of the names the tree's external libraries export as Dart *function*
  typedefs, resolved through the analyzer (`frontend/fn-typedefs`).

  Parse-only mode cannot tell a function typedef from a class, and
  ClojureDart's analyzer helper crashes when it resolves a function-typedef
  element, so the emitter maps these names to `Function` (RESEARCH M8.10).
  Returns #{} when nothing external is imported or the source project cannot
  be resolved (e.g. no `pub get`): conversion then behaves exactly as before,
  and any such hint fails loudly at compile time."
  [index parsed {:keys [dir] :as opts}]
  (let [uris (:external-imports index)]
    (if (empty? uris)
      #{}
      (let [ref (some->> parsed
                         (map :file)
                         sort
                         first
                         (io/file dir))]
        (try
          (frontend/fn-typedefs uris (assoc opts :ref (str ref)))
          (catch Exception e
            (println (str "dart2cljd: warning: could not resolve external "
                          "function typedefs (" (ex-message e)
                          "); hints naming Dart function typedefs will fail in "
                          "ClojureDart's analyzer helper"))
            #{}))))))

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
    :types       use the analyzer-resolved frontend (`astdump --types`) so
                 expressions carry `:static-type` facts (operator dispatch,
                 String concatenation); requires the source project to be
                 resolvable (`dart pub get`), otherwise a warning and the
                 syntactic fallback (M11)
    :exports-file path of the generated Dart shim re-exporting the tree's
                 external `export`s (optional; needed when a converted file
                 re-exports external packages)
    :exports-uri URI under which generated code requires that shim
                 (e.g. `package:mycljd/d2c_exports.dart`)
    :root/:dart  astdump frontend options (dart2cljd.frontend)

  Pass 1 parses every file, groups libraries with their `part` files (a part's
  declarations merge into its library's namespace; part files produce no
  output) and builds the cross-file project index (imports, type -> namespace,
  constructors/statics); pass 2 emits with that index, so cross-file imports
  become symbol requires and cross-file type references resolve to the
  generated namespaces.

  Returns `{:converted [...] :merged [...] :skipped [...] :failed [...]
  :warnings [...] :package ... :exit 0|1}`: the run never aborts on a per-file
  failure, it collects them all (each failure carries the exception, whose
  message has file/line/column)."
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
                         {:file rel :unit (parse-unit rel opts)}
                         (catch Exception e {:file rel :error e})))
                     files)
        ok (into [] (filter :unit) parsed)
        parse-failed (into [] (remove :unit) parsed)
        grouped (library/group ok {:excluded (set (map :file skipped))
                                   :parse-failed (set (map :file parse-failed))})
        libs (:libraries grouped)
        lib-ok (into [] (remove :error) libs)
        lib-failed (into [] (filter :error) libs)
        index (build-index (mapv #(select-keys % [:file :unit]) lib-ok) opts)
        index (assoc index :fn-typedefs (resolve-fn-typedefs index lib-ok opts))
        shim (write-exports-shim! opts index)
        warnings (cond-> (:warnings index)
                   (and (seq (mapcat :external (vals (:exports index))))
                        (nil? (:exports-uri opts)))
                   (conj (str "the converted tree re-exports external packages; "
                              "pass :exports-file/:exports-uri (CLI "
                              "--exports-file/--exports-uri) so importers can "
                              "resolve those names")))
        emitted (mapv (fn [{:keys [file unit]}]
                        (try
                          {:ok (convert-one file unit index opts)}
                          (catch Exception e {:file file :error e})))
                      lib-ok)
        converted (into [] (keep :ok) emitted)
        failed (->> (concat parse-failed
                            (:errors grouped)
                            (map (fn [{:keys [file error]}]
                                   {:file file :error error})
                                 lib-failed)
                            (remove :ok emitted))
                    (sort-by :file)
                    vec)
        units (into {} (map (fn [{:keys [file unit]}] [file unit])) lib-ok)
        [converted failed] (propagate-import-failures index units converted failed)
        converted-files (set (map :file converted))
        failed-files (set (map :file failed))
        merged (into []
                     (for [{:keys [file parts]} lib-ok
                           :when (contains? converted-files file)
                           part parts]
                       {:file part
                        :into file
                        :ns (path->ns file opts)}))
        ;; Parts of a failed library never made it into an output file: report
        ;; them as failed too, unless they already failed on their own.
        part-failed
        (reduce (fn [m {:keys [file parts]}]
                  (if (contains? converted-files file)
                    m
                    (reduce (fn [m part]
                              (if (contains? failed-files part)
                                m
                                (assoc m part
                                       {:file part
                                        :error (ex-info
                                                (str "dart2cljd: part of library "
                                                     file ", which failed to convert")
                                                {:dart2cljd/kind :library
                                                 :file part
                                                 :library file})})))
                            m
                            parts)))
                {}
                libs)
        failed (vec (sort-by :file (concat failed (vals part-failed))))]
    {:converted converted
     :merged merged
     :skipped skipped
     :failed failed
     :warnings warnings
     :exports-file shim
     :package (:package index)
     :exit (if (empty? failed) 0 1)}))
