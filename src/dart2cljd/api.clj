(ns dart2cljd.api
  "Public conversion API, shape-compatible with DartClojure's `convert`.

  Pipeline: Dart source -> bin/astdump.dart (EDN) -> dart2cljd.ast
  (normalize) -> dart2cljd.emit.* (forms) -> dart2cljd.printer (text).

  Snippet conversion is cached under `.dart2cljd-cache/` keyed by a hash of
  the snippet and of bin/astdump.dart, so repeated conversions in tests do
  not pay the Dart startup cost and stale entries are invalidated whenever
  the frontend changes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [dart2cljd.alias :as alias]
            [dart2cljd.ast :as ast]
            [dart2cljd.batch :as batch]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.frontend :as frontend]
            [dart2cljd.library :as library]
            [dart2cljd.printer :as printer]))

;; ---------------------------------------------------------------------------
;; options
;; ---------------------------------------------------------------------------

(defn- kwargs->map [args]
  (when (odd? (count args))
    (throw (ex-info (str "dart2cljd: odd number of keyword arguments: " (pr-str args))
                    {:dart2cljd/kind :options :args args})))
  (apply hash-map args))

(defn- snippet-aliases [{:keys [material flutter default-alias]}]
  (let [material (str (or material "m"))
        flutter (str (or flutter "f"))
        default (str (or default-alias material))
        base (alias/context [] {:default-alias default})]
    (assoc base
           :prefix->uri {material "package:flutter/material.dart"
                         flutter "cljd.flutter"}
           :uri->alias {"package:flutter/material.dart" material
                        "cljd.flutter" flutter})))

(defn- emit-snippet-decl [d ctx]
  (case (:ir d)
    :expr-stmt [(core/emit-expr (:expr d) ctx)]
    (decls/emit-decl d ctx)))

;; ---------------------------------------------------------------------------
;; conversion
;; ---------------------------------------------------------------------------

(defn convert-doc
  "Convert an astdump EDN document (raw or already normalized) to cljd.

  Options:
    :format         :string (default) | :sexpr
    :mode           :faithful (default) | :idiomatic
    :default-alias  alias for unresolved Dart types (default \"m\")
    :material       snippet alias for package:flutter/material.dart (\"m\")
    :flutter        snippet alias for cljd.flutter (\"f\")
    :ns             namespace name override (file mode)
    :width          printer line width (default 100)

  Returns the printed source string, or forms for :sexpr."
  [doc opts]
  (let [doc (if (:ir (:unit doc)) doc (ast/normalize-doc doc))
        unit (:unit doc)
        snippet? (= :snippet (:mode doc))
        aliases (or (:aliases opts)
                    (when snippet? (snippet-aliases opts)))
        ctx (decls/ctx-for aliases opts)
        forms (if snippet?
                (let [fs (vec (mapcat #(emit-snippet-decl % ctx) (:decls unit)))]
                  (if (= 1 (count fs)) fs (list* 'do fs)))
                (decls/emit-unit unit (assoc opts :aliases aliases)))]
    (case (or (:format opts) :string)
      :sexpr (if snippet? (first forms) forms)
      :string (printer/print-forms forms {:width (or (:width opts) 100)})
      (throw (ex-info (str "dart2cljd: unknown :format " (:format opts))
                      {:dart2cljd/kind :options :format (:format opts)})))))

;; ---------------------------------------------------------------------------
;; astdump frontend (see dart2cljd.frontend)
;; ---------------------------------------------------------------------------

(def astdump
  "Dump a Dart snippet (string) to an astdump EDN document.
  Options: :dart (binary), :root (dart2cljd checkout), :no-cache"
  frontend/astdump)

(def astdump-file
  "Dump a Dart file (path) to an astdump EDN document."
  frontend/astdump-file)

;; ---------------------------------------------------------------------------
;; single-file libraries (part merging)
;; ---------------------------------------------------------------------------

(defn- part-file
  "The part file a `part` URI points at, relative to `path`'s directory.
  Non-relative URIs (`package:`, `dart:`, absolute) are invalid Dart."
  [path uri]
  (when-not (and (string? uri)
                 (not (str/starts-with? uri "package:"))
                 (not (str/starts-with? uri "dart:"))
                 (not (str/starts-with? uri "/")))
    (throw (ex-info (str "dart2cljd: part URI " (pr-str uri)
                         " must be relative to " path)
                    {:dart2cljd/kind :library :file path :uri uri})))
  (let [f (io/file path)
        dir (or (.getParentFile f) (io/file "."))]
    (io/file dir uri)))

(defn- merge-file-parts
  "Read the `part` files of a library file and merge them into its unit.

  Single-file mode has no discovery/exclude policy: every part must exist and
  parse, otherwise conversion fails loudly. (`--dir` batch mode reports each
  part per file instead.)"
  [path unit opts]
  (let [uris (library/part-uris unit)]
    (if (empty? uris)
      unit
      (library/merge-parts
       unit
       (mapv (fn [uri]
               (let [f (part-file path uri)]
                 (when-not (.isFile f)
                   (throw (ex-info (str "dart2cljd: part " (pr-str uri)
                                        " not found (needed by " path ")")
                                   {:dart2cljd/kind :library :file path :uri uri})))
                 (let [punit (:unit (ast/normalize-doc
                                     (astdump-file (.getPath f) opts)))]
                   (when (seq (library/part-uris punit))
                     (throw (ex-info (str "dart2cljd: part " (pr-str uri)
                                          " declares its own parts")
                                     {:dart2cljd/kind :library
                                      :file (.getPath f)})))
                   punit)))
             uris)))))

;; ---------------------------------------------------------------------------
;; public API
;; ---------------------------------------------------------------------------

(defn convert
  "Dart source snippet -> cljd, DartClojure-style:
  `(convert \"Text('hi')\" :format :sexpr)`."
  [dart & args]
  (let [opts (kwargs->map args)]
    (convert-doc (ast/normalize-doc (astdump dart opts) opts) opts)))

(defn convert-file
  "Dart file -> cljd (includes the ns form).

  A library file's `part` files are read and merged into the output (one
  namespace per library, M9); a `part of` file cannot be converted on its
  own. In typed mode (`:types true`) the frontend's per-file function-typedef
  facts are passed through, so external function typedefs become `Function`
  hints without the separate `--fn-typedefs` pass (M12 follow-up)."
  [path & args]
  (let [opts (kwargs->map args)
        doc (ast/normalize-doc (astdump-file path opts) opts)
        unit (:unit doc)
        fn-typedefs (into #{} (mapcat val (:fn-typedefs doc)))]
    (when (library/part-of-directive unit)
      (throw (ex-info (str "dart2cljd: " path " is a `part of` file; convert its "
                           "library file instead (parts share a library's "
                           "namespace)")
                      {:dart2cljd/kind :library :file path})))
    (convert-doc (assoc doc :unit (merge-file-parts path unit opts))
                 (cond-> (assoc opts :path path)
                   (seq fn-typedefs) (assoc :fn-typedefs fn-typedefs)))))

(defn convert-dir
  "Batch mode (M6): translate a whole Dart directory tree.

  Options: `:dir :out :ns-prefix :exclude :package :mode :types` — see
  `dart2cljd.batch/convert-dir`. Returns the report map
  (`:converted :skipped :failed :exit`), never throws for per-file failures."
  [opts]
  (batch/convert-dir opts))

;; ---------------------------------------------------------------------------
;; CLI (flag-compatible with DartClojure's `dumch.dartclojure`)
;; ---------------------------------------------------------------------------

(def ^:private color-cycle
  ["\u001b[36m" "\u001b[33m" "\u001b[35m" "\u001b[32m" "\u001b[34m"])

(def ^:private ansi-reset "\u001b[0m")

(defn colorize
  "Deterministic ANSI coloring: strings green, delimiters by nesting depth."
  [^String s]
  (let [sb (StringBuilder.)
        depth (volatile! 0)
        in-string? (volatile! false)
        escaped? (volatile! false)]
    (dotimes [i (.length s)]
      (let [c (.charAt s i)]
        (cond
          @in-string?
          (do (.append sb c)
              (cond
                @escaped? (vreset! escaped? false)
                (= c \\) (vreset! escaped? true)
                (= c \") (vreset! in-string? false)))

          (= c \")
          (do (vreset! in-string? true)
              (.append sb "\u001b[32m")
              (.append sb c))

          (#{\( \[ \{} c)
          (let [color (nth color-cycle (mod @depth (count color-cycle)))]
            (vswap! depth inc)
            (.append sb color)
            (.append sb c))

          (#{\) \] \}} c)
          (do (vswap! depth dec)
              (.append sb ansi-reset)
              (.append sb c))

          :else (.append sb c))))
    (str sb)))

(defn- boolean-arg [s]
  (contains? #{"true" "1" "yes"} (str/lower-case (str s))))

(def cli-options
  [["-r" "--repl BOOLEAN" "REPL mode: read snippets until the end marker"
    :default false
    :parse-fn boolean-arg
    :validate [boolean? "Must be either true or false"]]
   ["-e" "--end TEXT" "Mode: text marking the end of a REPL snippet"
    :default ""
    :parse-fn str]
   ["-m" "--material MATERIAL" "material require alias"
    :default "m"
    :parse-fn str
    :validate [#(re-matches #"^[A-Za-z][A-Za-z0-9]*$" %)
               "invalid require alias for material"]]
   ["-f" "--flutter FLUTTER" "cljd.flutter require alias"
    :default "f"
    :parse-fn str
    :validate [#(re-matches #"^[A-Za-z][A-Za-z0-9]*$" %)
               "invalid require alias for cljd.flutter"]]
   ["-c" "--colors BOOLEAN" "colorize output"
    :default false
    :parse-fn boolean-arg
    :validate [boolean? "Must be either true or false"]]
   ["-i" "--idiomatic" "Idiomatic mode (Flutter sugar)"]
   [nil "--faithful" "Faithful mode (default)"]
   ["-w" "--width N" "Printer line width"
    :default 100
    :parse-fn #(Integer/parseInt %)
    :validate [pos? "Must be a positive number"]]
   [nil "--default-alias ALIAS" "Alias for unresolved Dart types (default \"m\")"
    :parse-fn str]
   [nil "--dart PATH" "Dart binary to run the astdump frontend"
    :parse-fn str]
   ["-p" "--path PATH" "Path to the Dart file to translate"
    :parse-fn str]
   [nil "--dir DIR" "Batch mode: Dart directory to translate"]
   [nil "--out DIR" "Batch mode: output directory for generated .cljd files"]
   [nil "--ns-prefix PREFIX" "Batch mode: namespace prefix for generated files"
    :parse-fn str]
   [nil "--exclude GLOB" "Batch mode: skip files matching GLOB (repeatable)"
    :default []
    :multi true
    :update-fn (fn [acc v] (conj acc v))
    :parse-fn str]
   [nil "--package NAME" "Batch mode: Dart package name (default: nearest pubspec.yaml)"
    :parse-fn str]
   [nil "--types" "Use analyzer resolution: expression static types (file/dir modes)"]
   [nil "--exports-file PATH" "Batch mode: write the external-exports Dart shim here"
    :parse-fn str]
   [nil "--exports-uri URI" "Batch mode: URI under which generated code requires the shim"
    :parse-fn str]
   ["-h" "--help"]])

(defn cli-opts
  "CLI options -> conversion kwargs."
  [{:keys [material flutter width faithful default-alias dart types]} idiomatic?]
  (cond-> {:material material
           :flutter flutter
           :width width
           :dart dart}
    (not (nil? default-alias)) (assoc :default-alias default-alias)
    types (assoc :types true)
    (and idiomatic? (not faithful)) (assoc :mode :idiomatic)))

(defn batch-opts
  "CLI options -> `convert-dir` kwargs."
  [{:keys [dir out ns-prefix exclude package exports-file exports-uri]} conversion-opts]
  (cond-> (assoc conversion-opts :dir dir :out out :exclude (vec exclude))
    (some? ns-prefix) (assoc :ns-prefix ns-prefix)
    (some? package) (assoc :package package)
    (some? exports-file) (assoc :exports-file exports-file)
    (some? exports-uri) (assoc :exports-uri exports-uri)))

(defn- show [s colors] (println (if colors (colorize (str s)) (str s))))

(defn- repl-instructions [end]
  (println (str "Paste dart code below, press enter"
                (when (seq end) (str ", write '" end "',"))
                " and see the result:\n")))

(defn- repl
  "Read snippets from stdin until `end` and print each translation.
  Conversion errors are reported and the session continues."
  [end convert-opts colors]
  (repl-instructions end)
  (loop [input (read-line) acc []]
    (if (nil? input)
      nil
      (if (= input end)
        (do
          (try
            (show (apply convert (str/join "\n" acc) (mapcat identity convert-opts)) colors)
            (catch Exception e
              (binding [*out* *err*]
                (println "Can't convert the code; e" (.getMessage e)))))
          (println)
          (recur (read-line) []))
        (recur (read-line) (conj acc input))))))

(defn- report-batch
  "Print a batch report: warnings and per-file failures to stderr (file:
  message, the message carries the source position), the summary to stdout.
  Returns the report's exit code."
  [{:keys [failed warnings exit] :as result}]
  (doseq [w warnings]
    (binding [*out* *err*]
      (println (str "dart2cljd: warning: " w))))
  (doseq [{:keys [file error]} failed]
    (binding [*out* *err*]
      (println (str "dart2cljd: " file ": " (ex-message error)))))
  (println (str "dart2cljd: " (batch/summary result)))
  exit)

(defn run-cli
  "Run the CLI; returns an exit code (0 ok, 1 conversion error, 2 usage)."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      (do (binding [*out* *err*] (run! println errors)) 1)

      (:help options)
      (do (println "dart2cljd: deterministic Dart -> ClojureDart translator\n")
          (println summary)
          0)

      (:repl options)
      (do (repl (:end options) (cli-opts options (:idiomatic options)) (:colors options))
          0)

      (:dir options)
      (if (nil? (:out options))
        (do (binding [*out* *err*]
              (println "dart2cljd: --dir requires --out"))
            1)
        (try
          (report-batch (convert-dir (batch-opts options
                                                 (cli-opts options (:idiomatic options)))))
          (catch clojure.lang.ExceptionInfo e
            (binding [*out* *err*] (println (.getMessage e)))
            1)))

      :else
      (let [opts (cli-opts options (:idiomatic options))
            path (or (:path options) (first arguments))]
        (cond
          (nil? path)
          (do (binding [*out* *err*]
                (println "no arguments passed; use --help for usage"))
              2)

          (and (:types options)
               (or (nil? (:path options)) (= "-" path)))
          (do (binding [*out* *err*]
                (println (str "dart2cljd: --types requires a Dart file (-p PATH); "
                              "snippets cannot be resolved")))
              1)

          :else
          (try
            (let [out (cond
                        (= "-" path) (apply convert (slurp *in*) (mapcat identity opts))
                        (:path options) (apply convert-file path (mapcat identity opts))
                        :else (apply convert path (mapcat identity opts)))]
              (show out (:colors options))
              0)
            (catch clojure.lang.ExceptionInfo e
              (binding [*out* *err*] (println (.getMessage e)))
              1)))))))

(defn -main
  "dart2cljd CLI: translate Dart code (argument, file, `--dir` tree or REPL).

  Flags mirror DartClojure's CLI (`-r -e -m -f -c -p`) plus `-i/--idiomatic`,
  `--faithful`, `-w/--width`, `--default-alias`, `--dart`, and the batch flags
  `--dir --out --ns-prefix --exclude --package`."
  [& args]
  (let [code (run-cli args)]
    (when-not (zero? code)
      (System/exit code))))
