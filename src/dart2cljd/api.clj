(ns dart2cljd.api
  "Public conversion API, shape-compatible with DartClojure's `convert`.

  Pipeline: Dart source -> bin/astdump.dart (EDN) -> dart2cljd.ast
  (normalize) -> dart2cljd.emit.* (forms) -> dart2cljd.printer (text).

  Snippet conversion is cached under `.dart2cljd-cache/` keyed by a hash of
  the snippet and of bin/astdump.dart, so repeated conversions in tests do
  not pay the Dart startup cost and stale entries are invalidated whenever
  the frontend changes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [dart2cljd.alias :as alias]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer])
  (:import (java.security MessageDigest)))

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
;; astdump frontend
;; ---------------------------------------------------------------------------

(defn- sha256 [^String s]
  (let [d (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest d (.getBytes s "UTF-8"))))))

(defn- cache-file [root key]
  (io/file root ".dart2cljd-cache" (str key ".edn")))

(defn- run-astdump
  "Run bin/astdump.dart with `args`, feeding `stdin` when given."
  [root cmd args stdin]
  (let [result (apply shell/sh cmd "run" "bin/astdump.dart"
                      (concat args (when stdin [:in stdin]) [:dir root]))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "dart2cljd: astdump failed: " (:err result))
                      {:dart2cljd/kind :astdump
                       :exit (:exit result)
                       :stderr (:err result)})))
    (edn/read-string (:out result))))

(defn astdump
  "Dump a Dart snippet (string) to an astdump EDN document.

  Options: :dart (binary), :root (dart2cljd checkout), :no-cache"
  [code {:keys [dart root no-cache] :as opts}]
  (let [root (or root (:dart2cljd-root opts) (System/getProperty "user.dir"))
        cmd (or dart (System/getenv "DART2CLJD_DART") "dart")
        dumper (io/file root "bin/astdump.dart")
        tool-hash (if (.exists dumper) (sha256 (slurp dumper)) "missing")
        key (sha256 (str tool-hash "\n" cmd "\n" code))
        cached (cache-file root key)]
    (if (and (not no-cache) (.exists cached))
      (edn/read-string (slurp cached))
      (let [doc (run-astdump root cmd ["-"] code)]
        (when-not no-cache
          (io/make-parents cached)
          (spit cached (pr-str doc)))
        doc))))

(defn astdump-file
  "Dump a Dart file (path) to an astdump EDN document."
  [path {:keys [dart root] :as opts}]
  (let [root (or root (:dart2cljd-root opts) (System/getProperty "user.dir"))
        cmd (or dart (System/getenv "DART2CLJD_DART") "dart")]
    (run-astdump root cmd [path] nil)))

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
  "Dart file -> cljd (includes the ns form)."
  [path & args]
  (let [opts (kwargs->map args)
        doc (ast/normalize-doc (astdump-file path opts) opts)]
    (convert-doc doc (assoc opts :path path))))

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
   ["-h" "--help"]])

(defn cli-opts
  "CLI options -> conversion kwargs."
  [{:keys [material flutter width faithful default-alias dart]} idiomatic?]
  (cond-> {:material material
           :flutter flutter
           :width width
           :dart dart}
    (not (nil? default-alias)) (assoc :default-alias default-alias)
    (and idiomatic? (not faithful)) (assoc :mode :idiomatic)))

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

      :else
      (let [opts (cli-opts options (:idiomatic options))
            path (or (:path options) (first arguments))]
        (if (nil? path)
          (do (binding [*out* *err*]
                (println "no arguments passed; use --help for usage"))
              2)
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
  "dart2cljd CLI: translate Dart code (argument, file or REPL) to ClojureDart.

  Flags mirror DartClojure's CLI (`-r -e -m -f -c -p`) plus `-i/--idiomatic`,
  `--faithful`, `-w/--width`, `--default-alias`, `--dart`."
  [& args]
  (let [code (run-cli args)]
    (when-not (zero? code)
      (System/exit code))))
