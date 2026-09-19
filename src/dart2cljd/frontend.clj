(ns dart2cljd.frontend
  "astdump frontend: Dart snippets/files -> astdump EDN documents.

  Runs `bin/astdump.dart` through the Dart SDK. Snippet dumps are cached under
  `.dart2cljd-cache/` keyed by a hash of the snippet and of bin/astdump.dart, so
  repeated conversions in tests do not pay the Dart startup cost and stale
  entries are invalidated whenever the frontend changes.

  This namespace exists (separate from dart2cljd.api) so that batch conversion
  can use the frontend without a require cycle: api depends on batch for the
  `--dir` CLI, batch depends on the frontend for parsing."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.security MessageDigest)))

(defn root
  "dart2cljd checkout to run the frontend from: `:root`, `DART2CLJD_ROOT`,
  `:dart2cljd-root`, or the current directory."
  [opts]
  (or (:root opts)
      (System/getenv "DART2CLJD_ROOT")
      (:dart2cljd-root opts)
      (System/getProperty "user.dir")))

(defn dart-bin
  "Dart binary to use: `:dart`, `DART2CLJD_DART`, or `dart` on PATH."
  [opts]
  (or (:dart opts) (System/getenv "DART2CLJD_DART") "dart"))

(defn- sha256 [^String s]
  (let [d (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest d (.getBytes s "UTF-8"))))))

(defn- cache-file [root key]
  (io/file root ".dart2cljd-cache" (str key ".edn")))

(defn- kernel-file [root]
  (io/file root ".dart2cljd-cache" "astdump.dill"))

(defn- kernel-stamp [root]
  (io/file root ".dart2cljd-cache" "astdump.dill.sha"))

(defn ensure-kernel!
  "Compile bin/astdump.dart to a kernel snapshot and return its path.

  `dart run bin/astdump.dart` costs ~40 s per invocation here (the analyzer
  package is JIT-compiled every time); a kernel snapshot brings that down to
  ~1 s. The snapshot is cached under `.dart2cljd-cache/` and recompiled
  whenever the frontend source hash changes (mirroring the snippet cache)."
  [root cmd]
  (let [src (io/file root "bin/astdump.dart")
        kernel (kernel-file root)
        stamp (kernel-stamp root)
        hash (sha256 (slurp src))]
    (if (and (.exists kernel)
             (.exists stamp)
             (= hash (str/trim (slurp stamp))))
      kernel
      (do
        (println "dart2cljd: compiling the astdump kernel (one-time, ~20 s)...")
        (io/make-parents kernel)
        (let [res (shell/sh cmd "compile" "kernel" "bin/astdump.dart"
                            "-o" (.getPath kernel) :dir root)]
          (when-not (zero? (:exit res))
            (throw (ex-info (str "dart2cljd: `" cmd " compile kernel` failed: "
                                 (:err res))
                            {:dart2cljd/kind :astdump
                             :exit (:exit res)
                             :stderr (:err res)}))))
        (spit stamp (str hash "\n"))
        kernel))))

(defn- run-astdump
  "Run the astdump frontend with `args`, feeding `stdin` when given.

  Uses the cached kernel snapshot when possible, `dart run` otherwise."
  [root cmd args stdin]
  (let [kernel (ensure-kernel! root cmd)
        result (apply shell/sh
                      (concat [cmd (.getPath kernel)] args
                              (when stdin [:in stdin])
                              [:dir root]))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "dart2cljd: astdump failed: " (:err result))
                      {:dart2cljd/kind :astdump
                       :exit (:exit result)
                       :stderr (:err result)})))
    (edn/read-string (:out result))))

(defn astdump
  "Dump a Dart snippet (string) to an astdump EDN document.

  Options: :dart (binary), :root (dart2cljd checkout), :no-cache"
  [code {:keys [no-cache] :as opts}]
  (let [root (root opts)
        cmd (dart-bin opts)
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
  "Dump a Dart file (path) to an astdump EDN document.

  `path` is resolved against the caller's directory: the frontend runs with
  its working directory set to the dart2cljd checkout."
  [path opts]
  (let [root (root opts)
        cmd (dart-bin opts)
        path (str (.getAbsoluteFile (io/file path)))]
    (run-astdump root cmd [path] nil)))

(defn fn-typedefs
  "Set of names `uris` export as Dart *function* typedefs.

  ClojureDart's analyzer helper throws `TypeAliasElement type not handled`
  when it resolves a function-typedef element, so the emitter maps these names
  to `Function` instead of emitting a hint (RESEARCH M8.10). `:ref` is a Dart
  file inside the source project (its `package_config.json` resolves `uris`).

  Returns #{} when there is nothing to resolve or the project cannot be
  resolved; conversion then keeps its ordinary hints, exactly as before."
  [uris {:keys [ref] :as opts}]
  (if (or (empty? uris) (nil? ref))
    #{}
    (let [root (root opts)
          cmd (dart-bin opts)
          ref (str (.getAbsoluteFile (io/file ref)))
          doc (run-astdump root cmd (into ["--fn-typedefs" ref] (sort uris)) nil)]
      (into #{} (comp (mapcat val) (remove str/blank?))
            (:fn-typedefs doc)))))
