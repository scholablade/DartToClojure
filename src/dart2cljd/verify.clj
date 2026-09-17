(ns dart2cljd.verify
  "M5 verification gate: compile and run the corpus with real ClojureDart.

  Steps, all deterministic:
    1. translate every `corpus/<name>.dart` (see `corpus/manifest.edn`) into
       `verify/src/verify/gen/<name>.cljd` under namespace
       `verify.gen.<name>` (mode from the manifest);
    2. regenerate `verify/src/verify/main.cljd` requiring all entries
       (`verify.checks` requires it, so one invocation covers everything);
    3. run the real ClojureDart compiler through a single
       `clj -M:cljd test verify.checks`: every generated corpus namespace must
       compile (it aborts on compile errors) and `flutter test` then executes
       the translated code, including the async path (`--compile-only` uses
       `clj -M:cljd compile verify.checks` and skips the tests).
       A single cljd invocation is deliberate: every run re-creates its
       analyzer helper (local-root deps have no git sha) and a second analyzer
       boot next to the previous run's async child can misread Flutter output
       as the EDN handshake (RESEARCH M5.5);
    4. strip the bogus imports of test output that ClojureDart's test-namespace
       renaming leaves in generated `lib/` Dart, then run `dart analyze` on
       every compiled Dart file and assert no issues (RESEARCH M5.6).

  Any failing step exits nonzero with the tool output; nothing is written
  outside `verify/`.

  Usage: clojure -M:verify [--compile-only] [--no-analyze] [--timeout SECS]
                           [--root DIR] [-h]"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [dart2cljd.api :as api]
            [dart2cljd.ast :as ast]
            [dart2cljd.batch :as batch]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer]))

(def ^:private default-timeout 1800)

;; ---------------------------------------------------------------------------
;; corpus
;; ---------------------------------------------------------------------------

(defn manifest
  "Read the corpus manifest (vector of {:name ... :mode ...})."
  [root]
  (let [file (io/file root "corpus/manifest.edn")]
    (when-not (.exists file)
      (throw (ex-info (str "dart2cljd.verify: missing " file)
                      {:dart2cljd/kind :verify :file (.getPath file)})))
    (edn/read-string (slurp file))))

(defn gen-ns
  "Namespace symbol under which a (file-kind) corpus entry is compiled."
  [name]
  (symbol (str "verify.gen." name)))

(defn entry-kind
  "Manifest entry kind: `:file` (default) or `:dir`."
  [entry]
  (or (:kind entry) :file))

(defn entry-ns
  "Namespace `verify.main` requires for an entry: `verify.gen.<name>` for
  file entries, `<ns-prefix>.<entry>` for directory entries."
  [entry]
  (case (entry-kind entry)
    :dir (symbol (str (:ns-prefix entry) "." (:entry entry)))
    (gen-ns (:name entry))))

(defn gen-source
  "Translate a normalized unit for the verify project (ns overridden)."
  [unit {:keys [name mode]}]
  (printer/print-forms
   (decls/emit-unit unit (cond-> {:ns (gen-ns name)
                                  :path (str "corpus/" name ".dart")}
                           (= :idiomatic mode) (assoc :mode :idiomatic)))))

(defn main-source
  "Generate verify.main, requiring every corpus entry (file and directory)."
  [entries]
  (str "(ns verify.main\n  (:require\n"
       (str/join "\n"
                 (for [entry entries]
                   (str "   [" (entry-ns entry) " :as " (:name entry) "]")))
       "))\n\n(defn main [] nil)\n"))

(def runtime-namespaces
  "Namespaces handed to the single `clj -M:cljd test` invocation.

  `verify.checks` requires `verify.main`, which requires every generated
  corpus namespace, so one invocation compiles everything. Keep this a single
  test namespace: ClojureDart renames test libs and namespaces compiled after
  the rename pick up a bogus import of the renamed test file (RESEARCH M5.6);
  the harness strips those imports before `dart analyze`."
  ["verify.checks"])

(defn strip-test-imports
  "Remove generated imports of `test/cljd-out` from compiled `lib/` Dart.

  Upstream `rename-lib` (RESEARCH M5.6) adds a nil import entry for the
  renamed test lib to every namespace, which the emitter renders as a bogus
  `import \"…/test/cljd-out/…\" as …;`. Those imports are unused; stripping
  them makes `dart analyze` meaningful. Pure text function; returns the input
  unchanged when there is nothing to strip."
  [text]
  (str/replace text
               #"(?m)^import \"[^\"]*test/cljd-out/[^\"]*\" as [^;]+;\n"
               ""))

(defn- strip-generated-test-imports!
  "Apply `strip-test-imports` to every generated Dart file under verify/."
  [root]
  (let [dir (io/file root "verify/lib/cljd-out")
        repaired (volatile! 0)]
    (when (.isDirectory dir)
      (doseq [^java.io.File f (file-seq dir)
              :when (and (.isFile f) (str/ends-with? (.getName f) ".dart"))]
        (let [text (slurp f)
              cleaned (strip-test-imports text)]
          (when (not= text cleaned)
            (spit f cleaned)
            (vswap! repaired inc)))))
    (when (pos? @repaired)
      (println (str "verify: stripped bogus test imports from " @repaired
                    " generated Dart file(s) (RESEARCH M5.6)")))))

(defn- write-entry!
  "Translate corpus/<name>.dart and write verify/src/verify/gen/<name>.cljd."
  [root entry]
  (let [{:keys [name]} entry
        dart-file (io/file root "corpus" (str name ".dart"))]
    (when-not (.exists dart-file)
      (throw (ex-info (str "dart2cljd.verify: missing " dart-file)
                      {:dart2cljd/kind :verify :file (.getPath dart-file)})))
    (let [doc (api/astdump-file (str "corpus/" name ".dart") {:root root})
          unit (:unit (ast/normalize-doc doc))
          out (io/file root "verify/src/verify/gen" (str name ".cljd"))]
      (io/make-parents out)
      (spit out (str (gen-source unit entry) "\n"))
      (.getPath out))))

(defn- write-dir-entry!
  "Batch-convert a `:kind :dir` entry into `:out` and check its failure set.

  The manifest declares which files must fail (`:expect-failures`); an
  unexpected conversion or failure is a gate error. Returns the
  `convert-dir` report."
  [root entry]
  (let [{:keys [name dir out ns-prefix package mode expect-failures]} entry
        result (api/convert-dir
                (cond-> {:dir (.getPath (io/file root dir))
                         :out (.getPath (io/file root out))
                         :root root}
                  ns-prefix (assoc :ns-prefix ns-prefix)
                  package (assoc :package package)
                  (= :idiomatic mode) (assoc :mode :idiomatic)))
        failed (set (map :file (:failed result)))
        expected (set expect-failures)]
    (when-not (= failed expected)
      (throw (ex-info (str "dart2cljd.verify: " name ": expected failures "
                           (pr-str expected) ", got " (pr-str failed)
                           (when-let [f (first (:failed result))]
                             (str " (" (ex-message (:error f)) ")")))
                      {:dart2cljd/kind :verify :entry name
                       :expected expected :failed failed})))
    (println (str "verify: translated " name " (" dir "): "
                  (batch/summary result)
                  (when (seq expected)
                    (str "; expected failures: " (str/join ", " (sort expected))))))
    result))

(defn- write-entries!
  "Translate all manifest entries; returns them unchanged."
  [root entries]
  (doseq [entry entries]
    (if (= :dir (entry-kind entry))
      (write-dir-entry! root entry)
      (println (str "verify: translated corpus/" (:name entry) ".dart -> "
                    (write-entry! root entry)))))
  entries)

(defn- delete-tree! [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-tree! c)))
  (.delete f))

(defn- clean-gen! [root entries]
  (let [dir (io/file root "verify/src/verify/gen")
        keep-files (set (for [e entries :when (= :file (entry-kind e))]
                          (str (:name e) ".cljd")))
        keep-dirs (set (for [e entries :when (= :dir (entry-kind e))]
                         (:name e)))]
    (when (.isDirectory dir)
      (doseq [^java.io.File f (.listFiles dir)
              :when (not (or (and (.isFile f) (contains? keep-files (.getName f)))
                             (and (.isDirectory f) (contains? keep-dirs (.getName f)))))]
        (delete-tree! f)))))

(defn generated-dart-files
  "Dart files the compile step should produce for the entries (paths relative
  to verify/), plus the generated verify.main."
  [root entries]
  (let [verify-dir (.toPath (io/file root "verify"))
        gen-dir (io/file root "verify/lib/cljd-out/verify/gen")
        rel (fn [^java.io.File f]
              (str (.relativize verify-dir (.toPath f))))]
    (mapv rel
          (concat
           (mapcat (fn [entry]
                     (if (= :dir (entry-kind entry))
                       (let [d (io/file gen-dir (:name entry))]
                         (when (.isDirectory d)
                           (->> (file-seq d)
                                (filter #(and (.isFile ^java.io.File %)
                                              (str/ends-with? (.getName ^java.io.File %) ".dart"))))))
                       [(io/file gen-dir (str (:name entry) ".dart"))]))
                   entries)
           [(io/file root "verify/lib/cljd-out/verify/main.dart")]))))

;; ---------------------------------------------------------------------------
;; steps
;; ---------------------------------------------------------------------------

(defn- run-step!
  "Run a command in `dir` under `timeout` seconds; throw on nonzero exit."
  [{:keys [dir timeout]} step cmd args]
  (println (str "verify: " step ": " cmd (when (seq args) (str " " (str/join " " args)))))
  (let [res (apply shell/sh
                   (into ["timeout" (str timeout) cmd]
                         (concat args [:out-enc "UTF-8" :dir dir])))
        exit (:exit res)]
    (when (or (nil? exit) (not (zero? exit)))
      (throw (ex-info (str "dart2cljd.verify: " step " failed"
                           (when (= 124 exit) " (timed out)"))
                      {:dart2cljd/kind :verify
                       :step step
                       :exit exit
                       :out (:out res)
                       :err (:err res)})))
    res))

(defn- print-result [{:keys [out err]}]
  (when (seq (str/trim (str out))) (println (str/trim (str out))))
  (when (seq (str/trim (str err))) (binding [*out* *err*] (println (str/trim (str err))))))

;; ---------------------------------------------------------------------------
;; runner
;; ---------------------------------------------------------------------------

(def cli-options
  [[nil "--compile-only" "Translate + compile + analyze; skip the runtime checks"]
   [nil "--no-analyze" "Skip `dart analyze` on the compiled Dart files"]
   [nil "--timeout SECONDS" "Per-step timeout applied to compile/analyze/test"
    :default default-timeout
    :parse-fn #(Integer/parseInt %)
    :validate [pos? "must be a positive number of seconds"]]
   [nil "--root DIR" "dart2cljd checkout (defaults to the current directory)"
    :parse-fn str]
   ["-h" "--help"]])

(defn run
  "Run the verification gate; returns an exit code."
  [{:keys [compile-only no-analyze timeout root] :as _opts}]
  (let [root (or root (System/getProperty "user.dir"))
        verify-dir (io/file root "verify")
        entries (manifest root)
        clj-bin (or (System/getenv "DART2CLJD_CLJ") "clj")
        dart-bin (or (System/getenv "DART2CLJD_DART") "dart")
        step-opts {:dir (.getPath verify-dir) :timeout timeout}]
    (when-not (.isDirectory verify-dir)
      (throw (ex-info (str "dart2cljd.verify: no verify/ project under " root)
                      {:dart2cljd/kind :verify :root root})))
    (println (str "verify: " (count entries) " corpus entries: "
                  (str/join ", " (map :name entries))))
    (clean-gen! root entries)
    (write-entries! root entries)
    (spit (io/file root "verify/src/verify/main.cljd") (main-source entries))
    (print-result
     (if compile-only
       (run-step! step-opts "compile" clj-bin
                  (into ["-M:cljd" "compile"] runtime-namespaces))
       (run-step! step-opts "compile + test" clj-bin
                  (into ["-M:cljd" "test"] runtime-namespaces))))
    (strip-generated-test-imports! root)
    (when-not no-analyze
      (doseq [dart (generated-dart-files root entries)]
        (print-result (run-step! step-opts (str "analyze " dart) dart-bin ["analyze" dart]))))
    0))

(defn- report-failure [e]
  (binding [*out* *err*]
    (let [{:keys [out err]} (ex-data e)
          out (str/trim (str out))
          err (str/trim (str err))]
      (if (or (seq out) (seq err))
        (do (println (ex-message e))
            (when (seq out) (println out))
            (when (seq err) (println err)))
        (println (str e))))))

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (cond
      errors
      (do (binding [*out* *err*] (run! println errors)) (System/exit 1))

      (:help options)
      (do (println "dart2cljd verification gate\n")
          (println summary)
          (System/exit 0))

      :else
      (try
        (let [code (run options)]
          (println "verify: ok")
          (System/exit code))
        (catch clojure.lang.ExceptionInfo e
          (report-failure e)
          (System/exit 1))))))
