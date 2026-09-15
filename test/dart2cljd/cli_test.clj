(ns dart2cljd.cli-test
  "M5 CLI: DartClojure-compatible flags (`-r -e -m -f -c -p`) plus the
  dart2cljd additions (`-i --faithful -w --default-alias --dart`)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.api :as api]))

(def ^:private opts
  "Default conversion kwargs as produced by the CLI for faithful mode."
  {:material "m" :flutter "f" :width 100 :faithful false
   :default-alias nil :dart nil})

(defmacro ^:private silently [& body]
  `(binding [*out* (java.io.StringWriter.)
             *err* (java.io.StringWriter.)]
     ~@body))

(deftest help-and-usage
  (testing "--help exits 0 and lists the flags"
    (let [out (with-out-str (is (zero? (api/run-cli ["--help"]))))]
      (is (re-find #"--idiomatic" out))
      (is (re-find #"--material" out))))
  (testing "no arguments is a usage error"
    (is (= 2 (silently (api/run-cli [])))))
  (testing "invalid options and non-numeric widths are rejected"
    (is (= 1 (silently (api/run-cli ["--nope"]))))
    (is (= 1 (silently (api/run-cli ["-w" "abc" "Text('x')"]))))))

(deftest mode-selection
  (testing "faithful is the default"
    (is (nil? (:mode (api/cli-opts opts false))))
    (is (nil? (:mode (api/cli-opts (assoc opts :faithful true) false)))))
  (testing "-i selects idiomatic mode"
    (is (= :idiomatic (:mode (api/cli-opts opts true)))))
  (testing "--faithful overrides -i"
    (is (nil? (:mode (api/cli-opts (assoc opts :faithful true) true))))))

(deftest alias-options-are-passed-through
  (is (= "mm" (:material (api/cli-opts (assoc opts :material "mm") false))))
  (is (= "fl" (:flutter (api/cli-opts (assoc opts :flutter "fl") false))))
  (is (= "x" (:default-alias (api/cli-opts (assoc opts :default-alias "x") false))))
  (is (= 42 (:width (api/cli-opts (assoc opts :width 42) false)))))

(deftest colorize-is-deterministic
  (let [plain "(a \"b\")"
        colored (api/colorize plain)]
    (is (re-find #"\u001b\[" colored))
    (is (= colored (api/colorize plain)))
    (testing "an ANSI reset precedes the closing delimiter"
      (is (re-find #"\u001b\[0m\)" colored)))))

(deftest ^:integration cli-snippet
  (let [dart-bin (or (System/getenv "DART2CLJD_DART")
                     "/home/Schola/Projects/develop/flutter/bin/dart")]
    (if (.exists (io/file dart-bin))
      (do
        (testing "a positional snippet is translated"
          (is (= "(m/Text \"hi\")"
                 (str/trim (with-out-str
                             (api/run-cli ["Text('hi')" "-m" "m" "--dart" dart-bin]))))))
        (testing "`-` reads the snippet from stdin"
          (is (= "(m/Text \"hi\")"
                 (str/trim (with-out-str
                             (with-in-str "Text('hi')"
                               (api/run-cli ["-" "--dart" dart-bin]))))))))
      (is true "dart binary not present; skipped"))))
