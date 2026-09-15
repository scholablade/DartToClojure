(ns dart2cljd.verify-test
  "M5 verification harness: corpus manifest, generated namespaces and the
  generated verify.main. The heavy end-to-end gate is `make verify`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.ast :as ast]
            [dart2cljd.verify :as verify]))

(def ^:private root ".")

(def ^:private m3-unit
  (delay (:unit (ast/normalize-doc
                 (edn/read-string (slurp (io/file root "test/fixtures/m3.edn")))))))

(def ^:private idiomatic-unit
  (delay (:unit (ast/normalize-doc
                 (edn/read-string
                  (slurp (io/file root "test/fixtures/idiomatic.edn")))))))

(deftest manifest-is-complete
  (let [entries (verify/manifest root)]
    (is (= #{"m3" "idiomatic"} (set (map :name entries))))
    (is (= :faithful (:mode (first (filter #(= "m3" (:name %)) entries)))))
    (is (= :idiomatic (:mode (first (filter #(= "idiomatic" (:name %)) entries)))))
    (testing "each entry has input and golden files"
      (doseq [{:keys [name]} entries]
        (is (.exists (io/file root "corpus" (str name ".dart"))))
        (is (.exists (io/file root "corpus" (str name ".cljd"))))))))

(deftest generated-namespaces
  (is (= 'verify.gen.m3 (verify/gen-ns "m3"))))
(deftest generated-source-matches-golden-modulo-ns
  (doseq [{:keys [name mode] :as entry} (verify/manifest root)
          :let [golden (slurp (io/file root "corpus" (str name ".cljd")))
                unit (if (= "m3" name) @m3-unit @idiomatic-unit)
                source (verify/gen-source unit entry)
                expected (str/trim-newline
                          (str/replace-first
                           golden #"\(ns [A-Za-z0-9_.-]+"
                           (str "(ns " (verify/gen-ns name))))]]
    (testing (str name " (" mode ")")
      (is (= expected source)))))

(deftest main-source-requires-every-entry
  (let [entries (verify/manifest root)
        src (verify/main-source entries)]
    (is (str/starts-with? src "(ns verify.main"))
    (doseq [{:keys [name]} entries]
      (is (str/includes? src (str "[verify.gen." name " :as " name "]"))))
    (is (str/includes? src "(defn main [] nil)"))))

(deftest runtime-namespaces-is-a-single-entry-point
  (testing "one namespace only; checks requires main, which requires all entries"
    (is (= ["verify.checks"] verify/runtime-namespaces))))

(deftest strip-test-imports
  (let [dart (str "import \"dart:core\" as dc;\n"
                  "import \"../../../../test/cljd-out/verify/checks_test.dart\" as lcov_checks;\n"
                  "import \"m3.dart\" as lcovg_m3;\n"
                  "\nvoid main() {}\n")
        cleaned (verify/strip-test-imports dart)]
    (testing "renamed test imports are removed"
      (is (not (str/includes? cleaned "test/cljd-out")))
      (is (str/includes? cleaned "import \"m3.dart\""))
      (is (str/includes? cleaned "import \"dart:core\"")))
    (testing "text without such imports is unchanged"
      (is (= "void main() {}\n" (verify/strip-test-imports "void main() {}\n"))))))
