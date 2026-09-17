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
    (is (= #{"m3" "idiomatic" "batchproj"} (set (map :name entries))))
    (is (= :faithful (:mode (first (filter #(= "m3" (:name %)) entries)))))
    (is (= :idiomatic (:mode (first (filter #(= "idiomatic" (:name %)) entries)))))
    (testing "each file entry has input and golden files"
      (doseq [{:keys [name] :as entry} entries
              :when (= :file (verify/entry-kind entry))]
        (is (.exists (io/file root "corpus" (str name ".dart"))))
        (is (.exists (io/file root "corpus" (str name ".cljd"))))))
    (testing "the directory entry names its source tree and expected failures"
      (let [entry (first (filter #(= :dir (verify/entry-kind %)) entries))]
        (is (= "test/batchproj/lib" (:dir entry)))
        (is (= "verify.gen.batchproj" (:ns-prefix entry)))
        (is (= ["orphan_part.dart"] (:expect-failures entry)))
        (is (= "main" (:entry entry)))
        (is (.isDirectory (io/file root (:dir entry))))))))

(deftest generated-namespaces
  (is (= 'verify.gen.m3 (verify/gen-ns "m3")))
  (is (= :file (verify/entry-kind {:name "m3"})))
  (is (= :dir (verify/entry-kind {:kind :dir :name "batchproj"})))
  (is (= 'verify.gen.batchproj.main
         (verify/entry-ns {:kind :dir :name "batchproj"
                           :ns-prefix "verify.gen.batchproj" :entry "main"})))
  (is (= 'verify.gen.idiomatic (verify/entry-ns {:name "idiomatic"}))))

(deftest generated-source-matches-golden-modulo-ns
  (doseq [{:keys [name mode] :as entry} (verify/manifest root)
          :when (= :file (verify/entry-kind entry))
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
    (doseq [entry entries]
      (is (str/includes? src (str "[" (verify/entry-ns entry)
                                  " :as " (:name entry) "]"))))
    (is (str/includes? src "[verify.gen.batchproj.main :as batchproj]"))
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
