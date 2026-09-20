(ns dart2cljd.batch-test
  "M6 batch directory conversion: discovery/excludes, path->ns, the project
  index, import resolution and the report policy. The heavy end-to-end gate
  is `make verify` (the directory fixture is compiled by real ClojureDart)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.api :as api]
            [dart2cljd.batch :as batch]
            [dart2cljd.frontend :as frontend]
            [dart2cljd.library :as library]))

(def ^:private fixture-dir "test/batchproj/lib")

(def ^:private dart-bin
  (or (System/getenv "DART2CLJD_DART")
      "/home/Schola/Projects/develop/flutter/bin/dart"))

(defn- dart? [] (.exists (io/file dart-bin)))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "dart2cljd-batch"
        (into-array java.nio.file.attribute.FileAttribute []))))

(defn- write-file! [dir rel content]
  (let [f (io/file dir rel)]
    (io/make-parents f)
    (spit f content)))

(defn- run-cli-captured [args]
  (let [out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        exit (binding [*out* out *err* err] (api/run-cli args))]
    {:exit exit :out (str out) :err (str err)}))

;; ---------------------------------------------------------------------------
;; discovery and paths
;; ---------------------------------------------------------------------------

(deftest path-to-ns
  (testing "the inverse of ClojureDart's ns-to-paths"
    (is (= 'foo.bar-baz (batch/path->ns "foo/bar_baz.dart" {})))
    (is (= 'main (batch/path->ns "main.dart" {})))
    (is (= 'models.item (batch/path->ns "models/item.dart" {}))))
  (testing ":ns-prefix is prepended"
    (is (= 'app.foo.bar-baz (batch/path->ns "foo/bar_baz.dart" {:ns-prefix "app"})))
    (is (= 'verify.gen.batchproj.main
           (batch/path->ns "main.dart" {:ns-prefix "verify.gen.batchproj"}))))
  (testing "segments that cannot round-trip through ns-to-paths fail loudly"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"path segment"
                          (batch/path->ns "my-file.dart" {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"path segment"
                          (batch/path->ns "a:b.dart" {})))))

(deftest discovery
  (let [{:keys [files skipped]} (batch/discover {:dir fixture-dir})]
    (testing "sorted relative paths, .dart only"
      (is (= ["barrel.dart" "main.dart" "models/item.dart"
              "models/item_names.dart" "models/item_tags.dart"
              "orphan_part.dart" "widgets/button.dart" "widgets/mixins.dart"]
             files)))
    (testing "*.g.dart is skipped by default"
      (is (= [{:file "generated.g.dart" :reason :excluded}] skipped)))))

(deftest exclusions
  (testing "user globs are matched against the relative path"
    (let [{:keys [files skipped]} (batch/discover {:dir fixture-dir
                                                   :exclude ["widgets/**"]})]
      (is (= ["barrel.dart" "main.dart" "models/item.dart"
              "models/item_names.dart" "models/item_tags.dart"
              "orphan_part.dart"]
             files))
      (is (= ["generated.g.dart" "widgets/button.dart" "widgets/mixins.dart"]
             (map :file skipped)))))
  (testing "a pattern without / also matches the file name at any depth"
    (let [{:keys [files]} (batch/discover {:dir fixture-dir
                                           :exclude ["button.dart" "orphan*"]})]
      (is (= ["barrel.dart" "main.dart" "models/item.dart"
              "models/item_names.dart" "models/item_tags.dart"
              "widgets/mixins.dart"] files))))
  (testing "other default excludes are recognized"
    (doseq [generated ["x.freezed.dart" "x.mocks.dart"]]
      (let [dir (temp-dir)]
        (write-file! dir "a.dart" "class A {}")
        (write-file! dir generated "class X {}")
        (let [{:keys [files skipped]} (batch/discover {:dir dir})]
          (is (= ["a.dart"] files))
          (is (= 1 (count skipped)))))))
  (testing "discovery is deterministic"
    (is (= (batch/discover {:dir fixture-dir})
           (batch/discover {:dir fixture-dir})))))

(deftest pubspec-package-is-found-upwards
  (is (= "batchproj" (batch/pubspec-package fixture-dir)))
  (is (= "batchproj" (batch/pubspec-package "test/batchproj/lib/models")))
  (let [dir (temp-dir)]
    (is (nil? (batch/pubspec-package dir)))))

(deftest relative-import-resolution
  (is (= "models/item.dart"
         (batch/resolve-relative "widgets/button.dart" "../models/item.dart")))
  (is (= "widgets/button.dart"
         (batch/resolve-relative "main.dart" "./widgets/button.dart")))
  (is (= "models/item.dart"
         (batch/resolve-relative "widgets/button.dart" ".././models/item.dart")))
  (testing "escaping the converted tree is ::missing"
    (is (= :dart2cljd.batch/missing
           (batch/resolve-relative "main.dart" "../../outside.dart")))
    (is (= :dart2cljd.batch/missing
           (batch/resolve-relative "widgets/button.dart" "../../../x.dart")))))

(deftest project-targets
  (let [index {:file->ns {"main.dart" 'app.main
                          "models/item.dart" 'app.models.item}
               :package "app"}]
    (testing "own package and relative imports resolve"
      (is (= 'app.models.item
             (batch/project-target index "main.dart"
                                   "package:app/models/item.dart")))
      (is (= 'app.models.item
             (batch/project-target index "widgets/button.dart"
                                   "../models/item.dart"))))
    (testing "external libraries stay string requires"
      (is (nil? (batch/project-target index "main.dart" "dart:math")))
      (is (nil? (batch/project-target index "main.dart"
                                      "package:flutter/material.dart"))))
    (testing "unresolved relative / own-package imports are ::missing"
      (is (= :dart2cljd.batch/missing
             (batch/project-target index "main.dart" "models/nope.dart")))
      (is (= :dart2cljd.batch/missing
             (batch/project-target index "main.dart" "package:app/nope.dart"))))))

;; ---------------------------------------------------------------------------
;; project index (requires the Dart frontend)
;; ---------------------------------------------------------------------------

(defn- fixture-parsed [& files]
  (mapv (fn [rel]
          {:file rel
           :unit (batch/parse-unit rel {:dir fixture-dir :dart dart-bin})})
        files))

(deftest project-index
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [parsed (fixture-parsed "barrel.dart" "main.dart" "models/item.dart"
                                 "models/item_names.dart" "models/item_tags.dart"
                                 "orphan_part.dart" "widgets/button.dart"
                                 "widgets/mixins.dart")
          grouped (library/group parsed {})
          index (batch/build-index (map #(select-keys % [:file :unit])
                                        (:libraries grouped))
                                   {:ns-prefix "app" :package "batchproj"})]
      (is (= {"barrel.dart" 'app.barrel
              "main.dart" 'app.main
              "models/item.dart" 'app.models.item
              "widgets/button.dart" 'app.widgets.button
              "widgets/mixins.dart" 'app.widgets.mixins}
             (:file->ns index)))
      (is (= #{"Item" "Box" "Button" "Label" "Badge" "Labelled" "Holder" "Tag"}
             (set (keys (:project-types index)))))
      (testing "part files are merged into their library (M9)"
        (is (= ["models/item_tags.dart" "models/item_names.dart"]
               (get-in grouped [:libraries 2 :parts])))
        (is (= {"models/item_tags.dart" "models/item.dart"
                "models/item_names.dart" "models/item.dart"}
               (:parts grouped)))
        (is (= ["orphan_part.dart"] (map :file (:errors grouped))))
        (is (str/includes? (ex-message (get-in grouped [:errors 0 :error]))
                           "does not declare this file as a part")))
      (testing "part declarations are indexed under their library"
        (is (= 'app.models.item (get-in index [:project-types "Tag" :ns])))
        (is (= #{nil} (get-in index [:project-types "Tag" :ctors])))
        (is (= 'app.models.item (get-in index [:project-values "tagLine" :ns])))
        (is (= 'app.models.item
               (get-in index [:project-values "itemDisplayName" :ns]))))
      (testing "private names are not indexed (library-scoped in Dart)"
        (is (nil? (get-in index [:project-types "_Meta"])))
        (is (nil? (get-in index [:project-values "_helper"]))))
      (testing "mixin declarations are indexed for cross-file inlining"
        (is (= :mixin (get-in index [:project-mixins "Labelled" :kind])))
        (is (= 'app.widgets.mixins (get-in index [:project-mixins "Labelled" :ns])))
        (is (= "Labelled" (get-in index [:project-mixins "Labelled" :decl :name]))))
      (testing "external imports are collected for the exports shim"
        (is (= #{"dart:math"} (:external-imports index))))
      (testing "class info: namespace, constructors and statics"
        (is (= 'app.models.item (get-in index [:project-types "Item" :ns])))
        (is (= :class (get-in index [:project-types "Item" :kind])))
        (is (= #{nil} (get-in index [:project-types "Item" :ctors])))
        (is (= {"kind" #{:field} "count" #{:field}
                "total" #{:getter :setter} "make" #{:method}}
               (get-in index [:project-types "Item" :statics]))))
      (testing "typedef targets are expanded in the declaring file's context"
        (is (= 'String (get-in index [:project-types "Label" :typedef-target]))))
      (testing "top-level values are indexed with their kinds"
        (is (= #{:fn} (get-in index [:project-values "tag" :kinds])))
        (is (= 'app.models.item (get-in index [:project-values "tag" :ns])))
        (is (= #{:var} (get-in index [:project-values "itemCount" :kinds])))
        (is (= #{:fn} (get-in index [:project-values "bumpCount" :kinds]))))
      (testing "no warnings for distinct names"
        (is (empty? (:warnings index)))))))

(deftest duplicate-type-names-warn-and-first-wins
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [dir (temp-dir)]
      (write-file! dir "a/item.dart" "class Item {\n  int a = 1;\n}\n")
      (write-file! dir "b/item.dart" "class Item {\n  int b = 2;\n}\n")
      (let [parsed (mapv (fn [rel]
                           {:file rel
                            :unit (batch/parse-unit rel {:dir dir :dart dart-bin})})
                         ["a/item.dart" "b/item.dart"])
            index (batch/build-index parsed {:package "dup"})]
        (is (= 1 (count (:warnings index))))
        (is (str/includes? (first (:warnings index)) "duplicate type Item"))
        (is (str/includes? (first (:warnings index)) "a/item.dart"))
        (testing "first file in sorted order wins"
          (is (= 'a.item (get-in index [:project-types "Item" :ns]))))))))

;; ---------------------------------------------------------------------------
;; library assembly (pure, no Dart frontend; M9)
;; ---------------------------------------------------------------------------

(defn- ir-unit
  "Minimal normalized unit for `dart2cljd.library/group` tests."
  [& {:keys [parts part-of library decls]}]
  {:ir :unit
   :decls (or decls [])
   :directives (cond-> []
                 library (conj {:ir :library :name library
                                :src {:line 1 :col 1}})
                 parts (into (map-indexed
                              (fn [i p]
                                {:ir :part
                                 :uri {:ir :string-lit :value p
                                       :src {:line (+ 2 i) :col 6}}
                                 :src {:line (+ 2 i) :col 1}})
                              parts))
                 part-of (conj {:ir :part-of
                                :uri (when (:uri part-of)
                                       {:ir :string-lit :value (:uri part-of)})
                                :library (:library part-of)
                                :src {:line 1 :col 1}}))})

(deftest library-grouping-is-pure
  (testing "parts merge into the library in directive order"
    (let [grouped (library/group
                   [{:file "main.dart"
                     :unit (ir-unit :library "my.lib"
                                    :parts ["a.dart" "b.dart"]
                                    :decls [{:ir :class :name "A"}])}
                    {:file "a.dart"
                     :unit (ir-unit :part-of {:uri "main.dart"}
                                    :decls [{:ir :class :name "B"}])}
                    {:file "b.dart"
                     :unit (ir-unit :part-of {:library "my.lib"}
                                    :decls [{:ir :class :name "C"}])}]
                   {})]
      (is (empty? (:errors grouped)))
      (is (= ["a.dart" "b.dart"] (get-in grouped [:libraries 0 :parts])))
      (is (= {"a.dart" "main.dart" "b.dart" "main.dart"} (:parts grouped)))
      (is (= ["A" "B" "C"]
             (map :name (get-in grouped [:libraries 0 :unit :decls]))))
      (is (empty? (filter #(contains? #{:part :part-of} (:ir %))
                          (get-in grouped [:libraries 0 :unit :directives]))))))
  (testing "a named `part of` resolves through `library name;`"
    (let [grouped (library/group
                   [{:file "main.dart"
                     :unit (ir-unit :library "my.lib" :parts ["b.dart"])}
                    {:file "b.dart" :unit (ir-unit :part-of {:library "my.lib"})}]
                   {})]
      (is (empty? (:errors grouped)))
      (is (= ["b.dart"] (get-in grouped [:libraries 0 :parts])))))
  (testing "a missing part fails its library"
    (let [grouped (library/group
                   [{:file "main.dart" :unit (ir-unit :parts ["gone.dart"])}]
                   {})]
      (is (str/includes? (ex-message (get-in grouped [:libraries 0 :error]))
                         "does not exist"))
      (is (= [] (get-in grouped [:libraries 0 :parts])))
      (is (nil? (get-in grouped [:libraries 0 :unit])))))
  (testing "an excluded part fails its library with the policy message"
    (let [grouped (library/group
                   [{:file "main.dart" :unit (ir-unit :parts ["gen.g.dart"])}]
                   {:excluded #{"gen.g.dart"}})]
      (is (str/includes? (ex-message (get-in grouped [:libraries 0 :error]))
                         "exclude pattern"))))
  (testing "a part claimed by two libraries fails both"
    (let [grouped (library/group
                   [{:file "a.dart" :unit (ir-unit :parts ["p.dart"])}
                    {:file "b.dart" :unit (ir-unit :parts ["p.dart"])}
                    {:file "p.dart" :unit (ir-unit :part-of {:uri "a.dart"})}]
                   {})]
      (is (= ["a.dart" "b.dart"]
             (map :file (filter :error (:libraries grouped)))))
      (is (str/includes? (ex-message (get-in grouped [:libraries 1 :error]))
                         "already a part of"))))
  (testing "an orphan `part of` file fails on its own"
    (let [grouped (library/group
                   [{:file "main.dart" :unit (ir-unit :parts [])}
                    {:file "p.dart" :unit (ir-unit :part-of {:uri "main.dart"})}]
                   {})]
      (is (= "p.dart" (:file (first (:errors grouped)))))
      (is (str/includes? (ex-message (:error (first (:errors grouped))))
                         "does not declare this file as a part"))))
  (testing "declarations are ordered by local type references (eager cljd types)"
    (let [uses {:ir :class :name "Uses"
                :members [{:ir :method :name "m"
                           :return-type {:ir :named-type :name "Tag"
                                         :prefix nil}}]}
          tag {:ir :class :name "Tag"}]
      (is (= ["Tag" "Uses"] (map :name (library/order-decls [uses tag]))))
      (testing "cycles keep the original order (loud at compile time)"
        (let [a {:ir :class :name "A"
                 :extends {:ir :named-type :name "B" :prefix nil}}
              b {:ir :class :name "B"
                 :extends {:ir :named-type :name "A" :prefix nil}}]
          (is (= ["A" "B"] (map :name (library/order-decls [a b])))))))))

;; ---------------------------------------------------------------------------
;; batch conversion (requires the Dart frontend)
;; ---------------------------------------------------------------------------

(defn- convert-fixture [out]
  (batch/convert-dir {:dir fixture-dir :out out :ns-prefix "batch"
                      :exports-file (str out "/d2c_exports.dart")
                      :exports-uri "package:batchproj/d2c_exports.dart"
                      :dart dart-bin}))

(deftest convert-dir-report-and-output
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [out (temp-dir)
          r (convert-fixture out)]
      (testing "the run keeps going: converted, merged, skipped and failed are separated"
        (is (= ["barrel.dart" "main.dart" "models/item.dart"
                "widgets/button.dart" "widgets/mixins.dart"]
               (map :file (:converted r))))
        (is (= ["models/item_tags.dart" "models/item_names.dart"]
               (map :file (:merged r)))
            "parts merge in part-directive order, into `models/item.dart`")
        (is (= ["models/item.dart" "models/item.dart"]
               (map :into (:merged r))))
        (is (= ["generated.g.dart"] (map :file (:skipped r))))
        (is (= ["orphan_part.dart"] (map :file (:failed r))))
        (is (= 1 (:exit r)))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "does not declare this file as a part")))
      (testing "cross-file imports become symbol requires"
        (let [main (slurp (io/file out "main.cljd"))]
          (is (str/includes? main "[batch.models.item :as item]"))
          (is (str/includes? main "[batch.widgets.button :as button]"))
          (is (str/includes? main "[\"dart:math\" :as math]")))
        (let [button (slurp (io/file out "widgets/button.cljd"))]
          (is (str/includes? button "[batch.models.item :as item]"))))
      (testing "cross-file type references resolve to generated namespaces"
        (let [main (slurp (io/file out "main.cljd"))
              button (slurp (io/file out "widgets/button.cljd"))]
          (is (str/includes? main "batch.models.item/Item-new"))
          (is (str/includes? main "batch.widgets.button/Button-new"))
          (is (str/includes? button "^batch.models.item/Item item"))
          (is (not (str/includes? button "^m/Item")))))
      (testing "cross-file constructors, statics and getters"
        (let [main (slurp (io/file out "main.cljd"))]
          (is (str/includes? main "batch.models.item/Item-kind"))
          (is (str/includes? main "(batch.models.item/Item-make \"box\")"))
          (is (str/includes? main "(.-label (batch.models.item/Item-make"))))
      (testing "cross-file top-level values keep their generated namespaces"
        (let [main (slurp (io/file out "main.cljd"))
              item (slurp (io/file out "models/item.cljd"))]
          (is (str/includes? main "(batch.models.item/tag item)"))
          (is (str/includes? main "(set! batch.models.item/itemCount 0)"))
          (is (str/includes? main "(set! batch.models.item/Item-count (+ batch.models.item/Item-count 2))"))
          (is (str/includes? main "(batch.models.item/set-total! 0)"))
          (is (str/includes? main "(batch.models.item/Item-total)"))
          (testing "same-file values stay bare"
            (is (str/includes? item "(set! itemCount (inc itemCount))"))
            (is (not (str/includes? item "(set! batch.models.item/itemCount"))))))
      (testing "cross-file typedefs expand to their target type"
        (let [button (slurp (io/file out "widgets/button.cljd"))]
          (is (str/includes? button "^String caption"))
          (is (not (str/includes? button "Label")))))
      (testing "local fields shadow project import aliases"
        (let [button (slurp (io/file out "widgets/button.cljd"))]
          (is (str/includes? button "(.-display item)"))))
      (testing "cross-file mixins are inlined and implemented"
        (let [barrel (slurp (io/file out "barrel.cljd"))
              mixins (slurp (io/file out "widgets/mixins.cljd"))]
          (is (str/includes? mixins "(deftype ^:mixin Labelled"))
          (is (str/includes? mixins "^String label"))
          (is (str/includes? barrel "[batch.widgets.mixins :as mixins]"))
          (is (str/includes? barrel "mixins/Labelled"))
          (is (not (str/includes? barrel "^:mixin mixins/Labelled"))
              "inlined mixins are implemented, not mixed in")
          (is (str/includes? barrel "(.describeLabelled this"))))
      (testing "the barrel requires the exports shim for its external exports"
        (let [shim (slurp (io/file out "d2c_exports.dart"))
              barrel (slurp (io/file out "barrel.cljd"))
              main (slurp (io/file out "main.cljd"))]
          (is (str/includes? shim "export 'package:flutter/material.dart';"))
          (is (str/includes? barrel
                             "[\"package:batchproj/d2c_exports.dart\" :as exports]"))
          (is (str/includes? main
                             "[\"package:batchproj/d2c_exports.dart\" :as exports]"))
          (is (str/includes? main "exports/Text"))
          (is (str/includes? main "^exports/Widget child"))
          (is (str/includes? main "batch.barrel/Badge-new"))))
      (testing "same-file top-level values stay bare under the shim"
        (let [main (slurp (io/file out "main.cljd"))]
          (is (str/includes? main "(def _motto"))
          (is (str/includes? main "_motto)"))
          (is (not (str/includes? main "exports/_motto")))))
      (testing "part declarations are merged into the library output (M9)"
        (let [item (slurp (io/file out "models/item.cljd"))
              main (slurp (io/file out "main.cljd"))]
          (testing "the library keeps its declarations and gains the parts'"
            (is (str/includes? item "(defn tagLine"))
            (is (str/includes? item "(deftype Tag"))
            (is (str/includes? item "(defn ^Tag Tag-new"))
            (is (str/includes? item "(defn itemDisplayName")))
          (testing "a private type declared in a part is renamed for ClojureDart"
            (is (str/includes? item "d2c_Meta"))
            (is (not (str/includes? item " _Meta"))
                "the private Dart name is renamed, not dropped"))
          (testing "part declarations are called across files through the library ns"
            (is (str/includes? main "(batch.models.item/tagLine"))
            (is (str/includes? main "(batch.models.item/Tag-new"))
            (is (str/includes? main "(batch.models.item/itemDisplayName")))))
      (testing "part files and skipped generated files are not written"
        (is (not (.exists (io/file out "models/item_tags.cljd"))))
        (is (not (.exists (io/file out "models/item_names.cljd"))))
        (is (not (.exists (io/file out "orphan_part.cljd")))))
      (testing "the skipped generated file is not written"
        (is (not (.exists (io/file out "generated.g.cljd"))))))))

(defn- convert-temp
  "Convert `files` (`{rel content}`) in a temp project; returns the report."
  [files]
  (let [dir (temp-dir)
        out (temp-dir)]
    (doseq [[rel content] files]
      (write-file! dir rel content))
    (batch/convert-dir {:dir dir :out out :dart dart-bin :package "tmp"})))

(deftest cross-file-loud-failures
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (do
      (testing "an unknown static member on another file's class"
      (let [r (convert-temp {"a.dart" "class A {\n  static int x = 1;\n}\n"
                             "b.dart" "import 'a.dart';\nvoid f() {\n  A.nope = 2;\n}\n"})]
        (is (= 1 (:exit r)))
        (is (= ["b.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "unknown static member"))))
    (testing "a cross-file getter called as a function"
      (let [r (convert-temp {"a.dart" "int get total => 1;\n"
                             "b.dart" "import 'a.dart';\nint f() {\n  return total();\n}\n"})]
        (is (= ["b.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "is a getter"))))
    (testing "a typedef that resolves to a type parameter"
      (let [r (convert-temp {"a.dart" "typedef Id<T> = T;\n"
                             "b.dart" (str "import 'a.dart';\n"
                                           "class B {\n"
                                           "  final Id value = 1;\n"
                                           "}\n")})]
        (is (= ["b.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "cannot expand typedef"))))
    (testing "a generic mixin whose members use the type parameter"
      (let [r (convert-temp {"a.dart" (str "mixin M<T> {\n"
                                           "  T id(T x) => x;\n"
                                           "}\n")
                             "b.dart" (str "import 'a.dart';\n"
                                           "class C with M<int> {}\n")})]
        (is (= ["a.dart" "b.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "type parameter"))))
    (testing "an applier member colliding with a mixin member"
      (let [r (convert-temp {"a.dart" "mixin M {\n  int get x => 1;\n}\n"
                             "b.dart" (str "import 'a.dart';\n"
                                           "class C with M {\n"
                                           "  int x = 2;\n"
                                           "}\n")})]
        (is (= ["b.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "collides")))))))

;; ---------------------------------------------------------------------------
;; part / part of policy (M9)
;; ---------------------------------------------------------------------------

(deftest part-policy
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (do
      (testing "a missing part fails its library"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "main.dart" "part 'gone.dart';\nint f() => 1;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp"})]
            (is (= ["main.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "does not exist"))
            (is (empty? (:converted r))))))
      (testing "an excluded part fails its library with an actionable message"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "main.dart" "part 'part.dart';\nint f() => 1;\n")
          (write-file! dir "part.dart" "part of 'main.dart';\nint g() => 2;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp" :exclude ["part.dart"]})]
            (is (= ["main.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "exclude pattern"))
            (is (= ["part.dart"] (map :file (:skipped r))))
            (is (empty? (:converted r))))))
      (testing "a part whose `part of` names another library fails both"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "main.dart" "part 'part.dart';\nint f() => 1;\n")
          (write-file! dir "other.dart" "int h() => 3;\n")
          (write-file! dir "part.dart" "part of 'other.dart';\nint g() => 2;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp"})]
            (is (= ["main.dart" "part.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "not this library"))
            (is (= ["other.dart"] (map :file (:converted r)))))))
      (testing "a part claimed by two libraries fails both"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "a.dart" "part 'part.dart';\nint f() => 1;\n")
          (write-file! dir "b.dart" "part 'part.dart';\nint h() => 3;\n")
          (write-file! dir "part.dart" "part of 'a.dart';\nint g() => 2;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp"})]
            (is (= ["a.dart" "b.dart" "part.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "is also a part of"))
            (is (empty? (:converted r))))))
      (testing "a part that declares its own parts fails"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "main.dart" "part 'part.dart';\nint f() => 1;\n")
          (write-file! dir "part.dart"
                       "part of 'main.dart';\npart 'nested.dart';\nint g() => 2;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp"})]
            (is (= ["main.dart" "part.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "could not be parsed")
                "the analyzer itself rejects a part that declares parts"))))
      (testing "a named `part of` without a matching `library` fails"
        (let [dir (temp-dir)
              out (temp-dir)]
          (write-file! dir "part.dart" "part of some.lib;\nint g() => 2;\n")
          (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin
                                      :package "tmp"})]
            (is (= ["part.dart"] (map :file (:failed r))))
            (is (str/includes? (ex-message (:error (first (:failed r))))
                               "no converted library declares")))))
      (testing "a `part of` file cannot be converted on its own"
        (let [dir (temp-dir)]
          (write-file! dir "part.dart" "part of 'main.dart';\nint g() => 2;\n")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"part of.*file"
                                (api/convert-file (str dir "/part.dart")
                                                  :dart dart-bin)))))
      (testing "single-file conversion merges the library's parts"
        (let [dir (temp-dir)]
          (write-file! dir "main.dart"
                       "part 'part.dart';\nint f() => g() + 1;\n")
          (write-file! dir "part.dart" "part of 'main.dart';\nint g() => 2;\n")
          (let [source (api/convert-file (str dir "/main.dart") :dart dart-bin)]
            (is (str/includes? source "(defn g"))
            (is (str/includes? source "(defn f"))))))))

(deftest convert-dir-is-deterministic
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [out-a (temp-dir)
          out-b (temp-dir)
          a (convert-fixture out-a)
          b (convert-fixture out-b)]
      (is (= (map (juxt :file :ns) (:converted a))
             (map (juxt :file :ns) (:converted b))))
      (is (= (map (juxt :file :into :ns) (:merged a))
             (map (juxt :file :into :ns) (:merged b))))
      (is (= (:skipped a) (:skipped b)))
      (is (= (map :file (:failed a)) (map :file (:failed b))))
      (doseq [{:keys [file]} (:converted a)
              :let [cljd (str/replace file #"\.dart$" ".cljd")]]
        (is (= (slurp (io/file out-a cljd))
               (slurp (io/file out-b cljd)))
            (str file " is byte-identical across runs"))))))

(deftest unresolved-relative-import-fails-per-file
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [dir (temp-dir)
          out (temp-dir)]
      (write-file! dir "main.dart" "import './nope.dart';\n\nint one() => 1;\n")
      (let [r (batch/convert-dir {:dir dir :out out :dart dart-bin})]
        (is (= 1 (:exit r)))
        (is (= ["main.dart"] (map :file (:failed r))))
        (is (str/includes? (ex-message (:error (first (:failed r))))
                           "cannot resolve import"))
        (is (empty? (:converted r)))))))

(deftest convert-dir-requires-dir-and-out
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires :dir"
                        (batch/convert-dir {:out "x"})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires :out"
                        (batch/convert-dir {:dir "x"}))))

;; ---------------------------------------------------------------------------
;; external function typedefs (M8.10)
;; ---------------------------------------------------------------------------

(deftest fn-typedefs-become-function-hints
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [src (temp-dir)
          out (temp-dir)]
      (write-file! src "holder.dart"
                   (str "import 'package:flutter/material.dart';\n"
                        "import 'package:dummy/dummy.dart';\n\n"
                        "class Holder {\n"
                        "  final VoidCallback onDone;\n"
                        "  final VoidCallback? onCancel;\n"
                        "  Holder(this.onDone, this.onCancel);\n"
                        "}\n"))
      ;; The resolved type-kind pass needs the source project's pub
      ;; `package_config.json`; stub it so the emitter mapping is tested
      ;; without a Flutter dependency.
      (with-redefs [frontend/fn-typedefs (fn [_ _] #{"VoidCallback"})]
        (let [r (batch/convert-dir {:dir src :out out :dart dart-bin
                                    :root (System/getProperty "user.dir")})
              source (slurp (io/file out "holder.cljd"))]
          (is (= 1 (count (:converted r))))
          (is (str/includes? source "^Function onDone"))
          (is (str/includes? source "^Function? onCancel"))
          (is (not (str/includes? source "VoidCallback"))))))))

(deftest dart-core-names-stay-bare
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [src (temp-dir)
          out (temp-dir)]
      (write-file! src "entries.dart"
                   (str "import 'package:dummy/dummy.dart';\n\n"
                        "final MapEntry entry = MapEntry('a', 1);\n"
                        "final Pattern pat = RegExp('x');\n"))
      (let [r (with-redefs [frontend/fn-typedefs (fn [_ _] #{})]
                (batch/convert-dir {:dir src :out out :dart dart-bin
                                    :root (System/getProperty "user.dir")}))
            source (slurp (io/file out "entries.cljd"))]
        (is (= 1 (count (:converted r))))
        (testing "dart:core names are not routed through the default alias/shim"
          (is (str/includes? source "(MapEntry \"a\" 1)"))
          (is (str/includes? source "(RegExp \"x\")"))
          (is (not (str/includes? source "m/MapEntry")))
          (is (not (str/includes? source "exports/MapEntry"))))))))

(deftest ^:integration fn-typedefs-mode-resolves-sdk-typedefs
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [names (frontend/fn-typedefs ["dart:async"]
                                      {:root (System/getProperty "user.dir")
                                       :dart dart-bin
                                       :ref "corpus/m3.dart"})]
      (is (contains? names "ZoneCallback"))
      (is (contains? names "ZoneUnaryCallback"))
      (is (not (contains? names "Future")) "classes are not function typedefs"))))

(deftest ^:integration typed-run-uses-doc-fn-typedefs
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [src (temp-dir)
          out (temp-dir)]
      (write-file! src "holder.dart"
                   (str "import 'dart:async';\n\n"
                        "class Holder {\n"
                        "  final ZoneCallback? onDone;\n"
                        "  Holder(this.onDone);\n"
                        "}\n"))
      ;; With :types the per-file typed dump resolves the function typedefs of
      ;; its external imports, so the separate `frontend/fn-typedefs` pass must
      ;; not run (it would throw here) and the hint still maps to `Function`
      ;; (M12 follow-up).
      (with-redefs [frontend/fn-typedefs
                    (fn [_ _]
                      (throw (ex-info "the fn-typedefs pass must be skipped with :types"
                                      {})))]
        (let [r (batch/convert-dir {:dir src :out out :dart dart-bin :types true
                                    :root (System/getProperty "user.dir")})
              source (slurp (io/file out "holder.cljd"))]
          (is (= 1 (count (:converted r))))
          (is (str/includes? source "^Function? onDone"))
          (is (not (str/includes? source "ZoneCallback"))))))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(deftest cli-batch
  (testing "a nonexistent --dir is a usage-style error"
    (let [{:keys [exit err]} (run-cli-captured ["--dir" "/nope/nope"
                                                "--out" "/tmp/opencode/nope"])]
      (is (= 1 exit))
      (is (str/includes? err "not a directory"))))
  (testing "--dir without --out is rejected"
    (let [{:keys [exit err]} (run-cli-captured ["--dir" fixture-dir])]
      (is (= 1 exit))
      (is (str/includes? err "--dir requires --out"))))
  (if-not (dart?)
    (is true "dart binary not present; skipped")
    (let [dir (temp-dir)
          {:keys [exit out err]} (run-cli-captured ["--dir" fixture-dir
                                                    "--out" dir
                                                    "--ns-prefix" "batch"
                                                    "--exports-file"
                                                    (str dir "/d2c_exports.dart")
                                                    "--exports-uri"
                                                    "package:batchproj/d2c_exports.dart"
                                                    "--dart" dart-bin])]
      (is (= 1 exit))
      (is (str/includes? out "converted 5, merged 2, skipped 1, failed 1"))
      (is (str/includes? err "orphan_part.dart"))
      (is (str/includes? err "does not declare this file as a part"))
      (is (.exists (io/file dir "main.cljd"))))))
