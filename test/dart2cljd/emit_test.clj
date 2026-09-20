(ns dart2cljd.emit-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.api :as api]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer]))

;; ---------------------------------------------------------------------------
;; fixtures
;; ---------------------------------------------------------------------------

(defn- fixture [name]
  (ast/normalize-doc (edn/read-string (slurp (io/resource (str "fixtures/" name))))))

(def parity-doc (delay (fixture "parity.edn")))
(def parity-ctx (delay (decls/ctx-for (decls/unit-aliases (:unit @parity-doc) {}) {})))
(def parity-exprs
  (delay (into {}
               (for [d (:decls (:unit @parity-doc))
                     :when (= :top-var-decl (:ir d))
                     v (:vars (:vars d))]
                 [(:name v) (:init v)]))))

(defn- emit-expr-str [name]
  (printer/flat (core/emit-expr (get @parity-exprs name) @parity-ctx)))

(def statements-doc (delay (fixture "statements.edn")))
(def statements-ctx (delay (decls/ctx-for (decls/unit-aliases (:unit @statements-doc) {}) {})))
(def statements-decls
  (delay (into {} (map (juxt :name identity) (:decls (:unit @statements-doc))))))

(defn- emit-defn-str [name]
  (printer/print-form (first (decls/emit-decl (get @statements-decls name) @statements-ctx))))

(def m3-doc (delay (fixture "m3.edn")))

(def m3-forms
  (delay (decls/emit-unit (:unit @m3-doc) {:path "corpus/m3.dart"})))

;; ---------------------------------------------------------------------------
;; M3 golden (whole-file faithful translation; regenerate: make golden)
;; ---------------------------------------------------------------------------

(deftest m3-golden
  (testing "corpus/m3.dart translates byte-identically to the reviewed golden file"
    (is (= (slurp (io/file "corpus/m3.cljd"))
           (str (printer/print-forms @m3-forms) "\n")))))

(deftest m3-key-forms
  (testing "M3 constructs survive emission"
    (let [out (printer/print-forms @m3-forms)
          by-name (fn [tag n]
                    (some #(and (seq? %) (= tag (first %)) (= n (second %)))
                          @m3-forms))]
      (is (some? (by-name 'deftype 'Point)))
      (is (some? (by-name 'deftype 'Child)))
      (is (some? (by-name 'deftype 'Color)))
      (is (some? (by-name 'defn 'fib)))
      (is (some? (by-name 'defn 'main)))
      (testing "record literals with computed fields are identity-wrapped"
        (is (re-find #"\(cljd\.core/identity #dart" out)))
      (testing "user operators dispatch through interop calls"
        (is (re-find #"\(\. p \+ \(Point-make 1\)\)" out)))
      (testing "make-fn return tags and scalar parameter hints"
        (is (re-find #"\(defn \^Point Point-new \[\^double x \^double y\]" out)))
      (testing "setters end in nil instead of returning a value"
        (is (re-find #"\^:setter norm \[this \^double v\] \(set! x v\) \(set! y v\) nil\)" out)))
      (testing "guarded returns guard every following statement"
        (is (re-find #"(?s)\(defn fib.*\(when \(not \(deref d2c\$done\)\)\s*\(do \(vreset! d2c\$result \(\+ \(fib" out)))
      (testing "override parameters stay unhinted when the local base is unhinted"
        (is (re-find #"\^String greet \[this other\]" out))))))

(deftest string-concatenation-and-closure-mutation
  (testing "String + / += emit str instead of the numeric + (M10)"
    (let [out (printer/print-forms @m3-forms)]
      (is (re-find #"\(vswap! out str \"!\"\)" out))
      (is (re-find #"\(vreset! out \(str \(deref out\) \"\?\" \(\.toString n\)\)\)" out))
      (is (re-find #"\.-label! this \(str \(\.-label this\) start\)" out))
      (is (re-find #"\(set! label \(str label \"\?\"\)\)" out))
      (testing "numeric compound assignment is unaffected"
        (is (re-find #"\(set! count \(\+ count 1\)\)" out)))))
  (testing "a local mutated inside a nested function is boxed as an atom"
    (let [out (printer/print-forms @m3-forms)]
      (is (re-find #"\(let \[total \(atom 0\) bump \(fn \[\] \(swap! total \+ n\) nil\)\]" out))
      (is (re-find #"\(let \[out \(atom \"x\"\) add \(fn \[\] \(swap! out str \"y\"\) nil\)\]" out)))))

(deftest fn-typedef-type-symbol
  (testing "external function typedefs map to Function (ClojureDart's analyzer
            helper crashes on function-typedef elements; RESEARCH M8.10)"
    (let [ctx (assoc (core/ctx {} {}) :fn-typedefs #{"VoidCallback"})
          named (fn [n nullable]
                  {:ir :named-type :prefix nil :name n :nullable nullable})]
      (is (= 'Function (core/type-symbol (named "VoidCallback" false) ctx)))
      (is (= 'Function? (core/type-symbol (named "VoidCallback" true) ctx)))
      (testing "names the pass did not resolve keep their ordinary hint"
        (is (= 'Widget (core/type-symbol (named "Widget" false) ctx)))))))

;; ---------------------------------------------------------------------------
;; expression parity (seeded from DartClojure's corpus, corrected to
;; canonical cljd syntax: nil instead of null, dot-symbols for named args,
;; alias-qualified types, .name static access, some-> for null-aware chains)
;; ---------------------------------------------------------------------------

(deftest parity-expressions
  (doseq [[name expected]
          {"p01_add" "(+ 1 2)"
           "p02_arith_chain" "(- (+ 1 2 3) 4)"
           "p03_div" "(/ 7 2)"
           "p04_intdiv" "(. 7 \"~/\" 2)"
           "p05_mod" "(. 7 % 3)"
           "p06_logic" "(or (and true false) (not true))"
           "p07_ifnull" "(or nil 3)"
           "p08_ternary" "(if (> 1 0) \"pos\" \"neg\")"
           "p09_eq" "(= 1 2)"
           "p10_neq" "(not= 1 2)"
           "p11_is" "(dart/is? 1 int)"
           "p12_is_not" "(not (dart/is? 1 String))"
           "p13_cast" "^num 1"
           "p14_interp" "(str \"Some \" name \" and \" (foo) \"!\")"
           "p15_interp_only" "(str x)"
           "p16_literal_interp" "(str \"a\" 1 \"b\")"
           "p17_raw" "\"Some $x ${y}\""
           "p18_adjacent" "(str \"a\" \"b\")"
           "p19_list" "[1 2 3]"
           "p20_list_typed" "[1 2]"
           "p21_list_if" "[(if true 1 2)]"
           "p22_list_for" "(vec (concat (for [i [1 2]] i)))"
           "p23_list_spread" "(vec (concat [1] xs))"
           "p23b_list_spread_if" "(vec (concat [1] (if c [2 3] nil) [4]))"
           "p23c_list_spread_if_else" "(vec (concat (if c [1] [2 3])))"
           "p23d_list_for_spread" "(vec (concat (loop [^int i 0 d2c$acc []] (if (< i n) (let [d2c$seg [i i]] (recur (inc i) (into d2c$acc d2c$seg))) d2c$acc))))"
           "p23e_list_null_aware" "(vec (concat (if (nil? maybe) [] [maybe])))"
           "p24_set" "#{1 2 3}"
           "p25_map" "{\"a\" 1 \"b\" 2}"
           "p26_map_spread" "(merge {\"a\" 1} other)"
           "p27_index" "(get a 0)"
           "p28_index_chain" "(get (get a 0) 1)"
           "p29_prop" "(.-field obj)"
           "p30_prop_chain" "(-> obj .-a .-b)"
           "p31_method" "(.doIt obj 1 2)"
           "p32_method_chain" "(-> obj (.a) .-b)"
           "p33_null_chain" "(some-> obj .-a .-b)"
           "p34_ctor" "(m/Text \"hi\" .style style)"
           "p35_ctor_named" "(Duration .milliseconds 250)"
           "p36_static_field" "m/Colors.blue"
           "p37_static_index" "(get m/Colors.red 500)"
           "p38_static_call" "(m/BorderRadius.circular 8)"
           "p39_static_chain" "m/Colors.grey.shade300"
            "p40_lambda" "(fn [^int a] (+ a 1))"
            "p41_lambda_block" "(fn [^int a] (+ a 1))"
           "p43_throw_expr" "(or x (throw (StateError \"boom\")))"
           "p44_cascade" "(doto (m/Point 1 2) (.-x! 3) (.-y! 4))"
           "p45_cascade_call" "(doto list (.add 1) (.add 2))"
           "p46_bitwise" "(bit-or (bit-and 1 2) (bit-xor 3 4))"
           "p47_shift" "(bit-shift-left 1 2)"
           "p48_prefix_neg" "(- x)"
           "p49_paren" "(* (+ 1 2) 3)"
           "p50_hex" "255"
           "p51_hex_color" "4293128957"
           "p52_double_exp" "1000.0"
           "p53_underscore_num" "1000"
           "p54_null_lit" "nil"
           "p55_bool_lit" "true"
           "p56_uri_parse" "(Uri.parse \"http://x\")"
           "p57_call" "(foo 1)"
           "p58_postfix_bang" "maybe"
           "p59_math_prefix" "(math/max 1 2)"
           "p60_map_nested" "{\"a\" [1 2]}"
           "p61_not_equal_chain" "(not (= a b))"
           "p62_and_chain" "(and a b c)"
           "p63_or_chain" "(or a b c)"
           "p64_comparison" "(< a b)"
           "p65_string_dq" "\"double\""
           "p66_prefixed_ctor" "(m/Text \"x\")"
           "p67_widget_column" "(m/Column .children [(m/Question (get (get questions _questionIndex) \"questionText\")) (-> ^List (get (get questions _questionIndex) \"answers\") (.map (fn [answer] (m/Answer _answerQuestion answer))) (.toList)) (m/Numb .sorted (and (< 1 2) (< 2 3) (< 3 a) (= a b)))])"
           "p68_widget_app" "(m/MaterialApp .title \"Cljd Demo\" .theme (m/ThemeData .primarySwatch m/Colors.blue) .home (m/Scaffold .appBar (m/AppBar .title (m/Text \"Home\")) .body (m/Center .child (m/Text \"Hi\"))))"
           "p69_ternary_lambda" "(m/TextButton .onPressed (if _searchEnabled (fn [] (pop) nil) nil))"
           "p70_interp_factory" "(m/Text (str \"Some \" field \" and \" (m/Factory.create)))"
           "p71_get_chain" "(get (get questions 1) \"two\")"
           "p72_const_list" "(m/SizedBox .height 8)"}]
    (testing name
      (is (= expected (emit-expr-str name))))))

;; ---------------------------------------------------------------------------
;; statements
;; ---------------------------------------------------------------------------

(deftest statement-parity
  (doseq [[name expected]
          {"s_if" "(defn s_if [^int a]\n  (when (> a 0) (do (print \"pos\") nil)))"
           "s_if_else" "(defn s_if_else [^int a]\n  (if (> a 0) (do (print \"pos\") nil) (do (print \"neg\") nil)))"
           "s_else_if" "(defn s_else_if [^int a]\n  (cond (> a 0) (do (print \"pos\") nil) (< a 0) (do (print \"neg\") nil) :else (do (print \"zero\") nil)))"
           "s_for_in" "(defn s_for_in [xs]\n  (doseq [x xs] (print x)))"
           "s_switch" "(defn s_switch [^int x]\n  (case x\n    1 (do (print \"one\") nil)\n    2 (do (print \"2or3\") nil)\n    3 (do (print \"2or3\") nil)\n    (do (print \"other\") nil)))"
           "s_try" "(defn s_try []\n  (try\n    (print 1)\n    nil\n    (catch StateError e st (print e) nil)\n    (catch Object e (print e) nil)\n    (finally (print 2))))"
           "s_return" "(defn s_return [^int a]\n  (+ a 1))"
           "s_expr_body" "(defn s_expr_body [^int a]\n  (* a 2))"
           "s_vars" "(defn s_vars []\n  (let [a 1 b 2] (print (+ a b)) nil))"
           "s_assert" "(defn s_assert [^int a]\n  (assert (> a 0) \"positive\"))"
           "s_nested_block" "(defn s_nested_block [^int a]\n  (do (print a) nil))"
           "s_await" "(defn ^:async s_await [f]\n  (await f))"}]
    (testing name
      (is (= expected (emit-defn-str name))))))

;; ---------------------------------------------------------------------------
;; ns / imports
;; ---------------------------------------------------------------------------

(deftest ns-emission
  (is (= (str "(ns parity\n"
              "  (:require\n"
              "   [\"dart:math\" :as math]\n"
              "   [\"package:flutter/material.dart\" :as m]))")
         (printer/print-form
          (first (decls/emit-unit (:unit @parity-doc) {:path "parity.dart"}))))))

(deftest top-level-vars
  (let [forms (decls/emit-unit (:unit @parity-doc) {:path "parity.dart"})
        defs (filter #(and (seq? %) (= 'def (first %))) forms)]
    (is (pos? (count defs)))
    (is (some #(= 'p01_add (second %)) defs))))

;; ---------------------------------------------------------------------------
;; failure policy
;; ---------------------------------------------------------------------------

(deftest unsupported-fails-loudly-with-position
  (testing "generators are not supported by ClojureDart"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"generators"
         (core/emit-expr
          {:ir :fn :src {:line 3 :col 1} :type-params [] :params []
           :body {:ir :block-body :async "async*" :src {:line 3 :col 1}
                  :block {:ir :block :stmts [] :src {:line 3 :col 1}}}}
          @parity-ctx))))
  (testing "Dart extensions are not supported"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"extensions"
         (decls/emit-decl {:ir :extension :name "X" :src {:line 1 :col 1}}
                          @parity-ctx))))
  (testing "unknown static members fail loudly"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"unknown static member"
         (core/emit-expr
          {:ir :prop :src {:line 1 :col 1}
           :target {:ir :id :name "Foo" :src {:line 1 :col 1}}
           :name "nope" :null-aware false}
          (assoc @parity-ctx :unit-info {:classes {"Foo" {:ctors #{nil}}}}))))))

;; ---------------------------------------------------------------------------
;; end-to-end (needs the Dart SDK; cached under .dart2cljd-cache)
;; ---------------------------------------------------------------------------

(def ^:private dart-bin
  (or (System/getenv "DART2CLJD_DART")
      "/home/Schola/Projects/develop/flutter/bin/dart"))

(deftest ^:integration convert-snippet-end-to-end
  (if (.exists (io/file dart-bin))
    (is (= "(m/Text \"hi\" .style (m/TextStyle .fontSize 12))"
           (api/convert "Text('hi', style: TextStyle(fontSize: 12))"
                        :format :string
                        :dart dart-bin)))
    (is true "dart binary not present; skipped")))

(deftest ^:integration mid-case-break-fails-loudly
  (if (.exists (io/file dart-bin))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"trailing terminator"
         (api/convert
          "void f(int x) { switch (x) { case 1: if (x > 0) break; print('a'); } }"
          :dart dart-bin)))
    (is true "dart binary not present; skipped")))
