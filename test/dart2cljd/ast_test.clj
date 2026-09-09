(ns dart2cljd.ast-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [dart2cljd.ast :as ast]))

(defn- src [line col] {:line line :col col})
(defn- n [raw] (ast/normalize-node raw))

(defn- read-fixture [name]
  (edn/read-string (slurp (io/resource (str "fixtures/" name)))))

(defn- ir-nodes
  "All IR nodes in a normalized tree, depth first."
  [x]
  (let [acc (atom [])]
    (letfn [(w [x]
              (cond
                (map? x) (do (when (:ir x) (swap! acc conj x)) (run! w (vals x)))
                (vector? x) (run! w x)))]
      (w x))
    @acc))

;; ---------------------------------------------------------------------------
;; identifiers, literals, types
;; ---------------------------------------------------------------------------

(deftest identifiers-and-literals
  (testing "SimpleIdentifier"
    (is (= {:ir :id :name "foo" :src (src 1 1)}
           (n {:ast :SimpleIdentifier :id "foo" :src (src 1 1)}))))
  (testing "PrefixedIdentifier keeps prefix and name"
    (is (= {:ir :prefixed-id :prefix "m" :name "Colors" :src (src 2 1)}
           (n {:ast :PrefixedIdentifier :prefix "m" :id "Colors" :src (src 2 1)}))))
  (testing "IntegerLiteral keeps raw spelling and value"
    (is (= {:ir :int-lit :raw "0xFF" :value 255 :src (src 3 1)}
           (n {:ast :IntegerLiteral :raw "0xFF" :value 255 :src (src 3 1)}))))
  (testing "BooleanLiteral"
    (is (= {:ir :bool-lit :value true :src (src 4 1)}
           (n {:ast :BooleanLiteral :value true :src (src 4 1)}))))
  (testing "NullLiteral has no extra fields"
    (is (= {:ir :null-lit :src (src 5 1)}
           (n {:ast :NullLiteral :src (src 5 1)}))))
  (testing "SimpleStringLiteral"
    (is (= {:ir :string-lit :raw "'hi'" :value "hi"
            :raw-string false :multiline false :src (src 6 1)}
           (n {:ast :SimpleStringLiteral :raw "'hi'" :value "hi"
               :raw-string false :multiline false :src (src 6 1)})))))

(deftest type-annotations
  (testing "NamedType"
    (is (= {:ir :named-type :prefix "m" :name "Text" :type-args [] :nullable true
            :src (src 1 1)}
           (n {:ast :NamedType :prefix "m" :id "Text" :type-args nil
               :nullable true :src (src 1 1)}))))
  (testing "GenericFunctionType"
    (is (= {:ir :fn-type :return-type nil :type-params [] :params [] :nullable false
            :src (src 2 1)}
           (n {:ast :GenericFunctionType :return-type nil :type-params nil
               :params {:ast :FormalParameterList :params [] :src (src 2 8)}
               :nullable false :src (src 2 1)}))))
  (testing "RecordTypeAnnotation"
    (is (= {:ir :record-type
            :positional [{:ir :record-type-pos-field :name nil
                          :type {:ir :named-type :prefix nil :name "int"
                                 :type-args [] :nullable false :src (src 3 2)}
                          :src (src 3 2)}]
            :named [{:ir :record-type-named-field :name "x"
                     :type {:ir :named-type :prefix nil :name "bool"
                            :type-args [] :nullable false :src (src 3 10)}
                     :src (src 3 7)}]
            :nullable false :src (src 3 1)}
           (n {:ast :RecordTypeAnnotation
               :positional [{:ast :RecordTypePositionalField :name nil
                             :type {:ast :NamedType :prefix nil :id "int"
                                    :type-args nil :nullable false :src (src 3 2)}
                             :src (src 3 2)}]
               :named [{:ast :RecordTypeNamedField :name "x"
                        :type {:ast :NamedType :prefix nil :id "bool"
                               :type-args nil :nullable false :src (src 3 10)}
                        :src (src 3 7)}]
               :nullable false :src (src 3 1)})))))

;; ---------------------------------------------------------------------------
;; annotations
;; ---------------------------------------------------------------------------

(deftest annotations-are-normalized
  (let [ir (n {:ast :TopLevelVariableDeclaration
               :annotations [{:ast :Annotation
                              :name {:ast :SimpleIdentifier :id "Deprecated" :src (src 1 2)}
                              :ctor nil
                              :type-args nil
                              :args {:ast :ArgumentList
                                     :args [{:ast :SimpleStringLiteral :raw "'x'" :value "x"
                                             :raw-string false :multiline false :src (src 1 14)}]
                                     :src (src 1 13)}
                              :src (src 1 1)}]
               :external false
               :vars {:ast :VariableDeclarationList :late false :final false :const true
                      :type nil :vars [] :src (src 1 20)}
               :src (src 1 1)})]
    (is (= [{:ir :annotation
             :name {:ir :id :name "Deprecated" :src (src 1 2)}
             :ctor nil
             :type-args []
             :args [{:ir :arg :name nil
                     :expr {:ir :string-lit :raw "'x'" :value "x"
                            :raw-string false :multiline false :src (src 1 14)}
                     :src (src 1 14)}]
             :src (src 1 1)}]
           (:annotations ir)))))

;; ---------------------------------------------------------------------------
;; calls, arguments, parameters
;; ---------------------------------------------------------------------------

(deftest calls-flatten-argument-lists
  (let [ir (n {:ast :MethodInvocation
               :target {:ast :SimpleIdentifier :id "f" :src (src 1 1)}
               :op "."
               :id "g"
               :type-args nil
               :args {:ast :ArgumentList
                      :args [{:ast :IntegerLiteral :raw "1" :value 1 :src (src 1 7)}
                             {:ast :NamedArgument :name "b"
                              :expr {:ast :IntegerLiteral :raw "2" :value 2 :src (src 1 14)}
                              :src (src 1 11)}]
                      :src (src 1 6)}
               :null-aware false
               :src (src 1 1)})]
    (is (= {:ir :call :src (src 1 1)
            :target {:ir :id :name "f" :src (src 1 1)}
            :op "." :name "g" :type-args [] :null-aware false
            :args [{:ir :arg :name nil
                    :expr {:ir :int-lit :raw "1" :value 1 :src (src 1 7)}
                    :src (src 1 7)}
                   {:ir :arg :name "b"
                    :expr {:ir :int-lit :raw "2" :value 2 :src (src 1 14)}
                    :src (src 1 11)}]}
           ir))))

(deftest params-flatten-and-fn-suffix
  (let [ir (n {:ast :FunctionDeclaration
               :name "add" :kind :fn :external false
               :return-type nil :type-params nil
               :params {:ast :FormalParameterList
                        :params [{:ast :RegularFormalParameter
                                  :kind :required-positional :name "a" :type nil
                                  :final false :const false :covariant false
                                  :default nil :super false :this false :fn-typed nil
                                  :src (src 1 6)}
                                 {:ast :RegularFormalParameter
                                  :kind :named :name "cb" :type nil
                                  :final false :const false :covariant false
                                  :default nil :super false :this false
                                  :fn-typed {:type-params nil
                                             :params {:ast :FormalParameterList
                                                      :params [] :src (src 1 20)}
                                             :nullable true}
                                  :src (src 1 14)}]
                        :src (src 1 5)}
               :body {:ast :EmptyFunctionBody :src (src 1 25)}
               :src (src 1 1)})]
    (is (= :fn-decl (:ir ir)))
    (is (= [{:ir :param :kind :required-positional :name "a" :type nil
             :final false :const false :covariant false :default nil
             :super false :this false :fn-typed nil :src (src 1 6)}
            {:ir :param :kind :named :name "cb" :type nil
             :final false :const false :covariant false :default nil
             :super false :this false
             :fn-typed {:ir :fn-suffix :type-params [] :params [] :nullable true
                        :src (src 1 14)}
             :src (src 1 14)}]
           (:params ir)))))

(deftest field-and-super-params-unify-to-param
  (doseq [tag [:FieldFormalParameter :SuperFormalParameter]]
    (is (= :param
           (:ir (n {:ast tag :kind :named :name "key" :type nil
                    :final false :const false :covariant false :default nil
                    :super (= tag :SuperFormalParameter)
                    :this (= tag :FieldFormalParameter)
                    :fn-typed nil :src (src 1 1)}))))))

;; ---------------------------------------------------------------------------
;; declarations
;; ---------------------------------------------------------------------------

(deftest variable-declaration-statement-unwraps
  (is (= {:ir :var-decl :late false :final true :const false :type nil
          :vars [{:ir :var :name "x" :init nil :src (src 1 7)}]
          :src (src 1 1)}
         (n {:ast :VariableDeclarationStatement
             :vars {:ast :VariableDeclarationList :late false :final true :const false
                    :type nil
                    :vars [{:ast :VariableDeclaration :name "x" :init nil :src (src 1 7)}]
                    :src (src 1 1)}
             :src (src 1 1)}))))

(deftest function-declaration-statement-unwraps
  (let [fd {:ast :FunctionDeclaration :name "f" :kind :fn :external false
            :return-type nil :type-params nil :params nil
            :body {:ast :EmptyFunctionBody :src (src 1 10)}
            :src (src 1 3)}]
    (is (= (assoc (n fd) :src (src 1 1))
           (n {:ast :FunctionDeclarationStatement :fn fd :src (src 1 1)})))))

(deftest type-aliases-unify
  (let [generic (n {:ast :GenericTypeAlias :name "Pair" :type-params nil
                    :type {:ast :NamedType :prefix nil :id "int" :type-args nil
                           :nullable false :src (src 1 16)}
                    :src (src 1 1)})
        fn-alias (n {:ast :FunctionTypeAlias :name "OldFn" :type-params nil
                     :return-type nil
                     :params {:ast :FormalParameterList :params [] :src (src 1 14)}
                     :src (src 1 1)})]
    (is (= :type-alias (:ir generic)))
    (is (= {:ir :named-type :prefix nil :name "int" :type-args [] :nullable false
            :src (src 1 16)}
           (:type generic)))
    (is (= {:ir :type-alias :name "OldFn" :type-params [] :src (src 1 1)
            :type {:ir :fn-type :return-type nil :type-params [] :params []
                   :nullable false :src (src 1 1)}}
           fn-alias))))

(deftest class-shape-is-canonical
  (let [ir (n {:ast :ClassDeclaration :abstract false :modifier nil :name "A"
               :type-params nil :extends nil :with nil :implements nil :native nil
               :members [] :src (src 1 1)})]
    (is (= {:ir :class :abstract false :modifier nil :name "A" :type-params []
            :extends nil :with [] :implements [] :native nil :members []
            :src (src 1 1)}
           ir))))

;; ---------------------------------------------------------------------------
;; loops
;; ---------------------------------------------------------------------------

(deftest for-in-unifies-variants
  (testing "with declaration"
    (is (= {:ir :for-in :src (src 1 1)
            :var {:ir :declared-ident :final true :const false :type nil :name "x"
                  :src (src 1 6)}
            :pattern nil :final true
            :iterable {:ir :id :name "xs" :src (src 1 12)}}
           (n {:ast :ForEachPartsWithDeclaration
               :var {:ast :DeclaredIdentifier :final true :const false :type nil
                     :name "x" :src (src 1 6)}
               :iterable {:ast :SimpleIdentifier :id "xs" :src (src 1 12)}
               :src (src 1 1)}))))
  (testing "with identifier synthesizes a declared-ident"
    (is (= {:ir :for-in :src (src 1 1)
            :var {:ir :declared-ident :final false :const false :type nil :name "i"
                  :src (src 1 1)}
            :pattern nil :final false
            :iterable {:ir :id :name "xs" :src (src 1 10)}}
           (n {:ast :ForEachPartsWithIdentifier
               :var "i"
               :iterable {:ast :SimpleIdentifier :id "xs" :src (src 1 10)}
               :src (src 1 1)}))))
  (testing "with pattern"
    (is (= {:ir :for-in :src (src 1 1)
            :var nil
            :pattern {:ir :var-pattern :name "p" :keyword "var" :type nil
                      :src (src 1 6)}
            :final true
            :iterable {:ir :id :name "xs" :src (src 1 20)}}
           (n {:ast :ForEachPartsWithPattern
               :pattern {:ast :DeclaredVariablePattern :name "p" :keyword "var"
                         :type nil :src (src 1 6)}
               :final true
               :iterable {:ast :SimpleIdentifier :id "xs" :src (src 1 20)}
               :src (src 1 1)})))))

(deftest for-parts-unify-variants
  (let [expected {:ir :for-parts :src (src 1 1)
                  :init {:ir :id :name "i" :src (src 1 5)}
                  :pattern nil
                  :cond {:ir :id :name "c" :src (src 1 10)}
                  :updaters [{:ir :postfix :op "++"
                              :operand {:ir :id :name "i" :src (src 1 13)}
                              :src (src 1 13)}]}]
    (is (= expected
           (n {:ast :ForPartsWithExpression
               :init {:ast :SimpleIdentifier :id "i" :src (src 1 5)}
               :cond {:ast :SimpleIdentifier :id "c" :src (src 1 10)}
               :updaters [{:ast :PostfixExpression :op "++"
                           :operand {:ast :SimpleIdentifier :id "i" :src (src 1 13)}
                           :src (src 1 13)}]
               :src (src 1 1)}))))
  (testing "with declarations"
    (is (= {:ir :for-parts :src (src 1 1)
            :init {:ir :var-decl :late false :final false :const false :type nil
                   :vars [{:ir :var :name "i" :init nil :src (src 1 5)}]
                   :src (src 1 5)}
            :pattern nil :cond nil :updaters []}
           (n {:ast :ForPartsWithDeclarations
               :init {:ast :VariableDeclarationList :late false :final false :const false
                      :type nil
                      :vars [{:ast :VariableDeclaration :name "i" :init nil :src (src 1 5)}]
                      :src (src 1 5)}
               :cond nil :updaters []
               :src (src 1 1)})))))

;; ---------------------------------------------------------------------------
;; collections and patterns
;; ---------------------------------------------------------------------------

(deftest set-and-map-literals
  (is (= :set-lit
         (:ir (n {:ast :SetOrMapLiteral :kind :set :const false :type-args nil
                  :elements [] :src (src 1 1)}))))
  (is (= :map-lit
         (:ir (n {:ast :SetOrMapLiteral :kind :map :const false :type-args nil
                  :elements [] :src (src 1 1)})))))

(deftest record-literal-fields-are-wrapped
  (is (= [{:ir :record-pos-field
           :expr {:ir :int-lit :raw "1" :value 1 :src (src 1 2)}
           :src (src 1 2)}
          {:ir :record-named-field :name "x"
           :expr {:ir :int-lit :raw "2" :value 2 :src (src 1 10)}
           :src (src 1 7)}]
         (:fields (n {:ast :RecordLiteral :const false
                      :fields [{:ast :IntegerLiteral :raw "1" :value 1 :src (src 1 2)}
                               {:ast :RecordNamedField :name "x"
                                :expr {:ast :IntegerLiteral :raw "2" :value 2 :src (src 1 10)}
                                :src (src 1 7)}]
                      :src (src 1 1)})))))

(deftest assigned-pattern-keeps-var-pattern-shape
  (is (= {:ir :var-pattern :name "a" :keyword nil :type nil :src (src 1 1)}
         (n {:ast :AssignedVariablePattern :name "a" :src (src 1 1)}))))

(deftest pattern-tags
  (is (= :guarded-pattern
         (:ir (n {:ast :GuardedPattern
                  :pattern {:ast :ConstantPattern
                            :expr {:ast :IntegerLiteral :raw "1" :value 1 :src (src 1 8)}
                            :src (src 1 8)}
                  :when {:ast :SimpleIdentifier :id "c" :src (src 1 15)}
                  :src (src 1 1)}))))
  (is (= :rest-pattern
         (:ir (n {:ast :RestPatternElement
                  :pattern {:ast :DeclaredVariablePattern :name "r" :keyword nil
                            :type nil :src (src 1 4)}
                  :src (src 1 1)})))))

;; ---------------------------------------------------------------------------
;; tags that the fixtures cannot exercise
;; ---------------------------------------------------------------------------

(deftest rare-tags
  (testing "PartDirective"
    (is (= {:ir :part :annotations [] :src (src 1 1)
            :uri {:ir :string-lit :raw "'x.dart'" :value "x.dart"
                  :raw-string false :multiline false :src (src 1 6)}}
           (n {:ast :PartDirective :annotations []
               :uri {:ast :SimpleStringLiteral :raw "'x.dart'" :value "x.dart"
                     :raw-string false :multiline false :src (src 1 6)}
               :src (src 1 1)}))))
  (testing "PartOfDirective joins library name tokens"
    (is (= {:ir :part-of :annotations [] :library "my.app" :uri nil :src (src 1 1)}
           (n {:ast :PartOfDirective :annotations []
               :library ["my" "." "app"] :uri nil :src (src 1 1)}))))
  (testing "NativeFunctionBody"
    (is (= {:ir :native-body :native "code" :src (src 1 1)}
           (n {:ast :NativeFunctionBody :native "code" :src (src 1 1)}))))
  (testing "SwitchCase (unreachable in analyzer 14 parser, spec kept)"
    (is (= {:ir :switch-case :labels [] :src (src 1 1)
            :expr {:ir :id :name "k" :src (src 1 8)}
            :stmts []}
           (n {:ast :SwitchCase :labels []
               :expr {:ast :SimpleIdentifier :id "k" :src (src 1 8)}
               :stmts [] :src (src 1 1)}))))
  (testing "WhenClause (inlined by astdump, spec kept)"
    (is (= {:ir :when-clause :expr {:ir :id :name "c" :src (src 1 6)}
            :src (src 1 1)}
           (n {:ast :WhenClause :expr {:ast :SimpleIdentifier :id "c" :src (src 1 6)}
               :src (src 1 1)})))))

;; ---------------------------------------------------------------------------
;; failure policy
;; ---------------------------------------------------------------------------

(deftest failures-are-loud
  (testing "astdump :generic nodes"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"generic node"
                          (n {:ast :MysteryNode :generic true :parts [] :src (src 9 4)}))))
  (testing "unknown AST tags"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unsupported AST node"
                          (n {:ast :NoSuchNode :src (src 1 1)}))))
  (testing "unknown schema version"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unsupported astdump schema version"
                          (ast/normalize-doc {:ast-version 99
                                              :unit {:ast :NullLiteral :src (src 1 1)}}))))
  (testing "parse errors fail by default and carry positions"
    (let [doc {:ast-version 1
               :errors [{:message "boom" :severity :error :line 2 :col 3}]
               :unit {:ast :NullLiteral :src (src 1 1)}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom at 2:3"
                            (ast/normalize-doc doc)))
      (is (= (:errors doc)
             (:errors (ast/normalize-doc doc {:fail-on-errors? false})))))))

;; ---------------------------------------------------------------------------
;; fixture-based tests
;; ---------------------------------------------------------------------------

(deftest sample-fixture-normalizes
  (let [doc (ast/normalize-doc (read-fixture "sample.edn"))
        unit (:unit doc)
        classes (fn [] (filter #(= :class (:ir %)) (:decls unit)))
        class-named (fn [n] (first (filter #(= n (:name %)) (classes))))]
    (is (= 1 (:ast-version doc)))
    (is (= :unit (:ir unit)))
    (is (= [] (:errors doc)))
    (is (pos? (count (:decls unit))))
    (testing "every node has :ir and :src"
      (is (every? #(and (keyword? (:ir %)) (map? (:src %))) (ir-nodes unit))))
    (testing "classes survive with their heritage"
      (is (some? (class-named "Animal")))
      (is (= "StatefulWidget" (:name (:extends (class-named "Counter")))))
      (is (= "State" (:name (:extends (class-named "_CounterState"))))))
    (testing "mutable local detection data is intact (postfix ++ / assign)"
      (is (some #(= :postfix (:ir %)) (ir-nodes unit)))
      (is (some #(= :assign (:ir %)) (ir-nodes unit))))
    (testing "string interpolation"
      (is (some #(= :string-interp (:ir %)) (ir-nodes unit))))
    (testing "patterns"
      (is (some #(= :var-pattern (:ir %)) (ir-nodes unit)))
      (is (some #(= :object-pattern (:ir %)) (ir-nodes unit))))))

(deftest surface-fixture-normalizes
  (let [doc (ast/normalize-doc (read-fixture "surface.edn"))
        tags (set (map :ir (ir-nodes (:unit doc))))]
    (is (= [] (:errors doc)))
    (doseq [tag [:class-alias :type-alias :null-aware-elem :cast-pattern
                 :null-assert-pattern :null-check-pattern :paren-pattern
                 :and-pattern :rel-pattern :for-parts :for-in :super :fn-suffix
                 :adjacent-strings :call-fn :labeled :empty-stmt]]
      (is (contains? tags tag) (str "missing " tag)))))

(deftest normalization-is-deterministic
  (doseq [fixture ["sample.edn" "surface.edn"]]
    (is (= (ast/normalize-doc (read-fixture fixture))
           (ast/normalize-doc (read-fixture fixture))))))
