(ns dart2cljd.ir
  "IR node types for dart2cljd, and the schema that maps astdump EDN onto them.

  Conventions
  -----------
  * Every IR node is a map with:
      :ir   keyword naming the construct (kebab-case)
      :src  {:line L :col C} 1-based position of the Dart node's begin token
    and, when the Dart node can carry metadata,
      :annotations  vector of :annotation nodes (present iff astdump emitted it)
  * Every field listed in `node-specs` is always present on the normalized
    node: absent optionals are nil, absent lists are []. This keeps the IR
    canonical so equality and hashing are meaningful.
  * Argument lists and formal parameter lists are flattened away:
      :args   [{:ir :arg :name <string|nil> :expr <node> :src ...} ...]
      :params [{:ir :param ...} ...]
  * Unifications (several analyzer tags -> one IR tag):
      GenericTypeAlias, FunctionTypeAlias            -> :type-alias
      VariableDeclarationStatement, VariableDeclarationList -> :var-decl
      FunctionDeclarationStatement, FunctionDeclaration     -> :fn-decl
      ForEachPartsWithDeclaration|Identifier|Pattern -> :for-in
      ForPartsWithDeclarations|Expression|Pattern    -> :for-parts
      DeclaredVariablePattern, AssignedVariablePattern -> :var-pattern
      SetOrMapLiteral                                -> :set-lit | :map-lit
      positional/ named record fields                -> :record-pos-field
                                                        :record-named-field
      regular/field/super formal parameters          -> :param

  Field modes
  -----------
      :scalar        copied verbatim (string/bool/number/nil/vector of strings)
      :child         single node or nil
      :children      vector of nodes, nil -> []
      :dotted        vector of token strings joined with \"\" (library names)
      :args          ArgumentList -> vector of :arg
      :params        FormalParameterList -> vector of :param
      :fn-suffix     function-typed parameter suffix -> :fn-suffix
      :record-fields mixed record literal fields -> :record-pos-field /
                     :record-named-field

  Field entries are [ir-key mode] or [ir-key mode raw-key] when the astdump
  key differs from the IR key (e.g. analyzer's :id becomes :name).

  Resolved facts (present only for `astdump --types` dumps)
  ---------------------------------------------------------
  Two cross-cutting annotations are attached to IR nodes when the resolved
  unit knows them (parse-only dumps carry neither):
    :dart-type     rendered static type of an expression (M11)
    :dart-element  what a name refers to (M12)

  `:dart-element` is one of, from the analyzer element classes:
    :local :param                       LocalVariableElement /
                                        FormalParameterElement (writes included)
    :top-fn :top-var                    TopLevelFunctionElement /
                                        TopLevelVariableElement
    :top-getter :top-setter             accessor whose enclosing element is a
                                        LibraryElement (top-level getter/setter,
                                        incl. the implicit accessors of a
                                        top-level variable)
    :method :getter :setter :field      instance members of a class/mixin/enum
    :static-method :static-getter       static members
    :static-setter :static-field
    :class :mixin :enum :typedef        type references
    :ctor                               constructor references
    :type-param :prefix :label          type parameters, import prefixes, labels
  Instance members (`:method :getter :setter :field`) are implicit-`this`
  accesses in Dart; the other kinds are not. Unmapped elements emit nothing.

  This namespace is pure data; normalization logic lives in dart2cljd.ast."
  (:refer-clojure :exclude []))

(def ast-version
  "Schema version emitted by bin/astdump.dart that this IR understands."
  1)

(def ^:private param-fields
  [[:kind :scalar]
   [:name :scalar]
   [:type :child]
   [:final :scalar]
   [:const :scalar]
   [:covariant :scalar]
   [:default :child]
   [:required :scalar]
   [:super :scalar]
   [:this :scalar]
   [:fn-typed :fn-suffix]])

(def node-specs
  "astdump :ast tag -> {:ir <tag-or-nil> :fields [[ir-key mode raw-key?] ...]}.

  An override in dart2cljd.ast/overrides may replace fields (and :ir) for tags
  whose IR shape is not a straight key-by-key copy."
  {;; -------------------------------------------------------------------
   ;; Unit & directives
   ;; -------------------------------------------------------------------
   :CompilationUnit
   {:ir :unit
    :fields [[:directives :children]
             [:decls :children]]}

   :LibraryDirective
   {:ir :library
    :fields [[:name :dotted]]}

   :ImportDirective
   {:ir :import
    :fields [[:uri :child]
             [:as :scalar]
             [:deferred :scalar]
             [:configurations :children]
             [:show :scalar]
             [:hide :scalar]]}

   :ExportDirective
   {:ir :export
    :fields [[:uri :child]
             [:show :scalar]
             [:hide :scalar]]}

   :PartDirective
   {:ir :part
    :fields [[:uri :child]]}

   :PartOfDirective
   {:ir :part-of
    :fields [[:library :dotted]
             [:uri :child]]}

   :Configuration
   {:ir :configuration
    :fields [[:name :dotted]
             [:uri :child]]}

   ;; -------------------------------------------------------------------
   ;; Declarations
   ;; -------------------------------------------------------------------
   :ClassDeclaration
   {:ir :class
    :fields [[:abstract :scalar]
             [:modifier :scalar]
             [:name :scalar]
             [:type-params :children]
             [:extends :child]
             [:with :children]
             [:implements :children]
             [:native :child]
             [:members :children]]}

   :ClassTypeAlias
   {:ir :class-alias
    :fields [[:modifier :scalar]
             [:name :scalar]
             [:type-params :children]
             [:superclass :child]
             [:with :children]
             [:implements :children]]}

   :MixinDeclaration
   {:ir :mixin
    :fields [[:base :scalar]
             [:name :scalar]
             [:type-params :children]
             [:on :children]
             [:implements :children]
             [:members :children]]}

   :EnumDeclaration
   {:ir :enum
    :fields [[:name :scalar]
             [:type-params :children]
             [:with :children]
             [:implements :children]
             [:constants :children]
             [:members :children]]}

   :EnumConstantDeclaration
   {:ir :enum-constant
    :fields [[:name :scalar]
             [:type-args :children]
             [:ctor :scalar]
             [:args :args]]}

   :ExtensionDeclaration
   {:ir :extension
    :fields [[:name :scalar]
             [:type-params :children]
             [:on :child]
             [:members :children]]}

   :ExtensionTypeDeclaration
   {:ir :extension-type
    :fields [[:name :scalar]
             [:type-params :children]
             [:const :scalar]
             [:ctor :scalar]
             [:params :params]
             [:implements :children]
             [:members :children]]}

   :FunctionDeclaration
   {:ir :fn-decl
    :fields [[:name :scalar]
             [:kind :scalar]
             [:external :scalar]
             [:return-type :child]
             [:type-params :children]
             [:params :params]
             [:body :child]]}

   ;; Analyzer 14 models top-level getters separately; astdump emits the
   ;; FunctionDeclaration shape for them.
   :TopLevelGetterDeclaration
   {:ir :fn-decl
    :fields [[:name :scalar]
             [:kind :scalar]
             [:external :scalar]
             [:return-type :child]
             [:type-params :children]
             [:params :params]
             [:body :child]]}

   :TopLevelVariableDeclaration
   {:ir :top-var-decl
    :fields [[:external :scalar]
             [:vars :child]]}

   :FieldDeclaration
   {:ir :field-decl
    :fields [[:static :scalar]
             [:covariant :scalar]
             [:abstract :scalar]
             [:external :scalar]
             [:fields :child]]}

   :MethodDeclaration
   {:ir :method
    :fields [[:name :scalar]
             [:kind :scalar]
             [:static :scalar]
             [:abstract :scalar]
             [:external :scalar]
             [:operator :scalar]
             [:return-type :child]
             [:type-params :children]
             [:params :params]
             [:body :child]]}

   :ConstructorDeclaration
   {:ir :ctor
    :fields [[:name :scalar]
             [:type :scalar]
             [:const :scalar]
             [:factory :scalar]
             [:external :scalar]
             [:params :params]
             [:initializers :children]
             [:redirect :child]
             [:body :child]]}

   :GenericTypeAlias
   {:ir :type-alias
    :fields [[:name :scalar]
             [:type-params :children]
             [:type :child]]}

   :FunctionTypeAlias
   ;; normalized by an override into the same :type-alias shape as
   ;; GenericTypeAlias, with :type being a synthesized :fn-type
   {:ir :type-alias
    :fields [[:name :scalar]
             [:type-params :children]
             [:return-type :child]
             [:params :params]]}

   ;; -------------------------------------------------------------------
   ;; Constructor initializers
   ;; -------------------------------------------------------------------
   :SuperConstructorInvocation
   {:ir :super-init
    :fields [[:name :scalar]
             [:args :args]]}

   :ConstructorFieldInitializer
   {:ir :field-init
    :fields [[:field :scalar]
             [:expr :child]]}

   :RedirectingConstructorInvocation
   {:ir :redirect-init
    :fields [[:name :scalar]
             [:args :args]]}

   :AssertInitializer
   {:ir :assert-init
    :fields [[:cond :child]
             [:message :child]]}

   ;; -------------------------------------------------------------------
   ;; Parameters
   ;; -------------------------------------------------------------------
   :RegularFormalParameter {:ir :param :fields param-fields}
   :FieldFormalParameter {:ir :param :fields param-fields}
   :SuperFormalParameter {:ir :param :fields param-fields}

   :TypeParameter
   {:ir :type-param
    :fields [[:name :scalar]
             [:bound :child]]}

   ;; -------------------------------------------------------------------
   ;; Statements
   ;; -------------------------------------------------------------------
   :Block
   {:ir :block
    :fields [[:stmts :children]]}

   :VariableDeclarationList
   {:ir :var-decl
    :fields [[:late :scalar]
             [:final :scalar]
             [:const :scalar]
             [:type :child]
             [:vars :children]]}

   :VariableDeclaration
   {:ir :var
    :fields [[:name :scalar]
             [:init :child]]}

   :VariableDeclarationStatement
   ;; normalized by an override to the same shape as VariableDeclarationList
   {:ir :var-decl
    :fields [[:vars :child]]}

   :ExpressionStatement
   {:ir :expr-stmt
    :fields [[:expr :child]]}

   :ReturnStatement
   {:ir :return
    :fields [[:expr :child]]}

   :IfStatement
   {:ir :if
    :fields [[:cond :child]
             [:case :child]
             [:then :child]
             [:else :child]]}

   :CaseClause
   {:ir :case-clause
    :fields [[:pattern :child]]}

   :ForStatement
   {:ir :for
    :fields [[:loop :child]
             [:body :child]]}

   :ForEachPartsWithDeclaration
   ;; normalized by an override to :for-in
   {:ir :for-in
    :fields [[:var :child]
             [:iterable :child]]}

   :ForEachPartsWithIdentifier
   ;; normalized by an override to :for-in
   {:ir :for-in
    :fields [[:var :scalar]
             [:iterable :child]]}

   :ForEachPartsWithPattern
   ;; normalized by an override to :for-in
   {:ir :for-in
    :fields [[:pattern :child]
             [:final :scalar]
             [:iterable :child]]}

   :ForPartsWithDeclarations
   ;; normalized by an override to :for-parts
   {:ir :for-parts
    :fields [[:init :child]
             [:cond :child]
             [:updaters :children]]}

   :ForPartsWithExpression
   ;; normalized by an override to :for-parts
   {:ir :for-parts
    :fields [[:init :child]
             [:cond :child]
             [:updaters :children]]}

   :ForPartsWithPattern
   ;; normalized by an override to :for-parts
   {:ir :for-parts
    :fields [[:pattern :child]
             [:cond :child]
             [:updaters :children]]}

   :DeclaredIdentifier
   {:ir :declared-ident
    :fields [[:final :scalar]
             [:const :scalar]
             [:type :child]
             [:name :scalar]]}

   :WhileStatement
   {:ir :while
    :fields [[:cond :child]
             [:body :child]]}

   :DoStatement
   {:ir :do-while
    :fields [[:body :child]
             [:cond :child]]}

   :SwitchStatement
   {:ir :switch-stmt
    :fields [[:expr :child]
             [:cases :children]]}

   :SwitchCase
   {:ir :switch-case
    :fields [[:labels :scalar]
             [:expr :child]
             [:stmts :children]]}

   :SwitchPatternCase
   {:ir :switch-pattern-case
    :fields [[:labels :scalar]
             [:pattern :child]
             [:stmts :children]]}

   :SwitchDefault
   {:ir :switch-default
    :fields [[:labels :scalar]
             [:stmts :children]]}

   :TryStatement
   {:ir :try
    :fields [[:body :child]
             [:catches :children]
             [:finally :child]]}

   :CatchClause
   {:ir :catch
    :fields [[:exception-type :child]
             [:exception-var :scalar]
             [:stack-var :scalar]
             [:body :child]]}

   :BreakStatement
   {:ir :break
    :fields [[:label :scalar]]}

   :ContinueStatement
   {:ir :continue
    :fields [[:label :scalar]]}

   :LabeledStatement
   {:ir :labeled
    :fields [[:labels :scalar]
             [:stmt :child]]}

   :Label
   {:ir :label
    :fields [[:id :scalar]]}

   :YieldStatement
   {:ir :yield
    :fields [[:star :scalar]
             [:expr :child]]}

   :AssertStatement
   {:ir :assert-stmt
    :fields [[:cond :child]
             [:message :child]]}

   :EmptyStatement
   {:ir :empty-stmt
    :fields []}

   :FunctionDeclarationStatement
   ;; normalized by an override to the same shape as FunctionDeclaration
   {:ir :fn-decl
    :fields [[:fn :child]]}

   :PatternVariableDeclarationStatement
   {:ir :pattern-var-decl-stmt
    :fields [[:decl :child]]}

   :PatternVariableDeclaration
   {:ir :pattern-var-decl
    :fields [[:final :scalar]
             [:pattern :child]
             [:expr :child]]}

   :PatternAssignment
   {:ir :pattern-assign
    :fields [[:pattern :child]
             [:expr :child]]}

   ;; -------------------------------------------------------------------
   ;; Expressions
   ;; -------------------------------------------------------------------
   :SimpleIdentifier
   {:ir :id
    :fields [[:name :scalar :id]]}

   :PrefixedIdentifier
   {:ir :prefixed-id
    :fields [[:prefix :scalar]
             [:name :scalar :id]]}

   :PropertyAccess
   {:ir :prop
    :fields [[:target :child]
             [:op :scalar]
             [:name :scalar :id]
             [:null-aware :scalar]]}

   :MethodInvocation
   {:ir :call
    :fields [[:target :child]
             [:op :scalar]
             [:name :scalar :id]
             [:type-args :children]
             [:args :args]
             [:null-aware :scalar]]}

   :InstanceCreationExpression
   {:ir :new
    :fields [[:const :scalar]
             [:ctor :child]
             [:args :args]]}

   :ConstructorName
   {:ir :ctor-name
    :fields [[:type :child]
             [:name :scalar]]}

   :FunctionExpressionInvocation
   {:ir :call-fn
    :fields [[:fn :child]
             [:type-args :children]
             [:args :args]]}

   :IndexExpression
   {:ir :index
    :fields [[:target :child]
             [:index :child]
             [:null-aware :scalar]]}

   :CascadeExpression
   {:ir :cascade
    :fields [[:target :child]
             [:null-aware :scalar]
             [:sections :children]]}

   :AssignmentExpression
   {:ir :assign
    :fields [[:lhs :child]
             [:op :scalar]
             [:rhs :child]]}

   :BinaryExpression
   {:ir :binary
    :fields [[:op :scalar]
             [:lhs :child]
             [:rhs :child]]}

   :PrefixExpression
   {:ir :prefix
    :fields [[:op :scalar]
             [:operand :child]]}

   :PostfixExpression
   {:ir :postfix
    :fields [[:op :scalar]
             [:operand :child]]}

   :ConditionalExpression
   {:ir :ternary
    :fields [[:cond :child]
             [:then :child]
             [:else :child]]}

   :IsExpression
   {:ir :is
    :fields [[:expr :child]
             [:negated :scalar]
             [:type :child]]}

   :AsExpression
   {:ir :as
    :fields [[:expr :child]
             [:type :child]]}

   :AwaitExpression
   {:ir :await
    :fields [[:expr :child]]}

   :ThrowExpression
   {:ir :throw
    :fields [[:expr :child]]}

   :ParenthesizedExpression
   {:ir :paren
    :fields [[:expr :child]]}

   :FunctionExpression
   {:ir :fn
    :fields [[:type-params :children]
             [:params :params]
             [:body :child]]}

   :NamedArgument
   ;; normally flattened into the enclosing :args vector as :arg
   {:ir :arg
    :fields [[:name :scalar]
             [:expr :child]]}

   :SuperExpression {:ir :super :fields []}
   :ThisExpression {:ir :this :fields []}
   :RethrowExpression {:ir :rethrow :fields []}

   :SwitchExpression
   {:ir :switch-expr
    :fields [[:expr :child]
             [:cases :children]]}

   :SwitchExpressionCase
   {:ir :switch-expr-case
    :fields [[:pattern :child]
             [:expr :child]]}

   ;; -------------------------------------------------------------------
   ;; Literals
   ;; -------------------------------------------------------------------
   :IntegerLiteral
   {:ir :int-lit
    :fields [[:raw :scalar]
             [:value :scalar]]}

   :DoubleLiteral
   {:ir :double-lit
    :fields [[:raw :scalar]
             [:value :scalar]]}

   :BooleanLiteral
   {:ir :bool-lit
    :fields [[:value :scalar]]}

   :NullLiteral {:ir :null-lit :fields []}

   :SimpleStringLiteral
   {:ir :string-lit
    :fields [[:raw :scalar]
             [:value :scalar]
             [:raw-string :scalar]
             [:multiline :scalar]]}

   :AdjacentStrings
   {:ir :adjacent-strings
    :fields [[:parts :children]]}

   :StringInterpolation
   {:ir :string-interp
    :fields [[:parts :children]]}

   :InterpolationString
   {:ir :interp-string
    :fields [[:raw :scalar]
             [:value :scalar]]}

   :InterpolationExpression
   {:ir :interp-expr
    :fields [[:brace :scalar]
             [:expr :child]]}

   ;; -------------------------------------------------------------------
   ;; Collections
   ;; -------------------------------------------------------------------
   :ListLiteral
   {:ir :list-lit
    :fields [[:const :scalar]
             [:type-args :children]
             [:elements :children]]}

   :SetOrMapLiteral
   ;; :ir is chosen by an override (:set-lit or :map-lit)
   {:ir :set-lit
    :fields [[:const :scalar]
             [:type-args :children]
             [:elements :children]]}

   :MapLiteralEntry
   {:ir :map-entry
    :fields [[:key :child]
             [:key-null-aware :scalar]
             [:value :child]
             [:value-null-aware :scalar]]}

   :RecordLiteral
   {:ir :record-lit
    :fields [[:const :scalar]
             [:fields :record-fields]]}

   :RecordNamedField
   {:ir :record-named-field
    :fields [[:name :scalar]
             [:expr :child]]}

   :SpreadElement
   {:ir :spread
    :fields [[:null-aware :scalar]
             [:expr :child]]}

   :NullAwareElement
   {:ir :null-aware-elem
    :fields [[:expr :child]]}

   :IfElement
   {:ir :if-element
    :fields [[:cond :child]
             [:case :child]
             [:then :child]
             [:else :child]]}

   :ForElement
   {:ir :for-element
    :fields [[:loop :child]
             [:body :child]]}

   ;; -------------------------------------------------------------------
   ;; Function bodies
   ;; -------------------------------------------------------------------
   :BlockFunctionBody
   {:ir :block-body
    :fields [[:async :scalar]
             [:block :child]]}

   :ExpressionFunctionBody
   {:ir :expr-body
    :fields [[:async :scalar]
             [:expr :child]]}

   :EmptyFunctionBody {:ir :empty-body :fields []}

   :NativeFunctionBody
   {:ir :native-body
    :fields [[:native :scalar]]}

   ;; -------------------------------------------------------------------
   ;; Type annotations
   ;; -------------------------------------------------------------------
   :NamedType
   {:ir :named-type
    :fields [[:prefix :scalar]
             [:name :scalar :id]
             [:type-args :children]
             [:nullable :scalar]]}

   :GenericFunctionType
   {:ir :fn-type
    :fields [[:return-type :child]
             [:type-params :children]
             [:params :params]
             [:nullable :scalar]]}

   :RecordTypeAnnotation
   {:ir :record-type
    :fields [[:positional :children]
             [:named :children]
             [:nullable :scalar]]}

   :RecordTypePositionalField
   {:ir :record-type-pos-field
    :fields [[:name :scalar]
             [:type :child]]}

   :RecordTypeNamedField
   {:ir :record-type-named-field
    :fields [[:name :scalar]
             [:type :child]]}

   :Annotation
   {:ir :annotation
    :fields [[:name :child]
             [:ctor :scalar]
             [:type-args :children]
             [:args :args]]}

   ;; -------------------------------------------------------------------
   ;; Patterns (Dart 3)
   ;; -------------------------------------------------------------------
   :ConstantPattern
   {:ir :const-pattern
    :fields [[:expr :child]]}

   :DeclaredVariablePattern
   {:ir :var-pattern
    :fields [[:name :scalar]
             [:keyword :scalar]
             [:type :child]]}

   :AssignedVariablePattern
   ;; normalized by an override to the same shape as DeclaredVariablePattern
   ;; (:keyword and :type are nil)
   {:ir :var-pattern
    :fields [[:name :scalar]]}

   :ListPattern
   {:ir :list-pattern
    :fields [[:type-args :children]
             [:elements :children]]}

   :MapPattern
   {:ir :map-pattern
    :fields [[:type-args :children]
             [:entries :children]]}

   :MapPatternEntry
   {:ir :map-pattern-entry
    :fields [[:key :child]
             [:value :child]]}

   :RecordPattern
   {:ir :record-pattern
    :fields [[:fields :children]]}

   :ObjectPattern
   {:ir :object-pattern
    :fields [[:type :child]
             [:fields :children]]}

   :PatternField
   {:ir :pattern-field
    :fields [[:name :scalar]
             [:pattern :child]]}

   :LogicalAndPattern
   {:ir :and-pattern
    :fields [[:lhs :child]
             [:rhs :child]]}

   :LogicalOrPattern
   {:ir :or-pattern
    :fields [[:lhs :child]
             [:rhs :child]]}

   :RelationalPattern
   {:ir :rel-pattern
    :fields [[:op :scalar]
             [:operand :child]]}

   :CastPattern
   {:ir :cast-pattern
    :fields [[:pattern :child]
             [:type :child]]}

   :NullAssertPattern
   {:ir :null-assert-pattern
    :fields [[:pattern :child]]}

   :NullCheckPattern
   {:ir :null-check-pattern
    :fields [[:pattern :child]]}

   :ParenthesizedPattern
   {:ir :paren-pattern
    :fields [[:pattern :child]]}

   :WildcardPattern
   {:ir :wildcard-pattern
    :fields [[:type :child]]}

   :RestPatternElement
   {:ir :rest-pattern
    :fields [[:pattern :child]]}

   :GuardedPattern
   {:ir :guarded-pattern
    :fields [[:pattern :child]
             [:when :child]]}

   :WhenClause
   {:ir :when-clause
    :fields [[:expr :child]]}})
