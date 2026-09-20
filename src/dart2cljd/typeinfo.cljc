(ns dart2cljd.typeinfo
  "Deterministic syntactic analysis of local variable types.

  Records which locals are known to hold instances of classes declared in
  the current unit, e.g. `final p = Point(1, 2);` or typed declarations
  `Point p = ...`, and which locals are known to be Dart scalars
  (`int`/`double`/`num`/`bool`/`String` declarations and literal
  initializers). The emitter uses this both to call Dart operators as cljd
  interop method calls (`(.+ a b)`), because ClojureDart's numeric functions
  do not dispatch to user-defined operators, and to skip runtime type-guard
  dispatch for receivers that cannot be unit classes.

  Like dart2cljd.mutable the analysis is purely syntactic and
  over-approximates shadowing scopes (a name bound in an inner scope may
  be attributed the outer type); results are stable and reproducible.

  No type information beyond the current unit is needed, so no resolution
  pass is required.")

(def ^:private scalar-type-names
  #{"int" "double" "num" "bool" "String"})

(def ^:private literal-irs
  #{:int-lit :double-lit :string-lit :bool-lit :null-lit})

(defn scalar-type?
  "True when a normalized type node names a Dart scalar type."
  [ty]
  (and (map? ty)
       (= :named-type (:ir ty))
       (nil? (:prefix ty))
       (contains? scalar-type-names (str (:name ty)))))

(defn- strip-paren [node]
  (if (= :paren (:ir node)) (recur (:expr node)) node))

(defn string-type?
  "True when a normalized type node names `String` (nullable or not)."
  [ty]
  (and (scalar-type? ty)
       (= "String" (str (:name ty)))))

(def ^:private string-call-names
  "Instance/static calls that return a Dart String, for the syntactic
  string tracking. `toString` and `toRadixString` exist on `Object`/`int`
  and are unambiguously String-returning; the static `String` factories are
  matched by their receiver name."
  #{"toString" "toRadixString"})

(defn string-expr?
  "True when a normalized expression node is syntactically known to hold a
  Dart String. `strings` is the set of names already known to be strings
  (locals, parameters and implicit-this String fields).

  Sound for well-typed Dart: string literals and string interpolations are
  Strings, `toString`/`toRadixString` return Strings, and `+` with a String
  operand is Dart's `String.+` concatenation (Dart rejects a non-String
  operand statically)."
  [node strings]
  (let [n (strip-paren node)]
    (case (:ir n)
      (:string-lit :string-interp :adjacent-strings) true
      :id (contains? strings (str (:name n)))
      :binary (and (= "+" (:op n))
                   (or (string-expr? (:lhs n) strings)
                       (string-expr? (:rhs n) strings)))
      :call (or (contains? string-call-names (str (:name n)))
                (and (= "String" (str (:name (:target n))))
                     (contains? #{"fromCharCode" "fromEnvironment"
                                  "fromCharCodes"}
                                (str (:name n)))))
      false)))

(defn- unit-class
  "Class name when `ty` is a bare named type naming a class of this unit."
  [ty classes]
  (when (and (map? ty)
             (= :named-type (:ir ty))
             (nil? (:prefix ty)))
    (let [n (str (:name ty))]
      (when (contains? classes n) n))))

(defn- ctor-call-class
  "Class constructed by an unqualified/qualified constructor call."
  [call classes]
  (let [target (:target call)]
    (cond
      (nil? target)
      (let [n (str (:name call))
            info (get classes n)]
        (when (contains? (:ctors info) nil) n))

      (and (= :id (:ir target))
           (contains? classes (str (:name target))))
      (let [n (str (:name target))
            info (get classes n)]
        (when (contains? (:ctors info) (:name call)) n))

      :else nil)))

(defn- init-class [init types classes]
  (when (map? init)
    (let [n (strip-paren init)]
      (case (:ir n)
        :new (unit-class (get-in n [:ctor :type]) classes)
        :call (ctor-call-class n classes)
        :id (get types (:name n))
        nil))))

(defn- definitely-non-string?
  "True when a variable declaration is syntactically known not to hold a
  String (used to drop a name whose declarations conflict under shadowing)."
  [v]
  (let [ty (:type v)
        init (:ir (strip-paren (:init v)))]
    (or (and (scalar-type? ty) (not (string-type? ty)))
        (contains? #{:int-lit :double-lit :bool-lit :null-lit} init))))

(defn local-types
  "Map of local name -> unit class name for locals constructed from unit
  classes, plus the sets of locals known to be scalars / Strings.

  `classes` is the unit-info `:classes` map (class name -> info map) and
  `params` the function parameters (`:type` carries scalar declarations).
  Returns {:classes {name class} :scalars #{name} :strings #{name}}.

  Like the other syntactic passes it is name-based and over-approximates
  shadowing; a name declared both as a String and with a definitely
  non-String declaration is dropped from `:strings` (conservative)."
  [block classes params]
  (let [types (volatile! {})
        scalars (volatile! #{})
        strings (volatile! #{})]
    (doseq [p params]
      (if-let [c (unit-class (:type p) classes)]
        (vswap! types assoc (:name p) c)
        (when (scalar-type? (:type p))
          (vswap! scalars conj (:name p))
          (when (string-type? (:type p))
            (vswap! strings conj (:name p))))))
    (letfn [(scan [n]
              (cond
                (map? n)
                (case (:ir n)
                  :var-decl
                  (doseq [v (:vars n)]
                    (if-let [c (or (unit-class (:type v) classes)
                                   (init-class (:init v) @types classes))]
                      (vswap! types assoc (:name v) c)
                      (when (or (scalar-type? (:type v))
                                (contains? literal-irs
                                           (:ir (strip-paren (:init v)))))
                        (vswap! scalars conj (:name v))))
                    (when (or (string-type? (:type v))
                              (string-expr? (:init v) @strings))
                      (vswap! strings conj (:name v)))
                    (when (and (contains? @strings (:name v))
                               (definitely-non-string? v))
                      (vswap! strings disj (:name v))))
                  (:fn :fn-decl) nil
                  (doseq [v (vals (dissoc n :ir :src :annotations))]
                    (scan v)))

                (coll? n) (doseq [v n] (scan v))))]
      (scan block)
      {:classes @types :scalars @scalars :strings @strings})))
