(ns dart2cljd.typeinfo
  "Deterministic syntactic analysis of local variable types.

  Records which locals are known to hold instances of classes declared in
  the current unit, e.g. `final p = Point(1, 2);` or typed declarations
  `Point p = ...`. The emitter uses this to call Dart operators as cljd
  interop method calls (`(.+ a b)`), because ClojureDart's numeric
  functions do not dispatch to user-defined operators.

  Like dart2cljd.mutable the analysis is purely syntactic and
  over-approximates shadowing scopes (a name bound in an inner scope may
  be attributed the outer type); results are stable and reproducible.

  No type information beyond the current unit is needed, so no resolution
  pass is required.")

(defn- strip-paren [node]
  (if (= :paren (:ir node)) (recur (:expr node)) node))

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

(defn local-types
  "Map of local name -> class name for locals constructed from unit classes.

  `classes` is the unit-info `:classes` map (class name -> info map)."
  [block classes]
  (let [types (volatile! {})]
    (letfn [(scan [n]
              (cond
                (map? n)
                (case (:ir n)
                  :var-decl
                  (doseq [v (:vars n)]
                    (when-let [c (or (unit-class (:type v) classes)
                                     (init-class (:init v) @types classes))]
                      (vswap! types assoc (:name v) c)))
                  (:fn :fn-decl) nil
                  (doseq [v (vals (dissoc n :ir :src :annotations))]
                    (scan v)))

                (coll? n) (doseq [v n] (scan v))))]
      (scan block)
      @types)))
