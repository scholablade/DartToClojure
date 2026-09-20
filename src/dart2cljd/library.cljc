(ns dart2cljd.library
  "Dart library assembly: `part` / `part of` directives (M9).

  A Dart library may declare `part 'x.dart';` files. Their top-level
  declarations share the library's imports and private scope, as if the part
  were spliced into the library file; the part declares `part of 'main.dart';`
  (or `part of name;` with a matching `library name;`). ClojureDart has one
  file per namespace, so the translator merges every part's declarations into
  the library unit before emission and writes a single `.cljd` for the
  library; part files produce no output of their own.

  `group` is pure: it works on parsed units keyed by their path relative to
  the converted tree and returns the merged libraries plus per-file failures
  (missing or excluded parts, `part of` directives that do not match a
  claimed part, parts claimed by two libraries, parts that declare their own
  parts). A broken part fails its library; a `part of` file that no converted
  library lists fails on its own."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; directive accessors
;; ---------------------------------------------------------------------------

(defn part-directives
  "The `:part` directives of a unit, in source order."
  [unit]
  (into [] (filter #(= :part (:ir %))) (:directives unit)))

(defn part-uris
  "The URIs of a unit's `:part` directives, in source order."
  [unit]
  (into [] (keep #(get-in % [:uri :value])) (part-directives unit)))

(defn part-of-directive
  "The `:part of` directive of a unit, or nil for a library file."
  [unit]
  (some #(when (= :part-of (:ir %)) %) (:directives unit)))

(defn library-name
  "The name of a unit's `library` directive (a dotted string), or nil."
  [unit]
  (some #(when (= :library (:ir %)) (:name %)) (:directives unit)))

(defn part-file?
  "True when the unit is a Dart part (`part of ...`)."
  [unit]
  (some? (part-of-directive unit)))

;; ---------------------------------------------------------------------------
;; relative URI resolution
;; ---------------------------------------------------------------------------

(def missing
  "Sentinel returned by `resolve-relative` when a URI escapes the converted
  tree (`::missing` in this namespace)."
  ::missing)

(defn resolve-relative
  "Resolve a relative Dart URI against the declaring file's directory (both
  relative to the converted tree root). Returns the normalized relative path,
  or `missing` when the URI escapes the tree."
  [file uri]
  (let [dir (str/join "/" (butlast (str/split file #"/")))
        parts (concat (when (seq dir) (str/split dir #"/"))
                      (str/split uri #"/"))]
    (loop [ps parts out []]
      (cond
        (empty? ps) (str/join "/" out)
        (= (first ps) ".") (recur (rest ps) out)
        (= (first ps) "..") (if (seq out)
                              (recur (rest ps) (pop out))
                              missing)
        :else (recur (rest ps) (conj out (first ps)))))))

(defn- relative-uri?
  "Dart `part`/`part of` URIs must be relative to the declaring library."
  [uri]
  (and (string? uri)
       (not (str/starts-with? uri "package:"))
       (not (str/starts-with? uri "dart:"))
       (not (str/starts-with? uri "/"))))

(defn- at
  "` at line:col` for a node's source position (empty when absent)."
  [node]
  (when-let [{:keys [line col]} (:src node)]
    (str " at " line ":" col)))

(defn- fail
  "A positioned per-file failure (see the batch report shape)."
  [file node msg]
  (ex-info (str "dart2cljd: " msg (at node))
           {:dart2cljd/kind :library :file file :node node}))

(defn- part-of-matches?
  "Does `part-unit`'s `part of` directive point at `lib-rel`/`lib-unit`?"
  [part-rel part-unit lib-rel lib-unit]
  (let [po (part-of-directive part-unit)]
    (cond
      (:uri po) (= (resolve-relative part-rel (get-in po [:uri :value])) lib-rel)
      (:library po) (= (:library po) (library-name lib-unit))
      :else false)))

(defn- lists-part?
  "Does library `lib` declare `part-rel` in one of its `part` directives?"
  [units lib part-rel]
  (boolean (some #(= part-rel (resolve-relative lib %))
                 (part-uris (get units lib)))))

(defn- part-of-desc
  "Human description of a part's `part of` directive."
  [part-unit]
  (let [po (part-of-directive part-unit)]
    (cond
      (:uri po) (str "(part of " (pr-str (get-in po [:uri :value])) ")")
      (:library po) (str "(part of " (:library po) ")")
      :else "(without a `part of` directive)")))

(defn- combine-errors
  "One exception per failed file: join multiple messages, first one wins for
  the position data."
  [errs]
  (if (= 1 (count errs))
    (first errs)
    (ex-info (str/join "; " (map ex-message errs))
             (assoc (ex-data (first errs)) :dart2cljd/errors (vec errs)))))

;; ---------------------------------------------------------------------------
;; assembly
;; ---------------------------------------------------------------------------

(def ^:private type-decl-irs
  "Declaration kinds that *declare* a Dart type."
  #{:class :class-alias :mixin :enum :extension :extension-type :type-alias})

(defn- type-decl-name [d]
  (when (contains? type-decl-irs (:ir d)) (:name d)))

(defn- type-refs
  "Bare local type names a declaration mentions anywhere (hints, heritage,
  field/parameter/return types, pattern tests). Prefixed names are external
  (`m/Widget`) and are not local type references."
  [node]
  (cond
    (map? node)
    (into (if (and (= :named-type (:ir node))
                   (nil? (:prefix node))
                   (string? (:name node)))
            #{(:name node)}
            #{})
          (mapcat type-refs (vals node)))
    (vector? node) (mapcat type-refs node)
    :else nil))

(defn order-decls
  "Stable topological order of declarations by local type references (M9).

  Dart is order-free, but ClojureDart resolves types eagerly while compiling a
  `deftype` or a method hint, so a type used in a hint or heritage clause must
  be emitted before its use. Part declarations are appended after the
  library's, which breaks the common \"library uses a part type\" direction;
  this sort puts dependencies first, preserving the original order everywhere
  else. A cycle keeps the original (Dart) order: it cannot be represented in
  one file and fails loudly at compile time, like any other forward reference
  did before."
  [decls]
  (let [decls (vec decls)
        names (into {} (keep-indexed (fn [i d]
                                       (when-let [n (type-decl-name d)]
                                         [n i])))
                    decls)
        deps (mapv (fn [d]
                     (let [self (type-decl-name d)]
                       (into #{} (comp (filter #(not= % self))
                                       (keep #(get names %)))
                             (type-refs d))))
                   decls)
        n (count decls)]
    (loop [remaining (vec (range n)) placed #{} order []]
      (if (empty? remaining)
        (mapv decls order)
        (if-let [i (some (fn [i] (when (every? placed (deps i)) i)) remaining)]
          (recur (filterv #(not= % i) remaining) (conj placed i) (conj order i))
          ;; A cycle keeps the original order for the rest (loud at compile).
          (into (mapv decls order) (map decls remaining)))))))

(defn merge-parts
  "Merge part units into a library unit: declarations are concatenated in part
  order (the library's first) and the `part`/`part of` directives are dropped.

  When there are parts, the merged declarations are ordered by local type
  references (`order-decls`), because ClojureDart resolves types eagerly and a
  library commonly references a type declared in one of its parts. A library
  without parts keeps its Dart declaration order byte-for-byte."
  [unit parts]
  (-> unit
      (update :directives
              (fn [ds]
                (into [] (remove #(contains? #{:part :part-of} (:ir %))) ds)))
      (update :decls (fn [decls]
                       (let [merged (into (vec decls) (mapcat :decls parts))]
                         (if (seq parts)
                           (order-decls merged)
                           merged))))))

(defn group
  "Group parsed units into libraries and merge their parts.

  `parsed` is a seq of `{:file rel :unit unit}` (deterministically ordered).
  Options:

    :excluded     set of rel paths skipped by exclude patterns
    :parse-failed set of rel paths that failed to parse

  Returns:

    {:libraries [{:file rel :unit merged-unit :parts [part-rel ...]} ...]
     :parts     {part-rel library-rel}
     :errors    [{:file part-rel :error ex-info} ...]}

  A library entry carries `:error` (and no `:unit`) when one of its parts is
  unresolved, excluded, malformed or claimed by another library; the entry's
  `:parts` still lists every part it resolved, so the caller can report them.
  `:errors` holds the parts that no converted library lists."
  [parsed {:keys [excluded parse-failed] :as _opts}]
  (let [units (into {} (map (juxt :file :unit)) parsed)
        order (mapv :file parsed)
        excluded (set excluded)
        parse-failed (set parse-failed)
        part-files (into [] (filter #(part-file? (get units %))) order)
        lib-files (into [] (remove #(part-file? (get units %))) order)
        name->libs (reduce (fn [m f]
                             (if-let [n (library-name (get units f))]
                               (update m n (fnil conj []) f)
                               m))
                           {}
                           lib-files)
        absent-msg (fn [rel uri]
                     (cond
                       (contains? parse-failed rel)
                       (str "part " (pr-str uri) " could not be parsed")

                       (contains? excluded rel)
                       (str "part " (pr-str uri) " was skipped by an exclude "
                            "pattern; drop the pattern or exclude the library")

                       :else
                       (str "part " (pr-str uri) " does not exist under --dir")))
        ;; Phase A: claims from every library's `part` directives.
        phase-a
        (reduce
         (fn [st lib]
           (let [unit (get units lib)]
             (reduce
              (fn [st d]
                (let [uri (get-in d [:uri :value])]
                  (cond
                    (not (relative-uri? uri))
                    (update-in st [:errors lib] (fnil conj [])
                               (fail lib d (str "part URI " (pr-str uri)
                                                " must be relative")))

                    :else
                    (let [rel (resolve-relative lib uri)]
                      (cond
                        (= ::missing rel)
                        (update-in st [:errors lib] (fnil conj [])
                                   (fail lib d (str "part " (pr-str uri)
                                                    " escapes the converted tree")))

                        (nil? (get units rel))
                        (update-in st [:errors lib] (fnil conj [])
                                   (fail lib d (absent-msg rel uri)))

                        :else
                        (let [punit (get units rel)
                              prev (get-in st [:claims rel])
                              st (update-in st [:parts lib] (fnil conj []) rel)]
                          (cond
                            (= prev lib)
                            (update-in st [:errors lib] (fnil conj [])
                                       (fail lib d (str "part " (pr-str rel)
                                                        " is listed twice")))

                            prev
                            (-> st
                                (update-in [:errors lib] (fnil conj [])
                                           (fail lib d (str "part " (pr-str rel)
                                                            " is already a part of "
                                                            prev)))
                                (update-in [:errors prev] (fnil conj [])
                                           (fail prev d (str "part " (pr-str rel)
                                                             " is also a part of "
                                                             lib))))

                            (seq (part-uris punit))
                            (update-in st [:errors lib] (fnil conj [])
                                       (fail lib d (str "part " (pr-str rel)
                                                        " declares its own parts")))

                            (not (part-of-matches? rel punit lib unit))
                            (update-in st [:errors lib] (fnil conj [])
                                       (fail lib d (str "part " (pr-str rel) " "
                                                        (part-of-desc punit)
                                                        ", not this library")))

                            :else
                            (assoc-in st [:claims rel] lib))))))))
              st
              (part-directives unit))))
         {:claims {} :parts {} :errors {}}
         lib-files)
        ;; Phase B: parts no parsed library claims (orphan `part of` files).
        orphan-errors
        (into []
              (keep
               (fn [p]
                 (when-not (contains? (:claims phase-a) p)
                   (let [punit (get units p)
                         po (part-of-directive punit)
                         uri (get-in po [:uri :value])
                         libname (:library po)]
                     (cond
                       (some? uri)
                       (cond
                         (not (relative-uri? uri))
                         (fail p po "part of URI must be relative")

                         :else
                         (let [rel (resolve-relative p uri)]
                           (cond
                             (= ::missing rel)
                             (fail p po (str "part of " (pr-str uri)
                                             " escapes the converted tree"))

                             (nil? (get units rel))
                             (fail p po (str "part of library " (pr-str uri)
                                             " was not converted (missing, "
                                             "excluded or failed)"))

                             (part-file? (get units rel))
                             (fail p po (str "part of " (pr-str uri)
                                             " is itself a part"))

                             (lists-part? units rel p)
                             ;; The library lists it, so phase A would have
                             ;; claimed it unless the library already failed.
                             (fail p po (str "library " (pr-str rel)
                                             " failed to claim this part"))

                             :else
                             (fail p po (str "library " (pr-str rel)
                                             " does not declare this file as a "
                                             "part")))))

                       (some? libname)
                       (let [libs (get name->libs libname)]
                         (cond
                           (empty? libs)
                           (fail p po (str "no converted library declares "
                                           "`library " libname ";`"))

                           (< 1 (count libs))
                           (fail p po (str "multiple converted libraries declare "
                                           "`library " libname ";`"))

                           (lists-part? units (first libs) p)
                           (fail p po (str "library " (pr-str (first libs))
                                           " failed to claim this part"))

                           :else
                           (fail p po (str "library " (pr-str (first libs))
                                           " does not declare this file as a "
                                           "part"))))

                       :else
                       (fail p po "part of directive without a URI or library name"))))))
              part-files)]
    {:libraries
     (mapv (fn [lib]
             (let [errs (get-in phase-a [:errors lib])
                   parts (get-in phase-a [:parts lib] [])]
               (cond-> {:file lib :parts parts}
                 (seq errs) (assoc :error (combine-errors errs))
                 (empty? errs) (assoc :unit (merge-parts (get units lib)
                                                         (map #(get units %) parts))))))
           lib-files)
     :parts (:claims phase-a)
     :errors (mapv (fn [e] {:file (:file (ex-data e)) :error e}) orphan-errors)}))
