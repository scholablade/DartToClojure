(ns dart2cljd.alias
  "Deterministic import -> require alias resolution (research item R6).

  Aliases come from an explicit `as` prefix when the Dart source has one,
  otherwise from `default-aliases` for well-known libraries, otherwise from
  a deterministic derivation of the URI's file stem. Nothing here depends on
  identifier casing for *aliasing*; casing is only used to tell Dart type
  names (UpperCamelCase, by language convention) from values when resolving
  member access and constructor calls.

  A context is a plain map:
    :prefix->uri  {\"m\" \"package:flutter/material.dart\", ...}
    :uri->alias   inverse of the above
    :referred     #{\"Colors\" ...} names imported with `show`
    :local-names  #{\"MyClass\" ...} names declared in the current file
    :default-alias alias (symbol or string) for otherwise-unresolved types,
                   or nil to leave them bare"
  (:require [clojure.string :as str]))

(def default-aliases
  "Well-known Dart library URI -> conventional cljd alias (house style)."
  {"package:flutter/material.dart" "m"
   "package:flutter/widgets.dart" "w"
   "package:flutter/cupertino.dart" "cupertino"
   "dart:convert" "c"
   "dart:async" "a"
   "dart:io" "io"
   "dart:math" "math"
   "dart:collection" "collection"
   "dart:typed_data" "typed-data"
   "dart:isolate" "isolate"
   "dart:developer" "developer"
   "cljd.flutter" "f"})

(def implicit-uris
  "Libraries that are available without a require."
  #{"dart:core"})

(def core-symbols
  "dart:core types and top-level functions (used without a library prefix)."
  #{"Object" "String" "int" "double" "num" "bool" "List" "Map" "Set" "Iterable"
    "Iterator" "Future" "Stream" "Duration" "DateTime" "Function" "Symbol"
    "Type" "Record" "Enum" "Comparable" "BigInt" "Uri" "RegExp" "Exception"
    "Error" "StateError" "ArgumentError" "UnsupportedError"
    "UnimplementedError" "RangeError" "FormatException" "NoSuchMethodError"
    "StackTrace" "StringBuffer" "Stopwatch" "Runes" "Null" "Never"
    "dynamic" "void" "print" "identical" "identity" "assert"})

(defn class-name?
  "Dart type-name convention: starts with an upper-case letter."
  [s]
  (boolean (and (string? s) (re-matches #"[A-Z].*" s))))

(defn suggest-alias
  "Deterministic alias suggestion for a Dart library URI, or nil when the
  library needs no alias (dart:core)."
  [uri]
  (when-not (contains? implicit-uris uri)
    (or (get default-aliases uri)
        (let [stem (-> uri
                       (str/split #"/")
                       last
                       (str/replace #"\.dart$" "")
                       (str/replace #"[^A-Za-z0-9_-]" "-")
                       (str/replace #"^[-]+|[-]+$" ""))]
          (when (seq stem) stem)))))

(defn context
  "Build an alias context from normalized `:import` IR nodes and options.

  Options:
    :default-alias  alias for unresolved type names (default \"m\");
                    pass nil to leave unresolved names bare
    :local-names    set of type names declared in the current file"
  ([imports] (context imports nil))
  ([imports {:keys [default-alias local-names]
             :or {default-alias "m" local-names #{}}}]
   (let [entries (for [imp imports
                       :let [uri (get-in imp [:uri :value])
                             explicit (:as imp)
                             alias (or explicit (suggest-alias uri))]
                       :when alias]
                   {:uri uri :alias alias :refer (set (:show imp))})]
     {:entries (vec entries)
      :prefix->uri (into {} (map (juxt :alias :uri)) entries)
      :uri->alias (into {} (map (juxt :uri :alias)) entries)
      :referred (into #{} (mapcat :refer) entries)
      :local-names (set local-names)
      :default-alias (some-> default-alias str)})))

(def snippet-context
  "Default context for bare snippets: Flutter material is assumed to be
  imported as `m` and cljd.flutter as `f` (matching DartClojure's CLI
  defaults)."
  (assoc (context [] {:default-alias "m"})
         :prefix->uri {"m" "package:flutter/material.dart"
                       "f" "cljd.flutter"}
         :uri->alias {"package:flutter/material.dart" "m"
                      "cljd.flutter" "f"}))

(defn import-prefix?
  "Is `p` a known import alias/prefix in this context?"
  [p ctx]
  (contains? (:prefix->uri ctx) (str p)))

(defn flutter-alias
  "Deterministic alias for cljd.flutter in this context: the existing binding
  when the library is already aliased, otherwise the first free candidate
  from [\"f\" \"fl\" \"cljd-flutter\"]. Idiomatic mode adds the require."
  [ctx]
  (let [prefix->uri (:prefix->uri ctx)]
    (or (some (fn [[a uri]] (when (= "cljd.flutter" uri) a)) prefix->uri)
        (first (remove #(contains? prefix->uri %) ["f" "fl"]))
        "cljd-flutter")))

(defn qualify
  "Resolve a bare Dart identifier to a cljd symbol.
  Referred names and dart:core names stay bare; known import prefixes are
  kept as `prefix/name`; type-like names (UpperCamelCase) get the default
  alias; everything else stays bare."
  [id ctx]
  (let [id (str id)]
    (cond
      (contains? (:referred ctx) id) (symbol id)
      (contains? (:local-names ctx) id) (symbol id)
      (contains? core-symbols id) (symbol id)
      (import-prefix? id ctx) (symbol id)
      (class-name? id) (if-let [a (:default-alias ctx)]
                         (symbol (str a "/" id))
                         (symbol id))
      :else (symbol id))))

(defn qualify-member
  "Qualify a static member access `Type.member` as the dotted symbol
  `alias/Type.member` (house style: `m/Colors.blue`)."
  [type-id member ctx]
  (symbol (str (qualify type-id ctx) "." member)))

(defn require-clause
  "Turn an `:import` IR node into a cljd ns `:require` vector."
  [imp]
  (let [uri (get-in imp [:uri :value])
        explicit (:as imp)
        alias (or explicit (suggest-alias uri))
        show (:show imp)]
    (cond-> [uri]
      alias (conj :as (symbol alias))
      (seq show) (conj :refer (mapv symbol show)))))

(defn require-clauses
  "All `:require` vectors for the given imports, skipping implicit libs and
  deterministic de-duplication."
  [imports]
  (->> imports
       (remove #(contains? implicit-uris (get-in % [:uri :value])))
       (map require-clause)
       distinct
       vec))
