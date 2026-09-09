(ns dart2cljd.api
  "Public conversion API, shape-compatible with DartClojure's `convert`.

  Pipeline: Dart source -> bin/astdump.dart (EDN) -> dart2cljd.ast
  (normalize) -> dart2cljd.emit.* (forms) -> dart2cljd.printer (text).

  Snippet conversion is cached under `.dart2cljd-cache/` keyed by a hash of
  the snippet and of bin/astdump.dart, so repeated conversions in tests do
  not pay the Dart startup cost and stale entries are invalidated whenever
  the frontend changes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [dart2cljd.alias :as alias]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer])
  (:import (java.security MessageDigest)))

;; ---------------------------------------------------------------------------
;; options
;; ---------------------------------------------------------------------------

(defn- kwargs->map [args]
  (when (odd? (count args))
    (throw (ex-info (str "dart2cljd: odd number of keyword arguments: " (pr-str args))
                    {:dart2cljd/kind :options :args args})))
  (apply hash-map args))

(defn- snippet-aliases [{:keys [material flutter default-alias]}]
  (let [material (str (or material "m"))
        flutter (str (or flutter "f"))
        default (str (or default-alias material))
        base (alias/context [] {:default-alias default})]
    (assoc base
           :prefix->uri {material "package:flutter/material.dart"
                         flutter "cljd.flutter"}
           :uri->alias {"package:flutter/material.dart" material
                        "cljd.flutter" flutter})))

(defn- emit-snippet-decl [d ctx]
  (case (:ir d)
    :expr-stmt [(core/emit-expr (:expr d) ctx)]
    (decls/emit-decl d ctx)))

;; ---------------------------------------------------------------------------
;; conversion
;; ---------------------------------------------------------------------------

(defn convert-doc
  "Convert an astdump EDN document (raw or already normalized) to cljd.

  Options:
    :format         :string (default) | :sexpr
    :mode           :faithful (default) | :idiomatic
    :default-alias  alias for unresolved Dart types (default \"m\")
    :material       snippet alias for package:flutter/material.dart (\"m\")
    :flutter        snippet alias for cljd.flutter (\"f\")
    :ns             namespace name override (file mode)
    :width          printer line width (default 100)

  Returns the printed source string, or forms for :sexpr."
  [doc opts]
  (let [doc (if (:ir (:unit doc)) doc (ast/normalize-doc doc))
        unit (:unit doc)
        snippet? (= :snippet (:mode doc))
        aliases (or (:aliases opts)
                    (if snippet?
                      (snippet-aliases opts)
                      (decls/unit-aliases unit opts)))
        ctx (decls/ctx-for aliases opts)
        forms (if snippet?
                (let [fs (vec (mapcat #(emit-snippet-decl % ctx) (:decls unit)))]
                  (if (= 1 (count fs)) fs (list* 'do fs)))
                (decls/emit-unit unit (assoc opts :aliases aliases)))]
    (case (or (:format opts) :string)
      :sexpr (if snippet? (first forms) forms)
      :string (printer/print-forms forms {:width (or (:width opts) 100)})
      (throw (ex-info (str "dart2cljd: unknown :format " (:format opts))
                      {:dart2cljd/kind :options :format (:format opts)})))))

;; ---------------------------------------------------------------------------
;; astdump frontend
;; ---------------------------------------------------------------------------

(defn- sha256 [^String s]
  (let [d (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest d (.getBytes s "UTF-8"))))))

(defn- cache-file [root key]
  (io/file root ".dart2cljd-cache" (str key ".edn")))

(defn- run-astdump
  "Run bin/astdump.dart with `args`, feeding `stdin` when given."
  [root cmd args stdin]
  (let [result (apply shell/sh cmd "run" "bin/astdump.dart"
                      (concat args (when stdin [:in stdin]) [:dir root]))]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "dart2cljd: astdump failed: " (:err result))
                      {:dart2cljd/kind :astdump
                       :exit (:exit result)
                       :stderr (:err result)})))
    (edn/read-string (:out result))))

(defn astdump
  "Dump a Dart snippet (string) to an astdump EDN document.

  Options: :dart (binary), :root (dart2cljd checkout), :no-cache"
  [code {:keys [dart root no-cache] :as opts}]
  (let [root (or root (:dart2cljd-root opts) (System/getProperty "user.dir"))
        cmd (or dart (System/getenv "DART2CLJD_DART") "dart")
        dumper (io/file root "bin/astdump.dart")
        tool-hash (if (.exists dumper) (sha256 (slurp dumper)) "missing")
        key (sha256 (str tool-hash "\n" cmd "\n" code))
        cached (cache-file root key)]
    (if (and (not no-cache) (.exists cached))
      (edn/read-string (slurp cached))
      (let [doc (run-astdump root cmd ["-"] code)]
        (when-not no-cache
          (io/make-parents cached)
          (spit cached (pr-str doc)))
        doc))))

(defn astdump-file
  "Dump a Dart file (path) to an astdump EDN document."
  [path {:keys [dart root] :as opts}]
  (let [root (or root (:dart2cljd-root opts) (System/getProperty "user.dir"))
        cmd (or dart (System/getenv "DART2CLJD_DART") "dart")]
    (run-astdump root cmd [path] nil)))

;; ---------------------------------------------------------------------------
;; public API
;; ---------------------------------------------------------------------------

(defn convert
  "Dart source snippet -> cljd, DartClojure-style:
  `(convert \"Text('hi')\" :format :sexpr)`."
  [dart & args]
  (let [opts (kwargs->map args)]
    (convert-doc (ast/normalize-doc (astdump dart opts) opts) opts)))

(defn convert-file
  "Dart file -> cljd (includes the ns form)."
  [path & args]
  (let [opts (kwargs->map args)
        doc (ast/normalize-doc (astdump-file path opts) opts)]
    (convert-doc doc (assoc opts :path path))))

(defn -main
  "Minimal CLI: read a Dart file or stdin and print cljd."
  [& args]
  (let [path (first args)
        code (if (and path (not= path "-"))
               (slurp path)
               (slurp *in*))
        out (if (and path (not= path "-"))
              (convert-file path)
              (convert code))]
    (println out)))
