(ns dart2cljd.emit.decls
  "IR declarations -> top-level cljd forms: ns/imports, defs, defns.

  Classes, enums, extensions, typedefs and mixins are M3 work and fail
  loudly here."
  (:require [clojure.string :as str]
            [dart2cljd.alias :as alias]
            [dart2cljd.emit.core :as core]
            [dart2cljd.emit.statements :as st]))

(defn ctx-for
  "Build an emission context for a unit (options may carry :aliases)."
  [aliases {:keys [mode] :or {mode :faithful}}]
  (core/ctx aliases {:mode mode
                     :emit-body st/emit-body
                     :emit-stmt st/emit-stmt}))

(defn unit-aliases
  "Alias context for a unit's imports, honoring :default-alias."
  [unit {:keys [default-alias local-names] :as opts}]
  (let [imports (filter #(= :import (:ir %)) (:directives unit))]
    (alias/context imports
                   {:default-alias (if (contains? opts :default-alias)
                                     default-alias
                                     "m")
                    :local-names (set local-names)})))

(defn- library-name [unit]
  (some #(when (= :library (:ir %)) (:name %)) (:directives unit)))

(defn- path->ns [path]
  (when path
    (let [base (-> path (str/split #"/") last (str/replace #"\.dart$" ""))]
      (when (seq base) (symbol base)))))

(defn- emit-top-var [d ctx]
  (let [vl (:vars d)
        const? (:const vl)
        name-sym (fn [n] (if const?
                           (with-meta (symbol n) {:const true})
                           (symbol n)))]
    (mapv (fn [v]
            (list 'def (name-sym (:name v))
                  (when (:init v) (core/emit-expr (:init v) ctx))))
          (:vars vl))))

(defn emit-decl
  "Emit one top-level declaration as a vector of forms."
  [d ctx]
  (case (:ir d)
    :fn-decl
    (do
      (when-not (= :fn (:kind d))
        (core/fail! d "top-level getters/setters are not supported yet"))
      (let [params (mapv #(core/param-symbol % ctx) (:params d))
            body (core/emit-fn-body (:body d) ctx)]
        [(list* 'defn (symbol (:name d)) params body)]))

    :top-var-decl
    (emit-top-var d ctx)

    (core/fail! d (str "unsupported top-level declaration " (:ir d)))))

(defn emit-unit
  "Normalized :unit IR -> vector of top-level forms, ns first."
  [unit {:keys [ns path aliases] :as opts}]
  (let [aliases (or aliases (unit-aliases unit opts))
        ctx (ctx-for aliases opts)
        ns-name (or ns (some-> (library-name unit) symbol) (path->ns path) 'main)
        requires (alias/require-clauses
                  (filter #(= :import (:ir %)) (:directives unit)))
        ns-form (list* 'ns ns-name
                       (when (seq requires)
                         [(list* :require requires)]))]
    (into [ns-form] (mapcat #(emit-decl % ctx) (:decls unit)))))
