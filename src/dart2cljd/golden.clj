(ns dart2cljd.golden
  "Regenerates the golden expectations from committed astdump fixtures.

  Golden pairs live in corpus/: `corpus/<name>.dart` is the Dart input,
  `corpus/<name>.cljd` the reviewed expected translation, and
  `corpus/manifest.edn` records the emission mode. This tool regenerates the
  .cljd files from the cached astdump fixtures (test/fixtures/<name>.edn,
  made by `make fixture`). Regeneration is explicit (`make golden`) and the
  diff is meant to be reviewed like code."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer]))

(def manifest
  (edn/read-string (slurp (io/file "corpus/manifest.edn"))))

(defn- emit-entry [{:keys [name mode]}]
  (let [doc (ast/normalize-doc
             (edn/read-string
              (slurp (io/file "test/fixtures" (str name ".edn")))))]
    (printer/print-forms
     (decls/emit-unit (:unit doc) (cond-> {:path (str name ".dart")}
                                    (= :idiomatic mode) (assoc :mode :idiomatic))))))

(defn -main [& [cmd]]
  (if (= "regen" cmd)
    (doseq [{:keys [name] :as entry} manifest]
      (let [out (io/file "corpus" (str name ".cljd"))]
        (spit out (str (emit-entry entry) "\n"))
        (println "wrote" (.getPath out))))
    (binding [*out* *err*]
      (println "usage: clojure -M -m dart2cljd.golden regen")
      (System/exit 1))))
