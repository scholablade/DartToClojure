(ns dart2cljd.golden
  "Regenerates the golden expectations from committed astdump fixtures.

  M3 keeps one golden pair (test/fixtures/m3.edn -> m3.expected.cljd); the
  golden corpus grows in M5. Regeneration is explicit (`make golden`), and
  the diff is meant to be reviewed like code."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [dart2cljd.ast :as ast]
            [dart2cljd.emit.decls :as decls]
            [dart2cljd.printer :as printer]))

(def ^:private pairs
  "fixture name -> paths (edn in, cljd golden out)."
  {"m3" ["test/fixtures/m3.edn" "corpus/m3.cljd"]})

(defn- emit-fixture [name]
  (let [[in _] (pairs name)
        doc (ast/normalize-doc (edn/read-string (slurp (io/file in))))]
    (printer/print-forms (decls/emit-unit (:unit doc) {:path (str name ".dart")}))))

(defn -main [& [cmd]]
  (if (= "regen" cmd)
    (doseq [[name [_ out]] pairs]
      (io/make-parents out)
      (spit (io/file out) (str (emit-fixture name) "\n"))
      (println "wrote" out))
    (binding [*out* *err*]
      (println "usage: clojure -M -m dart2cljd.golden regen")
      (System/exit 1))))
