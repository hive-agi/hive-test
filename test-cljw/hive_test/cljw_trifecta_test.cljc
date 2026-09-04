(ns hive-test.cljw-trifecta-test
  "One cljw-native proof covering golden, property, and mutation facets."
  (:require [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn magnitude [n]
  (if (neg? n) (- n) n))

(deftrifecta magnitude-trifecta
  #'hive-test.cljw-trifecta-test/magnitude
  {:golden-path "test/golden/cljw/magnitude.edn"
   :cases {:negative -5 :zero 0 :positive 8}
   :gen (gen/choose -1000 1000)
   :pred #(and (not (neg? %)) (<= % 1000))
   :num-tests 100
   :mutations [["always-zero" (constantly 0)]
               ["identity" identity]]})
