(ns hive-test.cljrs-trifecta-test
  "One cljrs-native proof covering golden, property, and mutation facets."
  ;; :default stays last: it matches everywhere, so anything after it is dead.
  (:require #?@(:clj [[clojure.test.check.generators :as gen]]
                :cljs [[clojure.test.check.generators :as gen]]
                :default [[hive-test.tcheck.generators :as gen]])
            [hive-test.trifecta :refer [deftrifecta]]))

(defn magnitude [n]
  (if (neg? n) (- n) n))

(deftrifecta magnitude-trifecta
  #'hive-test.cljrs-trifecta-test/magnitude
  {:golden-path "test/golden/cljrs/magnitude.edn"
   :cases {:negative -5 :zero 0 :positive 8}
   :gen (gen/choose -1000 1000)
   :pred #(and (not (neg? %)) (<= % 1000))
   :num-tests 100
   :mutations [["always-zero" (constantly 0)]
               ["identity" identity]]})
