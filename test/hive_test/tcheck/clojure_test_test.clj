(ns hive-test.tcheck.clojure-test-test
  "Facets for the portable defspec / quick-check: quick-check as a property
   over its contract, and the shim's own defspec over a real property."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-test.tcheck.clojure-test :as tc]
            [hive-test.tcheck.generators :as gen]
            [hive-test.tcheck.properties :as prop]))

(def ^:private below-limit (prop/for-all [x (gen/choose 0 9)] (< x 10)))
(def ^:private never (prop/for-all [_ gen/nat] false))

(defn- run-n [[n seed which]] (tc/quick-check n (if which below-limit never) seed))

(hst/deftrifecta-from-schema quick-check-runs-n-or-stops-at-the-first-failure
  run-n
  {:in  [:tuple [:int {:min 1 :max 30}] :int :boolean]
   :out tc/QuickCheckResult
   :rel (fn [[n seed which] out]
          (and (= seed (:seed out))
               (= which (:result out))
               (= (if which n 1) (:num-tests out))))
   :mutation true})

(tc/defspec the-shim-defspec-runs-a-property 25
  (prop/for-all [v (gen/vector gen/boolean 3)] (= 3 (count v))))

(deftest report-fails-through-clojure-test-on-a-failing-outcome
  (let [counts (mut/capture-test-results #(tc/report (tc/quick-check 5 never 1)))]
    (is (= {:pass 0 :fail 1 :error 0} counts)))
  (is (= {:pass 1 :fail 0 :error 0}
         (mut/capture-test-results #(tc/report (tc/quick-check 5 below-limit 1))))))
