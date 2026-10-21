(ns hive-test.tcheck.properties-test
  "Facets for the portable property runner: check-once as a property over its
   contract, and quick-check / report driven on a passing and a failing
   property, pinned as a golden."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.tcheck.clojure-test :as tc]
            [hive-test.tcheck.generators :as gen]
            [hive-test.tcheck.properties :as prop]
            [hive-test.tcheck.random :as rng]))

(def ^:private even-prop (prop/property [gen/small-integer] even?))

(defn- check-even [[seed size]] (prop/check-once even-prop (rng/make-rng seed) size))

(hst/deftrifecta-from-schema check-once-reports-the-drawn-args
  check-even
  {:in  [:tuple :int [:int {:min 1 :max 50}]]
   :out prop/Outcome
   :rel (fn [_ out] (and (= 1 (count (:args out)))
                         (= (:pass? out) (even? (first (:args out))))))
   :mutation true})

(deftest a-throwing-property-fails-with-the-error
  (let [out (prop/check-once (prop/property [gen/nat] (fn [_] (throw (ex-info "boom" {}))))
                             (rng/make-rng 1) 5)]
    (is (false? (:pass? out)))
    (is (= "boom" (ex-message (:error out))))))

(deftest-golden quick-check-outcomes "test/golden/tcheck/quick-check.edn"
  {:passing (tc/quick-check 20 (prop/for-all [x gen/nat] (nat-int? x)) 7)
   :failing (dissoc (tc/quick-check 20 even-prop 7) :error)})
