(ns hive-test.memory.store-contract.outcomes-test
  "Name-matched facets for hive-test.memory.store-contract: the optional
   protocol probes and the unbound-factory fixture as properties over their
   contracts, and a golden of the kit's per-test verdict against the
   conforming stub and each single-clause violator defined in
   hive-test.memory.store-contract-test (reused, not copied)."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.memory.store-contract :as contract]
            [hive-test.memory.store-contract-test :as kit-test]))

(hst/deftrifecta-from-schema plain-data-is-no-analytics-store
  contract/analytics-store?
  {:in  [:or :int :string :keyword [:map-of :keyword :int] [:vector :int]]
   :out :boolean
   :rel (fn [_ out] (false? out))
   :mutation true})

(hst/deftrifecta-from-schema plain-data-is-no-staleness-store
  contract/staleness-store?
  {:in  [:or :int :string :keyword [:map-of :keyword :int] [:vector :int]]
   :out :boolean
   :rel (fn [_ out] (false? out))
   :mutation true})

(defn- fixture-with-unbound-factory
  "Without a bound *store-factory* the fixture skips the test: nil, f unrun."
  [ret]
  (let [ran (atom false)]
    {:answer (contract/store-factory-fixture (fn [] (reset! ran true) ret)) :ran? @ran}))

(hst/deftrifecta-from-schema fixture-skips-when-no-factory-is-bound
  fixture-with-unbound-factory
  {:in  :any
   :out [:map [:answer :nil] [:ran? [:= false]]]
   :rel (fn [_ out] (= {:answer nil :ran? false} out))
   :mutation true})

(deftest fixture-runs-the-test-when-a-factory-is-bound
  (binding [contract/*store-factory* (constantly :store)]
    (is (= :ran (contract/store-factory-fixture (constantly :ran))))))

(def ^:private stores
  {:conforming  @#'kit-test/->stub
   :leaky-delete #(kit-test/->LeakyDeleteStore (@#'kit-test/->stub))
   :ciphertext  #(kit-test/->CiphertextIndexStore (@#'kit-test/->stub))
   :unranked    #(kit-test/->UnrankedSearchStore (@#'kit-test/->stub))})

(def ^:private clause-tests
  '[test-add-get-roundtrip test-add-delete-get test-search-similar-behavioral])

(deftest-golden kit-verdicts "test/golden/memory/store-contract-verdicts.edn"
  (into (sorted-map)
        (for [[k factory] stores]
          [k (into (sorted-map)
                   (for [sym clause-tests
                         :let [v (ns-resolve 'hive-test.memory.store-contract sym)]]
                     [sym (if (@#'kit-test/failed? (@#'kit-test/run-isolated factory @v)) :fail :pass)]))])))
