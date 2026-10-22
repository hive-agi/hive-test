(ns hive-test.properties.bounded-atom-test
  "Facets for hive-test.properties.bounded-atom: run-ops against the
   map-backed fake ApiFns behaves like a reference reduction over the op
   vectors (property over the contract), and a golden of fixed runs."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.properties.bounded-atom :as pba]))

(def ^:private fake-api
  {:bput!   (fn [a k v] (swap! a assoc k v))
   :bget    (fn [a k] (get @a k))
   :bclear! (fn [a] (reset! a {}))
   :sweep!  (fn [_ _] nil)
   :bcount  (fn [a] (count @a))})

(defn- reference
  "The model: what the fake's map must hold after `ops`."
  [ops]
  (reduce (fn [m [op k v]] (case op :bput (assoc m k v) :bclear {} m)) {} ops))

(defn- run-on-fake [ops] @(pba/run-ops fake-api (atom {}) ops))

(hst/deftrifecta-from-schema run-ops-matches-the-reference-model
  run-on-fake
  {:in  [:vector pba/Op]
   :out [:map-of :keyword :int]
   :rel (fn [ops m] (= (reference ops) m))
   :mutation true})

(deftest apply-op-returns-the-same-batom
  (let [a (atom {})]
    (doseq [op [[:bput :k 1] [:bget :k] [:sweep] [:bcount] [:bclear] [:unknown]]]
      (is (identical? a (pba/apply-op fake-api a op))))))

(deftest batom-generators-pair-opts-with-ops
  (doseq [seed (range 10)]
    (let [[opts ops] (gen/generate (pba/gen-batom-with-ops) 10 seed)]
      (is (contains? #{:lru :fifo} (:eviction-policy opts)))
      (is (<= 1 (count ops) 50)))
    (let [[opts _] (gen/generate (pba/gen-batom-with-ops-all-policies) 10 seed)]
      (is (contains? #{:lru :fifo :ttl} (:eviction-policy opts))))))

(deftest-golden fixed-runs "test/golden/properties/bounded-atom-runs.edn"
  {:puts        (into (sorted-map) (run-on-fake [[:bput :a 1] [:bput :b 2] [:bput :a 3]]))
   :cleared     (run-on-fake [[:bput :a 1] [:bclear] [:bput :c 9]])
   :reads-only  (run-on-fake [[:bget :a] [:sweep] [:bcount]])
   :empty       (run-on-fake [])})
