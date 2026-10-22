(ns hive-test.generators.kg-test
  "Facets for hive-test.generators.kg: relation-gen draws only from the set it
   is handed (property over its contract), the edge/datum generators stay in
   their documented ranges, and a golden of seeded draws."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.generators.kg :as gk]
            [hive-test.golden :refer [deftest-golden]]))

(defn- draw-relation [[rels seed]] (gen/generate (gk/relation-gen rels) 10 seed))

(hst/deftrifecta-from-schema relation-gen-draws-from-the-injected-set
  draw-relation
  {:in  [:tuple [:set {:min 1 :max 6} :keyword] :int]
   :out :keyword
   :rel (fn [[rels _] r] (contains? rels r))
   :mutation true})

(defn- draw-default-relation [seed] (gen/generate (gk/relation-gen) 10 seed))

(hst/deftrifecta-from-schema relation-gen-defaults-to-the-vocabulary
  draw-default-relation
  {:in  :int
   :out (into [:enum] gk/relation-types)
   :rel (fn [_ r] (contains? gk/relation-types r))
   :mutation true})

(deftest edge-generators-respect-their-ranges
  (doseq [seed (range 30)]
    (let [e (gen/generate gk/gen-edge-params-full 10 seed)]
      (is (<= 0.0 (:confidence e) 1.0))
      (is (contains? gk/source-types (:source-type e)))
      (is (re-matches #"node-.+" (:from e))))
    (let [c (gen/generate gk/gen-invalid-confidence 10 seed)]
      (is (or (> c 1.0) (< c 0.0))))
    (is (<= -2.0 (gen/generate gk/gen-delta 10 seed) 2.0))))

(defn- datum-kind [d]
  (cond (map? d) :map (= :db/add (first d)) :vector :else :collection))

(deftest-golden seeded-draws "test/golden/generators/kg-draws.edn"
  {:relations    (mapv draw-default-relation (range 8))
   :injected     (mapv #(draw-relation [#{:a :b :c} %]) (range 6))
   :source-types (mapv #(gen/generate gk/gen-source-type 10 %) (range 6))
   :datum-kinds  (mapv #(datum-kind (gen/generate gk/gen-tx-datum 10 %)) (range 8))
   :edge-keys    (vec (sort (keys (gen/generate gk/gen-edge-params-full 10 0))))})
