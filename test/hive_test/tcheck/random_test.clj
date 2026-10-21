(ns hive-test.tcheck.random-test
  "Facets for the splittable LCG: properties over its contracts, and a golden
   of the first draws from fixed seeds (the stream IS the contract)."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.tcheck.random :as rng]))

(hst/deftrifecta-from-schema make-rng-folds-the-seed-into-range
  rng/make-rng
  {:in  :int
   :out rng/Rng
   :rel (fn [n out] (= (:state out) (mod (Math/abs (long n)) 2147483648)))
   :mutation true})

(hst/deftrifecta-from-schema next-rng-is-a-function-of-the-state
  rng/next-rng
  {:in  rng/Rng
   :out rng/Rng
   :rel (fn [r out] (= out (rng/next-rng {:state (:state r)})))
   :mutation true})

(defn- bounded-draw [[r lo span]] (rng/bounded r lo (+ lo span)))

(hst/deftrifecta-from-schema bounded-stays-inside-the-interval
  bounded-draw
  {:in  [:tuple rng/Rng :int [:int {:min 0 :max 1000}]]
   :out [:tuple :int rng/Rng]
   :rel (fn [[r lo span] [n r']] (and (<= lo n (+ lo span)) (= r' (rng/next-rng r))))
   :mutation true})

(deftest split-yields-two-distinct-streams
  (let [[a b] (rng/split (rng/make-rng 3))]
    (is (not= (rng/value a) (rng/value b)))))

(deftest-golden first-draws "test/golden/tcheck/random-first-draws.edn"
  (into (sorted-map)
        (for [seed [0 1 42 -7]]
          [seed (->> (rng/make-rng seed) (iterate rng/next-rng) (take 4) (mapv rng/value))])))
