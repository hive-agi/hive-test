(ns hive-test.generators.bounded-atom-test
  "Facets for hive-test.generators.bounded-atom: op sequences honour their
   length bounds, overflow sequences exceed capacity with distinct keys
   (properties over the contracts), and a golden of seeded op mixes."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.generators.bounded-atom :as gba]
            [hive-test.golden :refer [deftest-golden]]))

(def ^:private op-kinds #{:bput :bget :bclear :sweep :bcount})

(defn- draw-ops [[lo span seed]] (gen/generate (gba/gen-op-sequence lo (+ lo span)) 30 seed))

(hst/deftrifecta-from-schema gen-op-sequence-stays-inside-its-bounds
  draw-ops
  {:in  [:tuple [:int {:min 0 :max 20}] [:int {:min 0 :max 20}] :int]
   :out [:vector [:cat [:enum :bput :bget :bclear :sweep :bcount] [:* :any]]]
   :rel (fn [[lo span _] ops] (and (<= lo (count ops) (+ lo span))
                                   (every? (comp op-kinds first) ops)))
   :mutation true})

(defn- draw-overflow [[cap over seed]] (gen/generate (gba/gen-overflow-bput-sequence cap over) 10 seed))

(hst/deftrifecta-from-schema overflow-sequence-exceeds-capacity-with-distinct-keys
  draw-overflow
  {:in  [:tuple [:int {:min 0 :max 30}] [:int {:min 0 :max 30}] :int]
   :out [:vector [:tuple [:= :bput] :keyword :any]]
   :rel (fn [[cap over _] ops] (and (= (+ cap over) (count ops))
                                    (apply distinct? (keyword "sentinel") (map second ops))))
   :mutation true})

(deftest option-generators-stay-in-range
  (doseq [seed (range 30)]
    (let [o (gen/generate gba/gen-bounded-atom-opts-capacity 10 seed)]
      (is (<= 1 (:max-entries o) 100))
      (is (contains? #{:lru :fifo} (:eviction-policy o)))
      (is (or (nil? (:ttl-ms o)) (<= 100 (:ttl-ms o) 10000))))
    (let [{:keys [keys ops]} (gen/generate gba/gen-keyed-bput-sequence 10 seed)]
      (is (= keys (mapv second ops))))))

(deftest-golden seeded-draws "test/golden/generators/bounded-atom-draws.edn"
  {:op-kinds   (mapv #(mapv first (draw-ops [2 4 %])) (range 4))
   :overflow   (mapv second (draw-overflow [2 1 0]))
   :policies   (mapv #(gen/generate gba/gen-eviction-policy 10 %) (range 6))
   :opts       (mapv #(into (sorted-map) (gen/generate gba/gen-bounded-atom-opts 10 %)) (range 3))
   :keyed-size (mapv #(count (:keys (gen/generate gba/gen-keyed-bput-sequence 10 %))) (range 4))})
