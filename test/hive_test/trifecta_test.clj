(ns hive-test.trifecta-test
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn encode [m] (pr-str m))

(def gen-small-map
  (gen/map gen/keyword gen/small-integer {:max-elements 5}))

(deftrifecta encode-roundtrip
  hive-test.trifecta-test/encode
  {:gen           gen-small-map
   :property-type :roundtrip
   :decode-fn     edn/read-string
   :num-tests     50})

(defn tag [x] {:id x :kind :thing})

(deftrifecta tag-structural
  hive-test.trifecta-test/tag
  {:gen           gen/small-integer
   :property-type :structural
   :required-keys #{:id :kind}
   :num-tests     50})

(deftest deftrifecta-forwards-property-keys
  (testing ":decode-fn reaches the :roundtrip emitter"
    (let [form  (macroexpand-1
                  `(deftrifecta rt hive-test.trifecta-test/encode
                     {:gen gen-small-map :property-type :roundtrip
                      :decode-fn edn/read-string}))
          calls (filter #(and (seq? %) (seq %)) (tree-seq coll? seq form))]
      (is (some #{`edn/read-string} (flatten form)))
      (is (not-any? #(nil? (first %)) calls))))
  (testing ":required-keys reaches the :structural emitter"
    (let [form (macroexpand-1
                 `(deftrifecta st hive-test.trifecta-test/tag
                    {:gen gen/small-integer :property-type :structural
                     :required-keys #{:id :kind}}))]
      (is (some #{#{:id :kind}} (tree-seq coll? seq form))))))

;; --- Reference-oracle and metamorphic property facets ---

(defn distinct-sorted
  "Subject: sorted distinct ints, via a sorted set."
  [xs]
  (vec (apply sorted-set xs)))

(defn- distinct-sorted-oracle
  "Independent reference: distinct then sort."
  [xs]
  (vec (sort (distinct xs))))

(deftrifecta distinct-sorted-oracle-facet
  hive-test.trifecta-test/distinct-sorted
  {:golden-path   "test/golden/trifecta/distinct-sorted.edn"
   :cases         {:empty [] :dupes [3 1 3 2 1] :sorted [1 2 3]}
   :gen           (gen/vector gen/small-integer)
   :property-type :oracle
   :oracle        distinct-sorted-oracle
   :num-tests     100
   :mutations     [["unsorted" (fn [xs] (vec (distinct xs)))]
                   ["keeps-dupes" (fn [xs] (vec (sort xs)))]]})

(defn- reverse-input
  "Metamorphic transform: input order must not matter."
  [xs]
  (vec (reverse xs)))

(deftrifecta distinct-sorted-invariance
  hive-test.trifecta-test/distinct-sorted
  {:gen           (gen/vector gen/small-integer)
   :property-type :metamorphic
   :transform     reverse-input
   :num-tests     100})

(defn- add-one-more
  "Metamorphic transform: appending an element can only grow the set."
  [xs]
  (conj xs 0))

(deftrifecta distinct-sorted-subset
  hive-test.trifecta-test/distinct-sorted
  {:gen           (gen/vector gen/small-integer)
   :property-type :metamorphic
   :transform     add-one-more
   :relation      (fn [out out'] (set/subset? (set out) (set out')))
   :num-tests     100})

(deftest metamorphic-and-oracle-facets-reject-bad-subjects
  (testing ":oracle disagrees with a broken subject"
    (let [prop (prop/for-all [xs (gen/vector gen/small-integer)]
                 (= (vec (distinct xs)) (distinct-sorted-oracle xs)))]
      (is (false? (:pass? (tc/quick-check 100 prop))))))
  (testing ":metamorphic forwards :transform and :relation"
    (let [form (macroexpand-1
                 `(deftrifecta mm hive-test.trifecta-test/distinct-sorted
                    {:gen gen/small-integer :property-type :metamorphic
                     :transform reverse-input :relation set/subset?}))
          leaves (set (flatten form))]
      (is (contains? leaves `reverse-input))
      (is (contains? leaves `set/subset?)))))
