(ns hive-test.mutation-test
  "Tests for mutation testing macros.
   Uses a simple accumulator to demonstrate that mutations are caught."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.mutation :as mut]))

;; --- Test subject: a simple merge-accumulator ---

(def ^:private acc (atom {}))

(defn accumulate!
  "Merge kv-map into the accumulator. Real implementation uses merge."
  [kv-map]
  (swap! acc merge kv-map))

(defn drain-acc!
  "Drain and return accumulator contents."
  []
  (let [v @acc]
    (reset! acc {})
    v))

;; --- Mutation witness: assoc-overwrite mutant ---

(mut/deftest-mutation-witness accumulate-merge-caught
  hive-test.mutation-test/accumulate!
  ;; Mutant: assoc overwrites instead of merging (simulates the piggyback bug)
  (fn [kv-map] (reset! acc kv-map))
  (fn []
    (reset! acc {})
    (accumulate! {:a 1 :b 2})
    (accumulate! {:c 3})
    (let [result (drain-acc!)]
      (is (= 3 (count result)) "all 3 keys present")
      (is (= 1 (:a result)) "key :a from first call")
      (is (= 3 (:c result)) "key :c from second call"))))

;; --- Multiple mutations suite ---

(mut/deftest-mutations accumulate-all-mutations-caught
  hive-test.mutation-test/accumulate!
  [["assoc-overwrites"  (fn [kv-map] (reset! acc kv-map))]
   ["drops-everything"  (fn [_] nil)]
   ["reverses-keys"     (fn [kv-map] (swap! acc merge (zipmap (vals kv-map) (keys kv-map))))]]
  (fn []
    (reset! acc {})
    (accumulate! {:x 1})
    (accumulate! {:y 2})
    (let [result (drain-acc!)]
      (is (= {:x 1 :y 2} result)))))

;; --- capture-test-results utility ---

(deftest capture-test-results-counts-correctly
  (testing "captures pass/fail/error counts"
    (let [results (mut/capture-test-results
                   (fn []
                     (is true)
                     (is true)
                     (is (= 1 2))))]
      (is (= 2 (:pass results)))
      (is (= 1 (:fail results)))
      (is (= 0 (:error results))))))

;; --- with-mutation restores original ---

(deftest with-mutation-restores-binding
  (testing "original binding is restored after with-mutation"
    (let [original accumulate!]
      (mut/with-mutation [hive-test.mutation-test/accumulate!
                          (fn [_] :mutant)]
        (is (= :mutant (accumulate! {}))))
      (is (= original accumulate!)
          "original fn restored after with-mutation"))))
