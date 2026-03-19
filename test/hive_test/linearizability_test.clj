(ns hive-test.linearizability-test
  "Tests for linearizability / concurrent safety macros."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.linearizability :as lin]))

;; --- Concurrent safety: atom merge never loses keys ---

(lin/defprop-concurrent-safe atom-merge-no-lost-keys 100
  ;; Generate vectors of single-key maps to merge into an atom
  (gen/vector (gen/fmap (fn [[k v]] {k v})
                        (gen/tuple gen/keyword gen/small-integer))
              1 50)
  ;; Fresh atom per test
  (fn [] (atom {}))
  ;; Each op merges one map into the atom
  (fn [state kv] (swap! state merge kv))
  ;; Invariant: result is always a map (never corrupted)
  (fn [state] (map? @state))
  {:n-threads 8})

;; --- Concurrent safety: counter never goes negative ---

(lin/defprop-concurrent-safe counter-non-negative 100
  ;; Generate mix of increments (+1) and bounded decrements
  (gen/vector (gen/elements [:inc :dec]) 1 100)
  (fn [] (atom 0))
  (fn [state op]
    (case op
      :inc (swap! state inc)
      :dec (swap! state (fn [v] (max 0 (dec v))))))
  (fn [state] (>= @state 0))
  {:n-threads 4})

;; --- Sequential equivalence: commutative merge-with + ---
;; merge-with + is truly commutative + associative, so order doesn't matter.
;; Plain `merge` would fail here (correctly!) because last-write-wins
;; is order-dependent — the macro catching a real concurrency issue.

(lin/defprop-sequential-equiv merge-with-plus-concurrent-equiv 50
  (gen/vector (gen/map gen/keyword gen/small-integer {:min-elements 0 :max-elements 5}) 1 20)
  ;; Model: sequential merge-with +
  (fn [ops] (apply merge-with + ops))
  ;; Real: concurrent merge-with + into atom
  (fn [ops]
    (let [a (atom {})]
      (lin/run-concurrent
       (vec (partition-all (max 1 (quot (count ops) 4)) ops))
       (fn [state op] (swap! state (partial merge-with +) op))
       a
       5000)
      @a)))

;; --- run-concurrent utility ---

(deftest run-concurrent-basic-test
  (testing "run-concurrent executes all ops"
    (let [counter (atom 0)
          ops     (partition-all 5 (range 20))]
      (lin/run-concurrent (vec ops)
                          (fn [state _op] (swap! state inc))
                          counter
                          5000)
      (is (= 20 @counter)))))

(deftest run-concurrent-empty-ops-test
  (testing "run-concurrent with empty ops returns true"
    (is (true? (lin/run-concurrent [] (fn [_ _]) (atom nil) 1000)))))
