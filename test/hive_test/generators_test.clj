(ns hive-test.generators-test
  "Tests that generators produce structurally valid data."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.generators.core :as gen-core]
            [hive-test.generators.result :as gen-result]
            [hive-test.generators.memory :as gen-mem]
            [hive-test.generators.kg :as gen-kg]
            [hive-test.generators.bounded-atom :as gen-ba]))

(defn sample [g n] (gen/sample g n))

;; --- Core generators ---

(deftest gen-non-blank-string-test
  (testing "generates non-blank strings"
    (doseq [s (sample gen-core/gen-non-blank-string 20)]
      (is (string? s))
      (is (not (clojure.string/blank? s))))))

(deftest gen-keyword-ns-test
  (testing "generates namespaced keywords"
    (doseq [kw (sample gen-core/gen-keyword-ns 20)]
      (is (keyword? kw))
      (is (some? (namespace kw))))))

(deftest gen-uuid-str-test
  (testing "generates valid UUID strings"
    (doseq [u (sample gen-core/gen-uuid-str 10)]
      (is (string? u))
      (is (uuid? (parse-uuid u))))))

(deftest gen-timestamp-test
  (testing "generates ISO-8601 timestamp strings"
    (doseq [ts (sample gen-core/gen-timestamp 10)]
      (is (string? ts))
      (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:00:00Z" ts)))))

;; --- Result generators ---

(deftest gen-err-category-test
  (testing "generates known error categories"
    (doseq [cat (sample gen-result/gen-err-category 20)]
      (is (keyword? cat))
      (is (some? (namespace cat))))))

(deftest gen-ok-value-test
  (testing "gen-ok-value produces printable values"
    (let [vals (sample gen-result/gen-ok-value 20)]
      (is (= 20 (count vals))))))

;; --- Memory generators ---

(deftest gen-memory-type-test
  (testing "generates valid memory types"
    (doseq [t (sample gen-mem/gen-memory-type 20)]
      (is (contains? (set gen-mem/memory-types) t)))))

(deftest gen-duration-test
  (testing "generates valid duration categories"
    (doseq [d (sample gen-mem/gen-duration 20)]
      (is (contains? (set gen-mem/duration-categories) d)))))

(deftest gen-memory-entry-test
  (testing "generates structurally valid memory entries"
    (doseq [entry (sample gen-mem/gen-memory-entry 10)]
      (is (map? entry))
      (is (contains? (set gen-mem/memory-types) (:type entry)))
      (is (string? (:content entry)))
      (is (not (clojure.string/blank? (:content entry))))
      (is (vector? (:tags entry)))
      (is (contains? (set gen-mem/duration-categories) (:duration entry))))))

;; --- KG generators ---

(deftest gen-relation-test
  (testing "generates valid relation types"
    (doseq [r (sample gen-kg/gen-relation 20)]
      (is (contains? gen-kg/relation-types r)))))

(deftest gen-confidence-test
  (testing "generates confidence in [0.0, 1.0]"
    (doseq [c (sample gen-kg/gen-confidence 50)]
      (is (<= 0.0 c 1.0)))))

(deftest gen-invalid-confidence-test
  (testing "generates out-of-bounds confidence"
    (doseq [c (sample gen-kg/gen-invalid-confidence 50)]
      (is (or (> c 1.0) (< c 0.0))))))

(deftest gen-edge-params-test
  (testing "generates structurally valid edge params"
    (doseq [edge (sample gen-kg/gen-edge-params 10)]
      (is (map? edge))
      (is (string? (:from edge)))
      (is (string? (:to edge)))
      (is (contains? gen-kg/relation-types (:relation edge)))
      (is (<= 0.0 (:confidence edge) 1.0)))))

(deftest gen-node-id-test
  (testing "generates node IDs with node- prefix"
    (doseq [id (sample gen-kg/gen-node-id 10)]
      (is (string? id))
      (is (clojure.string/starts-with? id "node-")))))

;; --- Bounded-atom generators ---

(deftest gen-eviction-policy-test
  (testing "generates valid eviction policies"
    (doseq [p (sample gen-ba/gen-eviction-policy 20)]
      (is (contains? (set gen-ba/eviction-policies) p)))))

(deftest gen-capacity-eviction-policy-test
  (testing "generates only capacity-evicting policies (no :ttl)"
    (doseq [p (sample gen-ba/gen-capacity-eviction-policy 20)]
      (is (contains? #{:lru :fifo} p)))))

(deftest gen-max-entries-test
  (testing "generates positive max-entries in [1, 100]"
    (doseq [n (sample gen-ba/gen-max-entries 50)]
      (is (pos-int? n))
      (is (<= 1 n 100)))))

(deftest gen-ttl-ms-test
  (testing "generates nil or positive TTL"
    (doseq [t (sample gen-ba/gen-ttl-ms 50)]
      (is (or (nil? t) (and (pos-int? t) (<= 100 t 10000)))))))

(deftest gen-bounded-atom-opts-test
  (testing "generates structurally valid bounded-atom options"
    (doseq [opts (sample gen-ba/gen-bounded-atom-opts 20)]
      (is (map? opts))
      (is (pos-int? (:max-entries opts)))
      (is (or (nil? (:ttl-ms opts)) (pos-int? (:ttl-ms opts))))
      (is (contains? (set gen-ba/eviction-policies) (:eviction-policy opts))))))

(deftest gen-bounded-atom-opts-capacity-test
  (testing "generates opts with capacity-evicting policies only"
    (doseq [opts (sample gen-ba/gen-bounded-atom-opts-capacity 20)]
      (is (contains? #{:lru :fifo} (:eviction-policy opts))))))

(deftest gen-entry-key-test
  (testing "generates keyword entry keys"
    (doseq [k (sample gen-ba/gen-entry-key 20)]
      (is (keyword? k))
      (is (clojure.string/starts-with? (name k) "k")))))

(deftest gen-bput-op-test
  (testing "generates valid bput! operations"
    (doseq [op (sample gen-ba/gen-bput-op 10)]
      (is (vector? op))
      (is (= :bput (first op)))
      (is (= 3 (count op)))
      (is (keyword? (second op))))))

(deftest gen-bget-op-test
  (testing "generates valid bget operations"
    (doseq [op (sample gen-ba/gen-bget-op 10)]
      (is (vector? op))
      (is (= :bget (first op)))
      (is (= 2 (count op)))
      (is (keyword? (second op))))))

(deftest gen-clear-op-test
  (testing "generates bclear operation"
    (doseq [op (sample gen-ba/gen-clear-op 5)]
      (is (= [:bclear] op)))))

(deftest gen-sweep-op-test
  (testing "generates sweep operation"
    (doseq [op (sample gen-ba/gen-sweep-op 5)]
      (is (= [:sweep] op)))))

(deftest gen-bounded-atom-op-test
  (testing "generates any valid bounded-atom operation"
    (let [ops (sample gen-ba/gen-bounded-atom-op 100)
          op-types (set (map first ops))]
      ;; Should produce a mix of operation types
      (is (every? #{:bput :bget :bclear :sweep :bcount} op-types))
      ;; With 100 samples and weighted frequency, should see at least bput and bget
      (is (contains? op-types :bput))
      (is (contains? op-types :bget)))))

(deftest gen-op-sequence-test
  (testing "generates sequences of operations"
    (doseq [ops (sample gen-ba/gen-op-sequence-small 5)]
      (is (vector? ops))
      (is (pos? (count ops)))
      (is (<= (count ops) 50))
      (is (every? vector? ops)))))

(deftest gen-keyed-bput-sequence-test
  (testing "generates tracked bput sequences with known keys"
    (doseq [result (sample gen-ba/gen-keyed-bput-sequence 5)]
      (is (map? result))
      (is (vector? (:keys result)))
      (is (vector? (:ops result)))
      (is (= (count (:keys result)) (count (:ops result))))
      (is (every? #(= :bput (first %)) (:ops result))))))
