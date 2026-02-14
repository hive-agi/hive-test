(ns hive-test.generators-test
  "Tests that generators produce structurally valid data."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.generators.core :as gen-core]
            [hive-test.generators.result :as gen-result]
            [hive-test.generators.memory :as gen-mem]
            [hive-test.generators.kg :as gen-kg]))

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
