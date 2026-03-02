(ns hive-test.generators.bounded-atom
  "Generators for bounded-atom operations and configuration.
   Used by property tests to prove bounded-atom invariants
   hold under random operation sequences."
  (:require [clojure.test.check.generators :as gen]))

;; =============================================================================
;; Configuration generators
;; =============================================================================

(def eviction-policies
  "Valid eviction policies for bounded atoms."
  [:lru :fifo :ttl])

(def gen-eviction-policy
  "Generator for eviction policy keywords."
  (gen/elements eviction-policies))

(def gen-capacity-eviction-policy
  "Generator for policies that perform capacity-based eviction (excludes :ttl)."
  (gen/elements [:lru :fifo]))

(def gen-max-entries
  "Generator for max-entries values (1 to 100)."
  (gen/choose 1 100))

(def gen-ttl-ms
  "Generator for TTL values: nil (no TTL) or a positive integer (100-10000ms)."
  (gen/one-of [(gen/return nil)
               (gen/choose 100 10000)]))

(def gen-bounded-atom-opts
  "Generator for valid bounded-atom constructor options."
  (gen/let [max-entries gen-max-entries
            ttl-ms gen-ttl-ms
            policy gen-eviction-policy]
    {:max-entries max-entries
     :ttl-ms ttl-ms
     :eviction-policy policy}))

(def gen-bounded-atom-opts-capacity
  "Generator for bounded-atom opts with capacity-evicting policies (no :ttl).
   Use this when testing capacity invariants — :ttl policy does not
   capacity-evict so the count invariant doesn't hold for it."
  (gen/let [max-entries gen-max-entries
            ttl-ms gen-ttl-ms
            policy gen-capacity-eviction-policy]
    {:max-entries max-entries
     :ttl-ms ttl-ms
     :eviction-policy policy}))

;; =============================================================================
;; Key/Value generators
;; =============================================================================

(def gen-entry-key
  "Generator for bounded-atom entry keys (keywords)."
  (gen/fmap #(keyword (str "k" %)) (gen/choose 0 999)))

(def gen-entry-value
  "Generator for bounded-atom entry values (any printable data)."
  gen/any-printable)

;; =============================================================================
;; Operation generators — [:op-keyword & args]
;; =============================================================================

(def gen-bput-op
  "Generator for a bput! operation: [:bput key value]."
  (gen/let [k gen-entry-key
            v gen-entry-value]
    [:bput k v]))

(def gen-bget-op
  "Generator for a bget operation: [:bget key]."
  (gen/fmap (fn [k] [:bget k]) gen-entry-key))

(def gen-clear-op
  "Generator for a bclear! operation: [:bclear]."
  (gen/return [:bclear]))

(def gen-sweep-op
  "Generator for a sweep! operation: [:sweep]."
  (gen/return [:sweep]))

(def gen-bcount-op
  "Generator for a bcount read operation: [:bcount]."
  (gen/return [:bcount]))

(def gen-bounded-atom-op
  "Generator for any bounded-atom operation."
  (gen/frequency [[60 gen-bput-op]    ;; mostly puts — stress capacity
                  [20 gen-bget-op]    ;; reads to exercise LRU touches
                  [5  gen-clear-op]   ;; occasional clear
                  [10 gen-sweep-op]   ;; occasional sweep
                  [5  gen-bcount-op]]))

(defn gen-op-sequence
  "Generator for a sequence of N random operations.
   Default: 1-200 operations."
  ([] (gen-op-sequence 1 200))
  ([min-ops max-ops]
   (gen/vector gen-bounded-atom-op min-ops max-ops)))

(def gen-op-sequence-small
  "Generator for a small sequence of operations (1-50).
   Faster property tests."
  (gen-op-sequence 1 50))

(def gen-op-sequence-large
  "Generator for a large sequence of operations (100-500).
   Stress-test capacity invariants."
  (gen-op-sequence 100 500))

;; =============================================================================
;; Specialized generators for specific invariant tests
;; =============================================================================

(defn gen-overflow-bput-sequence
  "Generator for a sequence of bput! operations guaranteed to exceed capacity.
   Generates (max-entries + overflow) distinct bput! operations."
  [max-entries overflow]
  (gen/let [ops (gen/vector gen-bput-op (+ max-entries overflow))]
    ;; Ensure distinct keys by rewriting keys sequentially
    (vec (map-indexed (fn [i [op _ v]] [op (keyword (str "overflow-k" i)) v])
                      ops))))

(def gen-keyed-bput-sequence
  "Generator for bput! operations with known keys for tracking.
   Returns {:keys keys :ops ops} where keys are the exact keys used."
  (gen/let [n (gen/choose 1 50)
            values (gen/vector gen-entry-value n)]
    (let [keys (mapv #(keyword (str "tracked-k" %)) (range n))
          ops (mapv (fn [k v] [:bput k v]) keys values)]
      {:keys keys :ops ops})))
