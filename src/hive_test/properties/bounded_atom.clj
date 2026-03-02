(ns hive-test.properties.bounded-atom
  "Property definitions for bounded-atom invariants.
   These are reusable property helpers that can be instantiated in any
   project's test suite that uses bounded-atom (e.g. hive-dsl tests).

   Provides:
   - apply-op: interpreter for generated op vectors against a bounded-atom
   - Property test helpers for capacity, idempotency, totality, TTL,
     LRU ordering, FIFO ordering, and put/get roundtrip"
  (:require [clojure.test.check.generators :as gen]
            [hive-test.generators.bounded-atom :as gen-ba]))

;; =============================================================================
;; Operation interpreter — executes [:op-keyword & args] against a bounded-atom
;; =============================================================================

(defn apply-op
  "Execute a single generated operation against a bounded-atom.
   Operations are vectors: [:bput k v], [:bget k], [:bclear], [:sweep], [:bcount].
   Requires bounded-atom API fns passed as opts map to avoid hard dep on hive-dsl.

   api-fns must contain:
     :bput!  — (fn [batom k v])
     :bget   — (fn [batom k])
     :bclear! — (fn [batom])
     :sweep! — (fn [batom name-kw])
     :bcount — (fn [batom])"
  [api-fns batom [op & args]]
  (case op
    :bput   (apply (:bput! api-fns) batom args)
    :bget   (apply (:bget api-fns) batom args)
    :bclear ((:bclear! api-fns) batom)
    :sweep  ((:sweep! api-fns) batom :prop-test)
    :bcount ((:bcount api-fns) batom)
    nil)
  batom)

(defn run-ops
  "Execute a sequence of operations against a bounded-atom.
   Returns the bounded-atom (for chaining/inspection)."
  [api-fns batom ops]
  (reduce (partial apply-op api-fns) batom ops))

;; =============================================================================
;; Generator combinators for property tests
;; =============================================================================

(defn gen-batom-with-ops
  "Generator producing [opts ops] pairs for property tests.
   Uses capacity-safe policies (excludes :ttl) by default."
  ([]
   (gen-batom-with-ops gen-ba/gen-bounded-atom-opts-capacity
                       gen-ba/gen-op-sequence-small))
  ([gen-opts gen-ops]
   (gen/tuple gen-opts gen-ops)))

(defn gen-batom-with-ops-all-policies
  "Generator producing [opts ops] pairs including :ttl policy."
  []
  (gen/tuple gen-ba/gen-bounded-atom-opts
             gen-ba/gen-op-sequence-small))
