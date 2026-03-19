(ns hive-test.linearizability
  "Concurrent safety and linearizability testing macros.

   Verifies that state invariants hold under concurrent execution,
   catching race conditions that sequential tests miss. Practical
   approximation of Jepsen-style testing for unit-level verification.

   Core macros:
   - defprop-concurrent-safe:  invariant holds after concurrent ops
   - defprop-sequential-equiv: concurrent result matches sequential model

   How it works (concurrent-safe):
   1. Generate a random operation sequence via test.check
   2. Create fresh state via setup-fn
   3. Split ops across N threads, synchronize start via CyclicBarrier
   4. All threads execute their ops simultaneously
   5. Assert invariant-pred holds on the final state

   This catches bugs where:
   - Two threads interleave state mutations (lost updates)
   - Merge/assoc races produce inconsistent state
   - Cleanup races with concurrent enqueue

   Example:
     (defprop-concurrent-safe atom-stays-bounded 50
       (gen/vector (gen/tuple (gen/keyword) gen/string-alphanumeric) 1 100)
       (fn [] (atom {}))
       (fn [state [k v]] (swap! state assoc k v))
       (fn [state] (map? @state)))"
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen])
  (:import [java.util.concurrent CyclicBarrier CountDownLatch TimeUnit]))

(defn run-concurrent
  "Execute op-chunks in parallel threads, synchronized by a CyclicBarrier.
   All threads start simultaneously after reaching the barrier.
   Returns true if all completed within timeout-ms, false on timeout."
  [op-chunks apply-op-fn! state timeout-ms]
  (if (empty? op-chunks)
    true
    (let [n-threads (count op-chunks)
          barrier   (CyclicBarrier. n-threads)
          latch     (CountDownLatch. n-threads)
          errors    (atom [])]
      (doseq [chunk op-chunks]
        (future
          (try
            (.await barrier)
            (doseq [op chunk]
              (apply-op-fn! state op))
            (catch Exception e
              (swap! errors conj e))
            (finally
              (.countDown latch)))))
      (let [completed? (.await latch timeout-ms TimeUnit/MILLISECONDS)]
        (when (seq @errors)
          (throw (ex-info "Concurrent execution had errors"
                          {:errors (mapv #(.getMessage %) @errors)})))
        completed?))))

(defmacro defprop-concurrent-safe
  "Generate a defspec verifying a state invariant under concurrent execution.

   Splits a generated operation sequence across n-threads threads,
   runs them simultaneously (via CyclicBarrier), and checks that
   invariant-pred holds on the final state.

   Arguments:
   - name:           defspec name
   - num-tests:      number of test iterations
   - gen-ops:        generator for a vector of operations
   - setup-fn:       (fn [] -> state) — creates fresh mutable state per test
   - apply-op-fn!:   (fn [state op] -> ignored) — applies one op (side-effecting)
   - invariant-pred: (fn [state] -> bool) — must hold after all ops complete
   - opts:           optional map with :n-threads (default 4), :timeout-ms (default 5000)

   Example:
     (defprop-concurrent-safe buffer-merge-safe 100
       (gen/vector (gen/map gen/keyword gen/string-alphanumeric) 1 20)
       (fn [] (atom {}))
       (fn [state blocks] (swap! state merge blocks))
       (fn [state] (map? @state))
       {:n-threads 8})"
  [name num-tests gen-ops setup-fn apply-op-fn! invariant-pred
   & [{:keys [n-threads timeout-ms]
       :or {n-threads 4 timeout-ms 5000}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [ops# ~gen-ops]
                   (let [state#    (~setup-fn)
                         chunks#   (if (< (count ops#) ~n-threads)
                                     (mapv vector ops#)
                                     (vec (partition-all
                                           (max 1 (quot (count ops#) ~n-threads))
                                           ops#)))]
                     (run-concurrent chunks# ~apply-op-fn! state# ~timeout-ms)
                     (~invariant-pred state#)))))

(defmacro defprop-sequential-equiv
  "Generate a defspec verifying concurrent execution matches a sequential model.

   Runs the same operations both:
   1. Sequentially via model-fn → expected result
   2. Concurrently via run-fn   → actual result

   Compares using equiv-fn (default =). Catches bugs where concurrent
   execution loses updates that sequential execution preserves.

   Use this when operations are commutative (order-independent) and
   the expected result is deterministic regardless of execution order.

   Arguments:
   - name:      defspec name
   - num-tests: number of test iterations
   - gen-ops:   generator for a vector of operations
   - model-fn:  (fn [ops] -> expected) — sequential reference implementation
   - run-fn:    (fn [ops] -> actual) — concurrent implementation under test
   - opts:      optional map with :equiv-fn (default =)

   Example:
     (defprop-sequential-equiv merge-concurrent-matches-sequential 100
       (gen/vector (gen/map gen/keyword gen/string-alphanumeric) 1 20)
       (fn [ops] (apply merge ops))
       (fn [ops]
         (let [a (atom {})]
           (run-concurrent (partition-all 4 ops)
                           (fn [state op] (swap! state merge op))
                           a 5000)
           @a)))"
  [name num-tests gen-ops model-fn run-fn
   & [{:keys [equiv-fn]
       :or {equiv-fn =}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [ops# ~gen-ops]
                   (~equiv-fn (~model-fn ops#) (~run-fn ops#)))))
