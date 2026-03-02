(ns hive-test.resource-assertions-test
  "Tests for resource assertion macros — yes, tests for the test utilities.

   These tests verify:
   - assert-bounded-growth catches overflow and passes when bounded
   - assert-thread-count-stable catches thread leaks and passes clean code
   - with-resource-accounting catches mixed leaks and passes clean code
   - assert-lifecycle-roundtrip works with ManagedExecutor
   - assert-stop-idempotent works
   - resource-cleanup-fixture integrates with use-fixtures"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.resource-assertions :as ra]
            [hive-dsl.bounded-atom :as ba]
            [hive-dsl.lifecycle :as lc]))

;; =============================================================================
;; Helper: run a test body and capture pass/fail without affecting outer test
;; =============================================================================

(defn- capture-test-results
  "Run body-fn in a clojure.test context that captures results.
   Returns {:pass N :fail N :error N}."
  [body-fn]
  (let [results (atom {:pass 0 :fail 0 :error 0})]
    (binding [clojure.test/report
              (fn [m]
                (when (#{:pass :fail :error} (:type m))
                  (swap! results update (:type m) inc)))]
      (try
        (body-fn)
        (catch Throwable _)))
    @results))

;; =============================================================================
;; Bounded atom assertions
;; =============================================================================

(deftest assert-bounded-growth-catches-overflow
  (testing "assert-bounded-growth fails when bounded atom exceeds capacity"
    (let [store (ba/bounded-atom {:max-entries 5
                                  :ttl-ms nil
                                  :eviction-policy :lru})]
      (ba/register-sweepable! store :test-overflow)
      (try
        ;; Directly shove entries into the underlying atom to bypass enforcement.
        ;; This simulates a bug where entries leak past the capacity check.
        (reset! (:atom store)
                (into {} (map (fn [i]
                                [(keyword (str "k" i))
                                 {:data i
                                  :created-at (System/currentTimeMillis)
                                  :last-accessed (System/currentTimeMillis)}])
                              (range 10))))
        (let [results (capture-test-results
                        (fn [] (ra/assert-bounded-growth (+ 1 1))))]
          (is (pos? (:fail results))
              "Should have at least one failure for overflow"))
        (finally
          (ba/unregister-sweepable! :test-overflow))))))

(deftest assert-bounded-growth-passes-when-bounded
  (testing "assert-bounded-growth passes when atoms stay within capacity"
    (let [store (ba/bounded-atom {:max-entries 100
                                  :ttl-ms nil
                                  :eviction-policy :lru})]
      (ba/register-sweepable! store :test-bounded)
      (try
        (let [results (capture-test-results
                        (fn []
                          (ra/assert-bounded-growth
                            (dotimes [i 50]
                              (ba/bput! store (keyword (str "k" i)) {:val i})))))]
          (is (zero? (:fail results))
              "Should have no failures when within capacity"))
        (finally
          (ba/unregister-sweepable! :test-bounded))))))

(deftest assert-atom-count-stable-test
  (testing "detects growth beyond threshold"
    (let [store   (ba/bounded-atom {:max-entries 100
                                    :ttl-ms nil
                                    :eviction-policy :lru})
          results (capture-test-results
                    (fn []
                      (ra/assert-atom-count-stable store 2
                        (fn []
                          (dotimes [i 10]
                            (ba/bput! store (keyword (str "k" i)) {:val i}))))))]
      (is (pos? (:fail results))
          "Should fail when growth exceeds threshold")))

  (testing "passes when growth within threshold"
    (let [store   (ba/bounded-atom {:max-entries 100
                                    :ttl-ms nil
                                    :eviction-policy :lru})
          results (capture-test-results
                    (fn []
                      (ra/assert-atom-count-stable store 5
                        (fn []
                          (dotimes [i 3]
                            (ba/bput! store (keyword (str "k" i)) {:val i}))))))]
      (is (zero? (:fail results))
          "Should pass when growth is within threshold"))))

;; =============================================================================
;; Thread count assertions
;; =============================================================================

(deftest assert-thread-count-stable-catches-leak
  (testing "assert-thread-count-stable fails when threads leak"
    (let [leaked-exec (atom nil)
          results (capture-test-results
                    (fn []
                      (ra/assert-thread-count-stable 0
                        ;; Start an executor and intentionally do NOT stop it
                        (let [exec (lc/start! (lc/->managed-executor 4))]
                          (reset! leaked-exec exec)
                          exec))))]
      ;; Clean up the leaked executor
      (when @leaked-exec
        (lc/stop! @leaked-exec))
      (is (pos? (:fail results))
          "Should fail when threads are leaked"))))

(deftest assert-thread-count-stable-passes-clean
  (testing "assert-thread-count-stable passes for properly cleaned up resources"
    (let [results (capture-test-results
                    (fn []
                      (ra/assert-thread-count-stable
                        (let [exec (lc/start! (lc/->managed-executor 2))]
                          (try
                            (lc/submit! exec #(+ 1 2))
                            (finally
                              (lc/stop! exec)))))))]
      (is (zero? (:fail results))
          "Should pass when threads are properly cleaned up"))))

;; =============================================================================
;; with-resource-accounting
;; =============================================================================

(deftest with-resource-accounting-catches-mixed-leaks
  (testing "with-resource-accounting fails when threads and registry leak"
    (let [leaked-exec (atom nil)
          results (capture-test-results
                    (fn []
                      (ra/with-resource-accounting {:thread-tolerance 0
                                                    :atom-tolerance 0
                                                    :settle-ms 150}
                        ;; Leak threads via unstopped executor and leak registry entry
                        (let [exec (lc/start! (lc/->managed-executor 2))
                              store (ba/bounded-atom {:max-entries 10
                                                      :ttl-ms nil
                                                      :eviction-policy :lru})]
                          (ba/register-sweepable! store :leak-test)
                          (reset! leaked-exec exec)
                          ;; Intentionally do NOT stop executor or unregister
                          exec))))]
      ;; Clean up
      (when @leaked-exec
        (lc/stop! @leaked-exec))
      (ba/unregister-sweepable! :leak-test)
      (is (pos? (:fail results))
          "Should fail when resources leak"))))

(deftest with-resource-accounting-passes-clean
  (testing "with-resource-accounting passes for clean resource usage"
    (let [results (capture-test-results
                    (fn []
                      (ra/with-resource-accounting {:thread-tolerance 2
                                                    :atom-tolerance 0
                                                    :settle-ms 150}
                        (let [exec (lc/start! (lc/->managed-executor 2))]
                          (try
                            (lc/submit! exec #(+ 1 2))
                            (finally
                              (lc/stop! exec)))))))]
      (is (zero? (:fail results))
          "Should pass when all resources are properly cleaned up"))))

;; =============================================================================
;; Lifecycle compliance assertions
;; =============================================================================

(deftest assert-lifecycle-roundtrip-with-executor
  (testing "assert-lifecycle-roundtrip works with ManagedExecutor"
    (let [exec (lc/->managed-executor 2)]
      (ra/assert-lifecycle-roundtrip exec))))

(deftest assert-lifecycle-roundtrip-with-channel
  (testing "assert-lifecycle-roundtrip works with ManagedChannel"
    (let [ch (lc/->managed-channel 10)]
      (ra/assert-lifecycle-roundtrip ch))))

(deftest assert-stop-idempotent-with-executor
  (testing "assert-stop-idempotent works with ManagedExecutor"
    (let [exec (lc/->managed-executor 2)]
      (ra/assert-stop-idempotent exec))))

(deftest assert-stop-idempotent-with-channel
  (testing "assert-stop-idempotent works with ManagedChannel"
    (let [ch (lc/->managed-channel 10)]
      (ra/assert-stop-idempotent ch))))

;; =============================================================================
;; resource-cleanup-fixture integration
;; =============================================================================

(deftest resource-cleanup-fixture-integration
  (testing "fixture passes for clean test body"
    (let [results (capture-test-results
                    (fn []
                      (ra/resource-cleanup-fixture
                        (fn []
                          ;; Clean body — no resource leaks
                          (let [exec (lc/start! (lc/->managed-executor 1))]
                            (try
                              (lc/submit! exec #(+ 1 1))
                              (finally
                                (lc/stop! exec))))))))]
      (is (zero? (:fail results))
          "Fixture should pass for clean test body")))

  (testing "fixture catches leaked resources"
    (let [results (capture-test-results
                    (fn []
                      (binding [ra/*fixture-opts* {:thread-tolerance 0
                                                   :atom-tolerance 0
                                                   :settle-ms 150}]
                        (ra/resource-cleanup-fixture
                          (fn []
                            ;; Leak: register a bounded atom and don't clean up
                            (let [store (ba/bounded-atom {:max-entries 5
                                                          :ttl-ms nil
                                                          :eviction-policy :lru})]
                              (ba/register-sweepable! store :fixture-leak-test)))))))]
      ;; The fixture's finally block clears the registry, so we don't need manual cleanup
      (is (pos? (:fail results))
          "Fixture should fail when registry grows"))))

;; =============================================================================
;; Thread count utility functions
;; =============================================================================

(deftest thread-count-returns-positive
  (testing "thread-count returns a positive integer"
    (let [tc (ra/thread-count)]
      (is (pos-int? tc) "Thread count should be a positive integer"))))

(deftest thread-names-returns-set
  (testing "thread-names returns a non-empty set of strings"
    (let [names (ra/thread-names)]
      (is (set? names))
      (is (seq names) "Should have at least one thread")
      (is (every? string? names)))))

(deftest thread-diff-computes-correctly
  (testing "thread-diff computes added and removed threads"
    (let [before #{"main" "gc" "finalizer"}
          after  #{"main" "gc" "pool-1"}
          diff   (ra/thread-diff before after)]
      (is (= #{"pool-1"} (:added diff)))
      (is (= #{"finalizer"} (:removed diff))))))
