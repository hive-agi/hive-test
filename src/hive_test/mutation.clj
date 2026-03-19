(ns hive-test.mutation
  "Mutation testing macros for regression confidence.

   Mutation testing verifies that your tests actually catch bugs by running
   them against intentionally mutated implementations. If a mutant passes
   all assertions, your tests have a blind spot for that class of bug.

   Core macros:
   - deftest-mutation-witness: verify one specific mutation is caught
   - deftest-mutations:        verify multiple mutations are all caught
   - with-mutation:             temporarily rebind a var to a mutant

   How it works:
   1. Run test assertions against the REAL implementation → must PASS
   2. Rebind the var-under-test to the MUTANT implementation
   3. Run the same assertions again (in an isolated reporter) → must FAIL
   4. If the mutant survives (all pass), report that the test has a blind spot

   Note: uses alter-var-root, which affects all threads. Mutation tests
   should not run in parallel with tests that use the same var.

   Example:
     (deftest-mutation-witness enqueue-merge-caught
       my.ns/enqueue!
       (fn [a p blocks] (swap! buffers assoc [a p] {:blocks blocks}))
       (fn []
         (my.ns/enqueue! \"a\" \"p\" {:x 1})
         (my.ns/enqueue! \"a\" \"p\" {:y 2})
         (is (= {:x 1 :y 2} (my.ns/drain! \"a\" \"p\")))))"
  (:require [clojure.test :as t]))

(defn capture-test-results
  "Execute f within an isolated test reporter. Returns
   {:pass N :fail N :error N} without polluting the outer test report.

   Captures :pass, :fail, and :error report events. All other events
   (like :begin-test-var) are silently dropped."
  [f]
  (let [results (atom {:pass 0 :fail 0 :error 0})]
    (binding [t/report
              (fn [{:keys [type]}]
                (case type
                  :pass  (swap! results update :pass inc)
                  :fail  (swap! results update :fail inc)
                  :error (swap! results update :error inc)
                  nil))]
      (f))
    @results))

(defmacro with-mutation
  "Execute body with var-sym temporarily rebound to mutant-fn.
   Restores the original binding in finally, even on exception.

   Usage:
     (with-mutation [my.ns/merge-fn (fn [& _] {})]
       (my.ns/merge-fn {:a 1} {:b 2}))  ;; => {}"
  [[var-sym mutant-fn] & body]
  `(let [original# (deref (var ~var-sym))]
     (try
       (alter-var-root (var ~var-sym) (constantly ~mutant-fn))
       (do ~@body)
       (finally
         (alter-var-root (var ~var-sym) (constantly original#))))))

(defmacro deftest-mutation-witness
  "Verify a specific mutation is caught by test assertions.

   Generates a deftest that:
   1. Runs test-fn normally → must PASS (all green)
   2. Runs test-fn with var-sym rebound to mutant-fn → must FAIL

   If the mutant survives step 2 (passes all assertions), the test
   reports a failure indicating a blind spot.

   Arguments:
   - name:      test name (symbol)
   - var-sym:   fully qualified var to mutate
   - mutant-fn: the broken implementation to test against
   - test-fn:   zero-arg function containing assertions (is, testing, etc.)"
  [name var-sym mutant-fn test-fn]
  `(t/deftest ~name
     (t/testing (str "mutation witness: " '~var-sym)
       ;; Phase 1: original must pass
       (t/testing "original implementation passes"
         (~test-fn))
       ;; Phase 2: mutant must be caught
       (t/testing "mutant is detected"
         (let [original# (deref (var ~var-sym))
               results#  (capture-test-results
                          (fn []
                            (try
                              (alter-var-root (var ~var-sym) (constantly ~mutant-fn))
                              (~test-fn)
                              (finally
                                (alter-var-root (var ~var-sym) (constantly original#))))))]
           (t/is (pos? (+ (:fail results#) (:error results#)))
                 (str "MUTATION SURVIVED: mutant of " '~var-sym
                      " passed all " (:pass results#) " assertions."
                      " Tests have a blind spot for this mutation.")))))))

(defmacro deftest-mutations
  "Verify multiple mutations of the same var are all caught.

   Like deftest-mutation-witness but tests a vector of mutations.
   Each mutation is tested independently — a surviving mutant
   reports which specific mutation wasn't caught.

   Arguments:
   - name:      test name (symbol)
   - var-sym:   fully qualified var to mutate
   - mutations: vector of [label mutant-fn] pairs
   - test-fn:   zero-arg function containing assertions

   Example:
     (deftest-mutations enqueue-all-mutations-caught
       my.ns/enqueue!
       [[\"assoc-overwrites\" (fn [a p b] (swap! buffers assoc [a p] {:blocks b}))]
        [\"drops-blocks\"     (fn [a p b] nil)]
        [\"swaps-keys\"       (fn [a p b] (swap! buffers assoc [p a] {:blocks b}))]]
       (fn []
         (my.ns/enqueue! \"a\" \"p\" {:x 1})
         (my.ns/enqueue! \"a\" \"p\" {:y 2})
         (is (= {:x 1 :y 2} (my.ns/drain! \"a\" \"p\")))))"
  [name var-sym mutations test-fn]
  `(t/deftest ~name
     (t/testing (str "mutation suite: " '~var-sym)
       ;; Original must pass
       (t/testing "original implementation passes"
         (~test-fn))
       ;; Each mutation must be caught
       (doseq [[label# mutant-fn#] ~mutations]
         (t/testing (str "mutation '" label# "' is caught")
           (let [original# (deref (var ~var-sym))
                 results#  (capture-test-results
                            (fn []
                              (try
                                (alter-var-root (var ~var-sym) (constantly mutant-fn#))
                                (~test-fn)
                                (finally
                                  (alter-var-root (var ~var-sym) (constantly original#))))))]
             (t/is (pos? (+ (:fail results#) (:error results#)))
                   (str "MUTATION SURVIVED: '" label#
                        "' passed all " (:pass results#) " assertions."))))))))
