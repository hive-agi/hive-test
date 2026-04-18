(ns hive-test.resource-assertions
  "Resource-leak detection macros and assertions for clojure.test.

   Provides composable assertions that catch:
   - Bounded atom growth beyond capacity
   - Thread leaks (threads not cleaned up after test body)
   - Resource accounting (thread + atom + registry combined)
   - Lifecycle protocol compliance (roundtrip, idempotent stop)

   Plus a test fixture for automatic resource-leak detection per test.

   All assertions integrate with clojure.test (is, testing, deftest).
   Thread-count checks include configurable tolerance for JVM
   background thread churn.

   Usage:
     (deftest my-resource-test
       (with-resource-accounting {}
         (let [exec (start! (->managed-executor 2))]
           (try (submit! exec #(+ 1 2))
                (finally (stop! exec))))))

     (use-fixtures :each resource-cleanup-fixture)"
  (:require [clojure.test :refer [is do-report]]
            [clojure.set :as set])
  (:import [java.lang.management ManagementFactory]))

;; =============================================================================
;; Thread counting — uses ThreadMXBean for accurate counts
;; =============================================================================

(defn thread-count
  "Current live thread count (daemon + non-daemon).
   Uses ManagementFactory/getThreadMXBean for accuracy."
  []
  (.getThreadCount (ManagementFactory/getThreadMXBean)))

(defn thread-names
  "Return a set of names of all live threads."
  []
  (into #{}
        (map #(.getName %))
        (keys (Thread/getAllStackTraces))))

(defn thread-diff
  "Return {:added #{names} :removed #{names}} between two thread-name sets."
  [before after]
  {:added   (set/difference after before)
   :removed (set/difference before after)})

;; =============================================================================
;; Bounded atom helpers
;; =============================================================================

(defn- bounded-atom-snapshot
  "Snapshot the entry count of a bounded atom."
  [batom]
  (count @(:atom batom)))

(defn ^:no-doc registry-snapshot
  "Snapshot the global sweepable-registry size.
   Requires hive-dsl.bounded-atom on the classpath.
   Returns 0 if namespace not loaded.
   Public because with-resource-accounting macro expands into caller ns."
  []
  (try
    (let [reg-fn (requiring-resolve 'hive-dsl.bounded-atom/registered-sweepables)]
      (count (reg-fn)))
    (catch Exception _
      0)))

(defn- clear-registry!
  "Clear the global sweepable-registry.
   No-op if hive-dsl.bounded-atom not on classpath."
  []
  (try
    (let [reg-fn (requiring-resolve 'hive-dsl.bounded-atom/registered-sweepables)
          unreg-fn (requiring-resolve 'hive-dsl.bounded-atom/unregister-sweepable!)]
      (doseq [k (keys (reg-fn))]
        (unreg-fn k)))
    (catch Exception _
      nil)))

;; =============================================================================
;; Bounded atom assertions
;; =============================================================================

(defmacro assert-bounded-growth
  "Assert that no registered bounded atom exceeds its max-entries after body.
   Evaluates body, then checks every atom in the sweepable-registry.
   Fails the test with details if any atom is over capacity.

   Usage:
     (assert-bounded-growth
       (dotimes [i 2000]
         (bput! my-store (keyword (str i)) {:val i})))"
  [& body]
  `(let [result# (do ~@body)]
     (try
       (let [reg-fn# (requiring-resolve 'hive-dsl.bounded-atom/registered-sweepables)]
         (doseq [[name-kw# batom#] (reg-fn#)]
           (let [count#      (count @(:atom batom#))
                 max-entries# (get-in batom# [:opts :max-entries])]
             (is (<= count# max-entries#)
                 (str "Bounded atom " name-kw#
                      " exceeded capacity: " count#
                      " entries > max " max-entries#)))))
       (catch Exception e#
         ;; If hive-dsl.bounded-atom not on classpath, skip check
         (when-not (instance? java.io.FileNotFoundException e#)
           (throw e#))))
     result#))

(defn assert-atom-count-stable
  "Assert that a bounded atom's entry count doesn't grow beyond threshold.
   Takes a before/after snapshot approach.

   Arguments:
     batom      — a bounded atom (map with :atom key)
     max-growth — maximum allowed growth in entry count
     f          — zero-arg function to execute as the body

   Fails the test if growth exceeds max-growth.

   Usage:
     (assert-atom-count-stable my-store 0
       (fn [] (do-some-work)))"
  [batom max-growth f]
  (let [before (bounded-atom-snapshot batom)
        result (f)
        after  (bounded-atom-snapshot batom)
        growth (- after before)]
    (is (<= growth max-growth)
        (str "Atom count grew by " growth
             " (before=" before ", after=" after
             "), exceeds max-growth=" max-growth))
    result))

;; =============================================================================
;; Thread count assertions
;; =============================================================================

(defmacro assert-thread-count-stable
  "Assert that thread count doesn't grow by more than tolerance during body.
   Default tolerance is 2 (for JVM/test-framework background threads).

   Usage:
     (assert-thread-count-stable
       (let [exec (start! (->managed-executor 4))]
         (stop! exec)))

     ;; With explicit tolerance:
     (assert-thread-count-stable 5
       (do-heavy-threaded-work))"
  [& args]
  (let [[tolerance body] (if (number? (first args))
                           [(first args) (rest args)]
                           [2 args])]
    `(let [names-before# (thread-names)
           count-before# (thread-count)
           result#       (do ~@body)
           ;; Small delay to let threads settle
           _#            (Thread/sleep 100)
           count-after#  (thread-count)
           names-after#  (thread-names)
           growth#       (- count-after# count-before#)]
       (is (<= growth# ~tolerance)
           (let [diff# (thread-diff names-before# names-after#)]
             (str "Thread count grew by " growth#
                  " (before=" count-before#
                  ", after=" count-after#
                  ", tolerance=" ~tolerance ")"
                  "\n  Added threads: " (pr-str (:added diff#)))))
       result#)))

;; =============================================================================
;; Resource accounting — the combined check
;; =============================================================================

(defmacro with-resource-accounting
  "Execute body while tracking resource consumption. After body,
   assert all resources returned to baseline (within tolerance).

   Tracks: thread count, bounded-atom registry size.
   Fails test with detailed diff if resources leaked.

   Options:
     :thread-tolerance — max thread growth allowed (default 2)
     :atom-tolerance   — max bounded-atom registry growth allowed (default 0)
     :report?          — print resource report even on success (default false)
     :settle-ms        — milliseconds to wait for threads to settle (default 200)

   Usage:
     (with-resource-accounting {:thread-tolerance 4}
       (let [exec (start! (->managed-executor 4))]
         (try (submit! exec #(+ 1 1))
              (finally (stop! exec)))))"
  [opts & body]
  `(let [opts#            (merge {:thread-tolerance 2
                                   :atom-tolerance   0
                                   :report?          false
                                   :settle-ms        200}
                                  ~opts)
         thread-before#   (thread-count)
         names-before#    (thread-names)
         registry-before# (registry-snapshot)
         result#          (do ~@body)
         _#               (Thread/sleep (:settle-ms opts#))
         thread-after#    (thread-count)
         names-after#     (thread-names)
         registry-after#  (registry-snapshot)
         thread-growth#   (- thread-after# thread-before#)
         registry-growth# (- registry-after# registry-before#)
         thread-diff#     (thread-diff names-before# names-after#)
         report#          {:thread-before   thread-before#
                           :thread-after    thread-after#
                           :thread-growth   thread-growth#
                           :threads-added   (:added thread-diff#)
                           :threads-removed (:removed thread-diff#)
                           :registry-before registry-before#
                           :registry-after  registry-after#
                           :registry-growth registry-growth#}]
     (when (:report? opts#)
       (println "Resource accounting report:" (pr-str report#)))
     (is (<= thread-growth# (:thread-tolerance opts#))
         (str "Thread leak detected: grew by " thread-growth#
              " (tolerance=" (:thread-tolerance opts#) ")"
              "\n  Added threads: " (pr-str (:added thread-diff#))))
     (is (<= registry-growth# (:atom-tolerance opts#))
         (str "Bounded-atom registry leak: grew by " registry-growth#
              " (tolerance=" (:atom-tolerance opts#) ")"
              "\n  Registry before=" registry-before#
              ", after=" registry-after#))
     result#))

;; =============================================================================
;; Lifecycle compliance assertions
;; =============================================================================

(defn assert-lifecycle-roundtrip
  "Assert that (stop! (start! resource)) returns to :stopped state
   without throwing. The resource must implement Lifecycle protocol.

   Usage:
     (assert-lifecycle-roundtrip (->managed-executor 2))"
  [resource]
  (try
    (let [start-fn (requiring-resolve 'hive-dsl.lifecycle/start!)
          stop-fn  (requiring-resolve 'hive-dsl.lifecycle/stop!)
          state-fn (requiring-resolve 'hive-dsl.lifecycle/lifecycle-state)
          started  (start-fn resource)
          _        (is (= :started (state-fn started))
                       "Resource should be in :started state after start!")
          stopped  (stop-fn started)]
      (is (= :stopped (state-fn stopped))
          "Resource should be in :stopped state after stop!")
      stopped)
    (catch Throwable t
      (do-report {:type     :fail
                  :message  (str "Lifecycle roundtrip threw: " (.getMessage t))
                  :expected "No exception during start!/stop! roundtrip"
                  :actual   t})
      nil)))

(defn assert-stop-idempotent
  "Assert that calling stop! twice on a started resource doesn't throw.
   Starts the resource, stops it twice, and verifies it stays :stopped.

   Usage:
     (assert-stop-idempotent (->managed-executor 2))"
  [resource]
  (try
    (let [start-fn (requiring-resolve 'hive-dsl.lifecycle/start!)
          stop-fn  (requiring-resolve 'hive-dsl.lifecycle/stop!)
          state-fn (requiring-resolve 'hive-dsl.lifecycle/lifecycle-state)
          started  (start-fn resource)
          stopped1 (stop-fn started)
          stopped2 (stop-fn stopped1)]
      (is (= :stopped (state-fn stopped1))
          "First stop! should reach :stopped state")
      (is (= :stopped (state-fn stopped2))
          "Second stop! should remain in :stopped state")
      stopped2)
    (catch Throwable t
      (do-report {:type     :fail
                  :message  (str "Idempotent stop! threw: " (.getMessage t))
                  :expected "No exception on double stop!"
                  :actual   t})
      nil)))

;; =============================================================================
;; Test fixture — automatic resource-leak detection per test
;; =============================================================================

(def ^:dynamic *fixture-opts*
  "Dynamic var for configuring resource-cleanup-fixture tolerances.
   Keys: :thread-tolerance, :atom-tolerance, :settle-ms"
  nil)

(defn resource-cleanup-fixture
  "use-fixtures :each fixture that:
   1. Snapshots resource counts before the test
   2. Runs the test
   3. Asserts no resource growth (within tolerance)
   4. Cleans up bounded-atom registry

   Tolerances can be configured via dynamic var *fixture-opts*.

   Usage:
     (use-fixtures :each resource-cleanup-fixture)

   Or with custom tolerance:
     (use-fixtures :each
       (fn [f]
         (binding [hive-test.resource-assertions/*fixture-opts*
                   {:thread-tolerance 5}]
           (resource-cleanup-fixture f))))"
  [f]
  (let [opts            (merge {:thread-tolerance 2
                                :atom-tolerance   0
                                :settle-ms        200}
                               *fixture-opts*)
        thread-before   (thread-count)
        names-before    (thread-names)
        registry-before (registry-snapshot)]
    (try
      (f)
      (finally
        (Thread/sleep (:settle-ms opts))
        (let [thread-after    (thread-count)
              names-after     (thread-names)
              registry-after  (registry-snapshot)
              thread-growth   (- thread-after thread-before)
              registry-growth (- registry-after registry-before)
              tdiff           (thread-diff names-before names-after)]
          (is (<= thread-growth (:thread-tolerance opts))
              (str "Fixture: thread leak detected, grew by " thread-growth
                   "\n  Added: " (pr-str (:added tdiff))))
          (is (<= registry-growth (:atom-tolerance opts))
              (str "Fixture: bounded-atom registry leak, grew by " registry-growth)))
        ;; Always attempt registry cleanup
        (clear-registry!)))))
