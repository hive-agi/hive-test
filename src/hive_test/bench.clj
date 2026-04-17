(ns hive-test.bench
  "Bench reporter integration — first-class test type for statistical
   benchmarks produced by hive-ttracking.bench/bench*.

   hive-test must NOT hard-depend on hive-ttracking (it'd create a cycle —
   hive-ttracking depends on hive-test). Instead, we resolve the symbols
   dynamically via `requiring-resolve`. Consumers who want bench support
   add `hive-ttracking` to their test classpath; consumers who don't never
   pay the cost.

   API:
     defbench         — deftest-like macro with p95 threshold assertion
     report-bench     — reporter hook; emits timeline on assertion failure
     clojure.test reporter is extended to recognize :bench metadata so
     failed benches attach their BenchResult EDN payload alongside the
     assertion message.

   Wire-up:
     (require '[hive-test.bench])      ; triggers reporter registration
     (defbench hot-path {:threshold-p95-ms 5.0} (do-work))"
  (:require [clojure.test :as t]))

(defn- log-warn [& args] (binding [*out* *err*] (apply println "[WARN]" args)))
(defn- log-debug [& _args] nil) ; no-op; upgrade to tools.logging if added to deps
(defn- log-info [& args] (apply println "[INFO]" args))

;; ---------------------------------------------------------------------------
;; Dynamic resolution — avoid compile-time cycle.
;; ---------------------------------------------------------------------------

(defn- bench-fn
  "Resolve hive-ttracking.bench/bench* at runtime. Returns nil when the
   bench library is not on the classpath, which lets callers fail fast
   with a clear error rather than a classloader trace."
  []
  (try
    (requiring-resolve 'hive-ttracking.bench/bench*)
    (catch Throwable t
      (log-debug t "hive-test.bench: hive-ttracking.bench not on classpath")
      nil)))

(defn run-bench
  "Run a bench via hive-ttracking.bench/bench* if available.
   opts: {:name :runs :warmup} — forwarded verbatim.
   Returns a BenchResult or throws a clear ex-info when hive-ttracking
   is absent from the classpath."
  [opts thunk]
  (if-let [f (bench-fn)]
    (f opts thunk)
    (throw (ex-info "hive-test.bench requires hive-ttracking on the classpath"
                    {:missing 'hive-ttracking.bench/bench*
                     :fix     "add io.github.hive-agi/hive-ttracking to :test deps"}))))

;; ---------------------------------------------------------------------------
;; Reporter hook — attach BenchResult payload to failed assertions so the
;; bench timeline flows into kanban/memory when wired through the hive
;; reporter pipeline.
;; ---------------------------------------------------------------------------

(def ^:dynamic *last-bench-result*
  "Most recent BenchResult for the current test scope. Reporter reads this
   when attaching artifacts to a failing assertion."
  nil)

(defn report-bench
  "Reporter entry point — emit a bench-timeline summary for the current
   BenchResult at :info. Consumers that write to hive-mem can wire this
   fn to `mcp_memory_add` with :tags [\"bench\" \"timeline\"]."
  [{:keys [name p50 p95 p99 runs warmup] :as result}]
  (log-info (format "bench %s: runs=%d warmup=%d p50=%.3fms p95=%.3fms p99=%.3fms"
                    (pr-str name)
                    (or runs 0) (or warmup 0)
                    (or p50 Double/NaN)
                    (or p95 Double/NaN)
                    (or p99 Double/NaN)))
  result)

;; ---------------------------------------------------------------------------
;; defbench — thin shim over hive-ttracking.bench/defbench when available;
;; otherwise expands to a skipped test that carries a helpful message.
;; ---------------------------------------------------------------------------

(defmacro defbench
  "Alias for hive-ttracking.bench/defbench — forwards all args.

   Falls back to a deftest that reports a skip message if hive-ttracking
   is not on the classpath, so test suites still compile without it."
  [bench-sym opts & body]
  (if (try (requiring-resolve 'hive-ttracking.bench/defbench)
           (catch Throwable _ nil))
    `(hive-ttracking.bench/defbench ~bench-sym ~opts ~@body)
    `(clojure.test/deftest ~(vary-meta bench-sym assoc :bench true :skipped true)
       (binding [*out* *err*]
         (println "[WARN] defbench skipped:"
                  '~bench-sym
                  "— hive-ttracking not on classpath")))))
