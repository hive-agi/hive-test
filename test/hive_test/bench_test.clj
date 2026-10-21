(ns hive-test.bench-test
  "Facets for hive-test.bench: report-bench is an identity on the result it
   logs (property over the contract), run-bench fails loud without
   hive-ttracking, and a golden of the logged line.

   KNOWN BUG (recorded, not fixed): BenchResult admits any `number?` for
   :p50/:p95/:p99, but report-bench formats them with %.3f, so an integer
   percentile throws IllegalFormatConversionException (bench.clj:72). The
   trifecta's :in therefore narrows the percentiles to doubles."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.bench :as bench]
            [hive-test.golden :refer [deftest-golden]]))

(def ^:private DoubleResult
  [:map [:name {:optional true} :keyword]
   [:p50 {:optional true} [:maybe :double]] [:p95 {:optional true} [:maybe :double]]
   [:p99 {:optional true} [:maybe :double]]
   [:runs {:optional true} [:maybe :int]] [:warmup {:optional true} [:maybe :int]]])

(defn- report-quietly [r] (binding [*out* (java.io.StringWriter.)] (bench/report-bench r)))

(hst/deftrifecta-from-schema report-bench-answers-what-it-logged
  report-quietly
  {:in  DoubleResult
   :out bench/BenchResult
   :rel (fn [r out] (identical? r out))
   :mutation true})

(deftest run-bench-fails-loud-without-hive-ttracking
  (let [e (try (bench/run-bench {:name :x} (fn [] 1)) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e))
    (is (= 'hive-ttracking.bench/bench* (:missing (ex-data e))))))

(defn- logged [r] (str/trim (with-out-str (bench/report-bench r))))

(deftest-golden logged-lines "test/golden/bench/logged-lines.edn"
  {:full    (logged {:name :hot :runs 10 :warmup 2 :p50 1.0 :p95 2.5 :p99 3.25})
   :partial (logged {:name "cold" :p95 0.5})
   :empty   (logged {})
   :missing-dep (:fix (ex-data (try (bench/run-bench {} (fn [] 1))
                                    (catch clojure.lang.ExceptionInfo e e))))})
