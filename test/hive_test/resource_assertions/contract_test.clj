(ns hive-test.resource-assertions.contract-test
  "Name-matched facets for hive-test.resource-assertions (the behavioural
   suite is hive-test.resource-assertions-test): thread-diff as a set-algebra
   property over its contract, and a golden of the clojure.test reports the
   assertions emit, captured by binding the dynamic clojure.test/report."
  (:require [clojure.set :as set]
            [clojure.test :as t :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.resource-assertions :as ra]))

(defn- diff-pair [[before after]] (ra/thread-diff before after))

(hst/deftrifecta-from-schema thread-diff-is-the-two-set-differences
  diff-pair
  {:in  [:tuple [:set [:enum "a" "b" "c" "d" "e"]] [:set [:enum "a" "b" "c" "d" "e"]]]
   :out [:map [:added [:set :string]] [:removed [:set :string]]]
   :rel (fn [[before after] {:keys [added removed]}]
          (and (= added (set/difference after before))
               (= removed (set/difference before after))
               (empty? (set/intersection added removed))
               (= after (set/union (set/difference before removed) added))))
   :mutation true})

(defn- reports-of
  "Run `f` with clojure.test/report captured; answer [result [{:type :message}]]."
  [f]
  (let [seen (atom [])]
    (binding [t/report (fn [m] (swap! seen conj (select-keys m [:type :message])))]
      (let [r (f)] [r @seen]))))

(deftest live-thread-readings-are-consistent
  (is (pos? (ra/thread-count)))
  (is (contains? (ra/thread-names) (.getName (Thread/currentThread))))
  (is (nat-int? (ra/registry-snapshot))))

(deftest-golden assertion-reports "test/golden/resource-assertions/reports.edn"
  (let [batom {:atom (atom {:a 1})}
        prefix (fn [[r ms]] [r (mapv #(update % :message (fn [s] (re-find #"^[^:]+" s))) ms)])]
    {:stable    (reports-of #(ra/assert-atom-count-stable batom 0 (fn [] :ok)))
     :grew-ok   (reports-of #(ra/assert-atom-count-stable batom 1 (fn [] (swap! (:atom batom) assoc :b 2) :grew)))
     :grew-over (reports-of #(ra/assert-atom-count-stable batom 0 (fn [] (swap! (:atom batom) assoc :c 3 :d 4) :over)))
     ;; :resource is no hive-dsl Lifecycle: both lifecycle assertions must
     ;; report a failure and answer nil, never throw.
     :roundtrip  (prefix (reports-of #(ra/assert-lifecycle-roundtrip :resource)))
     :idempotent (prefix (reports-of #(ra/assert-stop-idempotent :resource)))
     ;; Live thread counts vary across a cold suite: keep only the verdicts.
     :fixture   (binding [ra/*fixture-opts* {:settle-ms 0 :thread-tolerance 1000 :atom-tolerance 1000}]
                  (let [[r ms] (reports-of #(ra/resource-cleanup-fixture (fn [] :ran)))]
                    [r (mapv :type ms)]))}))
