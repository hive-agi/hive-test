(ns hive-test.fsm-analysis-test
  "Specs in the hive.events.fsm shape: handlers and predicates are present but
   irrelevant — analysis sees only the dispatch graph."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.fsm-analysis :as fa]
            [hive-test.stateful :as sf]))

(def ^:private start :hive.events.fsm/start)
(def ^:private end :hive.events.fsm/end)
(def ^:private error :hive.events.fsm/error)

(defn- spec [fsm] {:fsm fsm})

(def ^:private healthy
  (spec {start     {:handler identity :dispatches [[::process (constantly true)]]}
         ::process {:handler identity :dispatches [[end some?]
                                                   [error (constantly true)]]}}))

(deftest a-spec-whose-graph-closes-passes
  (let [r (fa/analyze healthy)]
    (is (true? (:ok? r)) (fa/report-str r))
    (is (false? (:truncated? r)))
    (is (= 4 (:states-explored r)))
    (is (empty? (:unreachable r)))
    (is (empty? (:dangling-targets r)))
    (is (empty? (:dead-ends r)))))

(deftest states-no-dispatch-leads-to-are-unreachable
  (let [r (fa/analyze (assoc-in healthy [:fsm ::orphan]
                                {:handler identity :dispatches [[end (constantly true)]]}))]
    (is (false? (:ok? r)))
    (is (= [::orphan] (:unreachable r)))
    (is (re-find #"UNREACHABLE" (fa/report-str r)))))

(deftest a-cycle-that-never-reaches-a-terminal-is-a-dead-end
  (let [r (fa/analyze (-> healthy
                          (assoc-in [:fsm ::process :dispatches] [[::loop (constantly true)]])
                          (assoc-in [:fsm ::loop] {:handler identity
                                                   :dispatches [[::loop (constantly true)]]})))]
    (is (false? (:ok? r)))
    (is (= #{start ::process ::loop} (into #{} (map :state) (:dead-ends r))))
    (testing "each dead end carries the shortest path to it"
      (is (every? (comp vector? :path) (:dead-ends r)))
      (is (= [[:dispatch ::process]] (:path (first (filter #(= ::process (:state %))
                                                          (:dead-ends r)))))))))

(deftest a-dispatch-to-nowhere-is-dangling
  (let [r (fa/analyze (assoc-in healthy [:fsm ::process :dispatches]
                                [[::nowhere (constantly true)]]))]
    (is (false? (:ok? r)))
    (is (= [{:from ::process :target ::nowhere}] (:dangling-targets r)))
    (is (re-find #"DANGLING" (fa/report-str r)))))

(deftest a-spec-without-the-start-state-is-reported
  (let [r (fa/analyze (spec {::process {:handler identity
                                        :dispatches [[end (constantly true)]]}}))]
    (is (false? (:ok? r)))
    (is (true? (:missing-start? r)))))

(deftest start-and-terminals-are-opts
  (let [r (fa/analyze (spec {:app/init {:handler identity
                                        :dispatches [[:app/done (constantly true)]]}})
                      {:start :app/init :terminals #{:app/done}})]
    (is (true? (:ok? r)) (fa/report-str r))))

(deftest machine-exposes-the-static-graph-to-stateful
  (let [g (sf/explore (fa/machine healthy) {})]
    (is (= #{start ::process end error} (set (keys (:states g)))))))
