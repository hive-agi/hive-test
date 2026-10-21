(ns hive-test.fsm-analysis.spec-test
  "Facets for hive-test.fsm-analysis: static-graph and analyze as properties
   over the contract's generated specs, and a golden of the findings for a
   healthy, an orphaned, a dangling and a dead-ended spec."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.fsm-analysis :as fa]
            [hive-test.golden :refer [deftest-golden]]))

(def ^:private start :hive.events.fsm/start)
(def ^:private end :hive.events.fsm/end)

(hst/deftrifecta-from-schema static-graph-mirrors-the-dispatches
  fa/static-graph
  {:in  fa/Spec
   :out [:map [:states [:set :any]] [:adjacency [:map-of :any [:set :any]]]]
   :rel (fn [spec g] (and (= (set (keys (:fsm spec))) (:states g))
                          (every? (fn [[id {:keys [dispatches]}]]
                                    (= (set (map first dispatches)) (get-in g [:adjacency id])))
                                  (:fsm spec))))
   :num-tests 25
   :mutation true})

(hst/deftrifecta-from-schema analyze-ok-iff-no-findings
  fa/analyze
  {:in  fa/Spec
   :out fa/Findings
   :rel (fn [_ r] (= (:ok? r) (and (not (:truncated? r)) (not (:missing-start? r))
                                   (empty? (:unreachable r)) (empty? (:dangling-targets r))
                                   (empty? (:dead-ends r)))))
   :num-tests 25
   :mutation true})

(deftest machine-follows-every-edge
  (let [m (fa/machine {:fsm {start {:dispatches [[end any?] [::a any?]]}}})]
    (is (= #{end ::a} (set ((get-in m [:commands :dispatch :args]) start))))))

(def ^:private specs
  {:healthy  {:fsm {start {:dispatches [[::work any?]]} ::work {:dispatches [[end any?]]}}}
   :orphan   {:fsm {start {:dispatches [[end any?]]} ::orphan {:dispatches [[end any?]]}}}
   :dangling {:fsm {start {:dispatches [[::nowhere any?]]}}}
   :dead-end {:fsm {start {:dispatches [[::loop any?]]} ::loop {:dispatches [[::loop any?]]}}}
   :no-start {:fsm {::a {:dispatches [[end any?]]}}}})

(deftest-golden findings "test/golden/fsm-analysis/findings.edn"
  (into (sorted-map)
        (for [[k spec] specs :let [r (fa/analyze spec)]]
          [k (assoc r :report (fa/report-str r))])))
