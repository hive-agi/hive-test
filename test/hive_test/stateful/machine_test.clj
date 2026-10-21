(ns hive-test.stateful.machine-test
  "Facets for hive-test.stateful over a bounded counter machine: exploration
   as a property over the contract (a counter to N reaches exactly N+1 states,
   its goal stays reachable), and a golden of check / dead-ends / invariant
   violations for a live, a dead-ended and an invariant-breaking machine."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.stateful :as sf]))

(defn- counter
  "Counter from 0 to `limit`; with `trap?` a :jump command leads to a sink
   from which the goal (= limit) is unreachable."
  [limit trap?]
  {:init     (fn [] 0)
   :commands (cond-> {:inc {:pre (fn [m _] (and (int? m) (< m limit))) :next (fn [m _] (inc m))}}
               trap? (assoc :jump {:pre (fn [m _] (= m 0)) :next (fn [_ _] :sink)}))
   :invariants {:non-negative (fn [m] (or (= :sink m) (<= 0 m)))}
   :goals    {:at-limit (fn [m] (= limit m))}})

(defn- explore-counter
  "Explore with a depth bound above the generated limit: the default
   :max-depth (20) would truncate counters past 20 and the relation would
   fail spuriously."
  [limit]
  (sf/explore (counter limit false) {:max-depth 64}))

(hst/deftrifecta-from-schema explore-reaches-every-counter-state
  explore-counter
  {:in  [:int {:min 1 :max 30}]
   :out sf/Graph
   :rel (fn [limit g] (and (= (inc limit) (count (:states g)))
                           (false? (:truncated? g))
                           (= (vec (repeat limit [:inc nil])) (sf/path-to g limit))))
   :mutation true})

(defn- check-counter [[limit trap?]] (sf/check (counter limit trap?)))

(hst/deftrifecta-from-schema check-finds-the-trap
  check-counter
  {:in  [:tuple [:int {:min 1 :max 10}] :boolean]
   :out sf/CheckResult
   :rel (fn [[_ trap?] r] (= (not trap?) (:ok? r)))
   :mutation true})

(deftest enabled-lists-guarded-successors
  (is (= [[:inc nil 1] [:jump nil :sink]] (vec (sf/enabled (counter 3 true) 0))))
  (is (= [] (vec (sf/enabled (counter 3 true) 3)))))

(deftest-golden check-outcomes "test/golden/stateful/check-outcomes.edn"
  (let [g (sf/explore (counter 3 true) {})]
    {:live        (sf/check (counter 3 false))
     :trapped     (sf/check (counter 3 true))
     :co-reachable (vec (sort-by str (sf/co-reachable g #(= 3 %))))
     :dead-ends   (sf/dead-ends g #(= 3 %))
     :violations  (sf/invariant-violations g {:below-two #(or (= :sink %) (< % 2))})
     :vacuity     (vec (sf/vacuity {:init (fn [] 0) :commands {} :goals {:z zero?}}
                                   (sf/explore {:init (fn [] 0) :commands {}} {})))
     :truncated   (dissoc (sf/check (counter 50 false) {:max-states 5}) :dead-ends)
     :report      (sf/report-str (sf/check (counter 3 true)))}))
