(ns hive-test.stateful
  "Model-based checking over a command algebra: safety (□ invariants) AND liveness
   (◇ goals) — the latter unexpressible by the other namespaces here.

   A Machine is data:

     {:init       (fn [] model)
      :commands   {:cmd-id {:args (fn [model] [args ...])   ; finite, model-dependent
                            :pre  (fn [model args] boolean) ; guard
                            :next (fn [model args] model')  ; pure model step
                            :run  (fn [sut args] result)    ; boundary (unused by check)
                            :post (fn [model args result] boolean)}}
      :invariants {:name (fn [model] boolean)}   ; must hold in every reachable state
      :goals      {:name (fn [model] boolean)}   ; must stay reachable from every one
      :ident      (fn [model] k)}                ; state identity (default: identity)

   `:args` yields a COLLECTION, not a generator — that is what makes the state space
   enumerable; `gen/elements` recovers a generator, the converse does not hold.

   Every command whose `:pre` holds is a successor (contrast hive.events.fsm, which
   resolves exactly one). `check` is exhaustive within :max-states/:max-depth and
   reports `:truncated?` when it hit them. Counterexamples are shortest paths, given
   as command sequences.

   Rationale: hive memory [[20260714012524-6370a2c4]]."
  #?(:cljs (:require-macros [hive-test.stateful]))
  (:require [clojure.string :as str]))

(def ^:private empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs cljs.core/PersistentQueue.EMPTY))

;; ── pure exploration ─────────────────────────────────────────────────────────

(defn enabled
  "Commands whose guard holds in `model`, as [[cmd-id args model'] ...]."
  [machine model]
  (for [[id {:keys [args pre next]}] (:commands machine)
        a  (if args (args model) [nil])
        :when (or (nil? pre) (pre model a))]
    [id a (next model a)]))

(defn explore
  "Breadth-first over reachable states. Returns
   {:root id :states {id model} :edges {id [[cmd args id'] ...]}
    :parent {id [parent-id cmd args]} :truncated? bool}.
   :parent is the BFS tree, so paths derived from it are shortest.
   :truncated? is true iff a bound cut off unexplored successors."
  [machine {:keys [max-states max-depth] :or {max-states 20000 max-depth 20}}]
  (let [id-of (or (:ident machine) identity)
        root  ((:init machine))
        rid   (id-of root)]
    (loop [q      (conj empty-queue [root 0])
           states {rid root}
           edges  {}
           parent {}
           trunc? false]
      (if-let [[model depth] (peek q)]
        (let [mid   (id-of model)
              succ  (enabled machine model)
              stop? (or (>= depth max-depth) (>= (count states) max-states))]
          (if stop?
            (recur (pop q) states edges parent (or trunc? (boolean (seq succ))))
            (let [fresh (remove (fn [[_ _ m']] (contains? states (id-of m'))) succ)]
              (recur (into (pop q) (map (fn [[_ _ m']] [m' (inc depth)])) fresh)
                     (into states (map (fn [[_ _ m']] [(id-of m') m'])) fresh)
                     (assoc edges mid (mapv (fn [[c a m']] [c a (id-of m')]) succ))
                     (into parent (map (fn [[c a m']] [(id-of m') [mid c a]])) fresh)
                     trunc?))))
        {:root rid :states states :edges edges :parent parent :truncated? trunc?}))))

(defn path-to
  "Shortest command sequence from the root to `id`: [[cmd args] ...]."
  [{:keys [root parent]} id]
  (loop [cur id acc ()]
    (if (= cur root)
      (vec acc)
      (if-let [[pid cmd args] (get parent cur)]
        (recur pid (conj acc [cmd args]))
        (vec acc)))))

(defn co-reachable
  "Ids of states from which some goal state is still reachable. Backward BFS over
   the full edge set."
  [{:keys [states edges]} goal?]
  (let [preds (reduce-kv (fn [acc from tos]
                           (reduce (fn [a [_ _ to]] (update a to (fnil conj #{}) from))
                                   acc tos))
                         {} edges)
        goal-ids (into #{} (comp (filter (fn [[_ m]] (goal? m))) (map key)) states)]
    (loop [q (into empty-queue goal-ids) live goal-ids]
      (if-let [id (peek q)]
        (let [fresh (remove live (get preds id #{}))]
          (recur (into (pop q) fresh) (into live fresh)))
        live))))

(defn dead-ends
  "Reachable states from which no goal state is reachable — the liveness violation.
   Each is {:state model :path [[cmd args] ...]}, the path being shortest."
  [{:keys [states] :as graph} goal? & [{:keys [limit] :or {limit 5}}]]
  (let [live (co-reachable graph goal?)]
    (->> states
         (remove (fn [[id _]] (contains? live id)))
         (take limit)
         (mapv (fn [[id model]] {:state model :path (path-to graph id)})))))

(defn invariant-violations
  "Reachable states breaking an invariant, each with the shortest path to it."
  [{:keys [states] :as graph} invariants & [{:keys [limit] :or {limit 5}}]]
  (->> (for [[id model] states
             [k pred]   invariants
             :when      (not (pred model))]
         {:invariant k :state model :path (path-to graph id)})
       (take limit)
       vec))

(defn vacuity
  "Reasons the check proves nothing (seq), or nil: no commands, no state beyond the
   initial one, or every goal already true at :init."
  [machine {:keys [states] :as graph}]
  (let [root  (get states (:root graph))
        goals (:goals machine)]
    (seq (cond-> []
           (empty? (:commands machine))
           (conj "no :commands — nothing to explore")

           (and (seq goals) (every? (fn [[_ g]] (g root)) goals))
           (conj "every :goal already holds at :init")

           (= 1 (count states))
           (conj "only the initial state is reachable — no command was ever enabled")))))

;; ── boundary ─────────────────────────────────────────────────────────────────

(defn check
  "Exhaustively check `machine` within :max-states/:max-depth. Returns
   {:ok? :states :truncated? :vacuity :invariant-violations :dead-ends}.
   :ok? is false if any of vacuity / invariant-violations / dead-ends is non-empty."
  [machine & [opts]]
  (let [graph (explore machine (or opts {}))
        vac   (vacuity machine graph)
        viols (invariant-violations graph (:invariants machine) opts)
        goal? (fn [m] (every? (fn [[_ g]] (g m)) (:goals machine)))
        dead  (when (seq (:goals machine)) (dead-ends graph goal? opts))]
    {:ok?                  (and (empty? vac) (empty? viols) (empty? dead))
     :states               (count (:states graph))
     :truncated?           (:truncated? graph)
     :vacuity              vac
     :invariant-violations viols
     :dead-ends            dead}))

(defn report-str
  "Failure report for a `check` result. Each path replays the counterexample."
  [{:keys [states truncated? vacuity invariant-violations dead-ends]}]
  (str "explored " states " states"
       (when truncated? " (TRUNCATED — bounds hit; nothing is proved beyond them)")
       (when (seq vacuity)
         (str "\nVACUOUS: " (str/join "; " vacuity)))
       (when (seq invariant-violations)
         (str "\nINVARIANT BROKEN:"
              (apply str (for [{:keys [invariant path]} invariant-violations]
                           (str "\n  " invariant " after " (pr-str path))))))
       (when (seq dead-ends)
         (str "\nDEAD END (goal became unreachable):"
              (apply str (for [{:keys [path]} dead-ends]
                           (str "\n  " (pr-str path))))))))

#?(:clj
   (defmacro defcheck
     "Emit a deftest asserting `machine` (a form) passes `check` within `opts`.
      Failure message is `report-str`."
     [name machine & [opts]]
     (let [deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)
           is-sym      (if (:ns &env) 'cljs.test/is     'clojure.test/is)]
       `(~deftest-sym ~name
         (let [r# (check ~machine ~(or opts {}))]
           (~is-sym (:ok? r#) (report-str r#)))))))
