(ns hive-test.fsm-analysis
  "Static analysis of a hive.events.fsm spec's dispatch graph — spec data in,
   findings data out. Consumes the spec SHAPE only; no hive-events dependency.

   Dispatch predicates are opaque at analysis time, so every dispatch is a
   POTENTIAL successor: findings are about possibility, not certainty. The graph
   is explored with hive-test.stateful, so :max-states/:max-depth bounds apply
   and :truncated? is reported.

   Privileged state ids are keywords, not code — defaults mirror
   hive.events.fsm (::start, ::end, ::halt, ::error); pass others via opts."
  (:require [clojure.string :as str]
            [hive-test.stateful :as sf]))

;; ── data ─────────────────────────────────────────────────────────────────────

(def default-start
  "Root state id assumed when opts :start is absent."
  :hive.events.fsm/start)

(def default-terminals
  "State ids that end a run (success, pause, failure) assumed when
   opts :terminals is absent."
  #{:hive.events.fsm/end :hive.events.fsm/halt :hive.events.fsm/error})

(defn static-graph
  "Conform a spec's :fsm map to plain adjacency data:
   {:states #{id ...} :adjacency {id #{target-id ...}}}."
  [spec]
  (let [fsm (:fsm spec)]
    {:states    (set (keys fsm))
     :adjacency (into {}
                      (map (fn [[id {:keys [dispatches]}]]
                             [id (into #{} (map first) dispatches)]))
                      fsm)}))

;; ── calculation ──────────────────────────────────────────────────────────────

(defn machine
  "A hive-test.stateful Machine over the spec's static dispatch graph: models are
   state ids and the single :dispatch command follows every edge. opts: :start,
   :terminals."
  [spec & [{:keys [start terminals]
            :or   {start default-start terminals default-terminals}}]]
  (let [{:keys [adjacency]} (static-graph spec)]
    {:init     (fn [] start)
     :commands {:dispatch {:args (fn [state] (vec (get adjacency state)))
                           :next (fn [_state target] target)}}
     :goals    {:terminal (fn [state] (contains? terminals state))}}))

(defn analyze
  "Static findings over the spec's dispatch graph. Returns
   {:ok?              bool  — no findings below
    :start            id
    :states-explored  n
    :truncated?       bool  — :max-states/:max-depth cut exploration short
    :missing-start?   bool  — :start is not a state of the spec
    :unreachable      [id ...]                 ; spec states never reached from :start
    :dangling-targets [{:from id :target id}]  ; dispatch to neither a spec state
                                               ; nor a terminal
    :dead-ends        [{:state id :path [[:dispatch target] ...]}]}
   ; reachable non-terminals from which no terminal is reachable

   opts: :start, :terminals, :max-states, :max-depth, :limit."
  [spec & [{:keys [start terminals]
            :or   {start default-start terminals default-terminals}
            :as   opts}]]
  (let [{:keys [states adjacency]} (static-graph spec)
        graph        (sf/explore (machine spec opts) opts)
        goal?        (fn [state] (contains? terminals state))
        dead         (sf/dead-ends graph goal? opts)
        reached      (set (keys (:states graph)))
        valid-target (into states terminals)
        unreachable  (into [] (remove reached) states)
        dangling     (into []
                           (comp (mapcat (fn [[from tos]] (map #(hash-map :from from :target %) tos)))
                                 (remove #(contains? valid-target (:target %))))
                           adjacency)
        missing?     (not (contains? states start))]
    {:ok?              (and (not missing?)
                            (empty? unreachable)
                            (empty? dangling)
                            (empty? dead))
     :start            start
     :states-explored  (count (:states graph))
     :truncated?       (:truncated? graph)
     :missing-start?   missing?
     :unreachable      unreachable
     :dangling-targets dangling
     :dead-ends        dead}))

(defn report-str
  "Failure report for an `analyze` result."
  [{:keys [states-explored truncated? missing-start? unreachable dangling-targets dead-ends]}]
  (str "explored " states-explored " states"
       (when truncated? " (TRUNCATED — bounds hit; nothing is proved beyond them)")
       (when missing-start? "\nMISSING START: :start is not a state of the spec")
       (when (seq unreachable)
         (str "\nUNREACHABLE:"
              (apply str (for [id unreachable] (str "\n  " id)))))
       (when (seq dangling-targets)
         (str "\nDANGLING TARGET:"
              (apply str (for [{:keys [from target]} dangling-targets]
                           (str "\n  " from " -> " target)))))
       (when (seq dead-ends)
         (str "\nDEAD END (no terminal reachable):"
              (apply str (for [{:keys [state path]} dead-ends]
                           (str "\n  " state " after " (pr-str path))))))))
