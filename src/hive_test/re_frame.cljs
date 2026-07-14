(ns hive-test.re-frame
  "Adapter: a re-frame app as a hive-test.stateful Machine.

   re-frame is a PROVIDED dependency — this namespace is only loaded by a build that
   already has it. hive-test itself does not depend on re-frame.

   The model is {:db app-db :fx [effect ...] :world w}: the db as the registered
   handlers see it, the EXTERNAL effects nobody has answered yet, and the world that
   answers them (a server, a clock — whatever is not the app).

     (machine
       {:init       (fn [] db0)
        :world      (fn [] world0)                       ; optional
        :events     {:cmd-id {:args (fn [model] [[:evt-id arg] ...])   ; event vectors
                              :pre  (fn [model event-v] boolean)}}
        :reply      (fn [model effect] [[event-v world'] ...])  ; how the world answers
        :external?  (fn [[k v]] boolean)                 ; default: not a re-frame builtin
        :invariants {:name (fn [model] boolean)}
        :goals      {:name (fn [model] boolean)}
        :abstract   (fn [model] view)
        :fx-ident   (fn [[k v]] value)})

   A step runs to completion: re-frame's own :fx/:dispatch are drained in order, as
   re-frame drains them. Only external effects (http, uploads) are parked in :fx, and
   the world answering one is itself a command — so exploration covers every
   interleaving of operator gestures and server replies. An effect the world has no
   reply for is dropped.

   `:external?` sees the whole effect, not just its key: park only what the world
   models. Every parked effect doubles the outstanding-request subsets the search must
   walk, so parking requests whose replies the model ignores buys nothing and costs a
   factor of two each.

   `:abstract` is the state identity: project the model to the slice under test, or the
   space grows with every unrelated flag (a toast carrying a random uuid and a
   timestamp is a new state forever).

   `:fx-ident` is the same for a parked effect, and it is NOT optional in practice: an
   effect map carrying closures (cljs-ajax puts its format fns in the :http-xhrio map)
   compares by identity, so two identical requests are never equal and the visited set
   never dedupes. Project the effect to the value that identifies it (method, uri,
   params, on-success). Parked effects are compared as a multiset — the order in which
   two independent requests are outstanding is not a different state.

   Rationale: hive memory [[20260714012524-6370a2c4]]."
  (:require [hive-test.stateful :as sf]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]))

(def ^:private impure-ids
  "Interceptors to drop: :coeffects reads the GLOBAL app-db (that is what inject-db
   is registered as), :do-fx RUNS the effects for real."
  #{:coeffects :do-fx})

(def ^:private builtin-fx
  "Effects re-frame itself interprets — the adapter drains these, the world does not."
  #{:db :fx :dispatch :dispatch-n :dispatch-later :deregister-event-handler})

(defn- seed-db [db]
  (interceptor/->interceptor
   :id     ::seed-db
   :before (fn [ctx] (interceptor/assoc-coeffect ctx :db db))))

(defn- handler-chain [event-id]
  (->> (registrar/get-handler :event event-id true)
       flatten
       (remove (fn [i] (contains? impure-ids (:id i))))))

(defn effects
  "The effects the registered handler for `event-v` ASKS for, given the db value `db`.
   Runs none of them. Returns the raw effects map (:db included when the handler set it)."
  [db event-v]
  (-> (interceptor/execute event-v (into [(seed-db db)] (handler-chain (first event-v))))
      :effects))

(defn- run-event
  "Apply one event to `db`. Returns {:db db' :queue [event-v ...] :fx [[k v] ...]}:
   re-frame's own dispatches go to :queue, external effects to :fx, the rest is dropped."
  [db event-v external?]
  (let [fx    (effects db event-v)
        pairs (concat (when-let [d (:dispatch fx)] [[:dispatch d]])
                      (map (fn [d] [:dispatch d]) (:dispatch-n fx))
                      (filter some? (:fx fx))
                      (dissoc fx :db :fx :dispatch :dispatch-n))]
    {:db    (get fx :db db)
     :queue (vec (keep (fn [[k v]] (when (= :dispatch k) v)) pairs))
     :fx    (vec (filter (fn [[k _ :as e]] (and (not= :dispatch k) (external? e))) pairs))}))

(defn- park
  "Add effects to the outstanding set, identified by `fx-id`. An effect already in
   flight is not parked twice: re-issuing the same request while it is outstanding is
   the same state, and treating it as a new one makes :fx grow without bound (the
   state space becomes infinite, and every search truncates)."
  [fx-id outstanding fresh]
  (reduce (fn [acc fx]
            (if (some (fn [p] (= (fx-id p) (fx-id fx))) acc)
              acc
              (conj acc fx)))
          (vec outstanding)
          fresh))

(defn step
  "Apply `event-v` to the model and drain re-frame's own dispatches to completion.
   Returns the model with :db advanced and external effects parked in :fx."
  [{:keys [db] :as model} event-v {:keys [external? fx-id]}]
  (loop [db db, queue [event-v], parked []]
    (if-let [ev (first queue)]
      (let [r (run-event db ev external?)]
        (recur (:db r)
               (into (vec (rest queue)) (:queue r))
               (into parked (:fx r))))
      (-> model
          (assoc :db db)
          (update :fx (fn [out] (park fx-id out parked)))))))

(defn- gesture-command [ctx {:keys [args pre]}]
  {:args (fn [model] (vec (args model)))
   :pre  (fn [model event-v] (or (nil? pre) (pre model event-v)))
   :next (fn [model event-v] (step model event-v ctx))})

(defn- reply-command
  "The world answering a parked effect. Enabled iff some parked effect has a reply."
  [ctx reply]
  {:args (fn [model]
           (for [fx             (:fx model)
                 [event world'] (or (reply model fx) [])]
             [fx event world']))
   :pre  (fn [model [fx _ _]] (boolean (some #(= fx %) (:fx model))))
   :next (fn [model [fx event-v world']]
           (-> model
               (update :fx (fn [q] (vec (remove #(= fx %) q))))
               (assoc :world world')
               (step event-v ctx)))})

(defn- unanswerable [reply model]
  (vec (remove (fn [fx] (seq (reply model fx))) (:fx model))))

(defn- drop-command
  "The world discarding, in one step, every parked effect it has no reply for. One step
   and not one per effect: N unanswerable effects would otherwise contribute N!
   orderings of states that differ only in what has already been thrown away."
  [reply]
  {:args (fn [model] (if (seq (unanswerable reply model)) [::all] []))
   :pre  (fn [model _] (boolean (seq (unanswerable reply model))))
   :next (fn [model _]
           (let [dead (set (unanswerable reply model))]
             (update model :fx (fn [q] (vec (remove dead q))))))})

(defn machine
  "Build a hive-test.stateful Machine from a registered re-frame app. See the ns
   docstring. Invariants and goals receive the whole model {:db :fx :world}."
  [{:keys [init world events reply external? invariants goals abstract fx-ident]}]
  (let [ext?  (or external? (fn [[k _]] (not (contains? builtin-fx k))))
        view  (or abstract :db)
        fx-id (or fx-ident identity)
        ctx   {:external? ext? :fx-id fx-id}]
    (cond-> {:init       (fn [] {:db (init) :fx [] :world (when world (world))})
             :commands   (into {} (map (fn [[id spec]] [id (gesture-command ctx spec)])) events)
             :invariants (or invariants {})
             :goals      (or goals {})
             :ident      (fn [model]
                           [(view model)
                            (frequencies (map fx-id (:fx model)))
                            (:world model)])}
      reply (-> (assoc-in [:commands ::reply] (reply-command ctx reply))
                (assoc-in [:commands ::drop]  (drop-command reply))))))

(defn check
  "Explore the re-frame Machine. Same result shape as hive-test.stateful/check."
  [spec & [opts]]
  (sf/check (machine spec) opts))
