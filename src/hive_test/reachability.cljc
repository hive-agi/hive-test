(ns hive-test.reachability
  "Action reachability: is every mutating backend action reachable from a UI
   control? Compiler-style mark and sweep over an action graph, as data.

   An action graph is EDN an extractor produces (hive-cljs `cljs actions` for
   the front end, a carto detector for the route table):

     {:service      :inventory
      :roots        [{:id [:page :flowers]}]                    ; mounted FE pages
      :controls     [{:id :usage/confirm :root [:page :flowers]
                      :label \"Confirmar\" :dispatch [:flowers/record-usage]}]
      :events       {:flowers/record-usage
                     {:http     [{:method :post :path \"/api/flowers/usage\"}]
                      :dispatch [:flowers/fetch-all]}}
      :routes       [{:method :post :path \"/api/flowers/usage\"
                      :route-name :flowers/usage :handler 'inv.flowers/record-usage-handler
                      :domain :flowers :mutating? true}]
      :route-tables [{:id 'inv.flowers/routes :callers ['inv.service/routes]}] ; optional
      :waivers      [{:handler 'inv.flowers/legacy-handler :reason \"cron only\"}]}

   Every node may carry :src {:file :line :qn}, copied onto its verdicts.

   Mark (`reachable`) is the least fixpoint from the roots through their
   controls, the events they dispatch (following :dispatch chains, cycle-safe)
   and the http calls of those events, to the routes the calls hit. A call
   joins a route on method + path template: path segments are compared one by
   one, a BE `:id` segment matches any FE segment, a non-literal FE segment
   (`:x`, `{x}`, `*`, or a non-string element of a vector path) is :param and
   matches only a BE :param.

   Sweep (`verdicts`) judges each ACTION, identified by its handler qn so the
   aliases of one handler count once. Mutating is POST/PUT/PATCH/DELETE unless
   a route says :mutating? itself, minus the waived routes.

   No IO, no carto, no malli at runtime: hive-test.stateful's shape.

   Rationale: hive memory [[20261127194239-773ad1db]] (HIVE-ACTION-COVERAGE P0)."
  (:require [clojure.string :as str]
            [hive-test.contract :as mc]))

;; SPDX-License-Identifier: MIT

;; ── schemas ──────────────────────────────────────────────────────────────────
;; Plain data, so the contracts below read whether or not malli is present.

(def Method
  "An HTTP method, lower-case keyword."
  [:enum :get :post :put :patch :delete :head :options])

(def Qn
  "A qualified name: a handler, a route table, a source form."
  [:or :symbol :string :keyword])

(def Src
  "Where a node was extracted from."
  [:map [:file {:optional true} :string]
        [:line {:optional true} :int]
        [:qn {:optional true} Qn]])

(def FePath
  "A front-end call path: a string, or a vector whose non-string elements are
   the non-literal (runtime) parts, e.g. [\"/flowers/stock/\" :id]."
  [:or :string [:vector :any]])

(def Root
  [:map [:id :any] [:src {:optional true} Src]])

(def Control
  [:map [:id :any] [:root :any]
        [:label {:optional true} :string]
        [:dispatch [:vector :keyword]]
        [:src {:optional true} Src]])

(def HttpCall
  [:map [:method Method] [:path FePath]])

(def Event
  [:map [:http {:optional true} [:vector HttpCall]]
        [:dispatch {:optional true} [:vector :keyword]]
        [:src {:optional true} Src]])

(def Route
  [:map [:method Method] [:path :string] [:handler Qn]
        [:route-name {:optional true} :any]
        [:domain {:optional true} :any]
        [:mutating? {:optional true} :boolean]
        [:src {:optional true} Src]])

(def RouteTable
  [:map [:id Qn] [:callers [:vector Qn]] [:src {:optional true} Src]])

(def Waiver
  "Removes matching routes from the mutating set (and, by method + path or
   :event, matching calls from :broken). :reason is required."
  [:map [:reason [:string {:min 1}]]
        [:handler {:optional true} Qn]
        [:route-name {:optional true} :any]
        [:method {:optional true} Method]
        [:path {:optional true} :string]
        [:event {:optional true} :keyword]])

(def Graph
  "An action graph (see the ns docstring)."
  [:map [:service {:optional true} :any]
        [:roots [:vector Root]]
        [:controls [:vector Control]]
        [:events [:map-of :keyword Event]]
        [:routes [:vector Route]]
        [:route-tables {:optional true} [:vector RouteTable]]
        [:waivers {:optional true} [:vector Waiver]]])

(def VerdictKind
  [:enum :covered :missing-ui :dead-alias :orphan-event :broken :unmounted-table])

(def Verdict
  "One finding. :id names its subject (a handler, an event, a call, a route
   table) and, with :verdict, is the identity a baseline compares on."
  [:map [:verdict VerdictKind] [:id :any] [:severity [:enum :info :warning :error]]])

(def Reached
  [:map [:roots [:set :any]] [:controls [:set :any]] [:events [:set :keyword]]
        [:undefined-events [:set :keyword]]
        [:calls [:set :map]] [:routes [:set :map]]])

(def BaselineEntry
  "A baseline row: a verdict reduced to its identity (a full Verdict also fits)."
  [:map [:verdict VerdictKind] [:id :any]])

(def BaselineDiff
  "New failures, baseline entries now fixed, and the ones still present; :baseline
   is the tightened baseline (the old one minus :fixed)."
  [:map [:new [:vector BaselineEntry]] [:fixed [:vector BaselineEntry]]
        [:unchanged [:vector BaselineEntry]] [:baseline [:vector BaselineEntry]]])

(def Report
  [:map [:pass? :boolean] [:service :any]
        [:counts [:map-of :keyword :int]]
        [:blocking [:vector Verdict]]
        [:verdicts [:vector Verdict]]
        [:waived [:vector [:map [:route :map] [:reason :string]]]]
        [:baseline-diff BaselineDiff]])

(def Problems
  "`validate`'s answer: :problems names each malformed node by its path."
  [:map [:valid? :boolean] [:problems [:vector [:map [:path [:vector :any]] [:problem :string]]]]])

(def ReportOpts
  [:? [:maybe [:map [:baseline {:optional true} [:maybe [:sequential BaselineEntry]]]]]])

;; ── path join ────────────────────────────────────────────────────────────────

(def ^:private param-marker
  "Stands in for a non-string element of a vector FE path while it is rendered."
  "{*}")

(defn- param-segment?
  "True for a template segment that is not a literal: `:id` (pedestal, reitit),
   `{id}` (Go, OpenAPI), `*rest` (wildcards), or one holding a rendered
   non-literal part."
  [s]
  (or (str/starts-with? s ":")
      (str/starts-with? s "*")
      (str/includes? s "{")))

(defn- render-path
  "A path as one string: vector elements are concatenated, non-strings
   becoming the param marker. The query string is dropped."
  [path]
  (let [s (if (string? path)
            path
            (apply str (map #(if (string? %) % param-marker) path)))]
    (first (str/split s #"\?" 2))))

(defn segments
  "A path normalised to segments: literal segments stay strings, a non-literal
   one becomes :param. Accepts a template string (\"/flowers/stock/:id\") or an
   FE path vector ([\"/flowers/stock/\" id-form]); empty segments are dropped,
   so leading, trailing and doubled slashes do not matter."
  [path]
  (into []
        (comp (remove str/blank?)
              (map #(if (param-segment? %) :param %)))
        (str/split (render-path path) #"/")))

(defn- segments-match?
  "FE segments `fe` hit the BE template segments `be`: same length, a BE :param
   takes any FE segment, a BE literal only the equal FE literal."
  [fe be]
  (and (= (count fe) (count be))
       (every? true? (map (fn [f b] (or (= :param b) (= f b))) fe be))))

(defn serves?
  "True when `route` serves the FE `call`: the same method and a path that
   matches the route's template segment by segment."
  [route call]
  (and (= (:method route) (:method call))
       (segments-match? (segments (:path call)) (segments (:path route)))))

;; ── validation ───────────────────────────────────────────────────────────────
;; Hand-written, so it runs without malli (hive-test declares no malli dep).

(def ^:private http-methods #{:get :post :put :patch :delete :head :options})

(defn- qn? [x] (or (symbol? x) (string? x) (keyword? x)))

(defn- problem [path msg] {:path path :problem msg})

(defn- node-problems
  "Problems of one node `x` at `path`, from [pred msg] checks."
  [path x checks]
  (into [] (keep (fn [[ok? msg]] (when-not (ok? x) (problem path msg)))) checks))

(defn- coll-problems
  "Problems of each element of `xs` under `k`, checked by `f` (path, x)."
  [k xs f]
  (if (sequential? xs)
    (vec (mapcat (fn [i x] (f [k i] x)) (range) xs))
    [(problem [k] "must be a vector")]))

(defn- root-problems [path r]
  (node-problems path r [[map? "a root is a map"] [#(contains? % :id) "a root needs :id"]]))

(defn- control-problems [path c]
  (node-problems path c [[map? "a control is a map"]
                         [#(contains? % :id) "a control needs :id"]
                         [#(contains? % :root) "a control needs :root"]
                         [#(and (vector? (:dispatch %)) (every? keyword? (:dispatch %)))
                          ":dispatch is a vector of event keywords"]]))

(defn- call-problems [path call]
  (node-problems path call [[#(contains? http-methods (:method %)) "an http call needs a lower-case :method"]
                            [#(or (string? (:path %)) (vector? (:path %)))
                             "an http call :path is a string or a vector"]]))

(defn- event-problems [k e]
  (let [path [:events k]]
    (if-not (and (keyword? k) (map? e))
      [(problem path "an event is keyword -> map")]
      (concat (when-not (every? keyword? (:dispatch e))
                [(problem (conj path :dispatch) ":dispatch is a vector of event keywords")])
              (mapcat (fn [i c] (call-problems (conj path :http i) c)) (range) (:http e))))))

(defn- route-problems [path r]
  (node-problems path r [[#(contains? http-methods (:method %)) "a route needs a lower-case :method"]
                         [#(string? (:path %)) "a route :path is a template string"]
                         [#(qn? (:handler %)) "a route needs a :handler qn"]]))

(defn- waiver-problems [path w]
  (node-problems path w [[#(and (string? (:reason %)) (not (str/blank? (:reason %))))
                          "a waiver needs a non-blank :reason"]
                         [#(some (partial contains? %) [:handler :route-name :path :event])
                          "a waiver needs a selector: :handler, :route-name, :path or :event"]]))

(defn validate
  "Structural check of an action graph, no malli needed. Returns
   {:valid? bool :problems [{:path [...] :problem str}]}."
  [graph]
  (let [ps (if-not (map? graph)
             [(problem [] "a graph is a map")]
             (concat (coll-problems :roots (:roots graph) root-problems)
                     (coll-problems :controls (:controls graph) control-problems)
                     (if (map? (:events graph))
                       (mapcat (fn [[k e]] (event-problems k e)) (:events graph))
                       [(problem [:events] "must be a map")])
                     (coll-problems :routes (:routes graph) route-problems)
                     (coll-problems :waivers (or (:waivers graph) []) waiver-problems)))]
    {:valid? (empty? ps) :problems (vec ps)}))

;; ── mark ─────────────────────────────────────────────────────────────────────

(defn- closure
  "Least fixpoint: every node reachable from `seeds` by `succ` (node -> nodes).
   A worklist over a visited set, so cycles terminate."
  [seeds succ]
  (loop [todo (vec seeds) seen #{}]
    (if-let [x (peek todo)]
      (if (contains? seen x)
        (recur (pop todo) seen)
        (recur (into (pop todo) (remove seen) (succ x)) (conj seen x)))
      seen)))

(defn- event-calls
  "The http calls of event `k`, each tagged with :event."
  [events k]
  (mapv #(assoc % :event k) (get-in events [k :http])))

(defn reachable
  "Mark: the least fixpoint from the graph's roots. Returns
   {:roots #{id} :controls #{id} :events #{kw} :undefined-events #{kw}
    :calls #{{:event :method :path}} :routes #{route}}.
   A control is reached when its :root is; an event when a reached control or
   event dispatches it; a call when its event is reached; a route when it
   serves a reached call. :undefined-events are dispatched but not declared."
  [{:keys [roots controls events routes]}]
  (let [root-ids  (into #{} (map :id) roots)
        live-ctl  (filter #(contains? root-ids (:root %)) controls)
        evs       (closure (mapcat :dispatch live-ctl) #(get-in events [% :dispatch]))
        calls     (into #{} (mapcat #(event-calls events %)) evs)]
    {:roots            root-ids
     :controls         (into #{} (map :id) live-ctl)
     :events           (into #{} (filter #(contains? events %)) evs)
     :undefined-events (into #{} (remove #(contains? events %)) evs)
     :calls            calls
     :routes           (into #{} (filter (fn [r] (some #(serves? r %) calls))) routes)}))

;; ── sweep ────────────────────────────────────────────────────────────────────

(def ^:private mutating-methods #{:post :put :patch :delete})

(def verdict-order
  "Verdict kinds in report order; also the :severity each one carries."
  {:missing-ui      [0 :error]
   :broken          [1 :error]
   :orphan-event    [2 :warning]
   :dead-alias      [3 :warning]
   :unmounted-table [4 :warning]
   :covered         [5 :info]})

(defn- mutating?
  "A route's own :mutating? wins; otherwise its method decides."
  [route]
  (if (contains? route :mutating?)
    (boolean (:mutating? route))
    (contains? mutating-methods (:method route))))

(defn- waiver-matches-route?
  "Every selector the waiver names agrees with the route; :event selects calls,
   not routes."
  [w route]
  (and (not (contains? w :event))
       (some (partial contains? w) [:handler :route-name :path])
       (every? (fn [k] (or (not (contains? w k))
                           (if (= k :path)
                             (= (segments (:path w)) (segments (:path route)))
                             (= (get w k) (get route k)))))
               [:handler :route-name :method :path])))

(defn- waiver-matches-call?
  "A waiver silences a broken call by :event, or by method + path."
  [w call]
  (or (and (contains? w :event) (= (:event w) (:event call))
           (or (not (contains? w :method)) (= (:method w) (:method call))))
      (and (not (contains? w :event)) (not (contains? w :handler))
           (contains? w :path) (= (:method w) (:method call))
           (= (segments (:path w)) (segments (:path call))))))

(defn- route-waiver [waivers route] (first (filter #(waiver-matches-route? % route) waivers)))

(defn- route-ref
  "A route as reported: method, path, name and source."
  [route]
  (select-keys route [:method :path :route-name :src]))

(defn- verdict [kind id m]
  (merge {:verdict kind :id id :severity (second (verdict-order kind))} m))

(defn- sort-key [v] [(first (verdict-order (:verdict v))) (pr-str (:id v))])

(defn- serving-handlers [routes call]
  (into (sorted-set-by #(compare (pr-str %1) (pr-str %2)))
        (comp (filter #(serves? % call)) (map :handler)) routes))

(defn- action-verdicts
  "Per handler: :covered when any of its routes is reached (and :dead-alias for
   each unreached route of it), :missing-ui when none is and it has a mutating,
   unwaived route. Unreached reads and fully waived actions get no verdict."
  [{:keys [routes events waivers]} {reached-routes :routes reached-events :events}]
  (let [orphan-calls (for [[k _] events :when (not (contains? reached-events k))
                           c (event-calls events k)]
                       c)]
    (mapcat
     (fn [[handler rs]]
       (let [hit    (filter #(contains? reached-routes %) rs)
             live   (filter #(and (mutating? %) (not (route-waiver waivers %))) rs)
             via    (into (sorted-set) (comp (filter (fn [c] (some #(serves? % c) hit))) (map :event))
                          (mapcat #(event-calls events %) reached-events))
             orphan (into (sorted-set) (comp (filter (fn [c] (some #(serves? % c) rs))) (map :event))
                          orphan-calls)]
         (cond
           (seq hit)
           (cons (verdict :covered handler {:routes (mapv route-ref hit) :via (vec via)})
                 (for [r rs :when (not (contains? reached-routes r))]
                   (verdict :dead-alias {:handler handler :method (:method r) :path (:path r)}
                            {:route (route-ref r) :covered-by (mapv route-ref hit)})))

           (seq live)
           [(verdict :missing-ui handler (cond-> {:routes (mapv route-ref live)}
                                           (seq orphan) (assoc :orphan-events (vec orphan))))]

           :else nil)))
     (group-by :handler routes))))

(defn- orphan-event-verdicts
  "Declared events with http effects no reached control leads to."
  [{:keys [events routes]} {reached-events :events}]
  (for [[k e] events
        :when (and (seq (:http e)) (not (contains? reached-events k)))]
    (verdict :orphan-event k
             (cond-> {:http (mapv #(select-keys % [:method :path]) (:http e))
                      :handlers (vec (into (sorted-set-by #(compare (pr-str %1) (pr-str %2)))
                                           (mapcat #(serving-handlers routes %))
                                           (event-calls events k)))}
               (:src e) (assoc :src (:src e))))))

(defn- broken-verdicts
  "Every declared FE call no route serves, reached or not; :reached? tells which."
  [{:keys [events routes waivers]} {reached-events :events}]
  (for [[k _] events
        c (event-calls events k)
        :when (and (not-any? #(serves? % c) routes)
                   (not-any? #(waiver-matches-call? % c) waivers))]
    (verdict :broken (select-keys c [:event :method :path])
             (cond-> {:segments (segments (:path c))
                      :reached? (contains? reached-events k)}
               (get-in events [k :src]) (assoc :src (get-in events [k :src]))))))

(defn- unmounted-table-verdicts
  "Route tables the extractor found no caller for (optional input)."
  [{:keys [route-tables]}]
  (for [{:keys [id callers src]} route-tables
        :when (empty? callers)]
    (verdict :unmounted-table id (cond-> {} src (assoc :src src)))))

(defn verdicts
  "Sweep: every finding over `graph`, sorted by severity then id. Each is
   {:verdict kind :id subject :severity :error|:warning|:info ...}:
   :covered / :missing-ui (id = handler qn), :dead-alias (id = {:handler
   :method :path}), :orphan-event (id = event), :broken (id = {:event :method
   :path}), :unmounted-table (id = route table). Waived routes never yield
   :missing-ui; `report` lists them under :waived."
  [graph]
  (let [g       (update graph :waivers #(or % []))
        reached (reachable g)]
    (vec (sort-by sort-key
                  (concat (action-verdicts g reached)
                          (orphan-event-verdicts g reached)
                          (broken-verdicts g reached)
                          (unmounted-table-verdicts g))))))

;; ── ratchet ──────────────────────────────────────────────────────────────────

(def blocking-kinds
  "Verdict kinds that fail `report` unless the baseline already holds them."
  #{:missing-ui :broken})

(defn- identity-of [v] (select-keys v [:verdict :id]))

(defn baseline-diff
  "Compare the failures in `current` (verdicts, only non-:covered ones count)
   against `baseline` (verdicts or {:verdict :id} rows). Returns
   {:new       failures absent from the baseline
    :fixed     baseline entries no longer present (auto-tighten)
    :unchanged failures present in both
    :baseline  the tightened baseline: the old one minus :fixed}.
   Rows are reduced to {:verdict :id} and sorted."
  [baseline current]
  (let [failing (into #{} (comp (remove #(= :covered (:verdict %))) (map identity-of)) current)
        base    (into #{} (map identity-of) baseline)
        sorted  #(vec (sort-by sort-key %))]
    {:new       (sorted (remove base failing))
     :fixed     (sorted (remove failing base))
     :unchanged (sorted (filter base failing))
     :baseline  (sorted (filter failing base))}))

(defn report
  "The gate over `graph`: {:pass? :service :counts :blocking :verdicts :waived
   :baseline-diff}. :pass? is false when any :missing-ui or :broken verdict is
   not in the baseline (waived routes never produce one). opts :baseline is the
   previous failure set (default none: every blocking verdict fails)."
  [graph & [{:keys [baseline]}]]
  (let [vs       (verdicts graph)
        diff     (baseline-diff baseline vs)
        new-ids  (set (:new diff))
        blocking (filterv #(and (contains? blocking-kinds (:verdict %))
                                (contains? new-ids (identity-of %)))
                          vs)
        waivers  (or (:waivers graph) [])]
    {:pass?         (empty? blocking)
     :service       (:service graph)
     :counts        (into (sorted-map) (frequencies (map :verdict vs)))
     :blocking      blocking
     :verdicts      vs
     :waived        (vec (for [r (:routes graph)
                               :let [w (route-waiver waivers r)]
                               :when (and w (mutating? r))]
                           {:route (assoc (route-ref r) :handler (:handler r)) :reason (:reason w)}))
     :baseline-diff diff}))

;; Contracts: `mc/=>` registers each only when malli is on the classpath
;; (see hive-test.contract); hive-test itself never depends on malli.

(mc/=> segments      [:=> [:cat FePath] [:vector [:or :string [:= :param]]]])
(mc/=> serves?       [:=> [:cat Route HttpCall] :boolean])
(mc/=> validate      [:=> [:cat :any] Problems])
(mc/=> reachable     [:=> [:cat Graph] Reached])
(mc/=> verdicts      [:=> [:cat Graph] [:vector Verdict]])
(mc/=> baseline-diff [:=> [:cat [:maybe [:sequential BaselineEntry]] [:sequential Verdict]] BaselineDiff])
(mc/=> report        [:=> [:cat Graph ReportOpts] Report])
