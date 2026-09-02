(ns hive-test.memory.store-contract-test
  "Self-verification for the shipped conformance kit.

   A conformance kit that nothing exercises is a kit nobody knows works. Two
   claims are checked here, and the second is the one that matters:

     1. the kit PASSES against a conforming in-memory store, so a green run in
        a backend repo means something;
     2. the kit FAILS against a store that violates a named clause, so a green
        run is not green-by-omission.

   The stub is deliberately small and lives here rather than being imported: it
   is the reference reading of the contract, not a backend."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test :refer [deftest is testing]]
            [hive-spi.memory.ports :as ports]
            [hive-test.memory.store-contract :as contract]))

;; =============================================================================
;; Expiry
;; =============================================================================

(defn- parse-instant
  "Parse an ISO-8601 timestamp to an Instant, or nil when unparseable."
  [s]
  (when (string? s)
    (try
      (java.time.Instant/parse s)
      (catch Exception _
        (try
          (.toInstant (java.time.ZonedDateTime/parse s))
          (catch Exception _ nil))))))

(defn- expired?
  "True when `entry` carries a past :expires-at and is not :permanent."
  [entry now]
  (and (not= :permanent (:duration entry))
       (when-let [t (parse-instant (:expires-at entry))]
         (.isBefore t now))))

;; =============================================================================
;; A conforming in-memory store
;; =============================================================================

(defn- tokens [s]
  (set (str/split (str/lower-case (str s)) #"\W+")))

(defrecord StubStore [state]
  ports/IMemoryStore
  (connect! [_ _config]
    (swap! state assoc :connected? true)
    {:success? true})
  (disconnect! [_]
    (swap! state assoc :connected? false)
    {:success? true :errors []})
  (connected? [_] (boolean (:connected? @state)))
  (health-check [_] {:healthy? true :backend "stub" :entry-count (count (:entries @state))})

  (add-entry! [_ entry]
    (swap! state assoc-in [:entries (:id entry)] entry)
    (:id entry))
  (get-entry [_ id] (get-in @state [:entries id]))
  (update-entry! [_ id updates]
    (swap! state update-in [:entries id] merge updates)
    (get-in @state [:entries id]))
  (delete-entry! [_ id]
    (swap! state update :entries dissoc id)
    true)

  (query-entries [_ opts]
    (cond->> (vals (:entries @state))
      (:type opts) (filter #(= (:type opts) (:type %)))
      true         (take (or (:limit opts) 100))
      true         vec))
  (search-similar [_ query-text opts]
    (let [q (tokens query-text)]
      (->> (vals (:entries @state))
           (filter #(seq (set/intersection q (tokens (:content %)))))
           (take (or (:limit opts) 10))
           vec)))
  (supports-semantic-search? [_] true)

  (cleanup-expired! [_]
    (let [now  (java.time.Instant/now)
          gone (->> (vals (:entries @state))
                    (filter #(expired? % now))
                    (mapv :id))]
      (swap! state update :entries #(apply dissoc % gone))
      {:count (count gone) :deleted-ids gone}))
  (entries-expiring-soon [_ _days _opts]
    (vec (filter :expires-at (vals (:entries @state)))))
  (find-duplicate [_ type content-hash _opts]
    (some (fn [e] (when (and (= type (:type e))
                             (= content-hash (:content-hash e)))
                    (:id e)))
          (vals (:entries @state))))

  (store-status [_]
    {:backend "stub" :configured? true :entry-count (count (:entries @state))
     :supports-search? true})
  (reset-store! [_]
    (swap! state assoc :entries {})
    {:success? true})

  ports/IMemoryStoreWithAnalytics
  (log-access! [_ id]
    (swap! state update-in [:entries id :access-count] (fnil inc 0))
    true)
  (record-feedback! [_ id feedback]
    (let [k (if (= :helpful feedback) :helpful-count :unhelpful-count)]
      (swap! state update-in [:entries id k] (fnil inc 0))
      true))
  (get-helpfulness-ratio [_ id]
    (let [e (get-in @state [:entries id])
          h (or (:helpful-count e) 0)
          u (or (:unhelpful-count e) 0)
          t (+ h u)]
      {:helpful-count h :unhelpful-count u :total t
       :ratio (if (zero? t) 0.0 (/ (double h) t))}))

  ports/IMemoryStoreWithStaleness
  (update-staleness! [_ id staleness-opts]
    (swap! state update-in [:entries id] merge staleness-opts)
    true)
  (get-stale-entries [_ threshold _opts]
    (->> (vals (:entries @state))
         (filter (fn [e]
                   (let [a (:staleness-alpha e) b (:staleness-beta e)]
                     (and a b (pos? (+ a b))
                          (> (/ (double b) (+ a b)) threshold)))))
         vec))
  (propagate-staleness! [_ _source-id _depth] 0))

(defn- ->stub [] (->StubStore (atom {:entries {} :connected? false})))

;; =============================================================================
;; A store that violates ONE clause: delete-entry! does not delete
;; =============================================================================

(defrecord LeakyDeleteStore [inner]
  ports/IMemoryStore
  (connect! [_ config] (ports/connect! inner config))
  (disconnect! [_] (ports/disconnect! inner))
  (connected? [_] (ports/connected? inner))
  (health-check [_] (ports/health-check inner))
  (add-entry! [_ entry] (ports/add-entry! inner entry))
  (get-entry [_ id] (ports/get-entry inner id))
  (update-entry! [_ id updates] (ports/update-entry! inner id updates))
  (delete-entry! [_ _id] true)                              ; the violation
  (query-entries [_ opts] (ports/query-entries inner opts))
  (search-similar [_ q opts] (ports/search-similar inner q opts))
  (supports-semantic-search? [_] (ports/supports-semantic-search? inner))
  (cleanup-expired! [_] (ports/cleanup-expired! inner))
  (entries-expiring-soon [_ d opts] (ports/entries-expiring-soon inner d opts))
  (find-duplicate [_ t h opts] (ports/find-duplicate inner t h opts))
  (store-status [_] (ports/store-status inner))
  (reset-store! [_] (ports/reset-store! inner)))

;; =============================================================================
;; Isolated runner
;; =============================================================================

(defn- run-isolated
  "Invoke contract test fn `f` against `factory` with its own report counters.
   Returns {:pass n :fail n :error n :out s}. All output is swallowed, so a
   deliberate failure does not read as a suite failure."
  [factory f]
  (let [w        (java.io.StringWriter.)
        counters (ref {:test 0 :pass 0 :fail 0 :error 0})]
    (binding [contract/*store-factory* factory
              test/*report-counters*   counters
              test/*testing-vars*      (list)
              test/*test-out*          w
              *out*                    w
              *err*                    w]
      (try (f)
           (catch Throwable _ (dosync (alter counters update :error inc)))))
    (assoc @counters :out (str w))))

(defn- failed?
  "True when the isolated run reported anything but success.

   A clojure.test.check property failure arrives at clojure.test/report with
   :type ::shrunk, which has no report method and so increments NO counter. The
   counters alone would read a failing property as a pass, so the captured
   output is checked too."
  [{:keys [fail error out]}]
  (or (pos? (+ fail error))
      (str/includes? out "shrunk")))

;; =============================================================================
;; 1. The kit passes against a conforming store
;; =============================================================================

(def ^:private contract-tests
  "Every deftest var in the kit. Read off the namespace so a test added to the
   kit is covered here without editing this list."
  (->> (ns-publics 'hive-test.memory.store-contract)
       (filter (fn [[_ v]] (:test (meta v))))
       (sort-by key)))

(deftest kit-passes-against-a-conforming-store
  (testing "the kit has tests to run"
    (is (seq contract-tests)))
  (doseq [[sym v] contract-tests]
    (let [result (run-isolated ->stub @v)]
      (testing (str sym)
        (is (not (failed? result))
            (str sym " did not pass against the conforming stub: "
                 (select-keys result [:fail :error]) " " (:out result)))))))

;; =============================================================================
;; 2. The kit fails against a store that violates a clause
;; =============================================================================

(deftest kit-catches-a-delete-that-does-not-delete
  (let [factory #(->LeakyDeleteStore (->stub))
        result  (run-isolated factory
                              @(ns-resolve 'hive-test.memory.store-contract
                                           'test-add-delete-get))]
    (testing "test-add-delete-get reports a failure when delete is a no-op"
      (is (failed? result)
          "the kit passed a store whose delete-entry! does nothing, so it is not testing deletion"))))

;; =============================================================================
;; 3. Skipping is explicit, not accidental
;; =============================================================================

(deftest optional-protocol-probes-are-honest
  (let [store (->stub)]
    (testing "the conforming stub advertises both optional protocols"
      (is (contract/analytics-store? store))
      (is (contract/staleness-store? store)))))
