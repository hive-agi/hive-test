(ns hive-test.stateful-test
  "A reconciliation model: one case, two sibling items, each with an invoice and a
   receipt. The operator's gestures are the commands.

   Three machines: without the schedule gesture (liveness fails), with it (passes),
   and with an unguarded attach (safety fails). Rationale: [[20260714012524-6370a2c4]]."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.stateful :as sf]
            [hive-test.trifecta :as tri]))

(def ^:private papers
  #{[:invoice "01"] [:invoice "02"] [:receipt "01"] [:receipt "02"]})

(defn- init []
  {:pending  papers
   :invoices #{}      ; uploaded, unattached
   :receipts #{}      ; uploaded, unallocated
   :items    {}})     ; item -> {:invoice <item|nil> :status :open|:settled}

(def ^:private upload
  {:args (fn [m] (vec (:pending m)))
   :pre  (fn [m paper] (contains? (:pending m) paper))
   :next (fn [m [kind item :as paper]]
           (-> m
               (update :pending disj paper)
               (update (if (= :invoice kind) :invoices :receipts) conj item)))})

(defn- schedule-from-invoice
  "Opens the item the invoice bills. Opening and attaching are distinct: an item may
   also be opened by an upstream import."
  []
  {:args (fn [m] (vec (:invoices m)))
   :pre  (fn [m i] (and (contains? (:invoices m) i)
                        (not (contains? (:items m) i))))
   :next (fn [m i] (assoc-in m [:items i] {:invoice nil :status :open}))})

(defn- attach
  "Attaches an invoice to an open item. `same-item?` false drops the guard."
  [{:keys [same-item?]}]
  {:args (fn [m] (for [i (:invoices m), k (keys (:items m))] [i k]))
   :pre  (fn [m [i k]]
           (and (contains? (:invoices m) i)
                (contains? (:items m) k)
                (nil? (get-in m [:items k :invoice]))
                (or (not same-item?) (= i k))))
   :next (fn [m [i k]]
           (-> m
               (update :invoices disj i)
               (assoc-in [:items k :invoice] i)))})

(defn- confirm
  "Allocates a receipt to an item that already has its invoice — the pair closes."
  [{:keys [same-item?]}]
  {:args (fn [m] (for [r (:receipts m), k (keys (:items m))] [r k]))
   :pre  (fn [m [r k]]
           (and (contains? (:receipts m) r)
                (get-in m [:items k :invoice])
                (or (not same-item?) (= r k))))
   :next (fn [m [r k]]
           (-> m
               (update :receipts disj r)
               (assoc-in [:items k :status] :settled)))})

(def ^:private no-cross-item-leak
  (fn [m] (every? (fn [[item {:keys [invoice]}]] (or (nil? invoice) (= item invoice)))
                  (:items m))))

(def ^:private all-settled
  (fn [m] (and (= 2 (count (:items m)))
               (every? (fn [[_ item]] (= :settled (:status item))) (:items m)))))

(defn- machine [{:keys [schedule? same-item?]}]
  {:init       init
   :commands   (cond-> {:upload  upload
                        :attach  (attach {:same-item? same-item?})
                        :confirm (confirm {:same-item? same-item?})}
                 schedule? (assoc :schedule-from-invoice (schedule-from-invoice)))
   :invariants {:no-cross-item-leak no-cross-item-leak}
   :goals      {:all-settled all-settled}})

(deftest without-the-schedule-gesture-nothing-can-ever-settle
  (testing "liveness: no invariant is broken, yet every reachable state is a dead end"
    (let [r (sf/check (machine {:schedule? false :same-item? true}))]
      (is (false? (:ok? r)))
      (is (empty? (:invariant-violations r)))
      (is (seq (:dead-ends r)))
      (is (vector? (:path (first (:dead-ends r))))))))

(deftest with-the-schedule-gesture-the-board-closes
  (let [r (sf/check (machine {:schedule? true :same-item? true}))]
    (is (true? (:ok? r)) (sf/report-str r))
    (is (empty? (:dead-ends r)))
    (is (false? (:truncated? r)))))

(deftest unguarded-attach-leaks-between-sibling-items
  (testing "safety: the unguarded attach books item 02's invoice on item 01"
    (let [r (sf/check (machine {:schedule? true :same-item? false}))
          v (first (:invariant-violations r))]
      (is (false? (:ok? r)))
      (is (= :no-cross-item-leak (:invariant v)))
      (is (seq (:path v)))
      (testing "and the leak also strands the sibling: its invoice is spent"
        (is (seq (:dead-ends r)))))))

(deftest vacuity-is-reported-instead-of-passing
  (let [r (sf/check {:init     (fn [] {:items {}})
                     :commands {}
                     :goals    {:trivial (constantly true)}})]
    (is (false? (:ok? r)))
    (is (seq (:vacuity r)))))

(tri/deftest-facets reconciliation-via-trifecta
  hive-test.stateful/check
  {:type          :property
   :property-type :stateful
   :machine       (machine {:schedule? true :same-item? true})})
