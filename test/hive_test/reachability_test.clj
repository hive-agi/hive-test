(ns hive-test.reachability-test
  "hive-test.reachability over the inventory flowers action graph as measured on
   main 5ab759f (hand-written from the HIVE-ACTION-COVERAGE acceptance data),
   the same graph with the two missing controls added, and properties of the
   fixpoint: monotone in controls, terminating on :dispatch cycles, and a waived
   route never yielding :missing-ui. Plan: [[20261127194239-773ad1db]]."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.reachability :as r]
            [clojure.set :as set]))

;; ── fixture: inventory flowers on main 5ab759f ───────────────────────────────

(def ^:private h
  "Handler qns of inventory's flowers domain."
  {:create-item   'inventory.flowers.handlers/create-flower-item-handler
   :list-items    'inventory.flowers.handlers/list-flower-items-handler
   :add-stock     'inventory.flowers.handlers/add-stock-handler
   :list-stock    'inventory.flowers.handlers/list-stock-handler
   :update-stock  'inventory.flowers.handlers/update-stock-handler
   :delete-stock  'inventory.flowers.handlers/delete-stock-handler
   :suppliers     'inventory.flowers.handlers/create-supplier-handler
   :usage         'inventory.flowers.handlers/record-usage-handler
   :purchase      'inventory.flowers.handlers/record-purchase-handler
   :usage-report  'inventory.flowers.handlers/usage-report-handler})

(defn- src [line] {:file "src/inventory/flowers/events.cljs" :line line})

(def flowers-5ab759f
  "Roots, controls, events and routes of the flowers pages on main 5ab759f."
  {:service  :inventory
   :roots    [{:id [:page :flowers]}
              {:id [:page :flowers-reports]}]
   :controls [{:id :flowers/mount :root [:page :flowers] :dispatch [:flowers/fetch-all]}
              {:id :flowers/filter :root [:page :flowers] :label "Filtrar"
               :dispatch [:flowers/set-filter]}
              {:id :flowers/usage-open :root [:page :flowers] :label "Registrar uso"
               :dispatch [:flowers/open-usage-modal]}
              {:id :flowers/usage-confirm :root [:page :flowers] :label "Confirmar"
               :dispatch [:flowers/record-usage]}
              {:id :flowers/usage-cancel :root [:page :flowers] :label "Cancelar"
               :dispatch [:flowers/close-modal]}
              {:id :flowers-reports/mount :root [:page :flowers-reports]
               :dispatch [:flowers/fetch-usage-report]}]
   :events   {:flowers/fetch-all         {:http [{:method :get :path "/api/flowers/items"}
                                                 {:method :get :path "/api/flowers/stock"}]
                                          :src (src 12)}
              :flowers/set-filter        {:dispatch [:flowers/fetch-all] :src (src 40)}
              :flowers/open-usage-modal  {:src (src 52)}
              :flowers/close-modal       {:src (src 60)}
              :flowers/record-usage      {:http [{:method :post :path "/api/flowers/usage"}]
                                          :dispatch [:flowers/close-modal :flowers/fetch-all]
                                          :src (src 70)}
              :flowers/fetch-usage-report {:http [{:method :get :path "/api/flowers/usage/report"}]
                                           :src (src 90)}
              :flowers/create-stock      {:http [{:method :post :path "/flowers/stock"}]
                                          :dispatch [:flowers/fetch-all] :src (src 120)}
              :flowers/update-stock      {:http [{:method :put :path ["/flowers/stock/" 'id]}]
                                          :dispatch [:flowers/fetch-all] :src (src 135)}
              :flowers/delete-stock      {:http [{:method :delete :path ["/flowers/stock/" 'id]}]
                                          :dispatch [:flowers/fetch-all] :src (src 150)}
              :flowers/create-supplier   {:http [{:method :post :path "/flowers/suppliers"}]
                                          :src (src 162)}
              :flowers/create-purchase   {:http [{:method :post :path "/api/flowers/purchases"}]
                                          :dispatch [:flowers/fetch-all] :src (src 175)}}
   :routes   [{:method :get :path "/api/flowers/items" :route-name :flowers/list-items
               :handler (:list-items h) :domain :flowers}
              {:method :post :path "/api/flowers/items" :route-name :flowers/create-item
               :handler (:create-item h) :domain :flowers}
              {:method :post :path "/flowers" :route-name :flowers/create-item-legacy
               :handler (:create-item h) :domain :flowers}
              {:method :get :path "/api/flowers/stock" :route-name :flowers/list-stock
               :handler (:list-stock h) :domain :flowers}
              {:method :post :path "/api/flowers/stock" :route-name :flowers/add-stock
               :handler (:add-stock h) :domain :flowers}
              {:method :post :path "/flowers/stock" :route-name :flowers/add-stock-legacy
               :handler (:add-stock h) :domain :flowers}
              {:method :put :path "/flowers/stock/:id" :route-name :flowers/update-stock
               :handler (:update-stock h) :domain :flowers}
              {:method :delete :path "/flowers/stock/:id" :route-name :flowers/delete-stock
               :handler (:delete-stock h) :domain :flowers}
              {:method :post :path "/flowers/suppliers" :route-name :flowers/create-supplier
               :handler (:suppliers h) :domain :flowers}
              {:method :post :path "/api/flowers/usage" :route-name :flowers/record-usage
               :handler (:usage h) :domain :flowers}
              {:method :get :path "/api/flowers/usage/report" :route-name :flowers/usage-report
               :handler (:usage-report h) :domain :flowers}
              {:method :post :path "/api/flowers/purchases" :route-name :flowers/record-purchase
               :handler (:purchase h) :domain :flowers}]})

(def flowers-with-controls
  "The same graph plus the \"Nova flor\" and \"Registrar compra\" controls, and
   the create-item event the first one dispatches."
  (-> flowers-5ab759f
      (update :controls conj
              {:id :flowers/new-item :root [:page :flowers] :label "Nova flor"
               :dispatch [:flowers/create-item]}
              {:id :flowers/purchase :root [:page :flowers] :label "Registrar compra"
               :dispatch [:flowers/create-purchase]})
      (assoc-in [:events :flowers/create-item]
                {:http [{:method :post :path "/api/flowers/items"}]
                 :dispatch [:flowers/fetch-all]})))

(defn- verdict-of
  "The :verdict `verdicts` gives subject `id`, or nil."
  [vs id]
  (some #(when (= id (:id %)) (:verdict %)) vs))

;; ── golden + acceptance ──────────────────────────────────────────────────────

(deftest-golden flowers-on-main "test/golden/reachability/flowers-5ab759f.edn"
  (let [rep (r/report flowers-5ab759f)]
    {:validate (r/validate flowers-5ab759f)
     :reached  (update-vals (r/reachable flowers-5ab759f) #(vec (sort-by pr-str %)))
     :report   rep}))

(deftest-golden flowers-with-the-missing-controls "test/golden/reachability/flowers-with-controls.edn"
  (r/report flowers-with-controls))

(deftest the-5ab759f-acceptance-table
  (let [rep (r/report flowers-5ab759f)
        vs  (:verdicts rep)]
    (is (true? (:valid? (r/validate flowers-5ab759f))))
    (is (false? (:pass? rep)))
    (is (= :missing-ui (verdict-of vs (:create-item h))))
    (is (= :missing-ui (verdict-of vs (:purchase h))))
    (is (= :orphan-event (verdict-of vs :flowers/create-purchase)))
    (is (= :covered (verdict-of vs (:usage h))))
    (testing "the only mutating route reached is the usage modal's Confirmar"
      (is (= [{:method :post :path "/api/flowers/usage"}]
             (->> (r/reachable flowers-5ab759f) :routes
                  (filter #(#{:post :put :patch :delete} (:method %)))
                  (mapv #(select-keys % [:method :path]))))))
    (testing "record-purchase names its orphan event"
      (is (= [:flowers/create-purchase]
             (:orphan-events (first (filter #(= (:purchase h) (:id %)) vs))))))
    (testing "every stock and supplier action fails with its own orphan event"
      (is (= #{(:add-stock h) (:update-stock h) (:delete-stock h) (:suppliers h)
               (:create-item h) (:purchase h)}
             (into #{} (map :id) (:blocking rep)))))))

(deftest the-missing-controls-cover-both-actions
  (let [vs (r/verdicts flowers-with-controls)]
    (is (= :covered (verdict-of vs (:create-item h))))
    (is (= :covered (verdict-of vs (:purchase h))))
    (is (nil? (verdict-of vs :flowers/create-purchase)))
    (testing "the unreached alias of create-flower-item is a dead-alias warning"
      (is (= [{:handler (:create-item h) :method :post :path "/flowers"}]
             (mapv :id (filter #(= :dead-alias (:verdict %)) vs))))
      (is (every? #(= :warning (:severity %)) (filter #(= :dead-alias (:verdict %)) vs))))))

;; ── join, broken calls, waivers, tables, ratchet ─────────────────────────────

(deftest the-join-matches-method-and-template
  (is (= ["flowers" "stock" :param] (r/segments ["/flowers/stock/" 'id])))
  (is (= ["flowers" "stock" :param] (r/segments "/flowers/stock/{id}")))
  (is (= ["api" "x"] (r/segments "//api/x/?q=1")))
  (is (r/serves? {:method :put :path "/flowers/stock/:id" :handler 'a}
                 {:method :put :path "/flowers/stock/42"}))
  (is (not (r/serves? {:method :post :path "/flowers/stock/:id" :handler 'a}
                      {:method :put :path "/flowers/stock/42"})))
  (testing "a runtime FE segment does not match a BE literal"
    (is (not (r/serves? {:method :get :path "/flowers/stock/report" :handler 'a}
                        {:method :get :path ["/flowers/stock/" 'id]})))))

(deftest an-unserved-call-is-broken-unless-waived
  (let [g  (assoc-in flowers-5ab759f [:events :flowers/record-usage :http]
                     [{:method :post :path "/api/flowers/usage-v2"}])
        vs (r/verdicts g)]
    (is (= {:event :flowers/record-usage :method :post :path "/api/flowers/usage-v2"}
           (:id (first (filter #(= :broken (:verdict %)) vs)))))
    (is (true? (:reached? (first (filter #(= :broken (:verdict %)) vs)))))
    (is (empty? (filter #(= :broken (:verdict %))
                        (r/verdicts (assoc g :waivers [{:event :flowers/record-usage
                                                        :reason "v2 ships next sprint"}])))))))

(deftest waivers-remove-a-route-and-are-reported
  (let [g   (assoc flowers-5ab759f :waivers
                   [{:handler (:suppliers h) :reason "suppliers are seeded by migration"}])
        rep (r/report g)]
    (is (nil? (verdict-of (:verdicts rep) (:suppliers h))))
    (is (= [{:route {:method :post :path "/flowers/suppliers" :route-name :flowers/create-supplier
                     :handler (:suppliers h)}
             :reason "suppliers are seeded by migration"}]
           (:waived rep))))
  (testing "a waiver without a reason is a validation problem"
    (is (false? (:valid? (r/validate (assoc flowers-5ab759f :waivers [{:handler (:suppliers h)}])))))))

(deftest a-route-table-nobody-calls-is-unmounted
  (let [vs (r/verdicts (assoc flowers-5ab759f :route-tables
                              [{:id 'inventory.flowers.routes/routes :callers ['inventory.service/routes]}
                               {:id 'inventory.flowers.routes/legacy :callers []}]))]
    (is (= ['inventory.flowers.routes/legacy]
           (mapv :id (filter #(= :unmounted-table (:verdict %)) vs))))))

(deftest the-baseline-ratchets
  (let [before (r/report flowers-5ab759f)
        base   (:new (:baseline-diff before))]
    (testing "every failure is new against an empty baseline"
      (is (= 11 (count base))))
    (testing "the same graph against its own baseline passes"
      (let [again (r/report flowers-5ab759f {:baseline base})]
        (is (true? (:pass? again)))
        (is (empty? (:new (:baseline-diff again))))))
    (testing "covering two actions fixes their entries and tightens the baseline"
      (let [d (:baseline-diff (r/report flowers-with-controls {:baseline base}))]
        (is (= #{(:create-item h) (:purchase h) :flowers/create-purchase} (set (map :id (:fixed d)))))
        (is (= [{:verdict :dead-alias
                 :id {:handler (:create-item h) :method :post :path "/flowers"}}]
               (:new d)))
        (is (= 8 (count (:baseline d))))))
    (testing "a new mutating route without a control fails against the baseline"
      (let [g (update flowers-5ab759f :routes conj
                      {:method :delete :path "/api/flowers/items/:id"
                       :handler 'inventory.flowers.handlers/delete-flower-item-handler})]
        (is (false? (:pass? (r/report g {:baseline base}))))))))

;; ── properties ───────────────────────────────────────────────────────────────
;; Small random graphs over finite pools, so calls and routes actually join and
;; :dispatch edges form cycles often.

(def ^:private event-ids [:e/a :e/b :e/c :e/d :e/e])
(def ^:private root-ids [:r/one :r/two :r/unmounted])
(def ^:private handlers ['h/one 'h/two 'h/three 'h/four])

(def ^:private gen-call
  (gen/hash-map :method (gen/elements [:get :post :put :delete])
                :path (gen/elements ["/a" "/a/:id" "/b" ["/a/" 'id] "/c/x" "/zzz"])))

(def ^:private gen-route
  (gen/hash-map :method (gen/elements [:get :post :put :delete])
                :path (gen/elements ["/a" "/a/:id" "/b" "/c/:k"])
                :handler (gen/elements handlers)))

(def ^:private gen-control
  (gen/hash-map :id gen/small-integer
                :root (gen/elements root-ids)
                :dispatch (gen/vector (gen/elements event-ids) 0 2)))

(def ^:private gen-graph
  (gen/hash-map
   :roots    (gen/fmap (fn [ids] (mapv #(hash-map :id %) (distinct ids)))
                       (gen/vector (gen/elements (pop root-ids)) 0 2))
   :controls (gen/vector gen-control 0 4)
   :events   (gen/fmap #(into {} %)
                       (gen/vector (gen/tuple (gen/elements event-ids)
                                              (gen/hash-map :http (gen/vector gen-call 0 2)
                                                            :dispatch (gen/vector (gen/elements event-ids) 0 3)))
                                   0 5))
   :routes   (gen/vector gen-route 0 6)))

(defn- covered [g] (into #{} (comp (filter #(= :covered (:verdict %))) (map :id)) (r/verdicts g)))

(defn- naive-events
  "Reached events by brute iteration to a fixpoint, the oracle for `reachable`."
  [{:keys [roots controls events]}]
  (let [seeds (set (mapcat :dispatch (filter #(some #{(:root %)} (map :id roots)) controls)))]
    (loop [s seeds]
      (let [s' (into s (mapcat #(get-in events [% :dispatch])) s)]
        (if (= s s') (set (filter #(contains? events %) s)) (recur s'))))))

(defspec adding-a-control-never-uncovers-an-action 200
  (prop/for-all [g gen-graph c gen-control]
    (let [g' (update g :controls conj c)
          [a b] (map r/reachable [g g'])]
      (and (every? #(set/subset? (% a) (% b)) [:controls :events :calls :routes])
           (set/subset? (covered g) (covered g'))))))

(defspec dispatch-cycles-terminate-at-the-least-fixpoint 200
  (prop/for-all [g gen-graph]
    (= (naive-events g) (:events (r/reachable g)))))

(defspec a-waived-route-never-yields-missing-ui 200
  (prop/for-all [g gen-graph handler (gen/elements handlers)]
    (let [vs (r/verdicts (assoc g :waivers [{:handler handler :reason "property"}]))]
      (not-any? #(and (= :missing-ui (:verdict %)) (= handler (:id %))) vs))))

(deftest a-self-dispatching-cycle-terminates
  (let [g {:roots [{:id :r}] :controls [{:id 1 :root :r :dispatch [:e/a]}]
           :events {:e/a {:dispatch [:e/b]} :e/b {:dispatch [:e/a :e/b]
                                                   :http [{:method :post :path "/a"}]}}
           :routes [{:method :post :path "/a" :handler 'h/one}]}]
    (is (= #{:e/a :e/b} (:events (r/reachable g))))
    (is (= :covered (verdict-of (r/verdicts g) 'h/one)))))

(hst/deftrifecta-from-schema segments-normalise-a-path
  r/segments
  {:in  r/FePath
   :out [:vector [:or :string [:= :param]]]
   :rel (fn [_ segs] (every? #(or (= :param %) (and (string? %) (not (re-find #"[/?]" %)))) segs))
   :num-tests 50})
