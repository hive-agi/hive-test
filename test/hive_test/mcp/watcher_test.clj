(ns hive-test.mcp.watcher-test
  "Facets for hive-test.mcp.watcher: latest-results takes the last n of the
   ring (property over its contract), the start/stop lifecycle against a
   sandboxed project and a loopback fake nREPL, and a golden of it.

   The ring buffer and watcher state are private defonce atoms with no
   injection seam, so only the empty ring and the public lifecycle are
   observable without with-redefs (recorded as a missing seam)."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mcp.fake-nrepl :as fake]
            [hive-test.mcp.watcher :as watcher]
            [hive-test.sandbox :as sb]))

(defn- take-latest [n] (vec (watcher/latest-results n)))

(hst/deftrifecta-from-schema latest-results-is-a-suffix-of-the-ring
  take-latest
  {:in  [:maybe [:int {:min 0 :max 60}]]
   :out [:vector :map]
   :rel (fn [n out] (= out (vec (cond->> (watcher/latest-results) n (take-last n)))))
   :mutation true})

(defn- lifecycle []
  (fake/with-fake-nrepl [f fake/passing]
    (sb/with-sandbox [h {".nrepl-port" (str (:port f)) "src/a.clj" "(ns a)" "test" nil}]
      (let [before  (watcher/watching?)
            state   (watcher/start-watching! (:dir h) :debounce-ms 10)
            during  (watcher/watching?)
            twice   (ex-message (try (watcher/start-watching! (:dir h)) nil
                                     (catch clojure.lang.ExceptionInfo e e)))
            stopped (watcher/stop-watching!)]
        {:before before :during during :after (watcher/watching?)
         :state-keys (vec (sort (keys state)))
         :project-dir? (= (:dir h) (:project-dir state))
         :second-start twice :stopped stopped
         :stop-again (watcher/stop-watching!)}))))

(deftest start-refuses-a-project-without-nrepl-port
  (sb/with-sandbox [h {}]
    (is (= {:project-dir (:dir h)}
           (ex-data (try (watcher/start-watching! (:dir h)) nil
                         (catch clojure.lang.ExceptionInfo e e)))))
    (is (false? (watcher/watching?)))))

(deftest a-clj-change-stores-a-run-in-the-ring
  (fake/with-fake-nrepl [f fake/passing]
    (sb/with-sandbox [h {".nrepl-port" (str (:port f)) "src/a.clj" "(ns a)"}]
      (let [n0 (count (watcher/latest-results))]
        (watcher/start-watching! (:dir h) :debounce-ms 10)
        (try
          (Thread/sleep 200)
          (spit ((:file h) "src/a.clj") "(ns a) ;; touched")
          (let [deadline (+ (System/currentTimeMillis) 8000)]
            (while (and (= n0 (count (watcher/latest-results)))
                        (< (System/currentTimeMillis) deadline))
              (Thread/sleep 50)))
          (let [r (last (watcher/latest-results 1))]
            (is (< n0 (count (watcher/latest-results))))
            (is (true? (:passed? r)))
            (is (string? (:timestamp r))))
          (finally (watcher/stop-watching!)))))))

(deftest-golden watch-lifecycle "test/golden/mcp/watcher-lifecycle.edn"
  (lifecycle))
