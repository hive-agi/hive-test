(ns hive-test.mcp.tools-test
  "Facets for hive-test.mcp.tools: the handlers answer maps for any MCP
   argument map (property over the contracts: a project without .nrepl-port
   is refused, never thrown), and a golden of every handler driven through a
   sandboxed project and a loopback fake nREPL."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mcp.fake-nrepl :as fake]
            [hive-test.mcp.tools :as tools]
            [hive-test.sandbox :as sb]))

(defn- run-without-port
  "handle-test-run in a fresh empty project, extra string args merged in."
  [args]
  (sb/with-sandbox [h {}]
    (tools/handle-test-run (assoc args "project_dir" (:dir h)))))

(hst/deftrifecta-from-schema handle-test-run-refuses-a-project-without-port
  run-without-port
  {:in  [:map-of [:enum "namespace" "test_id" "other"] :string]
   :out [:map [:error :string] [:passed? [:= false]]]
   :rel (fn [_ out] (= {:error "No .nrepl-port file found" :passed? false} out))
   :mutation true})

(defn- in-project [responses handler args]
  (fake/with-fake-nrepl [f responses]
    (sb/with-sandbox [h {".nrepl-port" (str (:port f))}]
      (-> (handler (assoc args "project_dir" (:dir h)))
          (dissoc :port)
          (update :raw-values #(some-> % vec))))))

(deftest every-tool-names-its-handler
  (is (= #{"test/status" "test/run" "test/failures" "test/watch-start" "test/watch-stop"}
         (set (map :name (vals tools/kaocha-tools)))))
  (is (every? (comp fn? :handler) (vals tools/kaocha-tools))))

(deftest-golden handlers "test/golden/mcp/tools-handlers.edn"
  {:run-ns       (in-project fake/passing tools/handle-test-run {"namespace" "a-test"})
   :run-failing  (in-project fake/failing tools/handle-test-run {})
   :status       (in-project fake/passing tools/handle-test-status {})
   :failures     (in-project [{"value" "nil"}] tools/handle-test-failures {})
   :failures-no-port (sb/with-sandbox [h {}] (tools/handle-test-failures {"project_dir" (:dir h)}))
   :watch-stop-idle (tools/handle-watch-stop {})
   :watch-cycle  (fake/with-fake-nrepl [f fake/passing]
                   (sb/with-sandbox [h {".nrepl-port" (str (:port f))}]
                     (let [started (tools/handle-watch-start {"project_dir" (:dir h)})
                           again   (tools/handle-watch-start {"project_dir" (:dir h)})
                           status  (tools/handle-test-status {"project_dir" (:dir h)})
                           stopped (tools/handle-watch-stop nil)]
                       {:started (= {:started true :project_dir (:dir h)} started)
                        :again again
                        :status-watching (:watching status)
                        :stopped stopped})))})
