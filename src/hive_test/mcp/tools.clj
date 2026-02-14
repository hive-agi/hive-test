(ns hive-test.mcp.tools
  "MCP tool definitions for the Kaocha test adapter."
  (:require [hive-test.mcp.nrepl :as nrepl]
            [hive-test.mcp.watcher :as watcher]))

(defn handle-test-status
  "Get current test health for a project."
  [{:strs [project_dir]}]
  (let [dir (or project_dir ".")]
    (if (watcher/watching?)
      ;; Return cached results from watcher
      (let [latest (last (watcher/latest-results))]
        (merge {:watching true} (or latest {:summary nil :passed? nil})))
      ;; One-shot status check
      (nrepl/test-status! dir))))

(defn handle-test-run
  "Run tests for a project via nREPL."
  [{:strs [project_dir namespace test_id]}]
  (let [dir  (or project_dir ".")
        port (nrepl/read-nrepl-port dir)]
    (if port
      (let [conn (nrepl/connect! port)]
        (try
          (let [responses (nrepl/run-tests! conn :ns namespace :test-id test_id)
                results   (nrepl/parse-test-results responses)]
            results)
          (finally
            (nrepl/disconnect! conn))))
      {:error "No .nrepl-port file found" :passed? false})))

(defn handle-test-failures
  "Get test failure details."
  [{:strs [project_dir]}]
  (let [dir  (or project_dir ".")
        port (nrepl/read-nrepl-port dir)]
    (if port
      (let [conn (nrepl/connect! port)]
        (try
          (let [responses (nrepl/eval! conn
                                       "(do (require 'kaocha.repl)
                                (let [result (kaocha.repl/run)]
                                  (when-not (zero? (+ (:fail result 0) (:error result 0)))
                                    {:failures (:fail result)
                                     :errors (:error result)
                                     :details (str result)})))")
                parsed (nrepl/parse-test-results responses)]
            parsed)
          (finally
            (nrepl/disconnect! conn))))
      {:error "No .nrepl-port file found"})))

(defn handle-watch-start
  "Start file watcher for a project."
  [{:strs [project_dir paths]}]
  (let [dir (or project_dir ".")
        opts (when paths {:paths paths})]
    (if (watcher/watching?)
      {:error "Watcher already running"}
      (do
        (apply watcher/start-watching! dir (mapcat identity opts))
        {:started true :project_dir dir}))))

(defn handle-watch-stop
  "Stop the file watcher."
  [_params]
  (if (watcher/watching?)
    (watcher/stop-watching!)
    {:error "No watcher running"}))

;; --- Tool definitions (MCP schema) ---

(def kaocha-tools
  "Map of tool-name -> tool definition for MCP registration."
  {:test/status
   {:name "test/status"
    :description "Get current test health and pass/fail counts"
    :parameters [{:name "project_dir" :type :string
                  :description "Path to project root (default: .)"
                  :required false}]
    :handler handle-test-status}

   :test/run
   {:name "test/run"
    :description "Run tests via nREPL (kaocha.repl or clojure.test)"
    :parameters [{:name "project_dir" :type :string
                  :description "Path to project root (default: .)"
                  :required false}
                 {:name "namespace" :type :string
                  :description "Specific namespace to test"
                  :required false}
                 {:name "test_id" :type :string
                  :description "Specific test ID"
                  :required false}]
    :handler handle-test-run}

   :test/failures
   {:name "test/failures"
    :description "Get detailed failure information from latest test run"
    :parameters [{:name "project_dir" :type :string
                  :description "Path to project root (default: .)"
                  :required false}]
    :handler handle-test-failures}

   :test/watch-start
   {:name "test/watch-start"
    :description "Start file watcher to auto-run tests on .clj changes"
    :parameters [{:name "project_dir" :type :string
                  :description "Path to project root (default: .)"
                  :required false}
                 {:name "paths" :type :array
                  :description "Directories to watch (default: [src test])"
                  :required false}]
    :handler handle-watch-start}

   :test/watch-stop
   {:name "test/watch-stop"
    :description "Stop the file watcher"
    :parameters []
    :handler handle-watch-stop}})
