(ns hive-test.mcp.watcher
  "File watcher for watch mode — monitors src/test dirs for .clj changes
   and triggers test re-runs via nREPL."
  (:require [hive-test.mcp.nrepl :as nrepl]
            [clojure.string :as str])
  (:import [java.nio.file FileSystems Paths StandardWatchEventKinds WatchService]
           [java.util.concurrent Executors TimeUnit]))

(defonce ^:private watcher-state (atom nil))
(defonce ^:private results-ring (atom []))
(def ^:private max-results 50)

(defn latest-results
  "Return the latest test results from the ring buffer."
  ([] (latest-results nil))
  ([n]
   (let [results @results-ring]
     (if n (take-last n results) results))))

(defn- store-result! [result]
  (swap! results-ring (fn [buf]
                        (let [buf (conj buf (assoc result :timestamp (str (java.time.Instant/now))))]
                          (if (> (count buf) max-results)
                            (vec (drop (- (count buf) max-results) buf))
                            buf)))))

(defn- clj-file? [path]
  (str/ends-with? (str path) ".clj"))

(defn- register-recursive!
  "Register a directory and all subdirectories with the watch service."
  [^WatchService ws dir]
  (let [path (Paths/get dir (into-array String []))]
    (.register path ws
               (into-array [StandardWatchEventKinds/ENTRY_MODIFY
                            StandardWatchEventKinds/ENTRY_CREATE]))
    ;; Register subdirs
    (doseq [f (-> path .toFile .listFiles)]
      (when (.isDirectory f)
        (register-recursive! ws (.getAbsolutePath f))))))

(defn start-watching!
  "Start watching project dirs for .clj changes. Re-runs tests on change.
   Returns the watcher state."
  [project-dir & {:keys [paths debounce-ms]
                  :or {paths ["src" "test"] debounce-ms 500}}]
  (when @watcher-state
    (throw (ex-info "Watcher already running" {})))

  (let [ws       (.newWatchService (FileSystems/getDefault))
        executor (Executors/newSingleThreadScheduledExecutor)
        running? (atom true)
        port     (nrepl/read-nrepl-port project-dir)
        _        (when-not port
                   (throw (ex-info "No .nrepl-port file" {:project-dir project-dir})))
        pending  (atom false)]

    ;; Register directories
    (doseq [p paths]
      (let [full-path (str project-dir "/" p)]
        (when (.exists (java.io.File. full-path))
          (register-recursive! ws full-path))))

    ;; Debounced test runner
    (let [run-tests-fn (fn []
                         (when (compare-and-set! pending true false)
                           (try
                             (let [conn (nrepl/connect! port)]
                               (try
                                 (let [responses (nrepl/run-tests! conn)
                                       result    (nrepl/parse-test-results responses)]
                                   (store-result! result))
                                 (finally
                                   (nrepl/disconnect! conn))))
                             (catch Exception e
                               (store-result! {:error (.getMessage e) :passed? false})))))]

      ;; Watcher thread
      (future
        (while @running?
          (try
            (when-let [key (.poll ws 1 TimeUnit/SECONDS)]
              (let [events (.pollEvents key)
                    has-clj? (some #(clj-file? (.context %)) events)]
                (when has-clj?
                  (reset! pending true)
                  (.schedule executor ^Runnable run-tests-fn
                             ^long debounce-ms TimeUnit/MILLISECONDS)))
              (.reset key))
            (catch Exception _
              nil))))

      (let [state {:watch-service ws
                   :executor executor
                   :running? running?
                   :project-dir project-dir}]
        (reset! watcher-state state)
        state))))

(defn stop-watching!
  "Stop the file watcher."
  []
  (when-let [{:keys [watch-service executor running?]} @watcher-state]
    (reset! running? false)
    (try (.close ^WatchService watch-service) (catch Exception _))
    (try (.shutdownNow ^java.util.concurrent.ExecutorService executor) (catch Exception _))
    (reset! watcher-state nil)
    {:stopped true}))

(defn watching?
  "Returns true if watcher is currently running."
  []
  (some? @watcher-state))
