(ns hive-test.mcp.nrepl
  "Babashka-compatible nREPL client for test execution.
   Connects to a running nREPL and evaluates Kaocha/clojure.test forms."
  (:require [bencode.core :as bencode]
            [clojure.string :as str])
  (:import [java.net Socket]
           [java.io PushbackInputStream]))

(defn read-nrepl-port
  "Read .nrepl-port file from project directory."
  [project-dir]
  (let [port-file (str project-dir "/.nrepl-port")]
    (when (.exists (java.io.File. port-file))
      (parse-long (str/trim (slurp port-file))))))

(defn connect!
  "Connect to nREPL at host:port. Returns connection map with socket and streams."
  ([port] (connect! "127.0.0.1" port))
  ([host port]
   (let [socket (Socket. ^String host ^int port)
         in     (PushbackInputStream. (.getInputStream socket))
         out    (.getOutputStream socket)]
     {:socket socket :in in :out out})))

(defn disconnect!
  "Close an nREPL connection."
  [{:keys [socket]}]
  (when socket (.close ^Socket socket)))

(defn send-msg!
  "Send a bencode message to nREPL."
  [{:keys [out]} msg]
  (bencode/write-bencode out msg))

(defn recv-msg!
  "Read a bencode message from nREPL."
  [{:keys [in]}]
  (bencode/read-bencode in))

(defn- bytes->str
  "Convert byte arrays in bencode response to strings."
  [v]
  (cond
    (instance? (Class/forName "[B") v) (String. ^bytes v "UTF-8")
    (map? v) (into {} (map (fn [[k v]] [(bytes->str k) (bytes->str v)])) v)
    (sequential? v) (mapv bytes->str v)
    :else v))

(defn eval!
  "Evaluate code on nREPL connection. Returns collected responses until :done status."
  [conn code & {:keys [session ns] :or {ns "user"}}]
  (let [msg (cond-> {"op" "eval" "code" code "ns" ns}
              session (assoc "session" session))]
    (send-msg! conn msg)
    (loop [responses []]
      (let [resp (bytes->str (recv-msg! conn))
            responses (conj responses resp)
            status (get resp "status")]
        (if (and status (some #{"done"} (if (sequential? status) status [status])))
          responses
          (recur responses))))))

(defn run-tests!
  "Run tests via nREPL. Uses kaocha.repl if available, falls back to clojure.test.
   Returns parsed test results."
  [conn & {:keys [ns test-id]}]
  (let [code (cond
               ;; Run specific namespace
               ns
               (str "(do (require 'clojure.test)"
                    "    (clojure.test/run-tests '" ns "))")
               ;; Run all via kaocha
               :else
               (str "(do (require 'kaocha.repl)"
                    "    (kaocha.repl/run))"))]
    (eval! conn code)))

(defn parse-test-results
  "Parse nREPL eval responses into a test summary map."
  [responses]
  (let [all-values (->> responses
                        (keep #(get % "value"))
                        (map str/trim))
        all-out    (->> responses
                        (keep #(get % "out"))
                        (apply str))
        all-err    (->> responses
                        (keep #(get % "err"))
                        (apply str))
        errors     (->> responses
                        (filter #(some #{"eval-error"} (get % "status" []))))
        ;; Try to parse {:test N :pass N :fail N :error N} from value
        summary    (try
                     (when-let [v (last all-values)]
                       (read-string v))
                     (catch Exception _ nil))]
    {:summary (when (map? summary) summary)
     :output all-out
     :errors (when (seq all-err) all-err)
     :raw-values all-values
     :passed? (and (map? summary)
                   (zero? (+ (get summary :fail 0)
                             (get summary :error 0))))}))

(defn test-status!
  "Get test health from a project. Connects, runs tests, disconnects."
  [project-dir]
  (if-let [port (read-nrepl-port project-dir)]
    (let [conn (connect! port)]
      (try
        (let [responses (run-tests! conn)
              results   (parse-test-results responses)]
          (assoc results :connected? true :port port))
        (catch Exception e
          {:connected? true :port port :error (.getMessage e) :passed? false})
        (finally
          (disconnect! conn))))
    {:connected? false :error "No .nrepl-port file found" :passed? false}))
