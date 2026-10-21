(ns hive-test.mcp.fake-nrepl
  "Test support: a real loopback nREPL stand-in. It speaks bencode on a
   ServerSocket, so hive-test.mcp.* is exercised through its socket port
   rather than with-redefs. Every eval is answered with `responses`
   (the last one carries status done), and the received messages are logged."
  (:require [bencode.core :as bencode])
  (:import [java.io PushbackInputStream]
           [java.net ServerSocket Socket SocketException]))

(defn- ->str [v] (if (bytes? v) (String. ^bytes v "UTF-8") v))

(defn- serve-conn [^Socket s responses log]
  (with-open [s s]
    (let [in (PushbackInputStream. (.getInputStream s)) out (.getOutputStream s)]
      (try
        (loop []
          (when-let [msg (bencode/read-bencode in)]
            (swap! log conj (into (sorted-map) (map (fn [[k v]] [(->str k) (->str v)])) msg))
            (doseq [r (butlast responses)] (bencode/write-bencode out r))
            (bencode/write-bencode out (assoc (last responses) "status" ["done"]))
            (recur)))
        (catch Exception _ nil)))))

(defn start!
  "Start a fake nREPL answering every eval with `responses` (a seq of
   string-keyed maps). Returns {:port :log :stop!}."
  [responses]
  (let [ss (ServerSocket. 0) log (atom [])]
    (future
      (try (loop [] (let [s (.accept ss)] (future (serve-conn s responses log))) (recur))
           (catch SocketException _ nil)))
    {:port (.getLocalPort ss) :log log :stop! #(.close ss)}))

(defmacro with-fake-nrepl
  "binding: [sym responses]. Runs body with a started fake, always stops it."
  [[sym responses] & body]
  `(let [~sym (start! ~responses)]
     (try ~@body (finally ((:stop! ~sym))))))

(def passing
  "A clojure.test summary value, as a kaocha/clojure.test run prints it."
  [{"out" "Testing x\n"} {"value" "{:test 2, :pass 5, :fail 0, :error 0}"}])

(def failing
  [{"out" "FAIL in (t)\n"} {"err" "boom\n"} {"value" "{:test 1, :pass 0, :fail 1, :error 0}"}])
