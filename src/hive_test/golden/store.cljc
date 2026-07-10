(ns hive-test.golden.store
  "Golden-snapshot persistence: a role-sized port plus adapters.

   GoldenStore is the port (read / write / exists). FileGoldenStore is the
   filesystem adapter; AtomGoldenStore is an in-memory adapter for exercising
   the golden system itself without touching disk."
  #?(:clj  (:require [clojure.edn :as edn]
                     [clojure.java.io :as io]
                     [clojure.string :as str]
                     [clojure.pprint :as pp])
     :cljs (:require [cljs.reader :as reader]
                     [cljs.pprint :as pp]))
  #?(:clj (:import [java.io File])))

(defprotocol GoldenStore
  "Persistence port for golden snapshots."
  (-read    [store path] "Golden value at path, or nil when absent.")
  (-write!  [store path value] "Persist value at path; returns value.")
  (-exists? [store path] "True when a golden exists at path."))

#?(:cljs (def ^:private fs (js/require "fs")))
#?(:cljs (def ^:private node-path (js/require "path")))

#?(:clj
   (defn- resolve-src
     "On-disk File if present, else a classpath resource (test/x → resource x)."
     [path]
     (let [f (File. ^String path)]
       (if (.exists f)
         f
         (io/resource (str/replace-first path #"^test/" ""))))))

(defrecord FileGoldenStore []
  GoldenStore
  (-read [_ path]
    #?(:clj  (when-let [src (resolve-src path)] (edn/read-string (slurp src)))
       :cljs (when (.existsSync fs path)
               (reader/read-string (.readFileSync fs path "utf8")))))
  (-write! [_ path value]
    #?(:clj  (let [f (File. ^String path)]
               (.mkdirs (.getParentFile f))
               (spit f (with-out-str (pp/pprint value)))
               value)
       :cljs (let [dir (.dirname node-path path)]
               (when-not (.existsSync fs dir)
                 (.mkdirSync fs dir #js {:recursive true}))
               (.writeFileSync fs path (with-out-str (pp/pprint value)))
               value)))
  (-exists? [_ path]
    #?(:clj  (some? (resolve-src path))
       :cljs (.existsSync fs path))))

(defrecord AtomGoldenStore [state]
  GoldenStore
  (-read    [_ path] (get @state path))
  (-write!  [_ path value] (swap! state assoc path value) value)
  (-exists? [_ path] (contains? @state path)))

(defn file-store
  "Filesystem-backed GoldenStore."
  []
  (->FileGoldenStore))

(defn memory-store
  "In-memory GoldenStore over an atom map; for testing the golden system."
  ([] (memory-store {}))
  ([init] (->AtomGoldenStore (atom init))))
