(ns hive-test.golden.root
  "Project-root anchoring: a strategy port plus a classpath adapter, and the
   pure `anchor` calculation that turns a relative golden path into an absolute
   one under the test namespace's own repository."
  #?(:clj (:require [clojure.java.io :as io]
                    [clojure.string :as str]))
  #?(:clj (:import [java.io File])))

(defprotocol ProjectRoot
  "Resolve a test namespace to the project-root dir its goldens anchor to."
  (-root-for [this test-ns] "Absolute root dir (File) or nil."))

#?(:clj
   (defn- walk-up
     "Nearest ancestor of `start` containing deps.edn or project.clj, or nil."
     [start]
     (loop [dir start]
       (when dir
         (if (or (.exists (io/file dir "deps.edn"))
                 (.exists (io/file dir "project.clj")))
           dir
           (recur (.getParentFile dir)))))))

#?(:clj
   (defn- ns-root*
     "Project root for a namespace via its classpath resource, or nil."
     [ns-sym]
     (let [base (-> (name ns-sym) (str/replace "-" "_") (str/replace "." "/"))
           res  (some #(io/resource (str base %)) [".clj" ".cljc" ".cljs"])]
       (when (and res (= "file" (.getProtocol res)))
         (walk-up (.getParentFile (io/file (.toURI res))))))))

#?(:clj (def ^:private ns-root (memoize ns-root*)))

(defrecord ClasspathProjectRoot []
  ProjectRoot
  (-root-for [_ test-ns] #?(:clj (ns-root test-ns)
                            :cljs (do test-ns nil)
                            :default (do test-ns nil))))

#?(:clj
   (defn- cwd-root*
     "Project root for the process working directory (user.dir), walking up
      to the nearest deps.edn/project.clj, or nil. Hosts whose io/resource
      cannot see classpath files (cljw) still resolve through this."
     []
     (when-let [ud (System/getProperty "user.dir")]
       (walk-up (io/file ud)))))

(defrecord CwdProjectRoot []
  ProjectRoot
  (-root-for [_ test-ns] #?(:clj (do test-ns (cwd-root*))
                            :cljs (do test-ns nil)
                            :default (do test-ns nil))))

(defrecord FirstOfProjectRoot [resolvers]
  ProjectRoot
  (-root-for [_ test-ns]
    (some #(-root-for % test-ns) resolvers)))

(def default-resolver
  "Classpath walk-up ProjectRoot — the default anchoring strategy. Falls back
   to a cwd walk-up when the classpath strategy yields nil (e.g. cljw, whose
   io/resource returns nil for files that are on the classpath)."
  (->FirstOfProjectRoot [(->ClasspathProjectRoot) (->CwdProjectRoot)]))

(defn anchor
  "Resolve a relative golden `path` against `test-ns`'s project root via
   `resolver`. Absolute paths and hosts without a filesystem port pass through
   unchanged."
  [resolver test-ns path]
  #?(:clj (let [f (File. ^String path)]
            (if (.isAbsolute f)
              path
              (if-let [r (-root-for resolver test-ns)]
                (.getPath (File. ^File r ^String path))
                path)))
     :cljs (do resolver test-ns path)
     :default (do resolver test-ns path)))
