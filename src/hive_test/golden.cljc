(ns hive-test.golden
  "Characterization / golden-snapshot testing.

   Captures a function's output and stores it as EDN. On subsequent runs,
   compares against the stored snapshot. Catches unintended behavioral
   changes during refactoring — the test locks down existing behavior
   even when you don't fully understand it yet.

   Core API:
   - assert-golden:    assertion comparing value against golden EDN file
   - deftest-golden:   deftest wrapper around assert-golden
   - update-golden!:   manually update a golden file

   Golden files are EDN, stored relative to the project root.
   Set UPDATE_GOLDEN=true environment variable to regenerate all snapshots.

   Workflow:
   1. Write deftest-golden with the expression to snapshot
   2. First run creates the golden file (test passes)
   3. Subsequent runs compare against the snapshot
   4. If behavior changes intentionally: UPDATE_GOLDEN=true lein test
   5. Review the diff in git to confirm the change is expected

   Example:
     (deftest-golden parse-defaults
       \"test/golden/parse-defaults.edn\"
       (parse-config default-config-string))

   ClojureScript note: golden files are read/written through the Node.js
   `fs`/`path` modules (this ns assumes a Node target on cljs). The JVM-only
   classpath-resource fallback in `resolve-golden` is unavailable on cljs —
   golden paths resolve purely as filesystem paths there."
  #?(:clj  (:require [clojure.test :as t]
                     [clojure.edn :as edn]
                     [clojure.java.io :as io]
                     [clojure.string :as str]
                     [clojure.pprint :as pp])
     :cljs (:require [cljs.test :as t :include-macros true]
                     [cljs.reader :as reader]
                     [cljs.pprint :as pp]))
  #?(:cljs (:require-macros [hive-test.golden]))
  #?(:clj (:import [java.io File])))

#?(:cljs (def ^:private fs (js/require "fs")))
#?(:cljs (def ^:private node-path (js/require "path")))

(def ^:private update-mode?
  "When true, golden files are overwritten instead of compared.
   Controlled by UPDATE_GOLDEN environment variable."
  #?(:clj  (= "true" (System/getenv "UPDATE_GOLDEN"))
     :cljs (= "true" (.. js/process -env -UPDATE_GOLDEN))))

(defn resolve-golden
  "Resolve a golden path to a readable source, or nil.

   On the JVM: prefers the cwd-relative File (project-root + write/update runs);
   falls back to a CLASSPATH resource so a golden committed under test/ is found
   even when the JVM cwd is a sibling project (e.g. a shared nREPL).
   `test/golden/x.edn` → resource `golden/x.edn`. Returns a File or URL (both
   slurpable), or nil when neither resolves.

   On ClojureScript (Node): there is no classpath, so this returns the path
   string when the file exists on disk, otherwise nil."
  [path]
  #?(:clj
     (let [f (File. ^String path)]
       (if (.exists f)
         f
         (io/resource (str/replace-first path #"^test/" ""))))
     :cljs
     (when (.existsSync fs path) path)))

(defn read-golden
  "Read a golden value from an EDN file. Returns nil if neither the cwd-relative
   file nor a classpath resource for `path` exists (on cljs: if the file does
   not exist on disk)."
  [path]
  (when-let [src (resolve-golden path)]
    #?(:clj  (edn/read-string (slurp src))
       :cljs (reader/read-string (.readFileSync fs src "utf8")))))

(defn- write-golden!
  "Write a value to a golden EDN file, creating parent dirs if needed."
  [path value]
  #?(:clj
     (let [f (File. ^String path)]
       (.mkdirs (.getParentFile f))
       (spit f (with-out-str (pp/pprint value))))
     :cljs
     (let [dir (.dirname node-path path)]
       (when-not (.existsSync fs dir)
         (.mkdirSync fs dir #js {:recursive true}))
       (.writeFileSync fs path (with-out-str (pp/pprint value))))))

(defn update-golden!
  "Manually update a golden file with a new value.
   Use when you've intentionally changed behavior and want to
   update the snapshot without setting UPDATE_GOLDEN=true globally."
  [path value]
  (write-golden! path value)
  value)

(defn assert-golden
  "Assert that value matches the golden snapshot stored at path.

   Behavior:
   - File doesn't exist → writes snapshot, test passes (first run)
   - UPDATE_GOLDEN=true → overwrites snapshot, test passes
   - Otherwise → compares value against stored snapshot

   Returns value on success for chaining."
  [path value]
  (cond
    (or update-mode? (nil? (resolve-golden path)))
    (do (write-golden! path value)
        (t/is true (str "Golden snapshot written: " path))
        value)

    :else
    (let [expected (read-golden path)]
      (t/is (= expected value)
            (str "Golden snapshot mismatch at " path
                 "\n  Run with UPDATE_GOLDEN=true to update."))
      value)))

(defmacro deftest-golden
  "Define a golden/characterization test.

   On first run, captures expr's output to golden-path as EDN.
   On subsequent runs, asserts the output hasn't changed.

   Arguments:
   - name:        test name (symbol)
   - golden-path: path to golden EDN file (string)
   - expr:        expression whose value is snapshotted

   Example:
     (deftest-golden config-shape
       \"test/golden/config-shape.edn\"
       (keys (load-config \"defaults\")))"
  [name golden-path expr]
  (if (:ns &env)
    `(cljs.test/deftest ~name
       (assert-golden ~golden-path ~expr))
    `(clojure.test/deftest ~name
       (assert-golden ~golden-path ~expr))))

(defmacro deftest-golden-fn
  "Like deftest-golden but takes a zero-arg function instead of an expression.
   Useful when setup is needed or the expression is complex.

   Arguments:
   - name:        test name (symbol)
   - golden-path: path to golden EDN file (string)
   - f:           zero-arg function returning the value to snapshot

   Example:
     (deftest-golden-fn api-response-shape
       \"test/golden/api-response.edn\"
       (fn []
         (let [resp (handler {:uri \"/status\" :method :get})]
           (select-keys resp [:status :headers]))))"
  [name golden-path f]
  (if (:ns &env)
    `(cljs.test/deftest ~name
       (assert-golden ~golden-path (~f)))
    `(clojure.test/deftest ~name
       (assert-golden ~golden-path (~f)))))
