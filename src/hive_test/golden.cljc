(ns hive-test.golden
  "Characterization / golden-snapshot testing.

   Captures a function's output as EDN; later runs compare against it, catching
   unintended behavioral changes during refactoring.

   Persistence is a swappable port (hive-test.golden.store/GoldenStore) and path
   anchoring a swappable strategy (hive-test.golden.root/ProjectRoot); rebind
   *store* / *project-root* to inject alternatives (e.g. an in-memory store).

   Core API: assert-golden, read-golden, update-golden!, anchor,
   deftest-golden, deftest-golden-fn. Golden paths anchor to the test's project
   root. Set UPDATE_GOLDEN=true to regenerate snapshots."
  #?(:clj  (:require [clojure.test :as t]
                     [hive-test.golden.store :as store]
                     [hive-test.golden.root :as root])
     :cljs (:require [cljs.test :as t :include-macros true]
                     [hive-test.golden.store :as store]
                     [hive-test.golden.root :as root]))
  #?(:cljs (:require-macros [hive-test.golden])))

(def ^:dynamic *store*
  "Injected GoldenStore port (DIP seam). Default: filesystem."
  (store/file-store))

(def ^:dynamic *project-root*
  "Injected ProjectRoot strategy (OCP seam). Default: classpath walk-up."
  root/default-resolver)

(def ^:private update-mode?
  "True when golden files are overwritten instead of compared (UPDATE_GOLDEN)."
  #?(:clj  (= "true" (System/getenv "UPDATE_GOLDEN"))
     :cljs (= "true" (.. js/process -env -UPDATE_GOLDEN))))

(defn anchor
  "Resolve a relative golden `path` against `test-ns`'s project root."
  [test-ns path]
  (root/anchor *project-root* test-ns path))

(defn read-golden
  "Golden value stored at `path`, or nil when absent."
  [path]
  (store/-read *store* path))

(defn update-golden!
  "Overwrite the golden at `path` with `value`; returns value."
  [path value]
  (store/-write! *store* path value))

(defn assert-golden
  "Assert `value` matches the golden snapshot at `path`.

   Absent golden or UPDATE_GOLDEN=true → write snapshot, pass (first run);
   otherwise compare against the stored snapshot. Returns value for chaining."
  [path value]
  (if (or update-mode? (not (store/-exists? *store* path)))
    (do (store/-write! *store* path value)
        (t/is true (str "Golden snapshot written: " path))
        value)
    (let [expected (read-golden path)]
      (t/is (= expected value)
            (str "Golden snapshot mismatch at " path
                 "\n  Run with UPDATE_GOLDEN=true to update."))
      value)))

(defmacro deftest-golden
  "Define a golden/characterization test: snapshot `expr` to `golden-path` on
   first run, assert unchanged thereafter."
  [name golden-path expr]
  (let [test-ns (list 'quote (ns-name *ns*))]
    (if (:ns &env)
      `(cljs.test/deftest ~name
         (assert-golden (anchor ~test-ns ~golden-path) ~expr))
      `(clojure.test/deftest ~name
         (assert-golden (anchor ~test-ns ~golden-path) ~expr)))))

(defmacro deftest-golden-fn
  "Like deftest-golden but snapshots the result of the zero-arg function `f`."
  [name golden-path f]
  (let [test-ns (list 'quote (ns-name *ns*))]
    (if (:ns &env)
      `(cljs.test/deftest ~name
         (assert-golden (anchor ~test-ns ~golden-path) (~f)))
      `(clojure.test/deftest ~name
         (assert-golden (anchor ~test-ns ~golden-path) (~f))))))
