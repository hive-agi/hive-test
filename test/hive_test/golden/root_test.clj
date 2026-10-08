(ns hive-test.golden.root-test
  "Tests for project-root anchoring (port + pure calculation)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-test.golden.root :as root]
            [hive-test.trifecta :refer [deftrifecta]])
  (:import [java.io File]))

(defn- fixed-root
  "A ProjectRoot stub that always resolves to `dir` (or nil)."
  [dir]
  (reify root/ProjectRoot
    (-root-for [_ _test-ns] (when dir (File. ^String dir)))))

(deftest anchor-joins-relative-to-root
  (testing "a relative path resolves under the resolver's root"
    (is (= (.getPath (File. (File. "/repo/x") "test/golden/a.edn"))
           (root/anchor (fixed-root "/repo/x") 'any.ns "test/golden/a.edn")))))

(deftest anchor-passthrough-absolute
  (testing "an absolute path is returned unchanged"
    (is (= "/abs/g.edn"
           (root/anchor (fixed-root "/repo") 'any.ns "/abs/g.edn")))))

(deftest anchor-passthrough-when-no-root
  (testing "nil root leaves the path unchanged (legacy cwd behaviour)"
    (is (= "test/golden/a.edn"
           (root/anchor (fixed-root nil) 'any.ns "test/golden/a.edn")))))

(deftest default-resolver-finds-hive-test-root
  (testing "the classpath resolver locates this repo's project root"
    (let [r (root/-root-for root/default-resolver 'hive-test.golden.root)]
      (is (some? r))
      (is (.exists (File. ^File r "deps.edn"))))))

(deftest cwd-resolver-finds-hive-test-root
  (testing "the cwd walk-up resolver (the cljw fallback) locates this repo's root"
    (let [r (root/-root-for (root/->CwdProjectRoot) 'any.ns)]
      (is (some? r))
      (is (.exists (File. ^File r "deps.edn"))))))

(defn first-root
  "Subject: the root path a FirstOfProjectRoot over fixed stubs `dirs` yields."
  [dirs]
  (some-> (root/-root-for (root/->FirstOfProjectRoot (mapv fixed-root dirs)) 'n)
          (.getPath)))

(deftrifecta first-of-project-root
  hive-test.golden.root-test/first-root
  {:golden-path "test/golden/root/first-of-project-root.edn"
   :cases {:empty      []
           :all-nil    [nil nil]
           :first-wins ["/a" "/b"]
           :skips-nil  [nil "/b" "/c"]}
   :gen (gen/vector (gen/one-of [(gen/return nil)
                                 (gen/fmap #(str "/" %) gen/string-alphanumeric)])
                    0 4)
   :pred #(or (nil? %) (string? %))
   :num-tests 100
   :mutations [["always-nil" (constantly nil)]
               ["last-wins" (fn [dirs] (some-> (last (remove nil? dirs)) (File.) (.getPath)))]]})

(defspec anchor-absolute-is-fixpoint 100
  (prop/for-all [seg (gen/such-that seq gen/string-alphanumeric)]
    (let [rooted (root/anchor (fixed-root "/r") 'n (str "test/" seg))]
      (and (.isAbsolute (File. ^String rooted))
           ;; re-anchoring an already-absolute path is a no-op
           (= rooted (root/anchor (fixed-root "/other") 'n rooted))))))
