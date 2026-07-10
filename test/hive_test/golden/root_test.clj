(ns hive-test.golden.root-test
  "Tests for project-root anchoring (port + pure calculation)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-test.golden.root :as root])
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

(defspec anchor-absolute-is-fixpoint 100
  (prop/for-all [seg (gen/such-that seq gen/string-alphanumeric)]
    (let [rooted (root/anchor (fixed-root "/r") 'n (str "test/" seg))]
      (and (.isAbsolute (File. ^String rooted))
           ;; re-anchoring an already-absolute path is a no-op
           (= rooted (root/anchor (fixed-root "/other") 'n rooted))))))
