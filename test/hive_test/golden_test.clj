(ns hive-test.golden-test
  "Tests for golden/characterization testing macros."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.golden :as golden])
  (:import [java.io File]))

(def ^:private test-golden-dir "test/golden/test-artifacts")

(defn- cleanup-golden-dir! []
  (let [dir (File. test-golden-dir)]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (.delete f))
      (.delete dir))))

;; --- assert-golden: creates file on first run ---

(deftest assert-golden-creates-file-test
  (testing "assert-golden creates golden file when it doesn't exist"
    (let [path (str test-golden-dir "/create-test.edn")]
      (try
        (cleanup-golden-dir!)
        (golden/assert-golden path {:a 1 :b [2 3]})
        (is (.exists (File. path)) "golden file created")
        (is (= {:a 1 :b [2 3]}
               (clojure.edn/read-string (slurp path)))
            "file contains correct EDN")
        (finally
          (cleanup-golden-dir!))))))

;; --- assert-golden: matches on second run ---

(deftest assert-golden-matches-existing-test
  (testing "assert-golden passes when value matches stored snapshot"
    (let [path (str test-golden-dir "/match-test.edn")]
      (try
        (cleanup-golden-dir!)
        ;; First run: creates snapshot
        (golden/assert-golden path {:x 42})
        ;; Second run: must match
        (golden/assert-golden path {:x 42})
        (finally
          (cleanup-golden-dir!))))))

;; --- assert-golden: detects drift ---

(deftest assert-golden-detects-mismatch-test
  (testing "assert-golden fails when value diverges from snapshot"
    (let [path (str test-golden-dir "/mismatch-test.edn")]
      (try
        (cleanup-golden-dir!)
        ;; Create snapshot
        (golden/assert-golden path {:original true})
        ;; Verify mismatch is detected (capture failures)
        (let [results (atom {:pass 0 :fail 0})]
          (binding [clojure.test/report
                    (fn [{:keys [type]}]
                      (case type
                        :pass (swap! results update :pass inc)
                        :fail (swap! results update :fail inc)
                        nil))]
            (golden/assert-golden path {:changed true}))
          (is (pos? (:fail @results))
              "mismatch should produce a test failure"))
        (finally
          (cleanup-golden-dir!))))))

;; --- update-golden! ---

(deftest update-golden-overwrites-test
  (testing "update-golden! overwrites existing snapshot"
    (let [path (str test-golden-dir "/update-test.edn")]
      (try
        (cleanup-golden-dir!)
        (golden/assert-golden path {:v1 true})
        (golden/update-golden! path {:v2 true})
        (is (= {:v2 true}
               (clojure.edn/read-string (slurp path)))
            "file updated to new value")
        (finally
          (cleanup-golden-dir!))))))

;; --- deftest-golden macro ---

(golden/deftest-golden simple-golden-macro
  "test/golden/test-artifacts/macro-test.edn"
  (sorted-map :a 1 :b 2 :c 3))
