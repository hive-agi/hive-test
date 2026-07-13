(ns hive-test.sandbox-test
  "Trifecta for the disposable sandbox: materialization, the fn plug point,
   guarded destruction, scoped macro, and the :sandbox isolation plug."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-test.isolation :as isolation]
            [hive-test.sandbox :as sb]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(deftest materializes-spec-tree
  (sb/with-sandbox [s {"src/scratch/fresh.clj" "(ns scratch.fresh)\n(defn hello [] :hi)\n"
                       "resources/config.edn"  "{:a 1}"
                       "empty-dir"             nil}]
    (testing "files land with verbatim content, parents auto-created"
      (is (= "(ns scratch.fresh)\n(defn hello [] :hi)\n"
             (slurp (get (:paths s) "src/scratch/fresh.clj"))))
      (is (= "{:a 1}" (slurp ((:file s) "resources/config.edn")))))
    (testing "nil content yields a directory"
      (is (.isDirectory (io/file ((:file s) "empty-dir")))))
    (testing ":file resolves paths absent from the spec"
      (is (= (str (io/file (:dir s) "not/yet/there.clj"))
             ((:file s) "not/yet/there.clj"))))))

(deftest fn-content-is-the-plug-point
  (sb/with-sandbox [s {"gen.txt" (fn [{:keys [dir path]}] (str "at " path " under " dir))}]
    (is (= (str "at gen.txt under " (:dir s))
           (slurp ((:file s) "gen.txt"))))))

(deftest with-sandbox-always-destroys
  (testing "normal exit"
    (let [dir (atom nil)]
      (sb/with-sandbox [s {"a.txt" "x"}]
        (reset! dir (:dir s)))
      (is (not (.exists (io/file @dir))))))
  (testing "exceptional exit"
    (let [dir (atom nil)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (sb/with-sandbox [s {"a.txt" "x"}]
                     (reset! dir (:dir s))
                     (throw (ex-info "boom" {})))))
      (is (not (.exists (io/file @dir)))))))

(deftest destroy-is-gated-and-idempotent
  (testing "refuses maps this ns did not mint"
    (is (nil? (sb/destroy! {:dir "/"})))
    (is (.exists (io/file "/"))))
  (testing "idempotent on a real handle, and :destroy! matches destroy!"
    (let [s (sb/sandbox! {"a.txt" "x"})]
      (is (true? ((:destroy! s))))
      (is (not (.exists (io/file (:dir s)))))
      (is (true? (sb/destroy! s))))))

(deftest prefix-names-the-temp-root
  (sb/with-sandbox [s {} {:prefix "my-fixture"}]
    (is (re-find #"my-fixture" (:dir s)))))

(deftest plugs-into-isolation-composition
  (let [fixture  (isolation/with-isolations
                   {:type :sandbox
                    :spec {"src/x.clj" "(ns x)"}
                    :prefix "iso-sandbox"})
        seen     (atom nil)]
    (fixture (fn []
               (reset! seen {:bound? (some? sb/*sandbox*)
                             :dir    (:dir sb/*sandbox*)
                             :body   (slurp ((:file sb/*sandbox*) "src/x.clj"))})))
    (testing "*sandbox* is bound inside the fixture body"
      (is (:bound? @seen))
      (is (re-find #"iso-sandbox" (:dir @seen)))
      (is (= "(ns x)" (:body @seen))))
    (testing "the tree is destroyed after the body"
      (is (not (.exists (io/file (:dir @seen))))))
    (testing "*sandbox* is nil outside"
      (is (nil? sb/*sandbox*)))))
