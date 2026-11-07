(ns hive-test.contract-test
  "hive-test.contract/=> expands to malli.core/=> on a JVM compile with malli
   on the classpath, and to nil when compiling for ClojureScript (&env :ns)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.contract :as hc]))

(defn- sample-inc [x] (inc x))

(hc/=> sample-inc [:=> [:cat :int] :int])

(deftest expands-to-malli-on-jvm
  (testing "malli is on the :test classpath (via hive-dsl), so the macro emits m/=>"
    (let [form (macroexpand-1 '(hive-test.contract/=> foo [:=> [:cat :any] :any]))]
      (is (seq? form))
      (is (= 'malli.core/=> (first form)))
      (is (= 'foo (second form)))
      (is (= [:=> [:cat :any] :any] (nth form 2))))))

(deftest registers-the-function-schema
  (testing "the top-level (hc/=> sample-inc ...) registered a malli function schema"
    (let [schemas (requiring-resolve 'malli.core/function-schemas)
          entry   (get-in (schemas) ['hive-test.contract-test 'sample-inc])]
      (is (some? entry))
      (is (= [:=> [:cat :int] :int]
             ((requiring-resolve 'malli.core/form) (:schema entry)))))))

(deftest expands-to-nil-under-cljs-env
  (testing "a ClojureScript compile (&env carries :ns) pays nothing"
    (let [expander @#'hc/=>]
      (is (nil? (expander nil {:ns {:name 'cljs.user}} 'foo [:=> [:cat :any] :any]))))))

(deftest malli-present-probe
  (is (true? (#'hc/malli-present?))))
