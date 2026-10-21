(ns hive-test.contract-coverage-test
  "Contract coverage gate over every hive-test source namespace: each public
   defn carries an hc/=> contract (registered when malli is on the test
   classpath) or a reasoned exemption, so CI keeps the number."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]))

(coverage/deftest-contract-coverage hive-test-contracts
  {:paths ["src"]
   :exempt {'hive-test.mutation/->subject-sym
            "macro-time helper: its input is a raw reader form (a symbol or (var sym)), consumed only during deftest-mutation / deftrifecta expansion"}})

(deftest the-gate-is-not-vacuous
  (is (<= 120 (count (:universe (coverage/coverage {:paths ["src"]}))))))
