(ns hive-test.cljw-runner
  "Native cljw boundary for hive-test's portable trifecta proof."
  (:require [clojure.test :as test]
            [hive-test.cljw-trifecta-test]))

(defn -main [& _]
  (let [{:keys [fail error]}
        (test/run-tests 'hive-test.cljw-trifecta-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
