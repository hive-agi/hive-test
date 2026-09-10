(ns hive-test.cljrs-runner
  "Native cljrs boundary for hive-test's portable trifecta proof."
  (:require [clojure.test :as test]
            [hive-test.cljrs-trifecta-test]))

(defn -main [& _]
  (let [{:keys [fail error]}
        (test/run-tests 'hive-test.cljrs-trifecta-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
