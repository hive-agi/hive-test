(ns hive-test.trifecta-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn encode [m] (pr-str m))

(def gen-small-map
  (gen/map gen/keyword gen/small-integer {:max-elements 5}))

(deftrifecta encode-roundtrip
  hive-test.trifecta-test/encode
  {:gen           gen-small-map
   :property-type :roundtrip
   :decode-fn     edn/read-string
   :num-tests     50})

(defn tag [x] {:id x :kind :thing})

(deftrifecta tag-structural
  hive-test.trifecta-test/tag
  {:gen           gen/small-integer
   :property-type :structural
   :required-keys #{:id :kind}
   :num-tests     50})

(deftest deftrifecta-forwards-property-keys
  (testing ":decode-fn reaches the :roundtrip emitter"
    (let [form  (macroexpand-1
                  `(deftrifecta rt hive-test.trifecta-test/encode
                     {:gen gen-small-map :property-type :roundtrip
                      :decode-fn edn/read-string}))
          calls (filter #(and (seq? %) (seq %)) (tree-seq coll? seq form))]
      (is (some #{`edn/read-string} (flatten form)))
      (is (not-any? #(nil? (first %)) calls))))
  (testing ":required-keys reaches the :structural emitter"
    (let [form (macroexpand-1
                 `(deftrifecta st hive-test.trifecta-test/tag
                    {:gen gen/small-integer :property-type :structural
                     :required-keys #{:id :kind}}))]
      (is (some #{#{:id :kind}} (tree-seq coll? seq form))))))
