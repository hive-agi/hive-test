(ns hive-test.mutation.combinators-test
  "Tests for mutation combinators + the Mutation value object."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.mutation :as mut]
            [hive-test.mutation.combinators :as cmb]))

(defn f [x] {:a x :b (* x 2)})

(deftest combinators-return-mutations
  (testing "combinators produce Mutation value objects"
    (is (mut/mutation? (cmb/always 1)))
    (is (mut/mutation? cmb/const-nil))
    (is (mut/mutation? (cmb/drop-key f :b)))))

(deftest combinator-behaviour
  (testing "each combinator's mutant fn does what it says"
    (is (= 42        ((:mutate (cmb/always 42)) :ignored :args)))
    (is (nil?        ((:mutate cmb/const-nil) 1)))
    (is (true?       ((:mutate cmb/const-true) 1)))
    (is (false?      ((:mutate cmb/const-false) 1)))
    (is (= :y        ((:mutate (cmb/echo-arg 1)) :x :y :z)))
    (is (= {:a 5}    ((:mutate (cmb/drop-key f :b)) 5)))
    (is (= {:a 5 :b 10 :c 9} ((:mutate (cmb/assoc-const f :c 9)) 5)))
    (is (= 41        ((:mutate (cmb/off-by-one (fn [x] (* x 10)))) 4)))
    (is (false?      ((:mutate (cmb/negate-pred even?)) 4)))))

(deftest as-pair-coercion
  (testing "as-pair normalizes Mutation, [label fn], and [label Mutation]"
    (let [m (cmb/always 0)]
      (is (= ["always" (:mutate m)] (mut/as-pair m)))
      (let [[l fx] (mut/as-pair ["custom" m])]
        (is (= "custom" l))
        (is (= 0 (fx :x))))
      (let [raw (fn [_] 0)]
        (is (= ["r" raw] (mut/as-pair ["r" raw])))))))

(defn subject [x] (* x 3))

;; integration: deftest-mutations accepts a bare Mutation AND a [label Mutation]
(mut/deftest-mutations subject-mutations-caught
  hive-test.mutation.combinators-test/subject
  [(cmb/always 0)
   ["off-by-one" (cmb/off-by-one hive-test.mutation.combinators-test/subject)]]
  (fn []
    (is (= 6 (subject 2)))
    (is (= 9 (subject 3)))))
