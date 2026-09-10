(ns hive-test.tcheck-test
  "The portable test.check subset: determinism, combinators, and reporting."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-test.tcheck.random :as rng]
            [hive-test.tcheck.generators :as gen]
            [hive-test.tcheck.properties :as prop]
            [hive-test.tcheck.clojure-test :as tc]))

;; ── The rng ──────────────────────────────────────────────────────────────────

(deftest the-rng-is-a-pure-function-of-its-seed
  (testing "same seed, same stream"
    (is (= (rng/value (rng/next-rng (rng/make-rng 7)))
           (rng/value (rng/next-rng (rng/make-rng 7))))))
  (testing "different seeds diverge"
    (is (not= (rng/value (rng/next-rng (rng/make-rng 7)))
              (rng/value (rng/next-rng (rng/make-rng 8)))))))

(deftest a-negative-seed-is-accepted
  (is (nat-int? (rng/value (rng/make-rng -12345)))))

(deftest bounded-stays-in-range
  (let [xs (loop [i 0 r (rng/make-rng 1) acc []]
             (if (< i 200)
               (let [[n r'] (rng/bounded r 3 9)]
                 (recur (inc i) r' (conj acc n)))
               acc))]
    (is (every? #(<= 3 % 9) xs))
    (is (< 1 (count (distinct xs))) "the range is actually explored")))

;; ── Generators ───────────────────────────────────────────────────────────────

(deftest choose-is-inclusive-and-bounded
  (is (every? #(<= -5 % 5) (gen/sample (gen/choose -5 5) 100))))

(deftest generate-is-deterministic
  (is (= (gen/generate (gen/choose 0 1000000))
         (gen/generate (gen/choose 0 1000000)))))

(deftest combinators-compose
  (testing "fmap"
    (is (every? even? (gen/sample (gen/fmap #(* 2 %) (gen/choose 0 50)) 30))))
  (testing "elements"
    (is (every? #{:a :b :c} (gen/sample (gen/elements [:a :b :c]) 30))))
  (testing "tuple"
    (is (every? #(= 2 (count %)) (gen/sample (gen/tuple gen/nat gen/nat) 20))))
  (testing "such-that"
    (is (every? odd? (gen/sample (gen/such-that odd? (gen/choose 1 99)) 20))))
  (testing "one-of"
    (is (every? #{0 1} (gen/sample (gen/one-of [(gen/return 0) (gen/return 1)]) 20)))))

(deftest such-that-gives-up-rather-than-looping-forever
  (is (thrown? #?(:clj Exception :cljs :default :default Exception)
               (gen/generate (gen/such-that (constantly false) gen/nat)))))

;; ── Properties ───────────────────────────────────────────────────────────────

(deftest a-true-property-passes
  (is (:result (tc/quick-check 50 (prop/for-all [n gen/nat] (>= n 0))))))

(deftest a-false-property-reports-its-input
  (let [outcome (tc/quick-check 50 (prop/for-all [n (gen/choose 5 5)] (> n 10)))]
    (is (false? (:result outcome)))
    (is (= [5] (:fail outcome)) "the failing input is reported")
    (is (= 1 (:num-tests outcome)) "it stops at the first failure")))

(deftest a-throwing-property-is-a-failure-not-a-crash
  (let [outcome (tc/quick-check 10 (prop/for-all [n gen/nat] (throw (ex-info "boom" {:n n}))))]
    (is (false? (:result outcome)))
    (is (some? (:error outcome)))))

(deftest a-failure-is-reproducible-from-its-seed
  (let [a (tc/quick-check 50 (prop/for-all [n (gen/choose 0 1000)] (< n 100)) 99)
        b (tc/quick-check 50 (prop/for-all [n (gen/choose 0 1000)] (< n 100)) 99)]
    (is (= (:fail a) (:fail b)))))

;; ── defspec ──────────────────────────────────────────────────────────────────

(tc/defspec defspec-emits-a-passing-test 25
  (prop/for-all [n (gen/choose 0 100)]
    (<= 0 n 100)))
