(ns hive-test.examples.pricing-test
  "Worked example: testing the pricing domain with the hive-test toolkit —
   deftrifecta (golden + property + mutation), a standalone golden table,
   standalone mutations with combinators, a raw test.check invariant, and an
   LSP contract check over every DiscountPolicy adapter."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :refer [deftest-mutations]]
            [hive-test.mutation.combinators :as mut]
            [hive-test.examples.pricing :as p]))

;; --- domain generators ---------------------------------------------------

(def gen-item
  (gen/let [sku  (gen/not-empty gen/string-alphanumeric)
            qty  (gen/choose 1 20)
            unit (gen/choose 1 100000)]
    (p/line-item sku qty unit)))

(def gen-policy
  (gen/one-of [(gen/return (p/->NoDiscount))
               (gen/fmap p/->PercentOff (gen/choose 0 10000))     ; 0-100% off
               (gen/let [t (gen/choose 0 100000)
                         o (gen/choose 0 100000)]
                 (p/->SpendThreshold t o))]))

;; --- 1) trifecta on `receipt` (map output) -------------------------------
;;   golden   — snapshots the receipt for three fixed item+policy cases
;;   property — every receipt satisfies total = subtotal - discount
;;   mutation — three broken receipts must be caught (combinators state intent)

(deftrifecta receipt
  #'p/receipt
  {:apply?      true                                          ; cases are [item policy]
   :golden-path "test/golden/examples/receipt.edn"
   :cases       {:none      [(p/line-item "A" 3 100) (p/->NoDiscount)]
                 :ten-pct   [(p/line-item "B" 2 500) (p/->PercentOff 1000)]
                 :threshold [(p/line-item "C" 10 100) (p/->SpendThreshold 500 200)]}
   :gen         (gen/tuple gen-item gen-policy)
   :pred        (fn [r] (= (:total-cents r)
                           (- (:subtotal-cents r) (:discount-cents r))))
   :mutations   [(mut/drop-key p/receipt :discount-cents)     ; forgets the discount key
                 (mut/assoc-const p/receipt :total-cents 0)    ; zeroes the total
                 (mut/always {})]})                            ; empty receipt

;; --- 2) standalone golden: a percent-off discount schedule ---------------

(deftest-golden percent-off-schedule
  "test/golden/examples/percent-off-schedule.edn"
  (let [pol (p/->PercentOff 1500)]                            ; 15% off
    (into (sorted-map)
          (for [sub [0 100 999 1000 10000]]
            [sub (p/discount-cents pol sub)]))))

;; --- 3) standalone mutations on `total-cents` (int output) ---------------
;;   deftest-mutations wants a bare symbol subject (it alter-var-root's it).

(deftest-mutations total-cents-mutations
  p/total-cents
  [(mut/always -1)                                            ; negative total
   (mut/off-by-one p/total-cents)                             ; total + 1
   ["ignores-policy" (fn [item _] (p/subtotal-cents item))]]  ; drops the discount
  (fn []
    (is (= 300 (p/total-cents (p/line-item "A" 3 100) (p/->NoDiscount))))
    (is (= 900 (p/total-cents (p/line-item "B" 2 500) (p/->PercentOff 1000))))))

;; --- 4) raw test.check invariant: 0 <= total <= subtotal -----------------

(defspec total-within-bounds 200
  (prop/for-all [item   gen-item
                 policy gen-policy]
    (let [sub   (p/subtotal-cents item)
          total (p/total-cents item policy)]
      (<= 0 total sub))))

;; --- 5) LSP: every adapter honours the DiscountPolicy contract ------------

(deftest policies-honour-the-contract
  (testing "discount is a non-negative int no larger than the subtotal"
    (doseq [pol [(p/->NoDiscount) (p/->PercentOff 2500) (p/->SpendThreshold 500 200)]
            sub [0 100 500 10000]]
      (let [d (p/discount-cents pol sub)]
        (is (int? d))
        (is (<= 0 d sub) (str pol " @ " sub))))))
