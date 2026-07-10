(ns hive-test.examples.pricing
  "A tiny pricing domain used to demonstrate the hive-test toolkit.

   Shape:
   - DiscountPolicy       — a PROTOCOL (the port): subtotal -> discount.
   - NoDiscount / PercentOff / SpendThreshold — RECORD adapters (strategy pattern).
   - LineItem / receipt   — VALUE OBJECTS (plain maps / defrecord).
   - subtotal-cents / total-cents / receipt — pure functions to characterize.

   All amounts are integer cents.")

;; --- value object --------------------------------------------------------

(defrecord LineItem [sku qty unit-cents])

(defn line-item
  "A line item: SKU, quantity, per-unit price in cents."
  [sku qty unit-cents]
  (->LineItem sku qty unit-cents))

(defn subtotal-cents
  "Undiscounted subtotal of a line item, in cents."
  [item]
  (* (:qty item) (:unit-cents item)))

;; --- port + adapters -----------------------------------------------------

(defprotocol DiscountPolicy
  "Compute a discount (cents) for a subtotal (cents). Contract: the result is a
   non-negative integer no larger than the subtotal."
  (discount-cents [policy subtotal]))

(defrecord NoDiscount []
  DiscountPolicy
  (discount-cents [_ _] 0))

(defrecord PercentOff [bps]                     ; basis points; 1% = 100 bps
  DiscountPolicy
  (discount-cents [_ subtotal]
    (quot (* subtotal bps) 10000)))

(defrecord SpendThreshold [threshold-cents off-cents]
  DiscountPolicy
  (discount-cents [_ subtotal]
    (if (>= subtotal threshold-cents)
      (min subtotal off-cents)                  ; never discount below a zero total
      0)))

;; --- pure functions over the port ----------------------------------------

(defn total-cents
  "Total for a line item under a discount policy: subtotal minus discount."
  [item policy]
  (let [sub (subtotal-cents item)]
    (- sub (discount-cents policy sub))))

(defn receipt
  "A priced receipt for a line item under a discount policy."
  [item policy]
  (let [sub  (subtotal-cents item)
        disc (discount-cents policy sub)]
    {:sku            (:sku item)
     :subtotal-cents sub
     :discount-cents disc
     :total-cents    (- sub disc)}))
