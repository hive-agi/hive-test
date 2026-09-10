(ns hive-test.tcheck.random
  "Splittable linear-congruential PRNG.

  Pure: an rng is a value, and every step returns the next one.")

(def ^:private modulus 2147483648)
(def ^:private multiplier 1103515245)
(def ^:private increment 12345)

(defn make-rng
  "An rng seeded by `n`."
  [n]
  (let [n (long n)]
    {:state (mod (if (neg? n) (- n) n) modulus)}))

(defn next-rng
  "The rng that follows `rng`."
  [rng]
  {:state (mod (+ (* multiplier (:state rng)) increment) modulus)})

(defn value
  "A non-negative long drawn from `rng`, below `modulus`."
  [rng]
  (:state rng))

(defn split
  "Two rngs that advance independently of each other."
  [rng]
  (let [a (next-rng rng)
        b (next-rng (make-rng (+ (:state a) 7919)))]
    [a b]))

(defn bounded
  "A long in [lo, hi] drawn from `rng`, with the rng that follows it."
  [rng lo hi]
  (let [span (inc (- hi lo))
        n (+ lo (mod (value rng) span))]
    [n (next-rng rng)]))
