(ns hive-test.tcheck.generators
  "The subset of `clojure.test.check.generators` that hive-test's macros emit.

  A generator is a function of [rng size] returning [value rng']. Shrinking is
  not modelled: a failure reports the input that produced it."
  (:refer-clojure :exclude [boolean char keyword list map not-empty set vector])
  (:require [hive-test.tcheck.random :as rng]))

(defn call
  "Draw one value from `gen`, returning [value rng']."
  [gen rnd size]
  (gen rnd size))

;; ── Primitives ───────────────────────────────────────────────────────────────

(defn return [v] (fn [rnd _] [v rnd]))

(defn choose
  "A long in [lo, hi], inclusive."
  [lo hi]
  (fn [rnd _] (rng/bounded rnd lo hi)))

(def nat (fn [rnd size] (rng/bounded rnd 0 (max 1 size))))

(def small-integer
  (fn [rnd size] (rng/bounded rnd (- (max 1 size)) (max 1 size))))

(def large-integer (choose -1000000 1000000))

(def boolean (fn [rnd _] (let [[n r] (rng/bounded rnd 0 1)] [(= n 1) r])))

(def char-alpha
  (fn [rnd _]
    (let [[n r] (rng/bounded rnd 97 122)]
      [(clojure.core/char n) r])))

;; ── Combinators ──────────────────────────────────────────────────────────────

(defn fmap [f gen]
  (fn [rnd size]
    (let [[v r] (call gen rnd size)]
      [(f v) r])))

(defn bind [gen f]
  (fn [rnd size]
    (let [[v r] (call gen rnd size)]
      (call (f v) r size))))

(defn such-that
  "Values from `gen` satisfying `pred`. Gives up after `tries` (default 100)."
  ([pred gen] (such-that pred gen 100))
  ([pred gen tries]
   (fn [rnd size]
     (loop [r rnd n tries]
       (let [[v r'] (call gen r size)]
         (cond
           (pred v) [v r']
           (pos? n) (recur r' (dec n))
           :else (throw (ex-info "such-that gave up" {:tries tries}))))))))

(defn tuple [& gens]
  (fn [rnd size]
    (loop [gs gens acc [] r rnd]
      (if (seq gs)
        (let [[v r'] (call (first gs) r size)]
          (recur (rest gs) (conj acc v) r'))
        [acc r]))))

(defn elements
  "One of `coll`."
  [coll]
  (let [v (clojure.core/vec coll)]
    (fn [rnd size]
      (let [[i r] (rng/bounded rnd 0 (dec (count v)))]
        [(nth v i) r]))))

(defn one-of
  "A value from one of `gens`."
  [gens]
  (fn [rnd size]
    (let [v (clojure.core/vec gens)
          [i r] (rng/bounded rnd 0 (dec (count v)))]
      (call (nth v i) r size))))

(defn frequency
  "A value from one of `pairs`, a seq of [weight gen]."
  [pairs]
  (one-of (mapcat (fn [[w g]] (repeat (max 1 w) g)) pairs)))

(defn vector
  "A vector of values from `gen`."
  ([gen]
   (fn [rnd size]
     (let [[n r] (rng/bounded rnd 0 (max 1 size))]
       (call (apply tuple (repeat n gen)) r size))))
  ([gen n] (apply tuple (repeat n gen)))
  ([gen lo hi]
   (fn [rnd size]
     (let [[n r] (rng/bounded rnd lo hi)]
       (call (apply tuple (repeat n gen)) r size)))))

(defn list [gen] (fmap #(apply clojure.core/list %) (vector gen)))

(defn set [gen] (fmap clojure.core/set (vector gen)))

(defn map
  "A map from `kgen` keys to `vgen` values."
  [kgen vgen]
  (fmap #(into {} %) (vector (tuple kgen vgen))))

(def string (fmap #(apply str %) (vector char-alpha)))

(def keyword (fmap #(clojure.core/keyword (if (= "" %) "k" %)) string))

(defn not-empty [gen] (such-that seq gen))

;; ── Drawing ──────────────────────────────────────────────────────────────────

(defn generate
  "One value from `gen`."
  ([gen] (generate gen 30))
  ([gen size] (generate gen size (rng/make-rng 42)))
  ([gen size rnd] (first (call gen rnd size))))

(defn sample
  "`n` values from `gen` (default 10)."
  ([gen] (sample gen 10))
  ([gen n]
   (loop [i 0 r (rng/make-rng 42) acc []]
     (if (< i n)
       (let [[v r'] (call gen r (inc i))]
         (recur (inc i) r' (conj acc v)))
       acc))))
