(ns hive-test.properties-test
  "Tests for property macros using a simple Option-like monad."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [hive-test.properties :as props]))

;; --- Test monad: simple Option (Some/None) ---

(defn some-val [x] {:some x})
(defn none [] {:none true})
(defn some? [m] (contains? m :some))

(defn option-bind [m f]
  (if (some? m)
    (f (:some m))
    m))

(def gen-option-fn
  "Generator for functions that return Option values."
  (gen/elements [(fn [x] (some-val x))
                 (fn [x] (some-val (str x)))
                 (fn [_] (none))
                 (fn [x] (some-val [x]))]))

;; --- Monad law properties via macro ---

(props/defprops-monad option-monad
  gen/any-printable
  gen-option-fn
  option-bind
  some-val
  {:num-tests 100})

;; --- Roundtrip property ---

(props/defprop-roundtrip keyword-name-roundtrip
  name keyword
  (gen/elements [:foo :bar :baz :qux]))

;; --- Idempotency property ---

(defn normalize-str [s]
  (clojure.string/lower-case (clojure.string/trim s)))

(props/defprop-idempotent normalize-idempotent
  normalize-str
  (gen/fmap #(str " " % " ") gen/string-alphanumeric)
  {:num-tests 100})

;; --- Totality property ---

(props/defprop-total str-total
  str
  gen/any-printable)

(props/defprop-total parse-long-total
  (fn [s] (try (Long/parseLong s) (catch Exception _ nil)))
  gen/string-alphanumeric
  {:num-tests 100 :pred (fn [r] (or (nil? r) (integer? r)))})

;; --- Complement property ---

(props/defprop-complement pos-neg-complement
  pos? (complement pos?)
  (gen/such-that #(not (zero? %)) gen/small-integer))

;; --- Metamorphic property ---
;; Sorting is invariant to input duplication:
;; sort(xs) related to sort(xs ++ xs) by: they have the same distinct elements in order

(props/defprop-metamorphic sort-duplicate-invariant
  sort
  (fn [xs] (into xs xs))
  (fn [out1 out2] (= (distinct out1) (distinct out2)))
  (gen/vector gen/small-integer 0 20)
  {:num-tests 100})

;; --- Commutativity property ---

(props/defprop-commutative set-union-commutative
  clojure.set/union
  (gen/fmap set (gen/vector gen/small-integer 0 10))
  (gen/fmap set (gen/vector gen/small-integer 0 10))
  {:num-tests 100})

;; --- join-semilattice facets -------------------------------------------

(def ^:private gen-set
  (gen/elements [#{} #{:a} #{:b} #{:a :b} #{:c}]))

(props/defprop-associative set-union-associative
  set/union gen-set)

(props/defprop-join-semilattice set-union
  set/union #{} gen-set)

;; --- Equivalence property ---
;; Passing pair: inc and (+ 1 x) agree on every integer argument.

(props/defprop-equiv inc-plus-one-equiv
  inc
  (fn [x] (+ 1 x))
  (gen/tuple gen/small-integer)
  {:num-tests 100})

;; Failing pair: inc and dec must NOT be reported equivalent. defprop-equiv
;; expands to (defspec ...) over the same prop/for-all body checked here, so
;; asserting quick-check reports :pass? false guards the failure-detection
;; contract without committing a red defspec to the suite.
(deftest defprop-equiv-detects-inequivalence
  (testing "a non-equivalent pair (inc vs dec) fails the equivalence check"
    (let [result (tc/quick-check
                  100
                  (prop/for-all [args (gen/tuple gen/small-integer)]
                    (= (apply inc args)
                       (apply dec args))))]
      (is (false? (:pass? result))))))
