(ns hive-test.properties-test
  "Tests for property macros using a simple Option-like monad."
  (:require [clojure.test :refer [deftest is testing]]
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
