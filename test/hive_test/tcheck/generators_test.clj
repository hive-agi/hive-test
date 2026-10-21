(ns hive-test.tcheck.generators-test
  "Facets for the portable generator subset: combinator laws as properties
   over the contracts, and a golden of seeded samples per combinator."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.tcheck.generators :as gen]
            [hive-test.tcheck.random :as rng]))

(defn- draw-choose [[lo span seed]]
  (gen/generate (gen/choose lo (+ lo span)) 10 (rng/make-rng seed)))

(hst/deftrifecta-from-schema choose-draws-inside-its-bounds
  draw-choose
  {:in  [:tuple :int [:int {:min 0 :max 100}] :int]
   :out :int
   :rel (fn [[lo span _] n] (<= lo n (+ lo span)))
   :mutation true})

(defn- draw-elements [[coll seed]]
  (gen/generate (gen/elements coll) 10 (rng/make-rng seed)))

(hst/deftrifecta-from-schema elements-draws-a-member
  draw-elements
  {:in  [:tuple [:vector {:min 1} :int] :int]
   :out :int
   :rel (fn [[coll _] x] (some #{x} coll))
   :mutation true})

(defn- sample-vector [[n seed]]
  (gen/generate (gen/vector gen/small-integer n) 10 (rng/make-rng seed)))

(hst/deftrifecta-from-schema vector-of-n-has-length-n
  sample-vector
  {:in  [:tuple [:int {:min 0 :max 20}] :int]
   :out [:vector :int]
   :rel (fn [[n _] v] (= n (count v)))
   :mutation true})

(deftest such-that-gives-up-loudly
  (is (thrown? clojure.lang.ExceptionInfo
               (gen/generate (gen/such-that (constantly false) gen/nat 3)))))

(deftest-golden seeded-samples "test/golden/tcheck/generator-samples.edn"
  {:choose    (gen/sample (gen/choose 1 6) 5)
   :elements  (gen/sample (gen/elements [:a :b :c]) 5)
   :tuple     (gen/sample (gen/tuple gen/boolean gen/nat) 3)
   :fmap      (gen/sample (gen/fmap inc (gen/return 1)) 2)
   :bind      (gen/sample (gen/bind (gen/choose 1 3) #(gen/vector (gen/return :x) %)) 3)
   :one-of    (gen/sample (gen/one-of [(gen/return :l) (gen/return :r)]) 5)
   :frequency (gen/sample (gen/frequency [[3 (gen/return :hi)] [1 (gen/return :lo)]]) 5)
   :list      (vec (map vec (gen/sample (gen/list gen/boolean) 3)))
   :set       (mapv (comp vec sort) (gen/sample (gen/set (gen/choose 0 3)) 3))
   :map       (mapv #(into (sorted-map) %) (gen/sample (gen/map (gen/choose 0 2) gen/boolean) 3))
   :not-empty (gen/sample (gen/not-empty (gen/vector gen/boolean)) 3)
   :string    (gen/sample gen/string 3)
   :keyword   (gen/sample gen/keyword 3)
   :call      (first (gen/call gen/large-integer (rng/make-rng 9) 5))})
