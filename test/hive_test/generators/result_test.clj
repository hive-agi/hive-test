(ns hive-test.generators.result-test
  "Facets for hive-test.generators.result: every Result generator, drawn at a
   fixed seed, yields the shape its constructors build (properties over the
   contracts), and a golden of the seeded err categories and fn outputs."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.generators.result :as gr]
            [hive-test.golden :refer [deftest-golden]]))

(defn- ok [v] {:ok v})
(defn- err [c] {:error c})

(def ^:private categories (set gr/error-categories))

(defn- draw-result [seed] (gen/generate (gr/gen-result ok err) 10 seed))

(hst/deftrifecta-from-schema gen-result-draws-an-ok-or-a-registered-err
  draw-result
  {:in  :int
   :out [:or [:map {:closed true} [:ok :any]] [:map {:closed true} [:error :keyword]]]
   :rel (fn [_ r] (or (contains? r :ok) (contains? categories (:error r))))
   :mutation true})

(defn- draw-err-with-extras [seed] (gen/generate (gr/gen-err-with-extras) 10 seed))

(hst/deftrifecta-from-schema gen-err-with-extras-always-carries-a-category
  draw-err-with-extras
  {:in  :int
   :out [:map [:error :keyword]]
   :rel (fn [_ r] (and (contains? categories (:error r)) (not (contains? r :ok))))
   :mutation true})

(defn- apply-result-fn [[seed x]] ((gen/generate (gr/gen-result-fn ok err) 10 seed) x))

(hst/deftrifecta-from-schema gen-result-fn-returns-results
  apply-result-fn
  {:in  [:tuple :int :int]
   :out [:or [:map [:ok :any]] [:map [:error [:= :test/generated]]]]
   :rel (fn [[_ x] r] (or (= r {:error :test/generated})
                          (contains? #{x (str x) [x] {:wrapped x}} (:ok r))))
   :mutation true})

(deftest gen-plain-fn-never-wraps-in-a-result
  (doseq [seed (range 20)]
    (let [f (gen/generate (gr/gen-plain-fn) 10 seed)]
      (is (not (contains? (let [v (f 7)] (if (map? v) v {})) :ok))))))

(deftest-golden seeded-draws "test/golden/generators/result-draws.edn"
  {:err-categories (mapv #(:error (gen/generate (gr/gen-err err) 10 %)) (range 6))
   :ok-kinds       (mapv #(-> (gen/generate (gr/gen-ok ok) 10 %) :ok type .getSimpleName) (range 4))
   :result-kinds   (mapv #(-> (draw-result %) keys first) (range 8))
   :result-fns     (mapv #(apply-result-fn [% 3]) (range 6))
   :plain-fns      (mapv #((gen/generate (gr/gen-plain-fn) 10 %) 3) (range 6))
   :extras-keys    (mapv #(vec (sort (map str (keys (draw-err-with-extras %))))) (range 3))})
