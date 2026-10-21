(ns hive-test.generators.code-test
  "Facets for hive-test.generators.code: symbol strings read back as the
   symbol kind their docstring promises, parameter maps stay small and
   keyword-keyed, and a seeded golden."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.generators.code :as gcode]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

(defspec simple-symbol-str-round-trips-to-an-unqualified-symbol 100
  (prop/for-all [s gcode/gen-simple-symbol-str]
    (let [sym (symbol s)]
      (and (m/validate :string s)
           (m/validate [:and :symbol [:fn simple-symbol?]] sym)
           (= s (str sym))))))

(defspec qualified-symbol-str-round-trips-to-a-qualified-symbol 100
  (prop/for-all [s gcode/gen-qualified-symbol-str]
    (let [sym (symbol s)]
      (and (m/validate :qualified-symbol sym)
           (= s (str sym))))))

(defspec params-map-is-small-and-keyword-keyed 100
  (prop/for-all [p gcode/gen-params-map]
    (m/validate [:map-of {:max 5} :keyword :any] p)))

(deftest-golden seeded-draws "test/golden/generators/code-draws.edn"
  {:simple    (mapv #(gen/generate gcode/gen-simple-symbol-str 10 %) (range 4))
   :qualified (mapv #(gen/generate gcode/gen-qualified-symbol-str 10 %) (range 4))
   :params-n  (mapv #(count (gen/generate gcode/gen-params-map 10 %)) (range 6))})
