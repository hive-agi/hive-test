(ns hive-test.generators.code
  "Generators for code-domain values: symbol strings and parameter maps.

   Domain-specific generators (e.g. clj-kondo var-usage records, analyzer
   nodes) belong in the consuming project's own test support, not here."
  (:require [clojure.test.check.generators :as gen]))

(def gen-simple-symbol-str
  "Generator for an unqualified symbol string like \"foo-bar\"."
  (gen/fmap str gen/symbol))

(def gen-qualified-symbol-str
  "Generator for a qualified symbol string like \"foo.bar/baz\"."
  (gen/fmap str gen/symbol-ns))

(def gen-params-map
  "Generator for a small keyword-keyed parameter map."
  (gen/map gen/keyword gen/simple-type-printable {:max-elements 5}))
