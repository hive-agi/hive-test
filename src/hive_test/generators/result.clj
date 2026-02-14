(ns hive-test.generators.result
  "Generators for the Result DSL (ok/err monadic type).
   Extracted from hive-mcp result_property_test.clj."
  (:require [clojure.test.check.generators :as gen]))

;; Error category taxonomy — mirrors hive-mcp.dns.result.taxonomy
(def error-categories
  "Known error categories from the Result DSL taxonomy."
  [:io/timeout :io/read-failure
   :sdk/invalid-request
   :kg/node-not-found
   :chroma/connection-failed
   :parse/invalid-json
   :effect/exception
   :transport/timeout
   :drone/spawn-failed
   :emacs/not-connected])

(def gen-err-category
  "Generator for error category keywords."
  (gen/elements error-categories))

(def gen-ok-value
  "Generator for ok result values (any printable scalar)."
  gen/any-printable)

(defn gen-ok
  "Generator for ok Results. Requires ok constructor fn.
   Usage: (gen-ok r/ok)"
  [ok-fn]
  (gen/fmap ok-fn gen/any-printable))

(defn gen-err
  "Generator for err Results. Requires err constructor fn.
   Usage: (gen-err r/err)"
  [err-fn]
  (gen/fmap err-fn gen-err-category))

(defn gen-result
  "Generator for any Result (ok or err). Requires constructors.
   Usage: (gen-result r/ok r/err)"
  [ok-fn err-fn]
  (gen/one-of [(gen-ok ok-fn) (gen-err err-fn)]))

(defn gen-result-fn
  "Generator for functions that return Results.
   Produces fns: (any-value -> Result). Requires constructors.
   Usage: (gen-result-fn r/ok r/err)"
  [ok-fn err-fn]
  (gen/elements [(fn [x] (ok-fn x))
                 (fn [x] (ok-fn (str x)))
                 (fn [_] (err-fn :test/generated))
                 (fn [x] (ok-fn [x]))
                 (fn [x] (ok-fn {:wrapped x}))]))

(defn gen-plain-fn
  "Generator for pure (non-Result) functions."
  []
  (gen/elements [identity
                 str
                 (fn [x] [x])
                 (fn [x] {:v x})
                 (fn [_] 42)]))

(defn gen-err-with-extras
  "Generator for err Results with extra metadata fields.
   Usage: (gen-err-with-extras)"
  []
  (gen/let [cat gen-err-category
            extras (gen/map gen/keyword-ns gen/any-printable {:max-elements 3})]
    (-> extras
        (dissoc :ok)
        (assoc :error cat))))
