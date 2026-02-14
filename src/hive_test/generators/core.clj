(ns hive-test.generators.core
  "Common generators for scalar values, collections, and identifiers."
  (:require [clojure.test.check.generators :as gen]))

(def gen-non-blank-string
  "Generator for non-blank strings."
  (gen/such-that (complement clojure.string/blank?)
                 gen/string-alphanumeric
                 100))

(def gen-keyword-ns
  "Generator for namespaced keywords like :foo/bar."
  (gen/let [ns-part gen-non-blank-string
            name-part gen-non-blank-string]
    (keyword ns-part name-part)))

(def gen-uuid-str
  "Generator for UUID strings."
  (gen/fmap str gen/uuid))

(def gen-timestamp
  "Generator for ISO-8601 timestamp strings."
  (gen/fmap (fn [[y m d h]]
              (format "%04d-%02d-%02dT%02d:00:00Z" y m d h))
            (gen/tuple (gen/choose 2024 2026)
                       (gen/choose 1 12)
                       (gen/choose 1 28)
                       (gen/choose 0 23))))

(def gen-project-id
  "Generator for project identifiers."
  (gen/fmap #(str "project-" %) gen-non-blank-string))

(def gen-agent-id
  "Generator for agent identifiers."
  (gen/fmap #(str "agent:" %) gen-non-blank-string))
