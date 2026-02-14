(ns hive-test.generators.memory
  "Generators for hive memory entries, types, and tags."
  (:require [clojure.test.check.generators :as gen]
            [hive-test.generators.core :as gen-core]))

(def memory-types
  "Valid memory entry types."
  [:axiom :principle :decision :convention :snippet :note :plan])

(def duration-categories
  "Valid duration/TTL categories."
  [:ephemeral :short :medium :long :permanent])

(def gen-memory-type
  "Generator for memory entry types."
  (gen/elements memory-types))

(def gen-duration
  "Generator for duration/TTL categories."
  (gen/elements duration-categories))

(def gen-tag
  "Generator for tag strings."
  (gen/fmap #(str "tag-" %) gen-core/gen-non-blank-string))

(def gen-tags
  "Generator for a vector of 0-5 tags."
  (gen/vector gen-tag 0 5))

(def gen-scope
  "Generator for scope strings."
  (gen/fmap #(str "scope-" %) gen/string-alphanumeric))

(def gen-memory-entry
  "Generator for memory entry maps."
  (gen/let [type gen-memory-type
            content gen-core/gen-non-blank-string
            tags gen-tags
            duration gen-duration]
    {:type type
     :content content
     :tags tags
     :duration duration}))

(def gen-memory-entry-full
  "Generator for memory entries with all optional fields."
  (gen/let [type gen-memory-type
            content gen-core/gen-non-blank-string
            tags gen-tags
            duration gen-duration
            scope gen-scope
            agent-id gen-core/gen-agent-id]
    {:type type
     :content content
     :tags tags
     :duration duration
     :scope scope
     :agent-id agent-id}))
