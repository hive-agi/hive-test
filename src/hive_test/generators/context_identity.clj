(ns hive-test.generators.context-identity
  "Generators for context identity ADTs."
  (:require [clojure.test.check.generators :as gen]
            [hive-dsl.context.identity :as ctx-id]))

(def gen-slave-id
  "Generate realistic slave-id strings like 'coordinator:947426'."
  (gen/fmap #(str "coordinator:" %) (gen/choose 100000 999999)))

(def gen-caller-id
  "Generate CallerId ADT values."
  (gen/one-of
   [(gen/return (ctx-id/caller-id :caller/coordinator))
    (gen/fmap #(ctx-id/caller-id :caller/named {:slave-id %})
              gen-slave-id)]))

(def gen-project-id
  "Generate project-id strings like 'project-abc123'."
  (gen/fmap #(str "project-" %)
            (gen/not-empty gen/string-alphanumeric)))

(def gen-project-scope
  "Generate ProjectScope ADT values."
  (gen/one-of
   [(gen/return (ctx-id/project-scope :project/global))
    (gen/fmap #(ctx-id/project-scope :project/scoped {:project-id %})
              gen-project-id)]))
