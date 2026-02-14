(ns hive-test.generators.kg
  "Generators for Knowledge Graph edges, nodes, and relations.
   Extracted from hive-mcp edges_property_test.clj."
  (:require [clojure.test.check.generators :as gen]
            [hive-test.generators.core :as gen-core]))

(def relation-types
  "Valid KG relation types — mirrors hive-mcp.knowledge-graph.schema/relation-types."
  #{:implements :supersedes :refines :contradicts
    :depends-on :derived-from :applies-to :co-accessed
    :projects-to})

(def source-types
  "Valid KG source types."
  #{:manual :automated :inferred :co-access})

(def gen-relation
  "Generator for KG relation type keywords."
  (gen/elements (vec relation-types)))

(def gen-source-type
  "Generator for KG source types."
  (gen/elements (vec source-types)))

(def gen-confidence
  "Generator for confidence scores in [0.0, 1.0]."
  (gen/double* {:min 0.0 :max 1.0 :NaN? false :infinite? false}))

(def gen-invalid-confidence
  "Generator for out-of-bounds confidence values."
  (gen/one-of
   [(gen/double* {:min 1.001 :max 100.0 :NaN? false :infinite? false})
    (gen/double* {:min -100.0 :max -0.001 :NaN? false :infinite? false})]))

(def gen-node-id
  "Generator for KG node IDs (timestamp-based format)."
  (gen/fmap #(str "node-" %) (gen/fmap str gen/uuid)))

(def gen-delta
  "Generator for confidence delta values."
  (gen/double* {:min -2.0 :max 2.0 :NaN? false :infinite? false}))

(def gen-edge-params
  "Generator for minimal edge parameter maps."
  (gen/let [from gen-node-id
            to gen-node-id
            relation gen-relation
            confidence gen-confidence]
    {:from from :to to :relation relation :confidence confidence}))

(def gen-edge-params-full
  "Generator for edge params with all optional fields."
  (gen/let [from gen-node-id
            to gen-node-id
            relation gen-relation
            confidence gen-confidence
            scope gen-core/gen-project-id
            source-type gen-source-type
            created-by gen-core/gen-agent-id]
    {:from from :to to :relation relation :confidence confidence
     :scope scope :source-type source-type :created-by created-by}))
