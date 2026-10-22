(ns hive-test.generators.memory-test
  "Facets for hive-test.generators.memory: entries conform to a closed malli
   MemoryEntry schema built from the namespace's own vocabularies, the full
   variant is a strict extension of the minimal one, and a seeded golden."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.generators.memory :as gm]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

(def MemoryType (into [:enum] gm/memory-types))
(def Duration (into [:enum] gm/duration-categories))
(def Tag [:re #"^tag-[A-Za-z0-9]+$"])

(def MemoryEntry
  [:map {:closed true}
   [:type MemoryType]
   [:content [:and :string [:fn (complement str/blank?)]]]
   [:tags [:vector {:max 5} Tag]]
   [:duration Duration]])

(def MemoryEntryFull
  [:map {:closed true}
   [:type MemoryType]
   [:content [:and :string [:fn (complement str/blank?)]]]
   [:tags [:vector {:max 5} Tag]]
   [:duration Duration]
   [:scope [:re #"^scope-[A-Za-z0-9]*$"]]
   [:agent-id [:re #"^agent:[A-Za-z0-9]+$"]]])

(deftest vocabularies-are-distinct-keyword-vectors
  (is (m/validate [:vector {:min 1} :keyword] gm/memory-types))
  (is (apply distinct? gm/memory-types))
  (is (m/validate [:vector {:min 1} :keyword] gm/duration-categories))
  (is (apply distinct? gm/duration-categories)))

(defspec scalar-generators-draw-from-their-vocabularies 100
  (prop/for-all [t gm/gen-memory-type d gm/gen-duration
                 tag gm/gen-tag tags gm/gen-tags s gm/gen-scope]
    (and (m/validate MemoryType t)
         (m/validate Duration d)
         (m/validate Tag tag)
         (m/validate [:vector {:max 5} Tag] tags)
         (str/starts-with? s "scope-"))))

(defspec memory-entry-conforms-to-the-closed-schema 100
  (prop/for-all [e gm/gen-memory-entry]
    (m/validate MemoryEntry e)))

(defspec full-entry-extends-the-minimal-shape 100
  (prop/for-all [e gm/gen-memory-entry-full]
    (and (m/validate MemoryEntryFull e)
         (m/validate MemoryEntry (dissoc e :scope :agent-id)))))

(deftest-golden seeded-draws "test/golden/generators/memory-draws.edn"
  {:types     (mapv #(gen/generate gm/gen-memory-type 10 %) (range 8))
   :durations (mapv #(gen/generate gm/gen-duration 10 %) (range 6))
   :entry     (gen/generate gm/gen-memory-entry 10 0)
   :full-keys (vec (sort (keys (gen/generate gm/gen-memory-entry-full 10 0))))})
