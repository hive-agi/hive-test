(ns hive-test.generators.context-identity-test
  "Facets for hive-test.generators.context-identity: generated ADT values
   round-trip through hive-dsl.context.identity's own string/parse pair, so the
   generators can only emit values the real port accepts."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.context.identity :as ctx-id]
            [hive-test.generators.context-identity :as gci]
            [malli.core :as m]))

(defspec slave-id-is-a-six-digit-coordinator-id 100
  (prop/for-all [s gci/gen-slave-id]
    (m/validate [:re #"^coordinator:[1-9]\d{5}$"] s)))

(defspec project-id-is-prefixed-and-non-empty 100
  (prop/for-all [p gci/gen-project-id]
    (m/validate [:re #"^project-[A-Za-z0-9]+$"] p)))

(defspec caller-id-round-trips-through-the-port 100
  (prop/for-all [cid gci/gen-caller-id]
    (= cid (ctx-id/parse-caller-id (ctx-id/caller-id-string cid)))))

(defspec project-scope-round-trips-through-the-port 100
  (prop/for-all [ps gci/gen-project-scope]
    (= ps (ctx-id/parse-project-scope (ctx-id/project-scope-string ps)))))

(deftest both-adt-variants-are-reachable
  (let [cids (map #(gen/generate gci/gen-caller-id 10 %) (range 40))
        pss  (map #(gen/generate gci/gen-project-scope 10 %) (range 40))]
    (is (< 1 (count (set (map ctx-id/caller-id-string cids)))))
    (is (some #(= "coordinator" (ctx-id/caller-id-string %)) cids))
    (is (some #(nil? (ctx-id/project-scope-string %)) pss))
    (is (some #(some? (ctx-id/project-scope-string %)) pss))))
