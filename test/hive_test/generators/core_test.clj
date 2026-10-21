(ns hive-test.generators.core-test
  "Facets for hive-test.generators.core: every generator's output conforms to
   the malli shape its docstring promises (property over 100 seeded draws), and
   a golden of seeded draws pins determinism."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.generators.core :as gc]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

(def NonBlank [:and :string [:fn (complement str/blank?)]])
(def Timestamp [:re #"^20(2[4-6])-(0[1-9]|1[0-2])-(0[1-9]|1\d|2[0-8])T([01]\d|2[0-3]):00:00Z$"])
(def UuidStr [:fn #(some? (parse-uuid %))])

(defspec non-blank-string-is-alphanumeric-and-non-blank 100
  (prop/for-all [s gc/gen-non-blank-string]
    (and (m/validate NonBlank s) (re-matches #"[A-Za-z0-9]+" s))))

(defspec keyword-ns-is-qualified-with-non-blank-parts 100
  (prop/for-all [k gc/gen-keyword-ns]
    (and (m/validate :qualified-keyword k)
         (not (str/blank? (namespace k)))
         (not (str/blank? (name k))))))

(defspec uuid-str-round-trips-through-parse-uuid 100
  (prop/for-all [s gc/gen-uuid-str]
    (and (m/validate [:and :string UuidStr] s)
         (= s (str (parse-uuid s))))))

(defspec timestamp-is-a-valid-iso-instant-in-range 100
  (prop/for-all [t gc/gen-timestamp]
    (and (m/validate Timestamp t)
         (inst? (java.util.Date/from (java.time.Instant/parse t))))))

(defspec project-and-agent-ids-carry-their-prefix 100
  (prop/for-all [p gc/gen-project-id a gc/gen-agent-id]
    (and (m/validate [:re #"^project-[A-Za-z0-9]+$"] p)
         (m/validate [:re #"^agent:[A-Za-z0-9]+$"] a))))

(deftest generators-are-seed-deterministic
  (doseq [g [gc/gen-non-blank-string gc/gen-keyword-ns gc/gen-uuid-str
             gc/gen-timestamp gc/gen-project-id gc/gen-agent-id]
          seed (range 5)]
    (is (= (gen/generate g 10 seed) (gen/generate g 10 seed)))))

(deftest-golden seeded-draws "test/golden/generators/core-draws.edn"
  {:non-blank  (mapv #(gen/generate gc/gen-non-blank-string 10 %) (range 4))
   :keyword-ns (mapv #(gen/generate gc/gen-keyword-ns 10 %) (range 4))
   :uuid-str   (mapv #(gen/generate gc/gen-uuid-str 10 %) (range 2))
   :timestamp  (mapv #(gen/generate gc/gen-timestamp 10 %) (range 4))
   :project-id (mapv #(gen/generate gc/gen-project-id 10 %) (range 3))
   :agent-id   (mapv #(gen/generate gc/gen-agent-id 10 %) (range 3))})
