(ns hive-test.sandbox-contract-test
  "Name-matched facets for hive-test.sandbox (the behavioural suite is
   hive-test.sandbox-test): sandbox! materializes exactly its spec and
   destroy! removes the root (property over the contracts), destroy! refuses
   maps it did not mint, and a golden of a materialized tree."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.isolation :as iso]
            [hive-test.sandbox :as sb]))

(defn- tree
  "Relative path -> body (nil for directories) of every entry under `dir`."
  [dir]
  (let [root (io/file dir)]
    (into (sorted-map)
          (for [^java.io.File f (rest (file-seq root))]
            [(str (.relativize (.toPath root) (.toPath f)))
             (when (.isFile f) (slurp f))]))))

(defn- round-trip
  "Create, observe, destroy; answer what was on disk and whether it is gone."
  [spec]
  (let [h (sb/sandbox! spec)
        seen (tree (:dir h))
        destroyed (sb/destroy! h)]
    {:paths (set (keys (:paths h))) :tree seen
     :destroyed destroyed :gone? (not (.exists (io/file (:dir h))))}))

(hst/deftrifecta-from-schema sandbox-materializes-its-spec-then-vanishes
  round-trip
  {:in  [:map-of [:enum "a.txt" "src/b.clj" "dir"] [:or :string :nil]]
   :out [:map [:paths [:set :string]] [:tree [:map-of :string [:maybe :string]]]
         [:destroyed [:= true]] [:gone? [:= true]]]
   :rel (fn [spec {:keys [paths tree]}]
          (and (= (set (keys spec)) paths)
               (every? (fn [[rel body]] (= (some-> body str) (get tree rel))) spec)))
   :mutation true})

(deftest destroy-refuses-foreign-handles
  (let [d (str (java.nio.file.Files/createTempDirectory "foreign"
                                                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (is (nil? (sb/destroy! {:dir d})))
    (is (.exists (io/file d)))
    (.delete (io/file d))))

(def ^:private seen-in-fixture (atom nil))

(deftest sandbox-isolation-binds-and-destroys
  ((iso/with-isolations {:type :sandbox :spec {"x.txt" "hi"}})
   #(reset! seen-in-fixture (:dir sb/*sandbox*)))
  (is (string? @seen-in-fixture))
  (is (not (.exists (io/file @seen-in-fixture)))))

(deftest-golden materialized-tree "test/golden/sandbox/materialized-tree.edn"
  (sb/with-sandbox [h {"a.txt" "alpha" "src/b.clj" "(ns b)" "empty" nil
                       "gen.txt" (fn [{:keys [path]}] (str "generated:" path))}]
    {:tree  (tree (:dir h))
     :paths (vec (sort (keys (:paths h))))
     :file-resolves-outside-spec? (= (str (io/file (:dir h) "nope")) ((:file h) "nope"))}))
