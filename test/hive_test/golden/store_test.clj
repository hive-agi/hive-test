(ns hive-test.golden.store-test
  "Tests for the GoldenStore port and its adapters."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [hive-test.golden.store :as store]
            [hive-test.golden :as golden])
  (:import [java.io File]))

(deftest atom-store-roundtrip
  (testing "in-memory store: exists?/read transition across a write"
    (let [s (store/memory-store)]
      (is (false? (store/-exists? s "p")))
      (is (nil? (store/-read s "p")))
      (is (= {:a 1} (store/-write! s "p" {:a 1})))
      (is (true? (store/-exists? s "p")))
      (is (= {:a 1} (store/-read s "p"))))))

(deftest atom-store-seeded
  (testing "memory-store accepts an initial map"
    (let [s (store/memory-store {"k" {:v 1}})]
      (is (true? (store/-exists? s "k")))
      (is (= {:v 1} (store/-read s "k"))))))

(deftest file-store-roundtrip
  (testing "filesystem store: write creates the file, read returns the value"
    (let [s    (store/file-store)
          path (str (System/getProperty "java.io.tmpdir")
                    "/hive-test-store-" (System/nanoTime) ".edn")]
      (try
        (is (false? (store/-exists? s path)))
        (store/-write! s path {:x [1 2 3]})
        (is (.exists (File. path)))
        (is (true? (store/-exists? s path)))
        (is (= {:x [1 2 3]} (store/-read s path)))
        (finally (.delete (File. path)))))))

(deftest golden-facade-injects-store
  (testing "assert-golden runs against an injected in-memory store — no filesystem"
    (let [s (store/memory-store)]
      (binding [golden/*store* s]
        (golden/assert-golden "vpath" {:v 1})     ; first run writes to memory
        (golden/assert-golden "vpath" {:v 1}))     ; second run matches
      (is (= {:v 1} (store/-read s "vpath"))))))

(defspec atom-store-read-after-write 100
  (prop/for-all [k gen/string-alphanumeric
                 v (gen/map gen/keyword gen/small-integer)]
    (let [s (store/memory-store)]
      (store/-write! s k v)
      (= v (store/-read s k)))))
