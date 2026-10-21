(ns hive-test.isolation-test
  "Facets for hive-test.isolation: composed fixtures run the test exactly
   once, wrapping outermost-first (properties over the contracts), and a
   golden of the enter/exit order and the unknown-spec refusal."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.isolation :as iso]))

(defn- tracing-fixture [log tag]
  (fn [f] (swap! log conj [:enter tag]) (f) (swap! log conj [:exit tag])))

(defn- trace-composition
  "Compose n tracing fixtures, run one test through them; answer the log."
  [n]
  (let [log (atom [])]
    ((iso/compose-fixtures (mapv #(tracing-fixture log %) (range n)))
     #(swap! log conj [:test]))
    @log))

(hst/deftrifecta-from-schema compose-fixtures-nests-outermost-first
  trace-composition
  {:in  [:int {:min 0 :max 8}]
   :out [:vector [:or [:tuple [:enum :enter :exit] :int] [:tuple [:= :test]]]]
   :rel (fn [n log] (= log (concat (map #(vector :enter %) (range n))
                                   [[:test]]
                                   (map #(vector :exit %) (reverse (range n))))))
   :mutation true})

(defn- runs-through-noops [specs]
  (let [n (atom 0)] ((apply iso/with-isolations specs) #(swap! n inc)) @n))

(hst/deftrifecta-from-schema with-isolations-runs-the-test-once
  runs-through-noops
  {:in  [:vector [:= :noop]]
   :out [:= 1]
   :rel (fn [_ n] (= 1 n))
   :mutation true})

(defmethod iso/emit-isolation ::tagged [{:keys [log tag]}]
  (tracing-fixture log tag))

(deftest map-specs-dispatch-on-type
  (let [log (atom [])]
    ((iso/with-isolations {:type ::tagged :log log :tag 1} :noop {:type ::tagged :log log :tag 2})
     #(swap! log conj [:test]))
    (is (= [[:enter 1] [:enter 2] [:test] [:exit 2] [:exit 1]] @log))))

(deftest-golden orders-and-refusals "test/golden/isolation/orders-and-refusals.edn"
  {:zero   (trace-composition 0)
   :two    (trace-composition 2)
   :unknown-keyword (ex-message (try (iso/with-isolations ::nope) nil
                                     (catch clojure.lang.ExceptionInfo e e)))
   :unknown-map     (ex-data (try (iso/with-isolations {:type ::nope :x 1}) nil
                                  (catch clojure.lang.ExceptionInfo e e)))})
