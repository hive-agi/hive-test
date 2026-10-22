(ns hive-test.mcp.nrepl-test
  "Facets for hive-test.mcp.nrepl: parse-test-results as a property over its
   contract (passed? iff the last value is a summary without fail/error), and
   a golden of the client driven through a loopback fake nREPL."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mcp.fake-nrepl :as fake]
            [hive-test.mcp.nrepl :as nrepl]
            [hive-test.sandbox :as sb]))

(def ^:private GenResponse
  "Responses with the shapes a run emits: output, error text, a summary value."
  [:map {:closed true}
   [:out {:optional true} :string]
   [:err {:optional true} :string]
   [:summary {:optional true} [:map {:closed true}
                               [:test [:int {:min 0 :max 5}]] [:pass [:int {:min 0 :max 5}]]
                               [:fail [:int {:min 0 :max 2}]] [:error [:int {:min 0 :max 2}]]]]])

(defn- ->wire [{:keys [out err summary]}]
  (cond-> {} out (assoc "out" out) err (assoc "err" err) summary (assoc "value" (pr-str summary))))

(defn- parse-generated [rs] (nrepl/parse-test-results (mapv ->wire rs)))

(hst/deftrifecta-from-schema parse-test-results-passes-iff-the-last-summary-is-clean
  parse-generated
  {:in  [:vector {:max 4} GenResponse]
   :out nrepl/TestResults
   :rel (fn [rs out]
          (let [s (last (keep :summary rs))]
            (and (= s (:summary out))
                 (= (:passed? out) (boolean (and s (zero? (+ (:fail s) (:error s))))))
                 (= (apply str (keep :out rs)) (:output out)))))
   :mutation true})

(deftest read-nrepl-port-reads-the-file-or-answers-nil
  (sb/with-sandbox [h {".nrepl-port" " 4242\n"}]
    (is (= 4242 (nrepl/read-nrepl-port (:dir h)))))
  (sb/with-sandbox [h {}]
    (is (nil? (nrepl/read-nrepl-port (:dir h))))))

(defn- status-through-fake [responses]
  (fake/with-fake-nrepl [f responses]
    (sb/with-sandbox [h {".nrepl-port" (str (:port f))}]
      (-> (nrepl/test-status! (:dir h)) (dissoc :port) (update :raw-values vec)))))

(deftest-golden client-round-trips "test/golden/mcp/nrepl-client.edn"
  {:passing (status-through-fake fake/passing)
   :failing (status-through-fake fake/failing)
   :no-port (sb/with-sandbox [h {}] (nrepl/test-status! (:dir h)))
   :sent    (fake/with-fake-nrepl [f fake/passing]
              (let [c (nrepl/connect! (:port f))]
                (try (nrepl/run-tests! c :ns "a.b-test")
                     (nrepl/eval! c "(+ 1 2)" :session "s1" :ns "my.ns")
                     (finally (nrepl/disconnect! c))))
              (Thread/sleep 50)
              @(:log f))
   :eval    (fake/with-fake-nrepl [f [{"value" "3"}]]
              (let [c (nrepl/connect! (:port f))]
                (try (nrepl/eval! c "(+ 1 2)") (finally (nrepl/disconnect! c)))))})
