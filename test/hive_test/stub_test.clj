(ns hive-test.stub-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.stub :refer [defstub]]
            [hive-test.stub-port :as port]
            [hive-test.trifecta :refer [deftrifecta]]))

(defprotocol StoreV1
  (fetch [this key]))

;; Static fixtures cover decorators; the scratch namespace below exercises
;; actual evolution of one protocol.
(defprotocol StoreV2
  (fetch-next [this key] [this key fallback])
  (put-next [this key value]))

(defstub StubV1 StoreV1 {:fetch (fn [_ key] [:found key])})
(defstub StubV2 StoreV2 {:fetch-next (fn [_ key & [fallback]]
                                      (or fallback [:found key]))})

(defrecord RecordingStore [delegate calls]
  StoreV2
  (fetch-next [_ key]
    (swap! calls conj [:fetch key])
    (fetch-next delegate key))
  (fetch-next [_ key fallback]
    (swap! calls conj [:fetch key fallback])
    (fetch-next delegate key fallback))
  (put-next [_ key value]
    (swap! calls conj [:put key value])
    (put-next delegate key value)))

(defn evolved-stub-behavior [key]
  (let [stub (->StubV2)
        calls (atom [])
        wrapped (->RecordingStore stub calls)
        old (fetch-next wrapped key)
        other-arity (fetch-next wrapped key :fallback)
        failure (try (put-next wrapped key 42)
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    {:implements? (satisfies? StoreV2 stub)
     :old old
     :other-arity other-arity
     :failure (select-keys failure [:stub :protocol :method])
     :calls @calls}))

(deftrifecta evolved-stub-trifecta
  #'evolved-stub-behavior
  {:golden-path "test/golden/stub/evolved.edn"
   :cases {:a :a}
   :gen (gen/elements [:a :b :c])
   :pred (fn [{:keys [implements? old other-arity failure calls]}]
           (and implements? (= :found (first old))
                (= other-arity :fallback)
                (= failure {:stub 'StubV2
                            :protocol 'hive-test.stub-test/StoreV2
                            :method 'put-next})
                (= 3 (count calls))
                (= [:put (second old) 42] (last calls))))
   :mutations [["not-a-protocol" (fn [_] {:implements? false})]]})

(deftest reexpansion-reads-all-current-methods
  (is (satisfies? StoreV1 (->StubV1)))
  (is (satisfies? StoreV2 (->StubV2)))
  (is (= [:found :k] (fetch-next (->StubV2) :k)))
  (is (= :fallback (fetch-next (->StubV2) :k :fallback)))
  (is (= {:stub 'StubV2 :protocol 'hive-test.stub-test/StoreV2 :method 'put-next}
         (try (put-next (->StubV2) :k 42)
              (catch clojure.lang.ExceptionInfo e (ex-data e)))))
  (let [error (try
                (binding [*ns* (the-ns 'hive-test.stub-test)]
                  (macroexpand '(hive-test.stub/defstub BadStub StoreV2
                                  {:missing (fn [_] nil)})))
                nil
                (catch clojure.lang.Compiler$CompilerException e
                  (ex-cause e))
                (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= 'missing (:method (ex-data error))))
    (is (= '[fetch-next put-next] (:valid-methods (ex-data error))))
    (is (every? #(.contains (ex-message error) (str %))
                '[missing fetch-next put-next]))))

(defstub AliasedStub port/ExternalPort
  {:lookup (fn [this key & [fallback]]
             (let [override-map [:receiver (instance? AliasedStub this)]]
               [override-map key fallback]))})

(deftest aliased-external-port-and-override-hygiene
  (let [stub (->AliasedStub)]
    (is (satisfies? port/ExternalPort stub))
    (is (= [[:receiver true] :key nil] (port/lookup stub :key)))
    (is (= [[:receiver true] :key :otherwise]
           (port/lookup stub :key :otherwise)))
    (is (= {:stub 'AliasedStub :protocol 'hive-test.stub-port/ExternalPort
            :method 'persist}
           (try (port/persist stub :key 42)
                (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(deftest same-protocol-reexpansion
  "A record expanded before protocol redefinition does NOT satisfy the new
   protocol object; re-expanding the unchanged defstub form repairs the stub."
  (let [scratch-name (symbol (str "hive-test.stub-evolution-" (gensym)))
        scratch (create-ns scratch-name)
        stub-form '(hive-test.stub/defstub EvolvingStub EvolvingPort
                     {:fetch (fn [this key] [:old (some? this) key])})]
    (try
      (binding [*ns* scratch]
        (refer 'clojure.core)
        (require 'hive-test.stub)
        (eval '(defprotocol EvolvingPort (fetch [this key])))
        (eval stub-form)
        (let [old ((var-get (ns-resolve scratch '->EvolvingStub)))]
          (is ((var-get (ns-resolve scratch 'satisfies?))
               (var-get (ns-resolve scratch 'EvolvingPort)) old))
          (eval '(defprotocol EvolvingPort
                   (fetch [this key])
                   (added [this key])))
          (let [new-protocol (var-get (ns-resolve scratch 'EvolvingPort))]
            (is (not (satisfies? new-protocol old)))
            (eval stub-form)
            (let [fresh ((var-get (ns-resolve scratch '->EvolvingStub)))
                  fetch-fn (var-get (ns-resolve scratch 'fetch))
                  added-fn (var-get (ns-resolve scratch 'added))]
              (is (satisfies? new-protocol fresh))
              (is (= [:old true :key] (fetch-fn fresh :key)))
              (is (= {:stub 'EvolvingStub
                      :protocol (symbol (str scratch-name) "EvolvingPort")
                      :method 'added}
                     (try (added-fn fresh :key)
                          (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))
      (finally (remove-ns scratch-name)))))
