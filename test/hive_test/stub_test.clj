(ns hive-test.stub-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.stub :refer [defstub]]
            [hive-test.trifecta :refer [deftrifecta]]))

(defprotocol StoreV1
  (fetch [this key]))

;; The protocol's next version adds an arity and a method. JVM interface
;; classes cannot be redefined in-process, so give the evolved version a new
;; name; both stubs are expanded from their respective live protocol Vars.
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
                  (ex-cause e)))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= 'missing (:method (ex-data error))))))
