(ns hive-test.stub-policy-test
  "Default policy, class targets and fault-injecting decorator composition for
   hive-test.stub/defstub."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.stub :as stub :refer [defstub]]
            [hive-test.stub-port :as port]
            [hive-test.trifecta :refer [deftrifecta]]))

(defrecord Plain [a])

(defstub ThrowingTagged port/TaggedPort)

(defstub DefaultedTagged port/TaggedPort
  {::stub/default (stub/by-tag {'String "none" 'Boolean false})
   :untagged (fn [_] :explicit)})

(defstub PartiallyDefaulted port/TaggedPort
  {::stub/default (stub/by-tag {'String "none"})})

(defn- re-find-free? [s fragment]
  (.contains ^String s ^String fragment))

(defn- thrown-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn default-policy-behavior
  "Observable behaviour of the tag->value default policy for a given label."
  [label-value]
  (let [defaulted (->DefaultedTagged)
        partial (->PartiallyDefaulted)]
    {:throws-without-default (select-keys (thrown-data #(port/label (->ThrowingTagged)))
                                          [:stub :method])
     :tagged-string (port/label defaulted)
     :tagged-boolean (port/open? defaulted)
     :override-beats-default (port/untagged defaulted)
     :partial-known (port/label partial)
     :partial-unknown (select-keys (thrown-data #(port/open? partial))
                                   [:method :tag])
     :echo label-value}))

(deftrifecta default-policy-trifecta
  #'default-policy-behavior
  {:golden-path "test/golden/stub/default_policy.edn"
   :cases {:a :a}
   :gen (gen/elements [:a :b :c])
   :pred (fn [{:keys [throws-without-default tagged-string tagged-boolean
                      override-beats-default partial-known partial-unknown]}]
           (and (= throws-without-default {:stub 'ThrowingTagged :method 'label})
                (= "none" tagged-string)
                (false? tagged-boolean)
                (= :explicit override-beats-default)
                (= "none" partial-known)
                (= partial-unknown {:method 'open? :tag 'Boolean})))
   :mutations [["nil-default" (fn [_] {:throws-without-default nil
                                       :tagged-string nil})]]})

(deftest class-target-is-refused-with-advice
  (let [error (try
                (binding [*ns* (the-ns 'hive-test.stub-policy-test)]
                  (macroexpand '(hive-test.stub/defstub Nope Plain)))
                nil
                (catch clojure.lang.Compiler$CompilerException e (ex-cause e))
                (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= 'Nope (:stub (ex-data error))))
    (is (re-find-free? (ex-message error) "stub the protocol"))))

;; A fault-injecting decorator: delegates until the nth call of a method, then
;; raises the configured fault. It sees the stub only through the protocol.
(defrecord FaultInjecting [delegate faults calls]
  port/ExternalPort
  (lookup [_ key]
    (let [n (get (swap! calls update :lookup (fnil inc 0)) :lookup)]
      (when-let [f (get-in faults [:lookup n])] (throw f)))
    (port/lookup delegate key))
  (lookup [_ key fallback]
    (port/lookup delegate key fallback))
  (persist [_ key value]
    (let [n (get (swap! calls update :persist (fnil inc 0)) :persist)]
      (when-let [f (get-in faults [:persist n])] (throw f)))
    (port/persist delegate key value)))

(defstub BackingPort port/ExternalPort
  {:lookup (fn [_ key & [fallback]] (or fallback [:found key]))
   :persist (fn [_ key value] [:saved key value])})

(deftest fault-injecting-decorator-wraps-a-stub
  (let [calls (atom {})
        boom (ex-info "disk full" {:fault :io})
        wrapped (->FaultInjecting (->BackingPort) {:persist {2 boom}} calls)]
    (is (= [:found :k] (port/lookup wrapped :k)))
    (is (= [:saved :k 1] (port/persist wrapped :k 1)))
    (is (identical? boom (try (port/persist wrapped :k 2)
                              (catch clojure.lang.ExceptionInfo e e))))
    (is (= [:saved :k 3] (port/persist wrapped :k 3)))
    (is (= {:lookup 1 :persist 3} @calls))))

(defstub AliasedBare port/ExternalPort)

(deftest decorating-an-unconfigured-stub-surfaces-the-stub-failure
  (let [wrapped (->FaultInjecting (->AliasedBare) {} (atom {}))]
    (is (= {:stub 'AliasedBare :protocol 'hive-test.stub-port/ExternalPort
            :method 'persist}
           (thrown-data #(port/persist wrapped :k 1))))))
