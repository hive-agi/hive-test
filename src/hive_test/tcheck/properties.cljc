(ns hive-test.tcheck.properties
  "`for-all`, matching `clojure.test.check.properties`."
  (:require [hive-test.tcheck.generators :as gen]
            [hive-test.contract :as mc]))

(defn property
  "A property over `gens`: `f` is called with one value per generator and is
  true when the property holds."
  [gens f]
  {:gens (vec gens) :f f})

(defn check-once
  "Run `prop` once against `rnd`, returning {:pass? :args :rng :error}."
  [prop rnd size]
  (loop [gs (:gens prop) args [] r rnd]
    (if (seq gs)
      (let [[v r'] (gen/call (first gs) r size)]
        (recur (rest gs) (conj args v) r'))
      (let [result (try
                     {:pass? (clojure.core/boolean (apply (:f prop) args))}
                     (catch #?(:clj Throwable :cljs :default :default Exception) e
                       {:pass? false :error e}))]
        (assoc result :args args :rng r)))))

;; Contracts: `mc/=>` registers each only when malli is on the classpath
;; (see hive-test.contract); hive-test itself never depends on malli.

(def Prop
  "A property value. Generates the always-true property over one int."
  [:map {:gen/fmap (fn [_] (property [gen/small-integer] (constantly true))) :gen/schema :nil}
   [:gens [:vector gen/Gen]]
   [:f ifn?]])

(def Outcome
  "One run of a property."
  [:map [:pass? :boolean] [:args [:vector :any]] [:rng [:map [:state :int]]] [:error {:optional true} :any]])

(mc/=> property   [:=> [:cat [:sequential gen/Gen] [:fn {:gen/elements [(constantly true)]} ifn?]] Prop])
(mc/=> check-once [:=> [:cat Prop [:map [:state [:int {:min 0}]]] [:int {:min 0}]] Outcome])

(defmacro for-all
  "`(for-all [x gen, y gen] body)` -> a property."
  [bindings & body]
  (let [pairs (partition 2 bindings)
        names (mapv first pairs)
        gens (mapv second pairs)]
    `(property [~@gens] (fn [~@names] ~@body))))
