(ns hive-test.tcheck.properties
  "`for-all`, matching `clojure.test.check.properties`."
  (:require [hive-test.tcheck.generators :as gen]))

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

(defmacro for-all
  "`(for-all [x gen, y gen] body)` -> a property."
  [bindings & body]
  (let [pairs (partition 2 bindings)
        names (mapv first pairs)
        gens (mapv second pairs)]
    `(property [~@gens] (fn [~@names] ~@body))))
