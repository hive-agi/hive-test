(ns hive-test.tcheck.clojure-test
  "`defspec` and `quick-check`, matching `clojure.test.check.clojure-test`."
  (:require [clojure.test :as t]
            [hive-test.tcheck.properties :as prop]
            [hive-test.tcheck.random :as rng]))

(def ^:dynamic *seed*
  "Seed for the next quick-check run. Bind to reproduce a failure."
  42)

(defn quick-check
  "Run `prop` `n` times. Returns {:result :num-tests :seed} and, on failure,
  the :fail arguments and the :failing-size that produced it."
  ([n prop] (quick-check n prop *seed*))
  ([n prop seed]
   (loop [i 0 r (rng/make-rng seed)]
     (if (< i n)
       (let [size (inc (mod i 50))
             {:keys [pass? args rng error]} (prop/check-once prop r size)]
         (if pass?
           (recur (inc i) rng)
           {:result false
            :num-tests (inc i)
            :seed seed
            :fail args
            :failing-size size
            :error error}))
       {:result true :num-tests n :seed seed}))))

(defn report
  "Assert `outcome` through clojure.test, naming the failing input."
  [outcome]
  (if (:result outcome)
    (t/is true)
    (t/is false
          (str "property failed after " (:num-tests outcome)
               " test(s), seed " (:seed outcome)
               ", input " (pr-str (:fail outcome))
               (when-let [e (:error outcome)]
                 (str ", threw " (pr-str (ex-message e))))))))

(defmacro defspec
  "`(defspec name n property)` -> a deftest running the property `n` times."
  ([name property] `(defspec ~name 100 ~property))
  ([name n property]
   `(t/deftest ~name
      (report (quick-check ~n ~property)))))
