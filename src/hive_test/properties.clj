(ns hive-test.properties
  "Property-based test macros that generate defspec forms.
   Provides reusable property templates for monad laws, roundtrips,
   idempotency, totality, and FSM termination."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(defmacro defprops-monad
  "Generate three defspec forms verifying monad laws:
   - left-identity:  (bind (unit x) f) = (f x)
   - right-identity: (bind m unit) = m
   - associativity:  (bind (bind m f) g) = (bind m #(bind (f %) g))

   Arguments:
   - prefix:  name prefix for the generated defspecs
   - gen-val: generator for values
   - gen-fn:  generator for monadic functions (a -> M b)
   - bind-fn: the bind/flatmap operation (M a, a -> M b) -> M b
   - unit-fn: the return/unit operation a -> M a
   - opts:    optional map with :num-tests (default 200)"
  [prefix gen-val gen-fn bind-fn unit-fn & [{:keys [num-tests]
                                             :or {num-tests 200}}]]
  (let [left-name  (symbol (str prefix "-left-identity"))
        right-name (symbol (str prefix "-right-identity"))
        assoc-name (symbol (str prefix "-associativity"))]
    `(do
       (defspec ~left-name ~num-tests
         (prop/for-all [v# ~gen-val
                        f# ~gen-fn]
                       (= (~bind-fn (~unit-fn v#) f#)
                          (f# v#))))

       (defspec ~right-name ~num-tests
         (prop/for-all [m# (gen/one-of [(gen/fmap ~unit-fn ~gen-val)])
                        ;; also test with error values if gen-fn can produce them
                        ]
                       (= (~bind-fn m# ~unit-fn)
                          m#)))

       (defspec ~assoc-name ~num-tests
         (prop/for-all [m# (gen/fmap ~unit-fn ~gen-val)
                        f# ~gen-fn
                        g# ~gen-fn]
                       (= (~bind-fn (~bind-fn m# f#) g#)
                          (~bind-fn m# (fn [x#] (~bind-fn (f# x#) g#)))))))))

(defmacro defprop-roundtrip
  "Generate a defspec verifying encode/decode roundtrip:
   (decode (encode x)) = x

   Arguments:
   - name:   defspec name
   - encode: encoding function
   - decode: decoding function
   - gen:    generator for input values
   - opts:   optional map with :num-tests (default 200)"
  [name encode decode gen & [{:keys [num-tests]
                              :or {num-tests 200}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [v# ~gen]
                   (= v# (~decode (~encode v#))))))

(defmacro defprop-idempotent
  "Generate a defspec verifying idempotency: f(f(x)) = f(x)

   Arguments:
   - name: defspec name
   - f:    the idempotent function
   - gen:  generator for input values
   - opts: optional map with :num-tests (default 200)"
  [name f gen & [{:keys [num-tests]
                  :or {num-tests 200}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [v# ~gen]
                   (= (~f (~f v#))
                      (~f v#)))))

(defmacro defprop-total
  "Generate a defspec verifying totality: f never throws for any valid input.

   Arguments:
   - name: defspec name
   - f:    the function under test
   - gen:  generator for input values
   - opts: optional map with :num-tests (default 200),
           :pred (optional predicate the result must satisfy)"
  [name f gen & [{:keys [num-tests pred]
                  :or {num-tests 200}}]]
  (if pred
    `(defspec ~name ~num-tests
       (prop/for-all [v# ~gen]
                     (let [result# (~f v#)]
                       (~pred result#))))
    `(defspec ~name ~num-tests
       (prop/for-all [v# ~gen]
                     (do (~f v#) true)))))

(defmacro defprop-fsm-terminates
  "Generate a defspec verifying FSM termination:
   run-fn always reaches a terminal state within max-turns.

   Arguments:
   - name:      defspec name
   - run-fn:    function (inputs -> result) that runs the FSM
   - gen-inputs: generator for FSM input maps
   - max-turns: maximum turns budget
   - opts:      optional map with :num-tests (default 100),
                :terminal-pred (predicate for terminal state, default :terminated?)"
  [name run-fn gen-inputs max-turns & [{:keys [num-tests terminal-pred]
                                        :or {num-tests 100
                                             terminal-pred :terminated?}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [inputs# ~gen-inputs]
                   (let [result# (~run-fn (assoc inputs# :max-turns ~max-turns))]
                     (and (~terminal-pred result#)
                          (<= (:turns result# 0) ~max-turns))))))

(defmacro defprop-complement
  "Generate a defspec verifying two predicates are exact complements:
   For all x: (pred-a x) = (not (pred-b x))

   Arguments:
   - name:   defspec name
   - pred-a: first predicate
   - pred-b: second predicate (complement of first)
   - gen:    generator for input values
   - opts:   optional map with :num-tests (default 200)"
  [name pred-a pred-b gen & [{:keys [num-tests]
                              :or {num-tests 200}}]]
  `(defspec ~name ~num-tests
     (prop/for-all [v# ~gen]
                   (not= (~pred-a v#) (~pred-b v#)))))
