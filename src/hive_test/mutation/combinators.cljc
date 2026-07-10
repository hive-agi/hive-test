(ns hive-test.mutation.combinators
  "Mutation combinators: build named Mutation values that state a mutant's
   INTENT instead of retyping the subject's body.

   Each returns a hive-test.mutation/Mutation, usable directly in a :mutations
   vector or wrapped in a [label mutation] pair to override the label.

   Input-independent combinators (always, const-*, echo-arg) ignore the subject.
   Output-transforming combinators (drop-key, assoc-const, off-by-one,
   negate-pred) WRAP the original fn — pass the subject value; the :mutations
   vector is evaluated before the var is rebound, so it captures the real impl."
  (:require [hive-test.mutation :as mut]))

(defn always
  "Mutant that ignores its args and always returns v."
  [v]
  (mut/mutation "always" (fn [& _] v)))

(def const-nil   "Mutant returning nil."   (mut/mutation "const-nil"   (fn [& _] nil)))
(def const-true  "Mutant returning true."  (mut/mutation "const-true"  (fn [& _] true)))
(def const-false "Mutant returning false." (mut/mutation "const-false" (fn [& _] false)))

(defn echo-arg
  "Mutant returning its nth arg unchanged (default 0) — a no-op for endofns."
  ([] (echo-arg 0))
  ([n] (mut/mutation (str "echo-arg-" n) (fn [& args] (nth args n nil)))))

(defn drop-key
  "Mutant that dissocs k from orig's map result."
  [orig k]
  (mut/mutation (str "drop-key-" k) (fn [& args] (dissoc (apply orig args) k))))

(defn assoc-const
  "Mutant that assocs k -> v onto orig's map result."
  [orig k v]
  (mut/mutation (str "assoc-const-" k) (fn [& args] (assoc (apply orig args) k v))))

(defn off-by-one
  "Mutant that increments orig's numeric result."
  [orig]
  (mut/mutation "off-by-one" (fn [& args] (inc (apply orig args)))))

(defn negate-pred
  "Mutant that logically negates orig's boolean result."
  [orig]
  (mut/mutation "negate-pred" (fn [& args] (not (apply orig args)))))
