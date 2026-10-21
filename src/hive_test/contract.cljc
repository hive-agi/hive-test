(ns hive-test.contract
  "Optional malli function contracts for hive-test's own fns.

   hive-test lands on the :test classpath of most of the fleet, so it declares
   no malli dependency (see deps.edn), and it must not depend on hive-schemas
   (hive-schemas depends on hive-test). `=>` keeps the contracts anyway: on a
   JVM Clojure compile with malli on the classpath it expands to
   `malli.core/=>`; everywhere else (no malli, ClojureScript, babashka, cljw,
   cljrs) it expands to nil. A consumer without malli pays nothing; one with
   malli (hive-test's own :test alias, every hive-schemas user) gets the
   contracts registered, instrumentable and counted by hive-schemas.coverage.

   Schemas passed here must be plain data plus clojure.core predicates, so the
   form reads and compiles whether or not malli is present."
  #?(:cljs (:require-macros [hive-test.contract])))

;; SPDX-License-Identifier: MIT

#?(:clj
   (defn- malli-present?
     "True when malli.core can be loaded by this JVM."
     []
     (try (require 'malli.core) true (catch Throwable _ false))))

(defmacro =>
  "`(=> fn-sym schema)`: register `schema` as fn-sym's malli function schema
   when compiling JVM Clojure with malli available; otherwise expand to nil."
  [fn-sym schema]
  #?(:clj  (when (and (nil? (:ns &env)) (malli-present?))
             `(malli.core/=> ~fn-sym ~schema))
     :default nil))
