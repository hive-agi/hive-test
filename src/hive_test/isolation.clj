(ns hive-test.isolation
  "Composable test-isolation fixtures, multimethod-driven.

   Mirrors the trifecta/emit-facet extension pattern: downstream projects
   register their own isolation strategies via `defmethod emit-isolation`.

   Solves the recurring problem of test fixtures that mutate global
   singletons (DataScript conns, agent registries, event bus) — when run
   inside a live nREPL these wipe production state. Each isolation
   strategy returns a per-thread `binding`/`with-redefs`-style fixture
   so prod state stays untouched even when tests share the JVM.

   Usage:

     (use-fixtures :each (iso/with-isolations :swarm-ds :agent-registry))

   Power-user form (specs with params):

     (use-fixtures :each (iso/with-isolations
                           :swarm-ds
                           {:type :events :reset-handlers? true}))

   Extending — downstream `(defmethod emit-isolation :my-state [_] ...)`
   returns a `(fn [f] ...)` fixture."
  (:require [clojure.test :as t]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Multimethod registry (Strategy Pattern, OCP)
;; =============================================================================

(defmulti emit-isolation
  "Return a clojure.test :each fixture fn `(fn [f] ...)` that sets up
   and tears down isolation for the given spec.

   Dispatches on:
     - keyword          → spec keyword itself (short form, no params)
     - map with :type   → :type value (long form with params)

   Extension: downstream registers via (defmethod emit-isolation :my-type ...).
   The defmethod body is called once per `with-isolations` call and must
   return a fixture fn. It MAY close over per-binding state allocated at
   defmethod time (e.g. fresh atoms per fixture-build)."
  (fn [spec]
    (cond
      (keyword? spec) spec
      (map? spec)     (:type spec)
      :else           ::unknown)))

(defmethod emit-isolation :default [spec]
  (throw (ex-info (str "Unknown isolation spec: " (pr-str spec)
                       ". Register via (defmethod emit-isolation :"
                       (if (map? spec) (:type spec) spec) " ...).")
                  {:spec spec})))

(defmethod emit-isolation :noop [_]
  (fn [f] (f)))

;; =============================================================================
;; Composition
;; =============================================================================

(defn compose-fixtures
  "Compose a seq of :each fixture fns into one fixture.
   Each fixture wraps the next; the test fn runs in the innermost scope.
   Empty seq returns a passthrough."
  [fxs]
  (reduce t/compose-fixtures (fn [g] (g)) fxs))

;; =============================================================================
;; Public API
;; =============================================================================

(defn with-isolations
  "Return a :each fixture composing the given isolation specs.

   Each arg is either a keyword (short form, dispatches to emit-isolation)
   or a map `{:type :foo, ...params}`. Result is suitable for `use-fixtures`.

   Order matters: outermost first. Earlier specs wrap later ones."
  [& specs]
  (compose-fixtures (mapv emit-isolation specs)))
