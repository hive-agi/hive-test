(ns hive-test.hooks.macros
  "clj-kondo analyze-call hooks for hive-test macros whose call shape has no
   `:lint-as` equivalent."
  (:require [clj-kondo.hooks-api :as api]))

(defn with-sandbox
  "Hook for (with-sandbox [sym spec ?opts] & body) — binds sym over body,
   analyzes spec and opts as expressions."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        [sym & exprs] (:children binding-vec)]
    {:node (api/list-node
            (list* (api/token-node 'let)
                   (api/vector-node
                    [sym (api/list-node (list* (api/token-node 'do) exprs))])
                   body))}))

(defn with-mutation
  "Hook for (with-mutation [var-sym mutant-fn] & body) — var-sym names an
   existing var, so nothing is bound; all forms are analyzed as expressions."
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)]
    {:node (api/list-node
            (list* (api/token-node 'do)
                   (concat (:children binding-vec) body)))}))

(defn defstub
  "Hook for (defstub Name protocol ?overrides) — defines the record Name and
   its ->Name constructor; protocol and overrides are analyzed as expressions."
  [{:keys [node]}]
  (let [[_ stub-name & exprs] (:children node)
        ctor (api/token-node (symbol (str "->" (:value stub-name))))]
    {:node (api/list-node
            (list (api/token-node 'do)
                  (api/list-node (list (api/token-node 'defrecord) stub-name (api/vector-node [])))
                  (api/list-node (list (api/token-node 'declare) ctor))
                  (api/list-node (list* (api/token-node 'do) exprs))))}))
