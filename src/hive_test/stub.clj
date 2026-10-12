(ns hive-test.stub
  "Protocol-derived test doubles. A stub's method set is read from the protocol
   Var at macro expansion time; recompile the stub after changing the protocol.")

(defmacro defstub
  "Define a fieldless record implementing every arity in the protocol Var's
   :sigs. Overrides are a literal map from method keyword (or symbol) to a
   function expression, called with the receiver and method arguments.

   Default policy: no return-shape metadata is inferred. Every unoverridden
   method throws ex-info with :stub, :protocol and :method; no implicit nil
   (or fabricated value) can mask an unhandled call. Override every method
   requiring a useful result. The zero-argument record constructor is ->Name."
  [stub protocol & [overrides]]
  (let [v (resolve protocol)
        definition (when (var? v) @v)
        sigs (:sigs definition)]
    (when-not (and (var? v) (map? sigs))
      (throw (ex-info (str "defstub requires a protocol Var: " protocol)
                      {:stub stub :protocol protocol})))
    (when-not (and (or (nil? overrides) (map? overrides))
                   (every? #(or (keyword? %) (symbol? %)) (keys overrides)))
      (throw (ex-info "defstub overrides must be a literal map keyed by method name"
                      {:stub stub :protocol protocol :overrides overrides})))
    (let [methods (set (map (comp symbol name) (keys sigs)))
          override-map (into {} (map (fn [[k f]] [(symbol (name k)) f]) overrides))
          unknown (seq (remove methods (keys override-map)))
          protocol-name (symbol (str (ns-name (:ns (meta v)))) (str (:name (meta v))))]
      (when unknown
        (throw (ex-info (str "Unknown defstub override for " protocol ": " (first unknown))
                        {:stub stub :protocol protocol :method (first unknown)
                         :unknown unknown})))
      `(defrecord ~stub []
         ~protocol
         ~@(for [[method {:keys [arglists]}] sigs
                 args arglists
                 :let [method-sym (symbol (name method))]]
             `(~method-sym ~args
                ~(if-let [f (get override-map method-sym)]
                   `(~f ~@args)
                   `(throw (ex-info
                             ~(str "Unconfigured stub method " stub " " protocol-name "/" method-sym)
                             {:stub '~stub :protocol '~protocol-name :method '~method-sym})))))))))
