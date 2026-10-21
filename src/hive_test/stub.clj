(ns hive-test.stub
  "Protocol-derived test doubles. A stub's method set is read from the protocol
   Var at macro expansion time; recompile the stub after changing the protocol.

   Default policy (decision): throw by default; defaults are opt-in. A method
   with no override throws ex-info (:stub :protocol :method), because no return
   shape has a default that suits every caller and a silent nil hides an
   unhandled call. A stub asks for defaults with the reserved override key
   :hive-test.stub/default, a function expression called with a context map
   {:stub :protocol :method :tag} followed by the method's arguments (receiver
   included). :tag is the symbol of the return type hint on the protocol
   arglist, e.g. (size ^long [this]), or nil when the protocol states none.
   [[by-tag]] builds such a function from a tag->value map and still throws for
   any tag the map does not name. A per-method override always wins over the
   default function.

   Records and deftypes: a record or deftype has no method set of its own, only
   the protocols and interfaces it implements, so the protocol path already
   covers it: stub the protocol (a defstub record is itself a record). Naming
   a class as the target is rejected at expansion with that advice.

   Decorators: a stub is an ordinary record of its protocol, so recording and
   fault-injecting decorators wrap it by delegation and need nothing from this
   namespace."
  (:require [hive-test.contract :as mc]))

(defn by-tag
  "Build a :hive-test.stub/default function for defstub from `tag->value`, a map
   from the return type hint symbol of a protocol arglist (nil for an unhinted
   one) to the value returned for it. A method whose tag is not a key throws
   ex-info with :stub, :protocol, :method and :tag, like an unconfigured method."
  [tag->value]
  (fn [{:keys [stub protocol method tag]} & _args]
    (if (contains? tag->value tag)
      (get tag->value tag)
      (throw (ex-info (str "Unconfigured stub method " stub " " protocol "/" method
                           " (no default for return tag " (pr-str tag) ")")
                      {:stub stub :protocol protocol :method method :tag tag})))))

;; Contract: registered only when malli is on the classpath (hive-test.contract).
(mc/=> by-tag [:=> [:cat [:map-of [:maybe :symbol] :any]] ifn?])

(defmacro defstub
  "JVM Clojure only: define a fieldless record implementing every arity in the
   protocol Var's :sigs. Overrides are a literal map from method keyword (or
   symbol) to a function expression, called with the receiver and arguments.

   Default policy: no return-shape metadata is inferred. Every unoverridden
   method throws ex-info with :stub, :protocol and :method; no implicit nil
   can mask an unhandled call. Override methods requiring a useful result.
   The zero-argument record constructor is ->Name. After protocol redefinition,
   re-expand the stub: old records do not implement the new protocol object.

   Opt-in defaults: the reserved override key :hive-test.stub/default takes a
   function expression called, for every method without its own override, with
   a context map {:stub :protocol :method :tag} then the method's arguments.
   :tag is the return type hint on the protocol arglist or nil. See [[by-tag]].

   The target must be a protocol Var. A record or deftype class is refused at
   expansion: it has no method set of its own, so stub the protocol it
   implements instead."
  [stub protocol & [overrides]]
  (let [v (resolve protocol)
        definition (when (var? v) @v)
        sigs (:sigs definition)]
    (when (class? v)
      (throw (ex-info (str "defstub target " protocol " is a class (record or deftype), not a protocol: "
                           "a class has no method set of its own; stub the protocol it implements")
                      {:stub stub :protocol protocol :class v})))
    (when-not (and (var? v) (map? sigs))
      (throw (ex-info (str "defstub requires a protocol Var: " protocol)
                      {:stub stub :protocol protocol})))
    (when-not (and (or (nil? overrides) (map? overrides))
                   (every? #(or (keyword? %) (symbol? %)) (keys overrides)))
      (throw (ex-info "defstub overrides must be a literal map keyed by method name"
                      {:stub stub :protocol protocol :overrides overrides})))
    (let [default-key :hive-test.stub/default
          default-fn (get overrides default-key)
          overrides (dissoc overrides default-key)
          methods (set (map (comp symbol name) (keys sigs)))
          valid-methods (vec (sort methods))
          override-map (into {} (map (fn [[k f]] [(symbol (name k)) f]) overrides))
          unknown (seq (remove methods (keys override-map)))
          protocol-name (symbol (str (ns-name (:ns (meta v)))) (str (:name (meta v))))]
      (when unknown
        (throw (ex-info (str "Unknown defstub override for " protocol ": " (first unknown)
                             "; valid methods: " valid-methods)
                        {:stub stub :protocol protocol :method (first unknown)
                         :unknown unknown :valid-methods valid-methods})))
      `(defrecord ~stub []
         ~protocol
         ~@(for [[method {:keys [arglists]}] sigs
                 args arglists
                 :let [method-sym (symbol (name method))
                       tag (:tag (meta args))
                       bare-args (with-meta args nil)]]
             `(~method-sym ~bare-args
                ~(cond
                   (contains? override-map method-sym)
                   `(~(get override-map method-sym) ~@bare-args)

                   (some? default-fn)
                   `(~default-fn {:stub '~stub :protocol '~protocol-name
                                  :method '~method-sym :tag '~tag}
                                 ~@bare-args)

                   :else
                   `(throw (ex-info
                             ~(str "Unconfigured stub method " stub " " protocol-name "/" method-sym)
                             {:stub '~stub :protocol '~protocol-name :method '~method-sym})))))))))
