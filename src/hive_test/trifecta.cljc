(ns hive-test.trifecta
  "Extensible three-facet test generation: golden + property + mutation.

   Architecture (DDD / SOLID):

     ┌─────────────┐
     │ deftest-facets │ ← power-user macro, takes explicit facet specs
     └──────┬──────┘
            │ dispatches via
     ┌──────▼──────┐
     │  emit-facet  │ ← open multimethod, dispatch on :type
     └──────┬──────┘
            │ delegates to
     ┌──────▼──────────┐  ┌────────────────┐  ┌──────────────────┐
     │  emit-property   │  │  emit-golden    │  │  emit-mutation   │
     │  (multimethod)   │  │  (multimethod)  │  │  (multimethod)   │
     └─────────────────┘  └────────────────┘  └──────────────────┘
       :totality            :cases              :golden-derived
       :pred                :cases-fn           :assert
       :idempotent          :expr               (downstream...)
       (downstream...)      (downstream...)

   Extension points (Open/Closed):
     1. (defmethod emit-facet :my-facet [...] ...)     — new facet type
     2. (defmethod emit-property :monotonic [...] ...) — new property type
     3. (defmethod emit-golden :stateful [...] ...)    — new golden source
     4. (defmethod emit-mutation :oracle [...] ...)    — new mutation strategy

   Convenience layer:
     deftrifecta  — flat spec map → auto-decomposes into facets
     deftest-facets — explicit facet vector → full control

   What trifecta does NOT cover (use individual macros):
     - Roundtrip (two functions): hive-test.properties/defprop-roundtrip
     - Complement (two predicates): hive-test.properties/defprop-complement
     - Invariant (state + ops): hive-test.properties/defprop-invariant
     - Multi-property (5+ props per fn): individual defspec"
  ;; trifecta is pure codegen: it never CALLS clojure.test/test.check at
  ;; runtime, it only EMITS forms. The only platform-specific symbols are
  ;; deftest / is (clojure.test vs cljs.test), which are chosen per-platform
  ;; via (:ns &env) dispatch threaded through ctx (see deftest-facets /
  ;; deftrifecta). defspec / for-all / golden.* / mutation.* live in
  ;; cross-platform .cljc namespaces, so the syntax-quote-emitted
  ;; fully-qualified symbols resolve identically on clj and cljs — no &env
  ;; dispatch needed for them. The aliases below are required only so those
  ;; syntax-quotes resolve at compile time on both platforms.
  (:require [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.properties :as prop]
            [hive-test.golden :as golden]
            [hive-test.mutation :as mut]
            [hive-test.stateful :as sf])
  ;; Self-require macros so cljs consumers use deftest-facets / deftrifecta
  ;; via plain :require/:refer.
  #?(:cljs (:require-macros [hive-test.trifecta])))

;; =============================================================================
;; Domain: Facet Registry (Strategy Pattern, OCP)
;; =============================================================================

(defmulti emit-facet
  "Generate a deftest/defspec form for a single test facet.
   Dispatches on (:type facet-spec).

   Arguments:
     facet-spec — map with :type and facet-specific keys
     ctx        — {:name sym, :var-sym sym} shared context

   Returns: a Clojure form (deftest/defspec), or nil to skip.

   Extension point: downstream defines new facet types via defmethod."
  (fn [facet-spec _ctx] (:type facet-spec)))

(defmethod emit-facet :default [spec _ctx]
  (throw (ex-info (str "Unknown facet type: " (:type spec)
                       ". Register via (defmethod emit-facet :" (:type spec) " ...)")
                  {:type (:type spec) :spec spec})))

;; =============================================================================
;; Domain: Property Type Registry (Strategy Pattern, OCP)
;; =============================================================================

(defmulti emit-property
  "Generate a defspec form for a specific property type.
   Dispatches on property-type keyword.

   Arguments:
     property-type — :totality, :pred, :idempotent, or downstream keyword
     spec          — facet spec map (contains :gen, :pred, :num-tests, etc.)
     ctx           — {:name sym, :var-sym sym}

   Returns: a defspec form.

   Extension point: downstream defines new property types via defmethod."
  (fn [property-type _spec _ctx] property-type))

(defmethod emit-property :stateful
  ;; Model-based facet: :machine is a form evaluating to a hive-test.stateful
  ;; Machine; :check-opts are passed to `check`. Emits a deftest (not a defspec)
  ;; because the exploration is exhaustive within bounds, not sampled.
  [_ {:keys [machine check-opts]} {:keys [name cljs?]}]
  (let [p-name      (symbol (str name "-stateful"))
        deftest-sym (if cljs? 'cljs.test/deftest 'clojure.test/deftest)
        is-sym      (if cljs? 'cljs.test/is 'clojure.test/is)]
    `(~deftest-sym ~p-name
      (let [r# (sf/check ~machine ~(or check-opts {}))]
        (~is-sym (:ok? r#) (sf/report-str r#))))))

;; =============================================================================
;; Domain: Golden Source Registry (Strategy Pattern, OCP)
;; =============================================================================

(defmulti emit-golden
  "Generate a deftest form for a golden snapshot source.
   Dispatches on source-type keyword.

   Arguments:
     source-type — :cases, :cases-fn, :expr, or downstream keyword
     spec        — facet spec map (contains :path, :cases, :xf, etc.)
     ctx         — {:name sym, :var-sym sym}

   Returns: a deftest form.

   Extension point: downstream defines new golden sources via defmethod."
  (fn [source-type _spec _ctx] source-type))

;; =============================================================================
;; Domain: Mutation Strategy Registry (Strategy Pattern, OCP)
;; =============================================================================

(defmulti emit-mutation
  "Generate a deftest form for a mutation testing strategy.
   Dispatches on strategy-type keyword.

   Arguments:
     strategy-type — :golden-derived, :assert, or downstream keyword
     spec          — facet spec map (contains :mutations, :golden-path, etc.)
     ctx           — {:name sym, :var-sym sym}

   Returns: a deftest form.

   Extension point: downstream defines new mutation strategies via defmethod."
  (fn [strategy-type _spec _ctx] strategy-type))

;; =============================================================================
;; Built-in: Golden Sources
;; =============================================================================

(defmethod emit-golden :cases
  [_ {:keys [path cases xf]} {:keys [name var-sym cljs? test-ns]}]
  (let [g-name (symbol (str name "-golden"))
        deftest-sym (if cljs? 'cljs.test/deftest 'clojure.test/deftest)
        xf-sym (gensym "xf")
        k-sym  (gensym "k")
        v-sym  (gensym "v")]
    `(~deftest-sym ~g-name
       (let [~xf-sym ~(or xf `identity)]
         (golden/assert-golden (golden/anchor '~test-ns ~path)
           (into (sorted-map)
             (map (fn [[~k-sym ~v-sym]] [~k-sym (~xf-sym (~var-sym ~v-sym))]))
             ~cases))))))

(defmethod emit-golden :cases-fn
  [_ {:keys [path cases xf]} {:keys [name var-sym cljs? test-ns]}]
  (let [g-name  (symbol (str name "-golden"))
        deftest-sym (if cljs? 'cljs.test/deftest 'clojure.test/deftest)
        xf-sym  (gensym "xf")
        k-sym   (gensym "k")
        args-sym (gensym "args")]
    `(~deftest-sym ~g-name
       (let [~xf-sym ~(or xf `identity)]
         (golden/assert-golden (golden/anchor '~test-ns ~path)
           (into (sorted-map)
             (map (fn [[~k-sym ~args-sym]] [~k-sym (~xf-sym (apply ~var-sym ~args-sym))]))
             ~cases))))))

(defmethod emit-golden :expr
  [_ {:keys [path expr]} {:keys [name cljs? test-ns]}]
  (let [g-name (symbol (str name "-golden"))
        deftest-sym (if cljs? 'cljs.test/deftest 'clojure.test/deftest)]
    `(~deftest-sym ~g-name
       (golden/assert-golden (golden/anchor '~test-ns ~path) ~expr))))

;; =============================================================================
;; Built-in: Property Types
;; =============================================================================

(defmethod emit-property :totality
  [_ {:keys [gen num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [v# ~gen]
         (do (~var-sym v#) true)))))

(defmethod emit-property :totality-fn
  [_ {:keys [gen num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [args# ~gen]
         (do (apply ~var-sym args#) true)))))

(defmethod emit-property :pred
  [_ {:keys [gen pred num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [v# ~gen]
         (~pred (~var-sym v#))))))

(defmethod emit-property :pred-fn
  [_ {:keys [gen pred num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [args# ~gen]
         (~pred (apply ~var-sym args#))))))

(defmethod emit-property :idempotent
  [_ {:keys [gen num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [v# ~gen]
         (= (~var-sym (~var-sym v#))
            (~var-sym v#))))))

(defmethod emit-property :roundtrip
  ;; Verify (decode (encode x)) = x. Requires :decode-fn in spec.
  [_ {:keys [gen decode-fn num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [v# ~gen]
         (= v# (~decode-fn (~var-sym v#)))))))

(defmethod emit-property :structural
  ;; Verify result always contains :required-keys. Requires :required-keys in spec.
  [_ {:keys [gen required-keys num-tests]} {:keys [name var-sym]}]
  (let [p-name (symbol (str name "-property"))
        nt (or num-tests 200)]
    `(tc/defspec ~p-name ~nt
       (prop/for-all [v# ~gen]
         (let [result# (~var-sym v#)]
           (every? #(contains? result# %) ~required-keys))))))

;; =============================================================================
;; Built-in: Mutation Strategies
;; =============================================================================

(defmethod emit-mutation :golden-derived
  [_ {:keys [mutations golden-path cases xf apply?]} {:keys [name var-sym cljs? test-ns]}]
  (let [m-name    (symbol (str name "-mutations"))
        is-sym    (if cljs? 'cljs.test/is 'clojure.test/is)
        xf-sym    (gensym "xf")
        gp-sym    (gensym "golden-path")
        exp-sym   (gensym "expected")
        seed-sym  (gensym "seed")
        k-sym     (gensym "k")
        input-sym (gensym "input")
        call      (if apply?
                    `(~xf-sym (apply ~var-sym ~input-sym))
                    `(~xf-sym (~var-sym ~input-sym)))]
    `(mut/deftest-mutations ~m-name
       ~var-sym
       ~mutations
       (fn []
         (let [~xf-sym ~(or xf `identity)
               ~gp-sym (golden/anchor '~test-ns ~golden-path)
               ;; first-run auto-seed: phase-1 runs the UNMUTATED var (mutation.cljc)
               ~exp-sym (or (golden/read-golden ~gp-sym)
                            (let [~seed-sym (into (sorted-map)
                                             (map (fn [[~k-sym ~input-sym]] [~k-sym ~call]))
                                             ~cases)]
                              (golden/update-golden! ~gp-sym ~seed-sym)
                              ~seed-sym))]
           (doseq [[~k-sym ~input-sym] ~cases]
             (~is-sym (= (get ~exp-sym ~k-sym) ~call)
                      (str "case " ~k-sym " mismatch"))))))))

(defmethod emit-mutation :assert
  [_ {:keys [mutations assert]} {:keys [name var-sym]}]
  (let [m-name (symbol (str name "-mutations"))]
    `(mut/deftest-mutations ~m-name
       ~var-sym
       ~mutations
       ~assert)))

;; =============================================================================
;; Built-in: Facet Type Dispatchers (SRP — each resolves sub-strategy)
;; =============================================================================

(defmethod emit-facet :golden
  [{:keys [cases expr] :as spec} ctx]
  (let [source-type (cond
                      expr   :expr
                      (:apply? spec) :cases-fn
                      cases  :cases
                      :else  nil)]
    (when source-type
      (emit-golden source-type spec ctx))))

(defmethod emit-facet :property
  [{:keys [pred idempotent? apply? property-type] :as spec} ctx]
  (let [pt (or property-type
               (cond
                 idempotent?       :idempotent
                 (and pred apply?) :pred-fn
                 pred              :pred
                 apply?            :totality-fn
                 :else             :totality))]
    (emit-property pt spec ctx)))

(defmethod emit-facet :mutation
  [{:keys [assert] :as spec} ctx]
  (let [strategy (if assert :assert :golden-derived)]
    (emit-mutation strategy spec ctx)))

;; =============================================================================
;; Subject normalization — accept a bare symbol OR a var literal
;; =============================================================================

(defn- ->subject-sym
  "Normalize a trifecta subject to the bare qualified symbol used for codegen.
   Accepts a bare symbol `ns/fn` or a var literal `#'ns/fn`."
  [subject]
  (if (and (seq? subject) (= 'var (first subject)))
    (second subject)
    subject))

;; =============================================================================
;; Public API: deftest-facets (power-user — explicit facet specs)
;; =============================================================================

(defmacro deftest-facets
  "Generate tests from explicit facet specifications.

   Each facet is a map with :type and type-specific keys.
   Dispatches to emit-facet multimethod — extensible by downstream.

   Example:
     (deftest-facets my-fn-tests my.ns/my-fn
       {:type :golden   :path \"test/golden/my-fn.edn\" :cases {:a 1 :b 2}}
       {:type :property :gen gen/int :pred pos?}
       {:type :mutation :mutations [[\"zero\" (fn [_] 0)]]
                        :golden-path \"test/golden/my-fn.edn\"
                        :cases {:a 1 :b 2}})"
  [name var-sym & facets]
  (let [ctx {:name name :var-sym (->subject-sym var-sym) :cljs? (boolean (:ns &env))
             :test-ns (ns-name *ns*)}]
    `(do ~@(keep #(emit-facet % ctx) facets))))

;; =============================================================================
;; Public API: deftrifecta (convenience — flat spec → three facets)
;; =============================================================================

(defn- spec->facets
  "Decompose a flat trifecta spec into individual facet specs.
   Internal — maps the convenience API to the facet registry."
  [{:keys [golden-path golden-expr cases xf apply?
           gen pred idempotent? property-type num-tests
           mutations assert] :as spec}]
  (filterv
    some?
    [(when (and golden-path (or cases golden-expr))
       (cond-> {:type :golden :path golden-path}
         cases      (assoc :cases cases)
         golden-expr (assoc :expr golden-expr)
         xf         (assoc :xf xf)
         apply?     (assoc :apply? apply?)))

     (when gen
       (cond-> {:type :property :gen gen :num-tests (or num-tests 200)}
         pred           (assoc :pred pred)
         idempotent?    (assoc :idempotent? true)
         property-type  (assoc :property-type property-type)
         apply?         (assoc :apply? apply?)))

     (when mutations
       (cond-> {:type :mutation :mutations mutations}
         assert      (assoc :assert assert)
         golden-path (assoc :golden-path golden-path)
         cases       (assoc :cases cases)
         xf          (assoc :xf xf)
         apply?      (assoc :apply? apply?)))]))

(defmacro deftrifecta
  "Generate golden + property + mutation tests from a flat spec.

   Convenience wrapper around deftest-facets. Decomposes the flat map
   into facet specs and dispatches through the extensible registry.

   Subject (var-sym): a bare qualified symbol `ns/fn` or a var literal `#'ns/fn`.
   Any arity: set `:apply? true` with arg-vector `:cases`/`:gen`.

   Spec keys:
     ;; Golden facet
     :golden-path   — EDN file path for snapshot (relative → project root)
     :cases         — {label input} map; f applied to each input
     :golden-expr   — (alternative) arbitrary expression to snapshot
     :xf            — transform on output before snapshot (default: identity)
     :apply?        — if true, cases values are [& args] vectors

     ;; Property facet
     :gen           — test.check generator
     :pred          — predicate (default: totality)
     :idempotent?   — if true, tests f(f(x)) = f(x)
     :property-type — explicit dispatch key for emit-property
     :num-tests     — iterations (default: 200)

     ;; Mutation facet
     :mutations     — [[label mutant-fn] ...] broken implementations
     :assert        — explicit assertion fn (overrides golden-derived)

   Omit a facet's required keys to skip it.

   Example:
     (deftrifecta resolve-consistency
       my.ns/resolve-consistency
       {:golden-path \"test/golden/resolve.edn\"
        :cases       {:strong :strong, :bounded :bounded}
        :xf          str
        :gen         gen-keyword
        :pred        #(or (nil? %) (instance? Enum %))
        :mutations   [[\"always-nil\" (fn [_] nil)]]})"
  [name var-sym spec]
  (let [ctx    {:name name :var-sym (->subject-sym var-sym) :cljs? (boolean (:ns &env))
                :test-ns (ns-name *ns*)}
        facets (spec->facets spec)]
    `(do ~@(keep #(emit-facet % ctx) facets))))