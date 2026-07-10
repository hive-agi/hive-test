# hive-test

Shared testing library for the [hive](https://github.com/hive-agi) ecosystem: a
**three-facet test generator** (golden + property + mutation), **golden /
characterization** testing with a swappable store, **mutation** testing,
reusable **generators**, **property macros**, and a **Kaocha MCP adapter**.

Cross-platform: the core (`trifecta`, `golden`, `mutation`, `properties`) is
`.cljc` and runs on both Clojure and ClojureScript.

> **Dependency-minimal by design.** hive-test's runtime deps are just Clojure +
> test.check. It is the foundational lib the rest of the ecosystem tests *with*,
> so it deliberately avoids depending on hive-dsl / hive-system (that would be a
> cycle). Effects at the golden boundary use plain `slurp`/`spit`.

## Installation

Add to your `deps.edn` `:test` alias:

```clojure
io.github.hive-agi/hive-test {:git/tag "v0.3.0" :git/sha "763e4bc"}
```

## Worked example

A complete, runnable example lives under `test/hive_test/examples/`:

- [`pricing.clj`](test/hive_test/examples/pricing.clj) — a small domain built from
  a `DiscountPolicy` **protocol** (port), record **adapters** (`NoDiscount` /
  `PercentOff` / `SpendThreshold`, the strategy pattern), a `LineItem` **value
  object**, and pure functions.
- [`pricing_test.clj`](test/hive_test/examples/pricing_test.clj) — exercises the
  whole toolkit against it: a `deftrifecta` (golden + property + mutation with
  combinators), a standalone `deftest-golden` table, standalone
  `deftest-mutations`, a raw test.check `defspec` invariant, and an LSP contract
  check over every policy adapter. Its committed goldens are in
  `test/golden/examples/`.

## Trifecta — golden + property + mutation in one form

`hive-test.trifecta/deftrifecta` decomposes a flat spec into up to three test
facets that verify a function from three independent angles:

- **golden** — snapshot outputs for known `:cases`, catch drift on change.
- **property** — an invariant that must hold across a `:gen` generator.
- **mutation** — prove the tests actually catch broken implementations.

A fresh trifecta goes **green on the first run** (the mutation facet auto-seeds
the golden from the unmutated baseline, independent of `clojure.test` var order).

```clojure
(ns my.app.parse-test
  (:require [clojure.test :refer [deftest is]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-test.mutation.combinators :as mut]
            [clojure.test.check.generators :as gen]
            [my.app.parse :as parse]))

(deftrifecta parse-config
  #'parse/parse-config                       ; var literal → clj-kondo resolves it
  {;; golden facet — snapshot outputs for labelled inputs
   :golden-path "test/golden/parse-config.edn"
   :cases       {:empty "" :one "a=1" :many "a=1;b=2"}

   ;; property facet — invariant over generated inputs
   :gen         gen/string-ascii
   :pred        map?

   ;; mutation facet — these broken impls must be caught
   :mutations   [(mut/always {})                          ; ignores input
                 (mut/drop-key parse/parse-config :a)]})   ; drops a key
;; => generates parse-config-golden, parse-config-property, parse-config-mutations
```

### Subject: bare symbol or var literal

The subject may be a bare qualified symbol `ns/fn` **or** a var literal
`#'ns/fn`. Prefer `#'ns/fn` — clj-kondo resolves it, so you drop the
`#_{:clj-kondo/ignore [:unresolved-symbol]}` and the subject `:require` counts as
a real use. Mutation (`alter-var-root`) still works either way.

### Any arity via `:apply?`

Set `:apply? true` and make `:cases` / `:gen` produce **argument vectors** —
each case becomes `(apply f args)`, so 2+-arg and fixture-carrying fns work:

```clojure
(deftrifecta find-refs
  #'analyzer/find-references                  ; (find-references analysis target)
  {:apply?      true
   :golden-path "test/golden/find-refs.edn"
   :cases       {:qualified [fixture "my.ns/foo"]
                 :bare      [fixture "foo"]}
   :gen         (gen/tuple gen-analysis gen-symbol-str)
   :pred        set?})
```

### Spec keys

| Key | Facet | Meaning |
|---|---|---|
| `:golden-path` | golden | EDN snapshot path (relative → project root) |
| `:cases` | golden, mutation | `{label input}` (or `{label [args]}` with `:apply?`) |
| `:golden-expr` | golden | snapshot an arbitrary expression instead of `:cases` |
| `:xf` | golden, mutation | transform output before snapshot (default `identity`) |
| `:apply?` | all | cases values are `[& args]` vectors |
| `:gen` | property | test.check generator |
| `:pred` | property | predicate (default: totality — just "doesn't throw") |
| `:idempotent?` | property | test `f(f(x)) = f(x)` |
| `:property-type` | property | explicit dispatch key (`:roundtrip`, `:structural`, …) |
| `:num-tests` | property | iterations (default 200) |
| `:mutations` | mutation | `[Mutation | [label fn] | [label Mutation]] …]` |
| `:assert` | mutation | explicit assertion fn (overrides golden-derived) |

Omit a facet's required keys to skip it. For full control over facet specs use
`hive-test.trifecta/deftest-facets`; the facet/property/golden/mutation registries
are open multimethods (`emit-facet`, `emit-property`, `emit-golden`,
`emit-mutation`) — add a `defmethod` to extend.

## Golden / characterization testing

`hive-test.golden` snapshots a value to EDN and compares against it on later
runs. **Commit the EDN** — it is the persisted regression baseline.

```clojure
(require '[hive-test.golden :refer [deftest-golden deftest-golden-fn assert-golden]])

(deftest-golden config-shape
  "test/golden/config-shape.edn"
  (keys (load-config "defaults")))

;; first run writes the snapshot (test passes); commit it.
;; later runs compare; UPDATE_GOLDEN=true regenerates.
```

Golden paths **anchor to the test namespace's project root** (classpath walk-up
to `deps.edn`/`project.clj`), not the process cwd — so goldens land in the right
repo even when tests run via a shared nREPL whose cwd is a sibling project.

### Swappable store (ports & adapters)

Persistence is a port, `hive-test.golden.store/GoldenStore` (`-read` / `-write!`
/ `-exists?`), with two adapters:

| Adapter | Persists? | Use for |
|---|---|---|
| `FileGoldenStore` (default) | yes — EDN on disk, committed | real golden/trifecta tests |
| `AtomGoldenStore` | no — in-memory | testing golden *machinery*, or no-baseline cases |

Inject via the `*store*` dynamic var — e.g. exercise the golden control flow with
zero filesystem:

```clojure
(require '[hive-test.golden :as golden]
         '[hive-test.golden.store :as store])

(binding [golden/*store* (store/memory-store)]
  (golden/assert-golden "in-mem" {:v 1})     ; writes to the atom
  (golden/assert-golden "in-mem" {:v 1}))     ; matches — nothing touches disk
```

> ⚠️ Don't point a **real** trifecta at `memory-store`: the golden is seeded and
> compared within one run, so it always passes and catches no regressions. The
> in-memory store is for testing the plumbing, not for characterizing code.

The anchoring strategy is likewise a port, `hive-test.golden.root/ProjectRoot`,
injectable via `*project-root*` (default: classpath walk-up).

## Mutation testing

`hive-test.mutation` verifies your tests actually catch bugs by rebinding the
subject to broken implementations and asserting the tests then fail.

```clojure
(require '[hive-test.mutation :refer [deftest-mutations deftest-mutation-witness]]
         '[hive-test.mutation.combinators :as mut])

(deftest-mutations enqueue-mutations-caught
  my.ns/enqueue!
  [(mut/always nil)                                 ; bare Mutation (self-labelled)
   ["drops-blocks" (fn [a p b] nil)]                ; [label fn]
   ["off-by-one"   (mut/off-by-one my.ns/size)]]     ; [label Mutation]
  (fn []                                            ; assertions on the real code
    (my.ns/enqueue! "a" "p" {:x 1})
    (is (= {:x 1} (my.ns/drain! "a" "p")))))
```

Each mutation is a **`Mutation` value object** (`{:label :mutate}`); construct
one with `mutation`, test with `mutation?`. `deftest-mutations` accepts a
`Mutation`, a `[label fn]` pair, or a `[label Mutation]` pair (outer label wins),
normalized by `as-pair`.

### Combinators — `hive-test.mutation.combinators`

State a mutant's *intent* instead of retyping the subject's body. Each returns a
`Mutation`.

| Combinator | Mutant behaviour |
|---|---|
| `(always v)` | ignore args, return `v` |
| `const-nil` / `const-true` / `const-false` | return that constant |
| `(echo-arg n)` | return the nth arg unchanged (no-op for endofns) |
| `(drop-key orig k)` | `dissoc` `k` from `orig`'s map result |
| `(assoc-const orig k v)` | `assoc` `k`→`v` onto `orig`'s map result |
| `(off-by-one orig)` | `inc` `orig`'s numeric result |
| `(negate-pred orig)` | logically negate `orig`'s boolean result |

Output-transforming combinators take the original fn — pass the subject value.
The `:mutations` vector is evaluated before the var is rebound, so it captures
the real implementation (no recursion under mutation).

`deftest-mutation-witness` checks a single mutation; `with-mutation` rebinds a
var to a mutant for the body's duration.

## Generators

### `hive-test.generators.core`

| Generator | Output |
|---|---|
| `gen-non-blank-string` | Non-blank alphanumeric strings |
| `gen-keyword-ns` | Namespaced keywords (`:foo/bar`) |
| `gen-uuid-str` | UUID strings |
| `gen-timestamp` | ISO-8601 timestamp strings |
| `gen-project-id` | `"project-<alphanumeric>"` |
| `gen-agent-id` | `"agent:<alphanumeric>"` |

### `hive-test.generators.code`

| Generator | Output |
|---|---|
| `gen-simple-symbol-str` | `"foo-bar"` |
| `gen-qualified-symbol-str` | `"foo.bar/baz"` |
| `gen-params-map` | small keyword-keyed map |

### `hive-test.generators.result`

Result-DSL generators, **parameterized by constructor fns** to avoid coupling to
any specific Result implementation.

```clojure
(require '[hive-test.generators.result :as gen-r]
         '[my-project.result :as r])

(gen-r/gen-ok r/ok)               ; -> Generator<{:ok <any>}>
(gen-r/gen-err r/err)             ; -> Generator<{:error <category>}>
(gen-r/gen-result r/ok r/err)     ; -> Generator<Result>
(gen-r/gen-result-fn r/ok r/err)  ; -> Generator<(a -> Result)>
(gen-r/gen-plain-fn)              ; -> Generator<(a -> b)>
gen-r/gen-err-category            ; -> Generator<keyword>
gen-r/gen-ok-value                ; -> gen/any-printable
```

### `hive-test.generators.kg` / `hive-test.generators.memory`

Domain generators for Knowledge-Graph edges and hive memory entries — see the
namespace docstrings for the full list (`gen-edge-params`, `gen-memory-entry`, …).

## Property macros

`hive-test.properties` expands into `defspec` forms for common algebraic
properties.

```clojure
(require '[hive-test.properties :as props])
```

| Macro | Verifies |
|---|---|
| `defprops-monad` | left-identity, right-identity, associativity (3 defspecs) |
| `defprop-total` | never throws for any valid input |
| `defprop-complement` | two predicates are exact complements |
| `defprop-roundtrip` | `(decode (encode x)) = x` |
| `defprop-idempotent` | `f(f(x)) = f(x)` |
| `defprop-fsm-terminates` | an FSM reaches a terminal state within a turn budget |

```clojure
(props/defprops-monad "result-monad"
  gen/any-printable                  ; gen-val
  (gen-r/gen-result-fn r/ok r/err)   ; gen-fn
  r/bind r/ok)                       ; bind-fn, unit-fn

(props/defprop-idempotent normalize-idempotent normalize gen-input)
```

## Kaocha MCP adapter

A stdio JSON-RPC server exposing Kaocha test execution as MCP tools:

```bash
bb -m hive-test.mcp.server
```

| Tool | Description |
|---|---|
| `test/run` | Run tests via nREPL (namespace or test-ID filtering) |
| `test/status` | Pass/fail counts and test health |
| `test/failures` | Detailed failure information |
| `test/watch-start` / `test/watch-stop` | Auto-test on `.clj` changes |

## Development

```bash
clj -A:dev      # dev REPL
clj -M:test     # run the suite (kaocha)
```

## License

MIT — see [LICENSE](LICENSE).
