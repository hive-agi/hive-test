# hive-test

Shared property-based testing library for the [hive](https://github.com/hive-agi) ecosystem.

Provides reusable **generators**, **property macros**, and a **Kaocha MCP adapter** so hive projects can write property tests without duplicating generator definitions.

## Installation

Add to your `deps.edn` `:test` alias:

```clojure
io.github.hive-agi/hive-test
{:git/tag "v0.1.1" :git/sha "99a66ad"}
```

## Generators

### `hive-test.generators.core`

Common scalar generators used across all domains.

| Generator | Output |
|---|---|
| `gen-non-blank-string` | Non-blank alphanumeric strings |
| `gen-keyword-ns` | Namespaced keywords (`:foo/bar`) |
| `gen-uuid-str` | UUID strings |
| `gen-timestamp` | ISO-8601 timestamp strings |
| `gen-project-id` | `"project-<alphanumeric>"` |
| `gen-agent-id` | `"agent:<alphanumeric>"` |

### `hive-test.generators.result`

Generators for the Result DSL (`ok`/`err` monadic type). **Parameterized by constructor fns** to avoid coupling to any specific Result implementation.

```clojure
(require '[hive-test.generators.result :as gen-r]
         '[my-project.result :as r])

;; Pass your constructors at the call site
(gen-r/gen-ok r/ok)                    ; -> Generator<{:ok <any>}>
(gen-r/gen-err r/err)                  ; -> Generator<{:error <category>}>
(gen-r/gen-result r/ok r/err)          ; -> Generator<Result>
(gen-r/gen-result-fn r/ok r/err)       ; -> Generator<(a -> Result)>
(gen-r/gen-plain-fn)                   ; -> Generator<(a -> b)>
(gen-r/gen-err-with-extras)            ; -> Generator<{:error <cat> ...extras}>

;; Also available as plain defs
gen-r/gen-err-category                 ; -> Generator<keyword>
gen-r/gen-ok-value                     ; -> gen/any-printable
```

### `hive-test.generators.kg`

Generators for Knowledge Graph edges, nodes, and relations.

```clojure
(require '[hive-test.generators.kg :as gen-kg])

gen-kg/gen-node-id              ; -> "node-<uuid>"
gen-kg/gen-relation             ; -> :implements, :refines, etc.
gen-kg/gen-confidence           ; -> double in [0.0, 1.0]
gen-kg/gen-invalid-confidence   ; -> double outside [0.0, 1.0]
gen-kg/gen-source-type          ; -> :manual, :automated, :inferred, :co-access
gen-kg/gen-delta                ; -> double in [-2.0, 2.0]
gen-kg/gen-edge-params          ; -> {:from, :to, :relation, :confidence}
gen-kg/gen-edge-params-full     ; -> above + :scope, :source-type, :created-by
```

### `hive-test.generators.memory`

Generators for hive memory entries.

```clojure
(require '[hive-test.generators.memory :as gen-mem])

gen-mem/gen-memory-type         ; -> :axiom, :decision, :note, etc.
gen-mem/gen-duration            ; -> :ephemeral, :short, :medium, :long, :permanent
gen-mem/gen-tags                ; -> ["tag-foo" "tag-bar"]
gen-mem/gen-memory-entry        ; -> {:type, :content, :tags, :duration}
gen-mem/gen-memory-entry-full   ; -> above + :scope, :agent-id
```

## Property Macros

`hive-test.properties` provides macros that expand into `defspec` forms, reducing boilerplate for common algebraic properties.

```clojure
(require '[hive-test.properties :as props])
```

### `defprops-monad`

Generates three defspecs verifying monad laws (left-identity, right-identity, associativity):

```clojure
(props/defprops-monad "result-monad"
  gen/any-printable                     ; gen-val
  (gen-r/gen-result-fn r/ok r/err)      ; gen-fn
  r/bind                                ; bind-fn
  r/ok)                                 ; unit-fn
;; Expands to: result-monad-left-identity, result-monad-right-identity, result-monad-associativity
```

### `defprop-total`

Verifies a function never throws for any valid input:

```clojure
(props/defprop-total p1-ok-totality r/ok gen/any-printable {:pred r/ok?})
```

### `defprop-complement`

Verifies two predicates are exact complements (`(not= (p x) (q x))` for all x):

```clojure
(props/defprop-complement p6 r/ok? r/err? (gen-r/gen-result r/ok r/err))
```

### `defprop-roundtrip`

Verifies `(decode (encode x)) = x`:

```clojure
(props/defprop-roundtrip json-roundtrip json/encode json/decode gen-value)
```

### `defprop-idempotent`

Verifies `f(f(x)) = f(x)`:

```clojure
(props/defprop-idempotent normalize-idempotent normalize gen-input)
```

### `defprop-fsm-terminates`

Verifies an FSM always reaches a terminal state within a turn budget:

```clojure
(props/defprop-fsm-terminates drone-fsm run-drone gen-drone-inputs 50)
```

## Kaocha MCP Adapter

A stdio JSON-RPC server that exposes Kaocha test execution as MCP tools. Run via Babashka:

```bash
bb -m hive-test.mcp.server
```

### Tools

| Tool | Description |
|---|---|
| `test/run` | Run tests via nREPL (namespace or test ID filtering) |
| `test/status` | Get pass/fail counts and test health |
| `test/failures` | Get detailed failure information |
| `test/watch-start` | Start file watcher for auto-test on `.clj` changes |
| `test/watch-stop` | Stop the file watcher |

## Development

```bash
# Start dev REPL
clj -A:dev

# Run tests
clj -A:test
```

## License

MIT - see [LICENSE](LICENSE).
