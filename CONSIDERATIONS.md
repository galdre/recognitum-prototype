# Frictions to adopting validated-record in Metabase

> **This is an LLM slop-report, written for my benefit. It is not a worklog and not a decision
> log.** Nothing in it is authoritative and nothing in it records what was actually decided or
> why. Where it appears to explain a design choice, it is reconstructing one after the fact —
> §12's claim that deferring field predicates to `seal` fixed a quadratic is invented, and the
> deferral it explains is spec. Read every claim as a lead to check, not a fact.
> Kept as scratch for polishing later, not as a deliverable. Not maintained: claims in here go
> stale silently and are not covered by the test suite.
> — galdre

A list of problems, not a plan. No recommendations, no sequencing, no judgement about whether any
of these is worth solving. Counts come from `grep` over `src/` and `enterprise/backend/src/` on
`transform-tests` at the time of writing; treat them as orders of magnitude.

## 1. The size of the existing malli investment

| | files | occurrences |
|---|---:|---:|
| `metabase.util.malli` required | 967 | 1590 |
| `mu/defn` | 704 | 5053 |
| `mr/def` (registry schemas) | 311 | 2130 |
| `:closed true` | 536 | 2664 |
| `defendpoint` | 160 | 1399 |
| `mu/defmethod` | 54 | 143 |
| `clojure.spec.alpha` | 11 | 12 |
| `schema.core` (plumatic) | 6 | 7 |

Anything introduced here coexists with malli rather than replacing it. Problems that follow:

- Two vocabularies for "what is a valid X", and a rule for which one a given piece of data uses.
- A schema and a validated type can drift: `::lib.schema/query` says one thing, a `defvalidated`
  says another, nothing checks they agree.
- `mu/defn` validates function *arguments*; this library validates *values*. A function taking a
  validated type still needs a malli schema to describe its other arguments, so both appear in the
  same signature.

## 2. Malli schemas are data; field predicates are opaque functions

`^{:valid pred}` takes a function. It cannot be inspected, composed, transformed, or rendered.
Consequences:

- **No registry.** 2130 `mr/def` sites exist precisely so schemas can be referenced by keyword
  across namespaces. A predicate has no name and no place to live.
- **No composition.** `[:merge ::a ::b]`, `[:maybe …]`, `[:multi …]`, `[:or …]` have no analogue.
  A validated type cannot be derived from another validated type.
- **No generation.** `malli.generator` drives property tests; an opaque predicate generates
  nothing.
- **No JSON Schema / OpenAPI.** `defendpoint` derives API documentation from schemas. A validated
  type appearing in a request or response body produces no documentation.
- **No `:closed true` equivalent at the schema level** — closedness here is a property of the
  runtime type, not something a schema can assert about a plain map.

## 3. Error-message contract

- `mu/with-api-error-message` (53 uses) and `me/humanize` (37 uses) shape what API clients see
  when validation fails.
- This library throws its own `ex-info` shapes (`:invalid-field`, `:invariant-violated`, …). They
  are not malli explain maps and will not humanize.
- Anything catching malli validation errors at an API boundary will not catch these, and vice
  versa. The `defendpoint` error path in particular assumes malli.

## 4. ClojureScript

`src/` holds 201 `.cljc` and 16 `.cljs` files against 1551 `.clj`. `metabase.lib` — the largest
body of schema-validated data in the codebase — is `.cljc` and runs in the browser.

- The prototype is JVM-only: `potemkin/def-map-type`, `deftype` with JVM interfaces
  (`clojure.lang.IKeywordLookup`, `ILookupThunk`, `IPersistentMap`).
- ClojureScript has different protocols (`ILookup`, `IAssociative`, `IMap`, `-assoc`) and no
  `IKeywordLookup` thunk mechanism. A `.cljc` version is a second implementation, not a port.
- Any type used in `lib` must exist on both platforms or `lib` splits.

## 5. Toucan 2 models

`toucan2.instance.Instance` is **already** a `potemkin.collections.PotemkinMap`. It implements
`IRecordChanges` (original vs. current), `IEditableCollection` (transients), `IWithModel`,
`IDispatchValue`, and `Realize`.

- A validated type is not an `Instance`, so it cannot flow through `t2/select`, `t2/insert!`,
  `t2/update!`, `t2/changes`, or model-aware dispatch.
- `t2/deftransforms` (96 uses) already converts column values on read and write — a second
  transformation layer on the same data needs an ordering story.
- Two `def-map-type` types in the same value graph (an `Instance` holding a validated type, or the
  reverse) is untested territory.
- `IRecordChanges` is the same idea as this library's dirty tracking, implemented differently.

## 6. Hydration adds keys on purpose

`t2/hydrate` appears 272 times across 118 files; `define-batched-hydration-method` 39 times.
Hydration's entire job is to `assoc` keys onto a model map that are not columns.

- A closed type rejects exactly this, and `clean` would strip precisely the data the caller asked
  for.
- Hydrated keys are frequently namespaced or nested, and their presence is conditional on the call
  site — so they cannot simply be declared as optional fields.
- The same problem applies to any code that decorates a model map for a response.

## 7. MBQL and `lib` are nested plain maps that get walked and rebuilt

538 occurrences of `:lib/type`; `lib.walk` used in 41 files (244 occurrences), `walk-stages` 47
times.

- MBQL is a deeply nested structure walked, transformed, and rebuilt constantly. Every node being
  a validated type means validating at every level on every rebuild.
- `lib.walk` reconstructs maps; whether a validated type survives that depends on the walker
  producing the type back rather than a plain map.
- Middleware freely does `assoc-in` / `update-in` into stage internals. Nested validated types
  would validate the inner value, then the outer, at each level.
- The QP's hot path is the worst possible place for per-write validation.
- Separately: `~/CLAUDE.md` forbids reaching past `lib` into raw MBQL maps. A validated MBQL node
  would have to be constructed and updated through `lib`, not by direct `assoc`.

## 8. Serialization and the reader

- **No reader literal.** `defrecord` gives you `#my.ns.Foo{…}`; this library deliberately does
  not. That closes a bypass but breaks `pr-str` / `read-string` round-tripping.
- **JSON.** 205 `json/encode` sites, 14 `add-encoder` registrations. A custom map type needs an
  encoder or it must serialize via its map interface.
- **serdes.** 40 `serdes/make-spec` definitions export and import content as YAML. Deserialization
  must go through the validating constructor or it reintroduces unvalidated values.
- **Caching / Nippy.** Anything freezing values to bytes needs a handler, and thawing must
  re-validate or the guarantee is gone at the point it matters most.
- **Settings.** 635 `defsetting` sites, many with `:type :json` — values round-trip through
  serialization on every read.

## 9. Normalization and decoding

`decode/normalize` appears 357 times across 60 files; `malli.transform` in 24 files.

- Metabase normalizes data *as part of* validating it — keywordizing, kebab-casing, coercing.
  `defvalidated` has no decode step: a predicate either accepts a value or rejects it.
- The places that most want a validated type (API bodies, stored JSON columns) are exactly the
  places currently relying on decode-then-validate.

## 10. Concurrency and the dynamic binding

`drafting` publishes its ownership token through a dynamic var.

- 321 `future` occurrences, 49 `core.async` references, 25 `bound-fn` uses. Binding conveyance
  covers `future` and agents; `core.async` `go` blocks and thread pools generally do not convey.
- A draft created on one thread and sealed on another sees no owner, and a nested `drafting` on a
  different thread will seal early.
- Lazy sequences realized outside the `drafting` extent have the same problem.
- The QP is streaming and reducible-based; where the extent ends is not obvious.

## 11. Build, lint, and module integration

- A new module needs an entry in `.clj-kondo/config/modules/config.edn` with `:api`, `:uses`, a
  `:team`, and to survive `metabase.core.modules-test`.
- `defvalidated` generates two types, several private vars, and `defmethod print-method` per
  model. clj-kondo will need to understand the macro or every generated name reads as unresolved.
- Generated `def`s inside a macro may trip existing linters and the ratchets.
- Cross-module use of a validated type means the type's namespace is in someone's `:uses`.

## 12. Performance in the places it would matter

Measured against hand-rolled validation with identical predicates: reads ~1.3×, a bare write 2.0×
(16 ns absolute), writes with invariants ~1.2×, `merge` and construct faster than hand-rolled.

- Those ratios are for a four-field type with two invariants. A `:model/Card` has dozens of
  columns; an MBQL stage is nested. Neither is characterized.
- The residual write overhead is dispatch through `potemkin`'s `AbstractMap` indirection, which a
  real `emit-defrecord` fork would avoid — that fork has not been done.
- Per-field predicates on collection-valued fields are O(collection). A single write to a large
  collection field pays a full scan at seal.
- No measurement exists for many fields, deep nesting, or large collections.

## 13. Migration and coexistence

- No incremental path is defined. A type is either validated or it is a map; there is no "validate
  these three fields for now" mode.
- Every existing caller of a model map does `assoc`, `update`, `select-keys`, `merge`, `dissoc`
  freely. Converting one model means auditing all of them — `dissoc` in particular now throws.
- Two representations of the same entity during a migration means conversion at every boundary
  between converted and unconverted code.
- Test fixtures build model maps directly (`mt/with-temp`, `t2/insert!`) and would need to
  construct through the validating path.

## 14. Gaps in the prototype itself

Things simply not built, listed so they are not mistaken for design decisions:

- JVM only (see §4).
- No transients — `IEditableCollection` is unimplemented, though `toucan2.instance.Instance`
  implements it and drafts are the natural place for it.
- No reader literal, no transit/Nippy/JSON handlers (see §8).
- No schema registry, composition, or generation (see §2).
- `clean` policy is chosen per call site; there is no type-level default.
- Invariant selection for multi-key writes is a linear scan over invariants; only single-key writes
  use the index.
- `seal` re-runs field predicates for every dirty key, including a key written and then written
  back to its original value.
- Drafts are persistent values, so a draft stashed mid-body and used after sealing is not
  invalidated — the same hole Clojure transients have, without transients' spent-handle check.
- Only one model (`Heist`, four fields, two invariants) has ever been defined with it.
- No test suite in the `clojure.test` sense — correctness is a demo script and a benchmark file.
