# recognitum — specification

The intended state of the library. Written as reference for implementation, not as a record of
how it was arrived at.

## What it is

A record-like type that carries proof of its own validation. If you are holding a `Heist`, every
check declared on `Heist` passed. A bare map with the right keys is not a `Heist`, `assoc`-ing
garbage into one does not produce one, and there is no constructor that skips the checks.

## Defining a type

```clojure
(ns recognitum.heist
  (:require [recognitum.core :as rec]))

(rec/defrecord Heist
  [name crew take cuts]

  {::name  non-blank-string?
   ::crew  crew-set?
   ::take  non-neg-int?
   ::cuts  cuts-map?

   ::cuts-sum-to-take
   ("The cuts add up to the take."
    [take cuts] (= take (reduce + 0 (vals cuts))))

   ::only-crew-get-cuts
   ("Only crew get a cut."
    [crew cuts] (every? crew (keys cuts)))}

  {::shape  #{::name ::crew ::take ::cuts}
   ::payout #{::cuts-sum-to-take ::only-crew-get-cuts}}

  some.ns/SomeProtocol
  (fiddlesticks [h x] ...)
  (bananas [h] ...))
```

Five positions: the type name, the fields, the checks, the certificates, and protocol
implementations. Certificates may be omitted — a map in the fourth position is certificates, a
symbol begins the protocol implementations.

`rec/defrecord` shadows `clojure.core/defrecord`. Consumers alias rather than refer, so both
kinds coexist in one namespace.

### Fields

A plain vector of symbols, as `defrecord` takes. Each becomes a field on the generated type and
a key **qualified by the defining namespace** — `:recognitum.heist/name`, not `:name`.

```clojure
(::h/take heist)
(let [{::h/keys [name take]} heist] ...)
(keys heist)   ;=> (:recognitum.heist/name :recognitum.heist/crew ...)
```

### Checks

A map of check name to check. Names are namespace-qualified keywords, unique within the type,
and are what failures report.

A check is written in one of three forms:

```clojure
::name  non-blank-string?                      ; shorthand
::valid-name ([name] (non-blank-string? name)) ; arglist and body
::cuts-sum-to-take                             ; with a docstring, as defn takes one
("The cuts add up to the take."
 [take cuts] (= take (reduce + 0 (vals cuts))))
```

The shorthand takes a bare function and expands to a check over the single field its key names.
`::name non-blank-string?` is `::name ([name] (non-blank-string? name))`.

**The arglist is the dependency declaration.** Its symbols must be field names, and the check
receives exactly those fields' values, positionally, in that order. Nothing else is in scope, so
a check cannot depend on a field it did not name. Exactly one arity: the arity *is* the
dependency set.

The macro derives a field → checks index from the arglists. That index drives which checks re-run
on a write and which are invalidated on a draft.

### The check contract

- Return `true` — exactly `true` — when valid.
- Return anything else to fail. The returned value is failure data and is reported.
- Exceptions are **not** caught. A throw is a bug in the check, not a failure of the data, and is
  never reported as invalid.

A function whose last expression is truthy but not `true` will be read as a failure. Predicates
must return booleans.

### Certificates

A map of certificate name to a set of check names. Certificates are a **draft-level** facility:
they name a subset of checks you can run early, before the value is complete.

They do not change what a sealed value guarantees. `seal` enforces every check on the type,
whether or not it belongs to a certificate.

## Drafts

A draft is an unvalidated working copy. Writes into it check nothing. It exists so a multi-step
update can pass through states no single write could produce.

A draft tracks which checks currently pass:

- Drafting from a sealed value starts with every check passed.
- Drafting from the type starts with none.
- A write clears every check whose arglist mentions the written field.
- Running a check sets its bit if it passes.

**Certificate-holding is derived, not asserted.** A draft holds a certificate when every check in
it currently passes. Overlapping certificates work without coordination, and running individual
checks can complete a certificate incidentally.

Certificates are point-in-time by contract, do not survive serialization, and take no part in
`=` or `hash`.

### Running checks on a draft

| | |
|---|---|
| `(check d ::check-name)` | run one check; does not throw |
| `(check! d ::check-name)` | run one check; throws on failure |
| `(certify d ::cert-name)` | run every check in a certificate; does not throw |
| `(certify! d ::cert-name)` | run every check in a certificate; throws on failure |

All four take and return a draft.

### `drafting`

```clojure
(drafting [h heist]
  (-> h impl/do-a-thing impl/do-another))
```

Binds a draft of `heist`, runs the body, seals the result. `init` may be a sealed value, the type
itself (to build from nothing), or a draft.

Re-entrant: a draft owned by an enclosing `drafting` passes through unsealed, so only the
outermost form seals. That is what lets an impl function taking and returning a draft be called
standalone or nested without knowing which.

The convention that falls out: public API takes and returns sealed values, the impl layer takes
and returns drafts, and `drafting` marks the boundary.

### Undeclared keys

A draft accepts any key. Sealing rejects undeclared keys as `:extraneous-keys`. `clean` strips
them instead; `dissoc` removes one.

## Sealing

`seal` returns a value validated exactly as if every check had been run. It runs the checks that
are not currently passing — work done incrementally on the draft is not repeated.

`seal` is idempotent. An already-sealed value passes through unchanged, so `seal` is a projection
and a sealed value is a fixed point of it. Anything that is neither a draft nor sealed throws
`:cannot-seal`.

A field never written is reported as `:missing-field`. `nil` is a legal value where a check
accepts it — "never written" and "written to nil" are different things, and a field no check
mentions still has to be written.

## Constructing

Both generated constructors validate:

```clojure
(->Heist "The Italian Job" #{:charlie :lyle} 3000 {:charlie 1500 :lyle 1500})
(map->Heist {::name "The Italian Job" ::crew #{:charlie :lyle} ...})
```

`map->Heist` rejects an undeclared key and reports an omitted one as `:missing-field`.

There is no unvalidated constructor. The positional factory `deftype` would intern is replaced,
and the draft type's is unmapped.

Where the fields are not all in hand at once, build the way you would update:

```clojure
(drafting [h Heist] (reduce apply-fragment h fragments))
```

## Reading and writing a sealed value

A sealed value behaves as a map: `get`, keyword lookup, destructuring, `keys`, `into {}`, `seq`,
`count`, `meta`.

`assoc` runs the checks depending on that field. `merge` applies every entry and then runs the
checks depending on any of them, once — so a multi-key update is atomic.

`dissoc` of a declared field throws `:cannot-dissoc`, proposing `(assoc m k nil)` where the
field's checks admit `nil`. A sealed value cannot hold an undeclared key, so `dissoc` of one is
the ordinary no-op.

### Draft-only writes

A type may be configured so that direct `assoc` and `merge` on a sealed value throw, requiring
writes to go through `drafting`.

This exists because checks may perform IO. A check that queries a database, run on every bare
`assoc`, means business logic makes a query per write. Under `drafting`, N writes cost one round
of checks at seal instead of N.

It is also useful without IO: it makes the impl-layer convention enforceable rather than merely
recommended.

## Printing and reading

A sealed value prints with a namespace-qualified tag:

```clojure
(pr-str heist)   ;=> "#recognitum.heist/Heist{...}"
```

The tag is the key `clojure.edn` wants. `(rec/data-readers)` supplies the map:

```clojure
(edn/read-string {:readers (rec/data-readers)} s)
```

**Reading validates.** The reader goes through `map->Heist`, so a literal cannot produce a value
that construction would have refused.

The library registers nothing globally. To reach `read` and `read-string`, put a
`data_readers.clj` at a classpath root mapping the tag to the validating constructor. That file
is a placeholder until its namespace is loaded, and `clojure.edn` ignores it entirely.

A draft prints `#<Heist draft {...}>` — explicitly unreadable, because it is unvalidated working
state.

## Errors

Everything throws `ex-info` with an `:error` key to dispatch on.

| `:error` | when |
|---|---|
| `:check-failed` | a check returned something other than `true` |
| `:unknown-field` | `assoc` of a key the type does not declare |
| `:extraneous-keys` | sealing a draft holding undeclared keys |
| `:missing-field` | sealing a value with a field never written |
| `:cannot-dissoc` | `dissoc` of a declared field |
| `:cannot-seal` | sealing something neither a draft nor sealed |

A check failure carries the check's name, its docstring, the fields it declared and their values,
and whatever the check returned.

## Performance

Because arglists are known at macro time, checks are called with direct field access rather than
keyword lookup.

---

## Open

- **Distinguishing IO checks from pure ones.** The batching argument for draft-only writes rests
  on some checks being expensive. Nothing in the syntax says which.
- **Whether `assoc` can be per-check-aware** while the draft-only flag stays per-type.
- **Where the draft-only flag is written** in the definition.
- **The failure-accumulation shape.** What `check` and `certify` return on failure, and whether
  failures live on the draft so a caller can collect all problems in one pass rather than one
  per round trip.
- **The printed form under namespaced keys.** `#recognitum.heist/Heist` plus a map whose every
  key carries the same namespace is redundant; whether the printed map drops the namespace and
  the reader re-qualifies is unresolved.
