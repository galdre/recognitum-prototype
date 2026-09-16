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

  {:options {:io-limited true}

   :checks
   {::name  non-blank-string?
    ::crew  crew-set?
    ::take  non-neg-int?
    ::cuts  cuts-map?

    ::cuts-sum-to-take
    ("The cuts add up to the take."
     [take cuts] (= take (reduce + 0 (vals cuts))))

    ::only-crew-get-cuts
    ("Only crew get a cut."
     [crew cuts ::roster] (every? (into crew roster) (keys cuts)))}

   :io-deps
   {::roster ([crew] (fetch-roster crew))}

   :certificates
   {::shape  #{::name ::crew ::take ::cuts}
    ::payout #{::cuts-sum-to-take ::only-crew-get-cuts}}}

  some.ns/SomeProtocol
  (fiddlesticks [h x] ...)
  (bananas [h] ...))
```

Three positions: the type name, the fields, and a definition map, followed by protocol
implementations. Every section of the definition map is optional.

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

`:checks` maps a check name to a check. Names are namespace-qualified keywords, unique within the
type, and are what failures report.

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

**The arglist is the dependency declaration.** It holds two kinds of entry:

- a **symbol**, naming a field of this type;
- a **keyword**, naming an io-dep of this type.

The body receives exactly those, positionally, in that order, and nothing else is in scope — so
nothing can depend on something it did not name. Exactly one arity: the arity *is* the dependency
set.

The same rule governs io-dep fetch functions, which may additionally be written in a batched
form. There is no second way to declare a dependency anywhere in the definition.

Every entry is resolved at macroexpansion. A symbol that is not a declared field, a keyword that
is not a declared io-dep, a name used for both a check and an io-dep, or a cycle among io-deps is
a compile error naming the offender. A keyword in an arglist always means an io-dep; check names
never appear in arglists, so checks have no ordering among themselves.

The macro derives a field → checks index from the arglists. That index drives which checks re-run
on a write and which are invalidated on a draft.

### The check contract

- Return `true` — exactly `true` — when valid.
- Return anything else to fail. The returned value is failure data and is reported.
- Exceptions are **not** caught. A throw is a bug in the check, not a failure of the data, and is
  never reported as invalid.

A function whose last expression is truthy but not `true` will be read as a failure. Predicates
must return booleans.

### IO dependencies

`:io-deps` names functions that fetch something from outside the value — a database row, an
upstream response. They are written exactly as checks are, arglist and all, and follow the same
arglist rule:

```clojure
::roster    ("The crew roster on file." [crew] (fetch-roster crew))
::vetted    ([::roster] (filter :vetted roster))
```

An io-dep may name another, so fetches form a dependency graph and run in topological order.
Cycles are rejected at macroexpansion.

A check depends on one the same way:

```clojure
::only-crew-get-cuts ("Only crew get a cut."
                      [crew cuts ::roster] ...)
```

Anything is IO-bound exactly when its arglist contains a keyword, which is visible where it is
written without consulting anything else.

A fetch does not decide validity. It produces a value; the checks naming it decide what that
value means. `nil` is not a failure, or a success — it is whatever the checks say it is.

Nothing is specified about what a fetch returns. Its only consumers are the checks naming it,
declared in the same form, so there is no contract to document and nothing to keep in sync at a
distance.

An exception from a fetch propagates. A check that throws is a bug; a fetch that throws is
usually the database being unreachable, and "we could not determine whether this is valid" is a
different claim from "this is invalid". Folding it into failure would have the type assert
something it does not know. An author who wants it treated as a failure catches it inside the
fetch — that direction loses no information, and the reverse is not recoverable.

#### Singular and batched fetches

A fetch may be written either way:

```clojure
::roster ([crew] (fetch-roster crew))                  ; singular
::roster ([[crew] :as rows] (fetch-rosters rows))      ; batched
```

The batched form binds the collection of argument tuples with `:as`, and the two are told apart
by the reader rather than by convention: `[[crew] :as rows]` is not a legal function arglist, so
the macro knows at expansion time which it has. (`[[crew]]` alone *is* legal, which is why the
`:as` is required. `:as` is unqualified and io-dep names are namespaced, so it cannot be mistaken
for a dependency.)

The library deduplicates in both cases. Distinct argument tuples are computed once, and a result
is distributed to every value sharing a tuple — a hundred values referencing five dashboards do
five fetches' worth of work either way. What the batched form adds is that those five arrive in
one call.

The return shapes differ accordingly: a singular fetch returns the value; a batched fetch returns
a map keyed by the argument tuple, `{[crew] result, [crew2] result2}`. Keying by the tuple rather
than aligning positionally is what lets the library do the deduplication rather than the author.

Checks are never batched. A check judges one value.

**Shape gates IO.** Nothing reaches a database until the values it will carry there are
established as well-formed, because the payload that triggers the query is the one least worth
trusting. Precisely:

- Pure checks run first, ungated.
- An io-dep fetches once every pure check over the fields it names has passed, and every io-dep
  it names has fetched.
- An IO-bound check runs once every io-dep it names has fetched.

The rule is recursive, so a fetch transitively waits on the fields of everything beneath it
without that having to be stated anywhere.

Because an io-dep declares its own fields, the gate is narrow: a fetch over `crew` waits on the
checks covering `crew`, not on the whole type. A malformed field nothing IO touches does not
hold up IO that does not touch it.

**A fetch is shared for the length of one validation call, and no longer.** Within a single
`check`, `certify`, or `seal`, an io-dep is fetched at most once however many checks name it —
that is what naming them is for. When the call returns, the fetched value is gone. A later call
fetches again.

So a draft never carries IO state. What it carries is which checks passed, which is a fact about
the checking, not about the world.

### Certificates

`:certificates` maps a certificate name to a set of check names. Certificates are a
**draft-level** facility:
they name a subset of checks you can run early, before the value is complete.

They do not change what a sealed value guarantees. `seal` enforces every check on the type,
whether or not it belongs to a certificate.

## Drafts

A draft is an unvalidated working copy. Writes into it check nothing. It exists so a multi-step
update can pass through states no single write could produce.

### The verdict mask

A draft records which of its checks currently pass. The record is a mutable field — the draft's
*data* is immutable, but which checks have been run against that data is not part of the data.

- Drafting from a sealed value starts with every check passing.
- Drafting from the type starts with none.
- Running a check sets its bit when it passes and clears it when it fails. Clearing is what makes
  re-running a check meaningful: a check that passed earlier and fails now must stop counting as
  passed, and without that there would be no way to un-pass anything.
- A write seeds the new draft's mask from the old one, clearing exactly the checks whose arglists
  mention the written field — one bitwise `and` against an invalidation mask the macro computes
  from the index.

The seed is a snapshot, so versions diverge rather than share. Checking `x'` says nothing about
`x`, and checking `x` says nothing about `x'`.

A draft is single-owner, as a transient is, and the mask is why that matters rather than merely
being conventional.

Verdicts are cached; the evidence behind them is not. io-deps are fetched within a validation
call and gone when it returns, so what a draft carries is that a check passed, never what it
consulted to decide. A verdict for a check that consulted IO is point-in-time, which is what the
certificate contract already says.

### Certificates on a draft

**Certificate-holding is derived, not asserted.** A draft holds a certificate when every check in
it currently passes. Overlapping certificates work without coordination, and running individual
checks can complete a certificate incidentally.

Certificates do not survive serialization and take no part in `=` or `hash`.

### Running checks on a draft

| | returns |
|---|---|
| `(check d ::check-name)` | `true`, or the failing check's data |
| `(check! d ::check-name)` | `true`, or throws |
| `(certify d ::cert-name)` | `true`, or `{check-name failure-data}` for those that failed |
| `(certify! d ::cert-name)` | `true`, or throws |

None of them return a draft. The verdict goes in the mask, so the return value is free to be the
answer to the question asked. `true` means passed, as it does for a check itself.

Running a check runs whatever it depends on that is not already passing — an IO-bound check
cannot run before its pure prerequisites, and the mask makes satisfied prerequisites free. So
`check` means *establish this*, not *run exactly this*.

Failures are not retained. The call that finds one returns it; a later call re-runs anything not
passing and produces it again.

### Metadata

`meta` on a draft returns its metadata with the library's own keys merged over the top:

```clojure
(meta d)
;=> {:user-key 1
;    :recognitum.core/passed       #{::name ::crew ::cuts-sum-to-take}
;    :recognitum.core/certificates #{::shape}}
```

Merging over rather than under is deliberate. `with-meta` is public, so anything a user could
set, a user could forge; keys the library writes last cannot be overridden by anything a caller
passes in.

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

It collects every failure rather than throwing on the first, and throws once carrying all of
them. That costs nothing when everything passes, since every check runs in that case anyway; the
extra work happens only when it is about to throw, which is where it is worth doing. The gate
makes it two-phase: collect all pure failures and throw if there are any, otherwise fetch and
collect the IO ones.

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

`:options {:io-limited true}` makes direct `assoc` and `merge` on a sealed value throw for any
field an IO-bound check depends on, requiring those writes to go through `drafting`. Fields no
IO check reaches are unaffected — declaring io-deps is what makes that distinction computable.

A check that queries a database, run on every bare `assoc`, means business logic makes a query
per write. Under `drafting`, N writes cost one round of checks at seal instead of N.

## Printing and reading

A sealed value prints with a namespace-qualified tag, and the printed map drops the namespace
that tag already carries:

```clojure
(pr-str heist)   ;=> "#recognitum.heist/Heist{:name \"The Italian Job\", :take 3000, ...}"
```

The reader accepts both forms: bare keys, which it qualifies from the tag, and fully qualified
keys, which it takes as given.

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

- **The bulk validation entry point.** Batched fetches exist to serve one, but nothing calls
  them yet. It cannot throw on the first bad value without defeating itself, so it returns
  per-value results — whether it also offers partial success, handing back the values that
  sealed alongside failures for those that did not, is the open part.
- **Whether a sealed value reports anything through `meta`.** On a draft the keys carry
  information; on a sealed value every check passes by definition, so they would be constant.
- **The order failures are reported in.** Checks come from a map literal, which does not preserve
  declaration order past eight entries, though the macro can.
- **What `:options` holds besides `:io-limited`.**
