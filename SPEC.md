# recognitum — specification

The intended state of the library. Written as reference for implementation, not as a record of
how it was arrived at.

## What it is

A record-like type that carries proof of its own validation. If you are holding a `Heist`, every
check declared on `Heist` passed. A bare map with the right keys is not a `Heist`, `assoc`-ing
garbage into one does not produce one, and there is no constructor that skips the checks.

## The baseline is `defrecord`

This is `clojure.core/defrecord` with a set of deliberate, opinionated changes. When any behaviour
is in question, the first question is what `defrecord` does for the same operation; the answer is
to match it unless this document records a deviation on purpose. A deviation this document does
not record is a defect.

**Deliberate deviations:** two generated types rather than one; checks run on every write;
`dissoc` of a declared field refused; undeclared keys refused on a sealed value; field keys
qualified by the defining namespace; a namespace-qualified print tag, because the reader's
record-literal path needs a static `create` a `deftype` cannot supply; the library's keys
merged over `meta`; a value being callable as a function of its keys, which the template
supplies and `defrecord` does not; and everything under Drafts, Certificates and IO
dependencies, which `defrecord` has no counterpart for.

**The generated types are records**, not maps wearing a record's shape. Both are built on
`recognitum.collections/AbstractRecord` rather than `potemkin.collections/AbstractMap`: they
carry `clojure.lang.IRecord` and `java.io.Serializable` and not `clojure.lang.MapEquivalence`,
`equiv` compares the class before it compares an entry and `hasheq` mixes the class in, while
`.equals` and `.hashCode` follow the map contract as a record's do, and `empty` refuses as a
record's does.

That is what makes "a bare map with the right keys is not a `Heist`" true under `=` rather than
only in prose, and what makes `(= (->A 1) (->B 1))` false for two unrelated types holding the
same fields. It is also what keeps `clojure.walk` honest: `walk` reaches for `empty` when it does
not find `IRecord`, so `(clojure.walk/postwalk identity heist)` handed back a
`PersistentArrayMap` — silently, through the most ordinary tool in the standard library — for as
long as the types were built on a map template. The table of the six defects that came of that,
measured against the baseline before they were fixed, is in this file's history.

One walk does not round-trip. `walk` rebuilds a record by `conj`-ing its entries back one at a
time, and on an `:io-limited` type each of those is the direct write to a sealed value the option
refuses — so `postwalk` over one throws `:io-limited`, naming the fields and pointing at
`drafting`, rather than quietly running a round of checks per entry. Loud is the point; the
degradation the marker exists to stop was the silent kind.

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

### Shape checks

**A check named after a field is that field's shape check.** `::name`, whether written as the
shorthand or as `::name ([name] …)`, is the check that says what a well-formed `name` is. The
naming convention is the declaration; nothing else marks it.

A shape check names exactly its own field, and nothing else — `::name ([name crew] …)` is a
compile error. That is what keeps the dependency structure a two-level graph: shape checks
depend on nothing, every other check depends only on shape checks, and no ordering among checks
ever needs computing.

A field with no shape check has no declared shape. Checks naming it run against whatever it
holds, and have to be total over anything. That is the author's choice, made loudly.

**Every other check waits on the shape checks of the fields it names.** `::cuts-sum-to-take
[take cuts]` runs only once `::take` and `::cuts` have passed, so it receives well-formed input by
construction and restates nothing. A throw from it is therefore a genuine bug in the check,
which is what the check contract already says a throw is.

A check whose gate has not passed does not run and is not reported. The shape failure is the
actionable one; "did not run because `cuts` is not a map" adds nothing to "`cuts` is not a map".

### Unset fields

A field that has not been written reads as `nil`, everywhere, exactly as it would on a
`defrecord` whose field was never set. There is no sentinel and no "never written" state distinct
from `nil`. A draft opened from the type starts with every field `nil`; whether `nil` is
acceptable there is the shape check's business and nothing else's; a field with no shape check
seals as `nil`.

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

An io-dep is an ordinary function, and nothing in the library needs real infrastructure to
demonstrate one. The example model fakes its fetches in process — an in-memory table stands in
for the database — which is enough to show a fetch resolving, the gate keeping a malformed value
away from it, deduplication across repeated arguments, and a check judging what comes back.

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

**Shape gates everything.** Nothing reaches a database until the values it will carry there are
established as well-formed, because the payload that triggers the query is the one least worth
trusting — and, one level up, no cross-field check runs on a field its shape check has rejected.
The same rule, three levels:

- Shape checks run first, ungated.
- Every other pure check runs once the shape checks of the fields it names have passed.
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

`:certificates` maps a certificate name to a set of check names. A built-in certificate covering
every check on the type exists without being declared, so bulk checking can run everything in one
pass rather than leaving whatever a named certificate omitted to run per value. It is called
`:recognitum.core/every-check`, and is the one certificate name a type may not declare for
itself.

A certificate is a declaration, so it is resolved at macroexpansion as an arglist is. A name it
covers that is not a check of this type, a certificate covering nothing — which every draft of
the type would hold, including one nothing had been run against — and a name already spelling a
check are each a compile error naming the offender.

Certificates are a **draft-level** facility:
they name a subset of checks you can run early, before the value is complete.

They do not change what a sealed value guarantees. `seal` enforces every check on the type,
whether or not it belongs to a certificate.

## Telling values apart

`defrecord` generates two types, so `instance?` no longer asks the question people usually mean.
`of-type?` asks it:

```clojure
(rec/of-type? Heist x)     ; a Heist, sealed or draft
```

| | |
|---|---|
| `(rec/of-type? Heist x)` | either form of `Heist` |
| `(instance? Heist x)` | a sealed `Heist`, specifically |
| `(rec/draft? x)` | any draft, of any type |
| `(rec/sealed? x)` | any sealed value, of any type |

"A draft of `Heist`" composes from two of them, which is the right amount of spelling-out for
something that specific. `of-type?` is a function rather than a generated per-type predicate, so
nothing is interned into the defining namespace that could collide with a hand-written one.

It is false for anything unregistered, including the type object itself — a class is not a value
of its type.

## Protocol implementations

Trailing forms in the definition are protocol implementations, written once and installed on both
generated types.

```clojure
(rec/defrecord Heist [name crew take cuts] {...}
  some.ns/SomeProtocol
  (promote      [h who bonus] (-> h (assoc ...) (assoc ...)))
  (total-payout [h]           (reduce + 0 (vals (::cuts h)))))
```

On the draft type the body runs as written. On the sealed type it is wrapped: the method opens a
draft of the value, runs the body, and seals the result. So a method called on a sealed value
cannot produce an invalid one whatever it does inside, and the same body serves the impl layer
unchanged — the re-entrancy that `drafting` already provides, applied without anyone asking for
it.

Inside the body, the receiver is a draft in both cases — so a body asking `(instance? Heist h)`
would surprise itself, and wants `(rec/of-type? Heist h)` instead.

**Only a draft of this type is sealed on the way out.** Most protocol methods return something
else entirely — a number, a string, a boolean — and those pass through untouched. A method
handing back an already-sealed value passes through as well.

**`unsealed` opts out**, as `reduced` opts out of a reduction:

```clojure
(begin-revision [h] (unsealed (assoc h ::name "draft title")))
```

The wrapper unwraps it and returns the draft unsealed. Both implementations unwrap it, so the
body means the same thing wherever it runs; on the draft type it is simply a no-op. What the
caller receives is a free draft, owned by nothing, adoptable by a later `drafting` or sealable by
hand. `(unsealed x)` where `x` is not a draft of this type throws — the marker asserts what is
being returned, and a wrong assertion should say so.

`drafting` does not honour `unsealed`. The wrapper is implicit, so opting out of it is fair; a
`drafting` form is the author asking to seal, and a marker that quietly defeated that would be a
hole rather than an escape hatch.

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

Every one of them checks everything it was asked to check and reports every failure. None stops
at the first.

Failures come back keyed by check name, in phase order — shape checks, then the other pure
checks, then the IO-bound ones — and in declaration order within a phase. Built with `array-map`
rather than as a literal, so insertion order holds at any size. Shape failures first is the
order you fix them in anyway.

### Checking many drafts at once

`check-all` and `certify-all` take a collection of drafts and run against all of them, so a
batched fetch sees every argument tuple in one call. Results are index-aligned with the input:
two drafts holding identical data are `=`, so keying by draft would silently collapse them.

Partial success is a workspace notion, so only the non-throwing forms offer it. `check-all` and
`certify-all` report per-value results and leave each draft's mask updated to match. The bang
forms collect every failure across every value and throw once with all of them.

Nothing bulk produces sealed values. A guarantee does not come in partial, and it does not need
to: bulk checking leaves the masks populated, so sealing each value afterwards finds its checks
already passing and performs no IO.

### Metadata

`meta` returns a value's metadata with the library's own keys merged over the top. Drafts and
sealed values answer the same way, so nothing has to establish which it is holding before asking:

```clojure
(meta d)
;=> {:user-key 1
;    :recognitum.core/passed       #{::name ::crew ::cuts-sum-to-take}
;    :recognitum.core/certificates #{::shape}}
```

On a sealed value every check passes and every certificate is held, so the same keys are present
and constant. They carry no information there, but a common interface is worth more than the
absence of a constant, and the merged portion is a compile-time constant for a sealed type.

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

A field never written reads as `nil` and is judged as `nil` by its shape check. There is no
separate notion of a missing field: absence is `nil`, as on a `defrecord`, and a field with no
shape check seals holding it.

## Constructing

Both generated constructors validate:

```clojure
(->Heist "The Italian Job" #{:charlie :lyle} 3000 {:charlie 1500 :lyle 1500})
(map->Heist {::name "The Italian Job" ::crew #{:charlie :lyle} ...})
```

`map->Heist` rejects an undeclared key. An omitted one is `nil`, as it would be on a `defrecord`,
and the shape check says whether that is acceptable.

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
| `:cannot-dissoc` | `dissoc` of a declared field |
| `:cannot-seal` | sealing something neither a draft nor sealed |

A `:check-failed` carries every failure, never one — each throwing entry point runs everything it
was asked to run first. They are keyed by check name in declaration order, and each carries the
check's docstring, the fields it declared and their values, and whatever the check returned.

## Performance

Because arglists are known at macro time, checks are called with direct field access rather than
keyword lookup.

---

## Open

`:options` holds `:io-limited` and nothing else so far. It exists so that it can hold more.
