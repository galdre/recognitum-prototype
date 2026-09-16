# recognitum

A record-like type that carries proof of its own validation.

If you are holding a `Heist`, every check declared on `Heist` passed. Not "someone probably
called `validate!` upstream" — the type cannot exist otherwise. A bare map that happens to have
the right keys is not a `Heist`, `assoc`-ing garbage into one does not produce one, and there is
no constructor that skips the checks.

## Defining a type

`defrecord` shadows `clojure.core/defrecord`, so require the library under an alias rather than
referring it — the two then coexist in one namespace without ceremony:

```clojure
(ns my.app.heist
  (:require [clojure.string :as str]
            [recognitum.core :as rec]))

;; Standing in for the database the io-deps below would really reach. Nothing in this
;; library needs real infrastructure to be exercised; an io-dep is an ordinary function.
(def house
  {:charlie {:vetted true} :lyle {:vetted true} :left-ear {:vetted true}
   :mole    {:vetted false}})

(defn fetch-rosters [rows]
  (into {} (map (fn [[crew :as tuple]] [tuple (into #{} (filter house) crew)])) rows))

(defn fetch-vetted [roster]
  (into #{} (filter #(get-in house [% :vetted])) roster))

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x))
                                (every? non-neg-int? (vals x))))

(defprotocol Job
  (rename       [h nm])
  (total-payout [h]))

(rec/defrecord Heist
  [name crew take cuts]

  {:options {:io-limited true}

   :checks
   {::name non-blank-string?
    ::crew crew-set?
    ::take non-neg-int?
    ::cuts cuts-map?

    ;; Neither of these is named after a field, so each waits on the shape checks of the
    ;; fields it names -- and restates neither of them.
    ::cuts-sum-to-take
    ("the cuts add up to the take"
     [take cuts] (= take (reduce + 0 (vals cuts))))

    ::only-crew-get-cuts
    ("only crew get a cut"
     [crew cuts] (every? crew (keys cuts)))

    ::crew-is-vouched-for
    ("everyone the house has on file for this crew is vetted"
     [::roster ::vetted] (= roster vetted))}

   :io-deps
   {::roster ([[crew] :as rows] (fetch-rosters rows))
    ::vetted ([::roster] (fetch-vetted roster))}

   :certificates
   {::shape  #{::name ::crew ::take ::cuts}
    ::payout #{::cuts-sum-to-take ::only-crew-get-cuts}}}

  Job
  (rename       [h nm] (assoc h ::name nm))
  (total-payout [h]    (reduce + 0 (vals (::cuts h)))))
```

Three positions — the type name, the fields, and a definition map — followed by protocol
implementations. Every section of the definition map is optional; a key that names no section is
a compile error, because a mistyped `:cheks` would define a type with no checks, and a value
asserting a validation that never ran is the one thing here that must not be constructible.

That is the whole of the form. The rest of this document is what each part of it buys.

## Fields

A plain vector of symbols, as `defrecord` takes. Each becomes a real field on the generated type
— so reads and writes are field access rather than map lookups — and a key **qualified by the
namespace the type is defined in**. A `Heist` written in `my.app.heist` answers to
`:my.app.heist/name`, which is what `::name` spells inside that file and `::h/name` spells
anywhere that aliases it. A bare `:name` is a key the type does not declare, so two types that
happen to share a field name share nothing else, and a map lifted out of one cannot be quietly
poured into another.

## Checks

`:checks` maps a check name — a namespace-qualified keyword, unique within the type, and what
failures report under — to a check, written in one of three forms:

```clojure
::name       non-blank-string?                      ; a bare fn over the field it names
::valid-name ([name] (non-blank-string? name))      ; arglist and body
::cuts-sum-to-take                                  ; with a docstring, as defn takes one
("the cuts add up to the take"
 [take cuts] (= take (reduce + 0 (vals cuts))))
```

**The arglist is the dependency declaration.** It holds two kinds of entry: a symbol, naming a
field of this type, and a keyword, naming an io-dep of this type. The body receives exactly
those, positionally, in that order, and nothing else is in scope — so nothing can depend on
something it did not name, and the field → checks index the macro derives from the arglists
cannot disagree with what the bodies actually read. Exactly one arity: the arity *is* the
dependency set.

Every entry is resolved while the macro runs. A symbol that is not a declared field, a keyword
that is not a declared io-dep, a name used for both a check and an io-dep, or a cycle among
io-deps is a compile error naming the offender. There is no second way to declare a dependency
anywhere in the form.

### Shape checks

**A check named after a field is that field's shape check.** `::name`, whether written as the
shorthand or as `::name ([name] …)`, is the check that says what a well-formed `name` is. The
naming convention is the declaration; nothing else marks it, and a shape check names exactly its
own field or the form does not compile.

**Every other check waits on the shape checks of the fields it names.** `::cuts-sum-to-take
[take cuts]` runs only once `::take` and `::cuts` have passed, so it receives well-formed input
by construction and restates nothing — which is why a throw out of it is a genuine bug in the
check, exactly as the contract below says a throw is.

A check whose gate has not passed does not run and is not reported. The shape failure is the
actionable one; "did not run because `cuts` is not a map" adds nothing to "`cuts` is not a map".
So failures come back in three phases — shape checks, then the other pure checks, then the
IO-bound ones — and in declaration order within a phase.

A field with no shape check has no declared shape. The checks naming it run against whatever it
holds and have to be total over anything. That is the author's choice, made loudly.

### The contract

- Return `true` — exactly `true` — when valid.
- Return anything else to fail. What was returned *is* the failure data, and is reported.
- Exceptions are **not** caught. A throw is a bug in the check rather than a claim about the
  data, and "we could not determine whether this is valid" is not "this is invalid".

A function whose last expression is truthy but not `true` is read as a failure. Predicates must
return booleans.

## Reading

A sealed value behaves as a map. The examples below are written from a consumer namespace that
has done `(:require [my.app.heist :as h])`, so `::h/take` is `:my.app.heist/take`.

```clojure
(::h/take heist)                       ;=> 3000
(get heist ::h/crew)                   ;=> #{:charlie :lyle :left-ear}
(let [{::h/keys [name take]} heist] …)  ; destructuring works
(keys heist)                           ;=> (:my.app.heist/name :my.app.heist/crew …)
(into {} heist)                        ;=> a plain map, keys and all
(:take heist)                          ;=> nil — a bare key is not a field
```

## Writing

`assoc` runs the checks that name the field:

```clojure
(assoc heist ::h/name "The Sequel")  ;=> #my.app.heist/Heist{…}
(assoc heist ::h/take "lots")        ;=> throws :check-failed
(assoc heist :getaway-car "mini")    ;=> throws :unknown-field
(assoc heist :take 4000)             ;=> throws :unknown-field — bare, so undeclared
```

`merge` applies every entry and then runs the checks naming any of them **once**, so a multi-key
update is atomic:

```clojure
;; fine — take and cuts change together
(merge heist {::h/take 4000 ::h/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})

;; throws :check-failed — the cuts no longer add up
(assoc heist ::h/take 4000)
```

`dissoc` of a declared field throws. A `Heist` without `::h/cuts` is not a `Heist`, and silently
degrading to a plain map is exactly the failure this type exists to prevent. Where a field's
checks admit `nil`, `(assoc heist ::h/cuts nil)` is how you spell absence — the error says so.

Undeclared keys are ordinary: `dissoc` removes one from a draft, and on a sealed value — which
cannot be holding one — it is the usual no-op. `clean` is how you strip them in bulk.

## Multi-step updates: `drafting`

A single `assoc` cannot express a change that breaks a check halfway through. `drafting` opens a
**draft** — an unvalidated working copy — runs your body, and seals the result:

```clojure
(defn update-the-thing [heist]
  (drafting [h heist]
    (-> h impl/do-a-thing impl/do-another)))
```

Inside the draft, writes check nothing. Reaching the end of `drafting` seals it, and what comes
back is validated exactly as if every check had been run against it. If anything fails, nothing
is returned — you still have your original value, untouched.

```clojure
(drafting [h heist]
  (reduce apply-changeset h changesets))   ; 500 writes, one round of checks
```

`init` may be a sealed value, a draft, or the type itself, which is how you build from nothing.
Sealing is idempotent, so a body that has already produced a finished value is fine: `seal` is a
projection and a sealed value is a fixed point of it.

**`drafting` is re-entrant.** A draft owned by an enclosing form passes through unsealed, so only
the outermost form seals. That is what lets an impl function taking and returning a draft be
called standalone or nested without knowing which. The convention that falls out: your public
API takes and returns sealed values, your impl layer takes and returns drafts, and `drafting`
marks the boundary.

## Protocol implementations

Trailing forms in the definition are protocol implementations, written once and installed on
both generated types. On the draft type the body runs as written. On the sealed type it is
wrapped: the method opens a draft of the receiver, runs the body against that, and seals the
result. So a method called on a sealed value cannot produce an invalid one whatever it does
inside, and the same body serves the impl layer unchanged:

```clojure
(h/rename heist "The Sequel")                       ;=> a sealed Heist
(drafting [x heist] (h/rename x "The Sequel"))      ;   the body ran on the draft, unsealed
(h/total-payout heist)                              ;=> 3000 — not a Heist, so passed through
```

**Only a draft of this type is sealed on the way out.** Most protocol methods return something
else entirely — a number, a string, a boolean — and those pass through untouched, as does a
method handing back an already-sealed value.

Inside the body the receiver is a draft in both cases, so a body asking `(instance? Heist h)`
would surprise itself and wants `(rec/of-type? Heist h)` instead.

**`unsealed` opts out**, as `reduced` opts out of a reduction:

```clojure
(begin-revision [h] (rec/unsealed (assoc h ::name "draft title")))
```

The wrapper unwraps it and hands the draft back unsealed — a free draft, owned by nothing,
adoptable by a later `drafting` or sealable by hand. Both implementations unwrap it, so the body
means the same thing wherever it runs. `(unsealed x)` where `x` is not a draft of this type
throws: the marker asserts what is being returned, and a wrong assertion should say so.
`drafting` does not honour it — the wrapper is implicit, so opting out of it is fair, but a
`drafting` form is you asking to seal.

## Telling values apart

`defrecord` generates two types, so `instance?` no longer asks the question people usually mean.
`of-type?` asks it:

| | |
|---|---|
| `(rec/of-type? Heist x)` | either form of `Heist` |
| `(instance? Heist x)` | a sealed `Heist`, specifically |
| `(rec/draft? x)` | any draft, of any type |
| `(rec/sealed? x)` | any sealed value, of any type |

"A draft of `Heist`" composes from two of them, which is the right amount of spelling out for
something that specific. `of-type?` is a function rather than a predicate generated per type, so
a definition interns nothing into your namespace that could collide with a name you wrote there
yourself. It is false for anything unregistered, the type object included — a class is not a
value of its type.

## What a draft has established

A draft records which of its checks currently pass. The record is mutable: the draft's *data* is
immutable, but which checks have been run against that data is not part of the data.

- Drafting from a sealed value starts with every check passing.
- Drafting from the type starts with none.
- Running a check sets its bit when it passes and clears it when it fails.
- A write clears exactly the checks that read the field written, and nothing else.

The seed is a snapshot, so versions diverge rather than share: checking `x'` says nothing about
`x`. A draft is single-owner, as a transient is, and this is why that matters rather than merely
being conventional.

You can run checks against a draft before it is complete:

| | returns |
|---|---|
| `(rec/check d ::check-name)` | `true`, or the failing check's data |
| `(rec/check! d ::check-name)` | `true`, or throws |
| `(rec/certify d ::cert-name)` | `true`, or `{check-name failure-data}` for those that failed |
| `(rec/certify! d ::cert-name)` | `true`, or throws |
| `(rec/passing d)` | the set of check names currently passing |
| `(rec/holding d)` | the set of certificate names currently held |

None of them return a draft. The verdict goes in the draft's record, which is what frees the
return value to be the answer to the question asked.

Running a check runs whatever it depends on that is not already passing, so `check` means
*establish this*, not *run exactly this* — and a prerequisite already passing is free. Failures
are not retained: the call that finds one returns it, and a later call runs anything not passing
and produces it again. Every one of these runs everything it was asked to run and reports every
failure; none stops at the first.

A field the draft has never been written to reads as `nil`, so a check naming one judges `nil`
and answers for it. There is no state distinct from `nil` to report, and nothing to be told
about before the checks can run.

### Metadata

`meta` returns a value's own metadata with the library's keys merged over the top. Drafts and
sealed values answer the same way, so nothing has to establish which it is holding before asking:

```clojure
(meta d)
;=> {:user-key 1
;    :recognitum.core/passed       #{::name ::crew ::cuts-sum-to-take}
;    :recognitum.core/certificates #{::shape}}
```

On a sealed value every check passed and every certificate is held, so those keys are present
and constant — a compile-time constant for the type, allocated once when it was defined. Merging
over rather than under is deliberate: `with-meta` is public, so anything a caller could set a
caller could forge, and keys the library writes last cannot be overridden by anything handed in.

## Certificates

`:certificates` maps a certificate name to the checks it covers — a group you can establish on a
draft early, before the value is complete:

```clojure
(rec/certify d ::h/shape)     ;=> true
(rec/holding d)               ;=> #{:my.app.heist/shape}
```

**Holding one is derived, not asserted.** A draft holds a certificate when every check in it
currently passes, which is one bitwise `and` against the draft's record. So overlapping
certificates need no coordination, running individual checks can complete a certificate
incidentally, and there is nowhere to record holding one except by having actually passed its
checks.

A built-in certificate covering every check on the type exists without being declared. It is
called `:recognitum.core/every-check` (the var `rec/every-check` holds it), and is the one
certificate name a type may not declare for itself.

Certificates are a draft-level facility, and they change nothing about what a sealed value
guarantees: `seal` enforces every check on the type, whether or not it belongs to one.

## IO dependencies

`:io-deps` names functions that fetch something from outside the value — a database row, an
upstream response. They are written exactly as checks are, arglist and all, and follow the same
rule, so an io-dep may name another and the fetches form a graph that runs in topological order.
Anything is IO-bound exactly when its arglist holds a keyword, which is visible where it is
written without consulting anything else.

A fetch does not decide validity. It produces a value, and the checks naming it decide what that
value means; `nil` is neither a failure nor a success. An exception from a fetch propagates
rather than becoming a failure — a check that throws is a bug, while a fetch that throws is
usually the database being unreachable, and folding that into failure would have the type assert
something it does not know. An author who wants it treated as a failure catches it inside the
fetch; that direction loses nothing, and the reverse is not recoverable.

A fetch may be written either way, and the reader tells them apart:

```clojure
::roster ([crew] (fetch-roster crew))               ; singular
::roster ([[crew] :as rows] (fetch-rosters rows))   ; batched
```

`[[crew] :as rows]` is not a legal function arglist, so the macro knows at expansion time which
it has. The inner vector is the dependency declaration either way, so the gate, the fetch order
and the field → checks index come out the same; what differs is the call. A batched fetch is
handed the collection of argument tuples and answers a map keyed by them, `{[crew] result}` —
keying by the tuple rather than aligning positionally is what lets the library do the
deduplication rather than you. Checks are never batched; a check judges one value.

**Shape gates IO.** Nothing reaches a database until the values it will carry there are
established as well-formed, because the payload that triggers the query is the one least worth
trusting:

- Pure checks run first, ungated.
- An io-dep fetches once every pure check over the fields it names has passed, and every io-dep
  it names has fetched.
- An IO-bound check runs once every io-dep it names has fetched.

The rule is recursive, so a fetch transitively waits on the fields of everything beneath it
without that having to be stated anywhere. And because an io-dep declares its own fields, the
gate is narrow: a fetch over `crew` waits on the checks covering `crew`, not on the whole type.

**A fetch is shared for the length of one validation call, and no longer.** Within a single
`check`, `certify` or `seal`, an io-dep is fetched at most once however many checks name it —
that is what naming it is for — and when the call returns the fetched value is gone. So a draft
never carries IO state. What it carries is which checks passed, which is a fact about the
checking rather than about the world.

### Checking many at once

`check-all` and `certify-all` take a collection of drafts of one type and run against all of
them, so a batched fetch sees every argument tuple in one call and distinct tuples are computed
once however many values share them:

```clojure
(rec/certify-all drafts rec/every-check)   ;=> [true true {::h/cuts-sum-to-take false} …]
```

Results are index-aligned with the input, because two drafts holding identical data are `=` and
keying by draft would silently collapse them. Partial success is a workspace notion, so only the
non-throwing forms offer it; `check-all!` and `certify-all!` collect every failure across every
value and throw once with all of them, keyed by index.

Nothing bulk produces sealed values. A guarantee does not come in partial, and it does not need
to: bulk checking leaves each draft holding its own verdicts, so sealing them afterwards finds
their checks already passing and performs no IO.

### Draft-only writes

```clojure
{:options {:io-limited true}}
```

makes direct `assoc` and `merge` on a **sealed** value throw for any field an IO-bound check
reads — its own fields and its fetches' alike. A check that queries a database, run on every
bare `assoc`, means business logic makes a query per write; under `drafting`, N writes cost one
round of checks at the seal instead of N.

```clojure
(assoc heist ::h/crew #{:charlie})              ;=> throws :io-limited
(drafting [x heist] (assoc x ::h/crew #{:charlie}))   ; how that write is spelled
(assoc heist ::h/name "The Lavender Hill Mob")  ;=> fine — no fetch reads ::h/name
```

It refuses rather than defers: you asked for a sealed value, and a sealed value is one every
check passed on, so there is nothing to defer to — nothing is fetched and no check runs. Fields
no IO-bound check reaches are unaffected, which is what declaring io-deps buys; the set is
computed from the arglists rather than restated by you.

## Building a new value

`defrecord` generates the two constructors `defrecord` does, both validating:

```clojure
(->Heist "The Italian Job" #{:charlie :lyle :left-ear} 3000
         {:charlie 1000 :lyle 1000 :left-ear 1000})

(map->Heist {::h/name "The Italian Job"
             ::h/crew #{:charlie :lyle :left-ear}
             ::h/take 3000
             ::h/cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})
```

Both run every check, because construction touches every field. `map->Heist` additionally
rejects a key the type does not declare; a key it declares and the map omits is `nil`, as it
would be on a `defrecord`, and that field's shape check says whether `nil` is acceptable.

It takes either spelling: a bare keyword naming a declared field is qualified into the type's
namespace, and anything else is taken as written, so a stray `:getaway-car` is reported as
`:getaway-car` rather than as something you never wrote. That is what lets a printed literal —
whose tag already carries the namespace, so the printed map drops it — read straight back
through the validating constructor.

There is no unvalidated constructor. The positional factory `deftype` would otherwise intern is
replaced, and the draft type's is unmapped — a `Heist` cannot be brought into existence except
through a check.

Where the fields are not all in hand at once, build it the way you would update it:

```clojure
(drafting [h Heist]
  (reduce apply-fragment h fragments))
```

A field never written is `nil` at the seal, and its shape check is what decides whether that is
acceptable. There is no separate notion of a missing field: absence is `nil`, as on a
`defrecord`, and a field with no shape check seals holding it.

## Undeclared keys

A draft accepts any key; sealing is where it is decided. By default an undeclared key is an
error:

```clojure
(drafting [h heist]
  (assoc h :getaway-car "mini"))   ;=> throws :extraneous-keys
```

Call `clean` to strip them instead. It is an ordinary draft → draft step, so it goes wherever you
want in the pipeline:

```clojure
(drafting [h heist]
  (-> h (assoc :getaway-car "mini") rec/clean))   ;=> #my.app.heist/Heist{…}
```

That gives you both policies — reject or strip — chosen per call site rather than baked into the
type. To drop one undeclared key rather than all of them, `dissoc` it.

## Errors

Everything throws `ex-info` with an `:error` key you can dispatch on. The keys are qualified —
`:recognitum.core/check-failed` is what you match on; the table writes them short.

| `:error` | when |
|---|---|
| `:check-failed` | a check returned something other than `true` |
| `:unknown-field` | `assoc` of a key the type does not declare |
| `:extraneous-keys` | sealing a draft holding undeclared keys |
| `:cannot-dissoc` | `dissoc` of a declared field — use `assoc … nil` if its checks admit nil |
| `:cannot-seal` | sealing something that is neither a draft nor an already-sealed value |
| `:io-limited` | a direct `assoc` or `merge` on a sealed value of an `:io-limited` type |

A `:check-failed` carries every failure, never one — each throwing entry point runs everything it
was asked to run first. They come back keyed by check name in declaration order, and each carries
the check's docstring, the dependencies it declared with what it was handed for each, and
whatever it returned:

```clojure
{:error    :recognitum.core/check-failed
 :record   Heist
 :failures {:my.app.heist/only-crew-get-cuts
            {:doc      "only crew get a cut"
             :values   #:my.app.heist{:crew #{:charlie :lyle :left-ear}
                                      :cuts {:charlie 1000 :stranger 100}}
             :returned false}}}
```

Mis-declaration is a compile error rather than a run-time one, so there is a second family of
`ex-info`s thrown while the macro runs — `::unknown-dependency`, `::unknown-section`,
`::unknown-option`, `::io-dep-cycle`, `::reserved-method` and their neighbours. Each names the
offender and the alternatives it could have meant.

## Printing and reading

A sealed value prints with a namespace-qualified tag, and the printed map drops the namespace
that tag already carries:

```clojure
(pr-str heist)
;=> "#my.app.heist/Heist{:crew #{…}, :cuts {…}, :name \"The Italian Job\", :take 3000}"
```

The reader puts it back. It accepts both spellings — bare keys, which it qualifies from the tag,
and fully qualified keys, which it takes as given — so a literal written either way reads, and
both go through the same validating constructor.

That tag is exactly the key `clojure.edn` wants, and `data-readers` supplies the map:

```clojure
(edn/read-string {:readers (rec/data-readers)} s)
```

**Reading is validating.** The reader *is* `map->Heist`, so a literal cannot smuggle in a value
that would have been rejected on construction — a bad one throws at read time rather than
producing something that lies about itself.

The library never registers anything globally, deliberately. `clojure.edn` takes `:readers` and
consults nothing else, and it is the reader you want for anything arriving from outside the
process. Registering from the macro would not work anyway: `clojure.main` thread-binds
`*data-readers*`, so a registration made while loading a namespace is invisible to the thread
that loaded it and visible to every other one.

To make a tag available to plain `read` and `read-string`, put a `data_readers.clj` at the root
of your classpath — `src/data_readers.clj`, or `dev/data_readers.clj` for a fixture. It is a map
literal of tag to fully-qualified var, and pointing it at the validating constructor is the whole
trick:

```clojure
{my.app.heist/Heist my.app.heist/map->Heist}
```

Clojure merges every such file into `*data-readers*` at startup. Two things to know: the entry is
a *placeholder* until the namespace is loaded, so reading the tag before requiring
`my.app.heist` throws `Attempting to call unbound fn`; and `clojure.edn` ignores these files
entirely, so `:readers` is still how you reach the safe reader.

A draft prints as explicitly unreadable, because it is unvalidated working state and a form
implying it round-trips would be claiming something untrue:

```clojure
(pr-str (rec/draft heist))   ;=> "#<Heist draft {…}>"
```

## Running it

```
clojure -X:test                                        # the suite
clojure -M:dev -e "(require 'recognitum.demo)(recognitum.demo/run)"
clojure -M:bench -e "(require 'recognitum.bench)(recognitum.bench/run)"
```

The demo is the fastest way in: `dev/recognitum/heist.clj` is the worked example above, and
`recognitum.demo/run` prints every facility in this document against it, including the fetches
— `dev/recognitum/roster.clj` is an in-memory table that counts its calls, so the lines about
one fetch per validation call and about the gate are showing what happened rather than asserting
it.

## What it costs

Against a plain `defrecord` carrying the same checks written by hand, so the validation work is
identical on both sides (`bench/recognitum/bench.clj`). Criterium `quick-benchmark` on an AMD
Ryzen 9 7950X, OpenJDK 26.0.2, Linux 6.18.43, 2026-09-16:

| | hand-rolled | this library |
|---|---|---|
| read a field | 2.2 ns | 2.3 ns |
| write one field, no cross-field check on it | 16.5 ns | 10.4 ns |
| write one field, two cross-field checks on it | 203 ns | 172 ns |
| `merge` two fields | 814 ns | 1251 ns |
| construct | 1358 ns | 1532 ns |
| one rename in a `drafting` | 237 ns | 838 ns |
| 15 renames in a `drafting` | 700 ns | 1294 ns |
| one add-member (3 writes) in a `drafting` | 1384 ns | 1655 ns |
| 15 add-members in a `drafting` | 4394 ns | 5007 ns |

A write to a sealed value costs less than the hand-rolled twin, which is the arglists paying
for themselves: the checks depending on the field are known when the type is compiled, and each
one is called with the fields it named handed to it directly rather than looked up out of the
value.

A `drafting` costs a fixed few hundred nanoseconds beyond the checks it runs — a draft opened, a
seal performed, a verdict mask consulted — which is the whole of the gap at one write and is
most of the way amortized by fifteen. Where a draft is opened and written to with nothing that
invalidates anything, the seal runs nothing and costs nothing beyond that fixed part.

`merge` is the row the library loses, and loses honestly: it refuses an undeclared key, builds
one intermediate map for the whole write so that the checks run once at the end rather than per
entry, and constructs the new value through it.

Two caveats. Run-to-run spread is real — a few percent between consecutive runs on an idle
machine, more on a busy one — so do not trust either column to three figures. And a table is
only ever as current as its last run:

```
clojure -M:bench -e "(require 'recognitum.bench)(recognitum.bench/run)"
```

What does not depend on re-running it is the shape of the comparison. `recognitum.cost-test`
holds three of these rows to a multiple of the hand-rolled side, so a change that makes a
validation call pay a large fixed cost — which is what the `drafting` rows read before this
table was last re-measured — fails the suite rather than waiting to be noticed here.
