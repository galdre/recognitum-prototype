# recognitum

A record-like type that carries proof of its own validation.

`defrecord` defines a type from a vector of fields and a map of named **checks**, functions over
the fields that say what a valid value is, and generates two types from it: a **sealed value**,
which every check declared on the type has passed, and a **draft**, an unvalidated working copy
of one that records which of its checks currently pass. A bare map with the right keys is not a
`Heist`, `assoc` of a value that fails a check does not produce one, and no constructor skips
the checks.

## The baseline is `defrecord`

This is `clojure.core/defrecord` with a set of deliberate changes. What is not listed below is
what `defrecord` does: the generated types are records, so `record?` is true of them, they carry
`java.io.Serializable`, equality compares the class before it compares an entry, `empty`
refuses, and `clojure.walk` finds an `IRecord`.

The deliberate changes:

- Two types are generated rather than one: a sealed value and a draft.
- Checks run on every write.
- `dissoc` of a declared field is refused.
- An undeclared key is refused on a sealed value.
- Field keys are qualified by the namespace the type is defined in.
- The print tag is namespace-qualified.
- The library's keys are merged over `meta`.
- A value is callable as a function of its keys, as a map is.
- A protocol method marked `^:drafting` is handed a draft of the receiver on the sealed
  type, and what it returns is sealed.
- A definition may not implement `clojure.lang.IFn` at one or two arguments; those calls
  are the map's.

## Defining a type

`defrecord` shadows `clojure.core/defrecord`, so require the library under an alias rather than
referring it:

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

    ;; Neither of these is named after a field, so each is an invariant validation: it waits
    ;; on the shape validations of the fields it names, and restates neither of them.
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
  ^:drafting
  (rename       [h nm] (assoc h ::name nm))
  (total-payout [h]    (reduce + 0 (vals (::cuts h)))))
```

Three positions — the type name, the fields, and a definition map — followed by protocol
implementations. Every section of the definition map is optional; a key that names no section is
a compile error.

## Fields

A vector of symbols, as `defrecord` takes. Each becomes a field of the generated type, so reads
and writes are field access rather than map lookups, and a key qualified by the namespace the
type is defined in. A `Heist` written in `my.app.heist` answers to `:my.app.heist/name`, which is
`::name` inside that file and `::h/name` anywhere that aliases it. A bare `:name` is a key the
type does not declare, so two types that share a field name share nothing else.

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

The arglist is the dependency declaration. It holds two kinds of entry: a symbol, naming a field
of this type, and a keyword, naming an **io-dep** of this type, a function declared under
`:io-deps` that fetches something from outside the value. The body receives exactly those,
positionally, in that order, and nothing else is in scope. Exactly one arity: the arity is the
dependency set.

Every entry is resolved while the macro runs. A symbol that is not a declared field, a keyword
that is not a declared io-dep, a name used for both a check and an io-dep, or a cycle among
io-deps is a compile error naming the offender.

There are three kinds of check. A **shape validation** states the shape of a single field, what
a well-formed value of it is on its own: `::cuts` says what a well-formed `cuts` is. It is named
after the field and reads only that field. An **invariant validation** states a relational
invariant across the fields: `::cuts-sum-to-take` is a relationship between `take` and `cuts`
that holds of every valid `Heist`. An **external validation** states a relational invariant
between the value and an external resource, read through an io-dep: `::crew-is-vouched-for` is
a relationship between this `Heist` and what the house has on file, true as of when it ran.

### Shape validations

A check named after a field is that field's shape validation. `::name`, whether written as the
shorthand or as `::name ([name] …)`, is the check that says what a well-formed `name` is. A shape
validation names exactly its own field, or the form does not compile.

Every other check waits on the shape validations of the fields it names. `::cuts-sum-to-take
[take cuts]` runs only once `::take` and `::cuts` have passed, so it receives well-formed input.
A check does not run while a shape validation it waits on is failing, and is not reported; the
shape validation's failure is what is reported. Failures come back shape validations first, then
invariant validations, then external validations, in declaration order within each.

A field with no shape validation has no declared shape. The checks naming it run against
whatever it holds and have to be total over anything.

### The contract

- Return `true` — exactly `true` — when valid.
- Return anything else to fail. What was returned is the failure data, and is reported.
- Exceptions are not caught. A throw is a bug in the check rather than a claim about the data.

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

`assoc` runs the checks that read the field, directly or through an io-dep:

```clojure
(assoc heist ::h/name "The Sequel")  ;=> #my.app.heist/Heist{…}
(assoc heist ::h/take "lots")        ;=> throws :check-failed
(assoc heist :getaway-car "mini")    ;=> throws :unknown-field
(assoc heist :take 4000)             ;=> throws :unknown-field — bare, so undeclared
```

`merge` applies every entry and then runs the checks reading any of them once, so a multi-key
update is atomic:

```clojure
;; fine — take and cuts change together
(merge heist {::h/take 4000 ::h/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})

;; throws :check-failed — the cuts no longer add up
(assoc heist ::h/take 4000)
```

`dissoc` of a declared field throws. Where a field's checks admit `nil`,
`(assoc heist ::h/cuts nil)` spells absence, and the error says so.

Undeclared keys are ordinary: `dissoc` removes one from a draft, and on a sealed value, which
cannot hold one, it is the usual no-op. `clean` strips them in bulk.

## Multi-step updates: `drafting`

A single `assoc` cannot express a change that breaks a check halfway through. **`drafting`**
opens a draft, runs the body against it, and **seals** the result: validates it and returns the
sealed value.

```clojure
(defn update-the-thing [heist]
  (drafting [h heist]
    (-> h impl/do-a-thing impl/do-another)))
```

Inside the draft, writes check nothing. If a check fails at the seal, `drafting` throws and
nothing is returned.

```clojure
(drafting [h heist]
  (reduce apply-changeset h changesets))   ; 500 writes, one round of checks
```

The second position of the binding takes a sealed value, a draft, or the type itself, to build
one from nothing. `seal` is idempotent, so a body may end in a call that has already sealed.

`drafting` is re-entrant. A draft an enclosing form owns passes through unsealed, so only the
outermost form seals, and a function taking and returning a draft can be called standalone or
nested.

## Protocol implementations

Trailing forms in the definition are protocol implementations, written once and installed on
both generated types. On the draft type the body runs as written.

On the sealed type it depends on what the method returns. A method that returns a modified
receiver is a **drafting method**, and says so with `:drafting` metadata, written either on
the method form or on the method name:

```clojure
^:drafting (rename [h nm] (assoc h ::name nm))
(^:drafting rename [h nm] (assoc h ::name nm))
```

A drafting method is wrapped: it opens a draft of the receiver, runs the body against that,
and seals the result, so it cannot produce an invalid value whatever it does in between.

An unmarked method receives the sealed value and its body runs as written. A write it makes
is an ordinary write to a sealed value, so it runs the checks reading the field written and
throws `:check-failed` where one of them does not pass, and throws `:io-limited` where the
field is one the option covers. What the mark buys is what `drafting` buys: many writes under
one round of checks, and a state in between that no single write could reach.

```clojure
(h/rename heist "The Sequel")                       ;=> a sealed Heist
(drafting [x heist] (h/rename x "The Sequel"))      ;   the body ran on the draft, unsealed
(h/total-payout heist)                              ;=> 3000 — unmarked, and not a Heist anyway
```

Only a draft of this type is sealed on the way out. A number, a string, a boolean, an
already-sealed value, a draft of another type, all pass through untouched.

In a drafting method the receiver is a draft; otherwise it is whichever form the method was
called on. So `(instance? Heist h)` inside a body answers about the form it was handed, and
`(rec/of-type? Heist h)` is the question that was meant.

`recur` works in a method body exactly as it does on a `defrecord`: it targets the body itself,
with the receiver excluded, so `(tally [h n] (if (pos? n) (recur (dec n)) h))` means here what
it would mean there.

**`unsealed`** marks a draft a method returns as one that is not to be sealed, as `reduced`
marks a value that ends a reduction:

```clojure
^:drafting
(begin-revision [h] (rec/unsealed (assoc h ::name "draft title")))
```

A drafting method returning `(unsealed d)` returns `d` itself, owned by no `drafting`, so a
later one adopts it. On the draft type the marker is unwrapped and nothing else changes, so a
body means the same thing on either type. `(unsealed x)` throws where `x` is not a draft, and
the method throws where it is a draft of another type. In an unmarked method on the sealed
type the write above produces a sealed value, so `unsealed` of it throws `:not-a-draft`; a
method that uses it is a drafting method and carries the mark. `drafting` and `seal` unwrap
the marker and seal the draft inside it.

## Telling values apart

`defrecord` generates two types, so `instance?` holds of a sealed `Heist` and not of a draft of
it. `of-type?` tests the type; `draft?` and `sealed?` test the form:

| | |
|---|---|
| `(rec/of-type? Heist x)` | either form of `Heist` |
| `(instance? Heist x)` | a sealed `Heist`, specifically |
| `(rec/draft? x)` | any draft, of any type |
| `(rec/sealed? x)` | any sealed value, of any type |

`(and (draft? x) (of-type? Heist x))` is a draft of `Heist`. `of-type?` is false for anything
that is not a value of a registered type, the type object included.

## What a draft records

A draft records which of its checks currently pass. The record is mutable: the draft's data is
immutable, and which checks have been run against that data is not part of the data.

- A draft of a sealed value starts with every check passing.
- A draft of the type starts with none.
- Running a check records it as passing when it passes, and clears the record when it fails.
- A write clears exactly the checks that read the field written, directly or through an io-dep.

A write returns a new draft whose record is a copy of this one's, so what is run against one
says nothing about the other.

Checks can be run against a draft before it is complete:

| | returns |
|---|---|
| `(rec/check x ::name)` | `true`, or `{check-name failure-data}` for those that failed |
| `(rec/check! x ::name)` | `true`, or throws |
| `(rec/recheck x ::name)` | the same, running everything named whether or not it is recorded as passing |
| `(rec/recheck! x ::name)` | `true`, or throws |
| `(rec/passed x)` | the set of check names currently passing |
| `(rec/certificates x)` | the set of certificate names currently held |

`::name` is a check's name or a **certificate**'s, a name the type gives to a subset of its
checks. A lone check is a certificate containing itself. The answer is keyed by check name, so a
`false` a check returned is not mistaken for a report, and a check that did not run because one
it waits on is failing is absent from it; the failing check's entry is what says why.

None of them return a draft. Each result is written into the draft's record, which `passed` and
`certificates` read.

Running a check first runs whatever it waits on that is not already recorded as passing, so a
check the certificate does not name can appear among the failures; a prerequisite already
recorded as passing does not run again. A failure is not recorded: the call that finds one
returns it, and a later call runs the failing check again. Every one of these runs everything it
was asked to run and reports every failure; none stops at the first.

A field the draft has never been written to reads as `nil`, so a check naming one judges `nil`.

All six answer for a sealed value too. A sealed value passes every check, so `check` returns
`true` there without running anything, `passed` names every check the type declares and
`certificates` every certificate it has.

### Re-running on purpose

`recheck` runs what it names whether or not the draft already records it as passing. An
external validation is true as of when it ran; `recheck` is how to run it again.

On a draft the result is written back: a check that passed before and fails now is no longer
recorded as passing, and the next `seal` runs it again. A sealed value has no record to write
and cannot change, so `recheck` reports the current result and `passed` still names every check.

### Metadata

`meta` returns a value's own metadata with the library's keys merged over the top. Drafts and
sealed values answer the same way:

```clojure
(meta d)
;=> {:user-key 1
;    :recognitum.core/passed       #{::name ::crew ::cuts-sum-to-take}
;    :recognitum.core/certificates #{::shape}}
```

On a sealed value every check passed and every certificate is held, so those keys are constant.
The library's keys are merged over the caller's, so a key set through `with-meta` cannot
override one the library writes.

## Certificates

`:certificates` maps a certificate name to the checks it covers, so a group of them can be run
against a draft, under one name, before the value is complete:

```clojure
(rec/check d ::h/shape)       ;=> true
(rec/certificates d)          ;=> #{:my.app.heist/shape}
```

A draft holds a certificate when every check in it is currently passing. There is no other way
to hold one.

A built-in certificate covering every check on the type exists without being declared. It is
called `:recognitum.core/every-check` (the var `rec/every-check` holds it), and is the one
certificate name a type may not declare for itself.

A certificate changes nothing about what sealing requires: `seal` runs every check on the type,
whether or not it belongs to one. A sealed value holds every certificate its type declares, the
built-in included.

## IO dependencies

`:io-deps` maps an io-dep name to a fetch — a database row, an upstream response. A fetch is
written exactly as a check is, arglist and all, so an io-dep may name another; fetches run in
dependency order, and a cycle among them is a compile error naming the ring. A check is an
external validation exactly when its arglist holds a keyword.

A fetch does not decide validity. It produces a value, and the checks naming it decide what that
value means; `nil` is neither a failure nor a success. An exception from a fetch propagates
rather than becoming a failure; an author who wants it treated as a failure catches it inside
the fetch.

A fetch may be written singular or batched:

```clojure
::roster ([crew] (fetch-roster crew))               ; singular
::roster ([[crew] :as rows] (fetch-rosters rows))   ; batched
```

The inner vector of the batched form is the dependency declaration, as the singular arglist is,
so what a check waits on, the fetch order and the field → checks index come out the same; what
differs is the call. A batched fetch is called with the collection of argument tuples and returns
a map keyed by them, `{[crew] result}`, and distinct tuples are fetched once. Checks are never
batched; a check judges one value.

Nothing is fetched until every pure check over the fields the fetch reads has passed:

- Pure checks run before any fetch.
- An io-dep is fetched once every pure check over the fields it names has passed, and every
  io-dep it names has been fetched.
- An external validation runs once every io-dep it names has been fetched.

An io-dep declares its own fields, so a malformed field no fetch reads holds up no fetch: a
fetch over `crew` waits on the pure checks over `crew`, and a fetch that names another io-dep
waits on that io-dep's fields as well.

Within a single `check`, `recheck` or `seal`, an io-dep is fetched at most once however many
checks name it, and what it returned is not kept after the call returns. A draft carries no IO
state; what it carries is which checks passed.

### Checking many at once

`check-all` takes a collection of values and runs what it names against all of them, so a
batched fetch sees every argument tuple in one call and distinct tuples are fetched once however
many values share them:

```clojure
(rec/check-all drafts rec/every-check)   ;=> [true true {::h/cuts-sum-to-take false} …]
```

Results are index-aligned with the input, since two drafts holding identical data are `=`.
`check-all!` and `recheck-all!` collect every failure across every value and throw once with all
of them, keyed by index.

The collection may hold more than one type, and sealed values alongside drafts. Fetches are
declared per type, so a mixed group is one fetch pass per type, batched within each; the name
resolves per type, and a type that does not declare it fails the whole call before anything
runs. A sealed value passes every check and answers `true`.

`recheck-all` and `recheck-all!` run everything named whether or not a value records it as
passing, so re-verifying a batch against a database is one fetch pass per type.

No group call produces a sealed value. Each draft's record is written with its results, so
sealing one afterwards finds its checks already passing and fetches nothing.

### Draft-only writes

```clojure
{:options {:io-limited true}}
```

makes direct `assoc` and `merge` on a sealed value throw for any field an external validation
reads, its own fields and its fetches' alike, so those writes go through `drafting`, where N
writes cost one round of checks at the seal instead of N.

```clojure
(assoc heist ::h/crew #{:charlie})              ;=> throws :io-limited
(drafting [x heist] (assoc x ::h/crew #{:charlie}))   ; how that write is spelled
(assoc heist ::h/name "The Lavender Hill Mob")  ;=> fine — no fetch reads ::h/name
```

Nothing is fetched and no check runs before the throw. Fields no external validation reads are
unaffected; the set is computed from the arglists.

The option takes `true`, `false`, or a symbol naming a var holding one, read when the type is
compiled, so a later `alter-var-root` leaves the types already compiled against it alone.

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

Both run every check. `map->Heist` also rejects a key the type does not declare; a key it
declares and the map omits is `nil`, as it would be on a `defrecord`, and that field's shape
validation says whether `nil` is acceptable.

`map->Heist` takes either spelling: a bare keyword naming a declared field is qualified into the
type's namespace, and anything else is taken as written, so a stray `:getaway-car` is reported
as `:getaway-car`. A printed literal, whose tag carries the namespace the printed map drops,
reads back through it.

There is no unvalidated constructor. The positional factory `deftype` would otherwise intern is
replaced, and the draft type's is unmapped.

Where the fields are not all in hand at once, open a draft of the type:

```clojure
(drafting [h Heist]
  (reduce apply-fragment h fragments))
```

A field never written is `nil` at the seal, and its shape validation decides whether that is
acceptable. A field with no shape validation seals holding it.

## Undeclared keys

A draft accepts any key; the seal decides. By default an undeclared key is an error:

```clojure
(drafting [h heist]
  (assoc h :getaway-car "mini"))   ;=> throws :extraneous-keys
```

`clean` strips them instead. It is an ordinary draft → draft step, so it goes anywhere in the
pipeline:

```clojure
(drafting [h heist]
  (-> h (assoc :getaway-car "mini") rec/clean))   ;=> #my.app.heist/Heist{…}
```

To drop one undeclared key rather than all of them, `dissoc` it.

## Errors

Everything this library refuses at run time throws an `ex-info` whose data carries `:error`, the
keyword to dispatch on, and, wherever a type is involved, `:record`, that type's name. The keys
are qualified: `:recognitum.core/check-failed` is what to match on, and `:check-failed` matches
nothing.

A `:check-failed` carries every failure: each throwing entry point runs everything it was asked
to run before it throws. They arrive under `:failures`, keyed by check name, shape validations
first, then invariant validations, then external validations, in declaration order within each.
Those are the same keys, in the same order, that `check` answers with when it does not answer
`true`; a throw adds a report under each one, carrying the check's docstring, the dependencies
it declared with what it was given for each, and what it returned:

```clojure
{:error    :recognitum.core/check-failed
 :record   Heist
 :failures {:my.app.heist/only-crew-get-cuts
            {:doc      "only crew get a cut"
             :values   #:my.app.heist{:crew #{:charlie :lyle :left-ear}
                                      :cuts {:charlie 1000 :stranger 100}}
             :returned false}}}
```

A caller dispatches on the keyword and reads the rest of the map:

```clojure
(try
  (rec/seal d)
  (catch clojure.lang.ExceptionInfo e
    (case (:error (ex-data e))
      :recognitum.core/check-failed    (report (:failures (ex-data e)))
      :recognitum.core/extraneous-keys (rec/seal (rec/clean d))
      (throw e))))
```

Which errors an operation can throw is in that operation's docstring, with the conditions that
produce each and the keys it carries: `(doc rec/seal)`.

## Printing and reading

A sealed value prints with a namespace-qualified tag, and the printed map drops the namespace
that tag carries:

```clojure
(pr-str heist)
;=> "#my.app.heist/Heist{:crew #{…}, :cuts {…}, :name \"The Italian Job\", :take 3000}"
```

The reader accepts both spellings, bare keys, which it qualifies from the tag, and fully
qualified keys, which it takes as given, and both go through the validating constructor.

That tag is the key `clojure.edn` takes, and `data-readers` supplies the map:

```clojure
(edn/read-string {:readers (rec/data-readers)} s)
```

Reading is validating: the reader is `map->Heist`, so a literal whose contents fail a check
throws at read time.

The library registers no tag globally. `clojure.edn` takes `:readers` and consults nothing else.
To make a tag available to plain `read` and `read-string`, put a `data_readers.clj` at the root
of the classpath — `src/data_readers.clj`, or `dev/data_readers.clj` for a fixture — mapping the
tag to the validating constructor:

```clojure
{my.app.heist/Heist my.app.heist/map->Heist}
```

Clojure merges every such file into `*data-readers*` at startup. The entry is a placeholder
until the namespace is loaded, so reading the tag before requiring `my.app.heist` throws
`Attempting to call unbound fn`; and `clojure.edn` ignores these files, so `:readers` is still
how it reaches the tag.

A draft prints as unreadable, because it is unvalidated working state:

```clojure
(pr-str (rec/draft heist))   ;=> "#<Heist draft {…}>"
```

## Running it

```
clojure -X:test                                        # the suite
clojure -M:dev -e "(require 'recognitum.demo)(recognitum.demo/run)"
clojure -M:bench -e "(require 'recognitum.bench)(recognitum.bench/run)"
```

`dev/recognitum/heist.clj` is the worked example above, and `recognitum.demo/run` prints every
facility in this document against it, fetches included: `dev/recognitum/roster.clj` is an
in-memory table that counts its calls, so the lines about one fetch per validation call and
about what a fetch waits on show what happened.

## What it costs

Against a plain `defrecord` carrying the same checks written by hand, so the validation work is
identical on both sides (`bench/recognitum/bench.clj`). Criterium `quick-benchmark` on an AMD
Ryzen 9 7950X, OpenJDK 26.0.2, Linux 6.18.43, 2026-09-17:

| | hand-rolled | this library |
|---|---|---|
| read a field | 2.1 ns | 2.3 ns |
| write one field, no invariant validation on it | 13.2 ns | 14.8 ns |
| write one field, two invariant validations on it | 217 ns | 175 ns |
| `merge` two fields | 860 ns | 430 ns |
| construct | 1418 ns | 870 ns |
| read method on a sealed value | 29.3 ns | 29.5 ns |
| write method on a sealed value | 15.5 ns | 11.9 ns |
| one rename in a `drafting` | 257 ns | 211 ns |
| 15 renames in a `drafting` | 766 ns | 652 ns |
| one add-member (3 writes) in a `drafting` | 1450 ns | 982 ns |
| 15 add-members in a `drafting` | 4983 ns | 4309 ns |

The `drafting` rows carry a fixed cost of about a hundred nanoseconds beyond the checks they
run — a draft opened, a seal performed, the draft's record of which checks pass consulted — and
still come in under the hand-rolled side at one write. So does a write to a sealed value with
checks on the field: the checks reading a field are known when the type is compiled, and each is
called with the fields it named directly rather than looking them up out of the value, which is
what the hand-rolled twin has to do. The rows under 20 ns are noise; both sides are a dozen
nanoseconds there and run-to-run spread swamps the gap. Do not trust either column to three
figures.

To re-measure:

```
clojure -M:bench -e "(require 'recognitum.bench)(recognitum.bench/run)"
```

`recognitum.cost-test` holds most of these rows to a multiple of the hand-rolled side, and two
more — sealing a draft with one check left to run, and a group call against the same values one
at a time — to a multiple of the library's own cheapest path to the same work, so a change that
makes a validation call pay a large fixed cost fails the suite.
