# recognitum

Types whose values carry proof that their checks passed.

`rec/defproven` defines a type from its fields and its named **checks**. Every way to make a
value of the type runs the checks or throws, so holding a value is holding proof that they
passed: `(rec/sealed? x)` is the whole test, and `(instance? Heist x)` gives the same answer
until `Heist` is redefined. A value reads and compares as a map, and every write to it is
checked.

```clojure
(require '[recognitum.core :as rec])
```

## Install

Add the library to `deps.edn` as a git dependency:

```clojure
{:deps {io.github.galdre/recognitum-prototype {:git/tag "v2-defproven-2026-10-28"}}}
```

A git dependency also needs the sha of its commit. Run `clj -X:deps git-resolve-tags` once:
it finds the commit of each `:git/tag` in `deps.edn` and adds its `:git/sha`.

Report problems as issues at <https://github.com/galdre/recognitum-prototype/issues>.

## A heist

```clojure
(ns crew.heist
  (:require [recognitum.core :as rec] [crew.house :as house]))

(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (name    [] (non-blank? name))
  (crew    [] (crew-set? crew))
  (take    [] (non-neg-int? take))
  (cuts    [] (cuts-map? cuts))
  (getaway [] (string? getaway))

  (cuts-sum-to-take [] "the cuts add up to the take" (sums-to take cuts))
  (only-crew-get-cuts [cuts-sum-to-take] (every? crew (keys cuts)))
  (crew-vetted []
    (every? true? (vals (rec/fetch-each house/vetted crew))))
  (getaway-not-crew []
    (rec/when-extant getaway (not (contains? crew getaway)))))
```

This defines the class `crew.heist.Heist`, imported into the namespace, and two constructors:

```clojure
(map->Heist {::name "The Italian Job" ::crew #{"Charlie" "Stella"} ::take 2000
             ::cuts {"Charlie" 1000 "Stella" 1000}})
(->Heist "The Italian Job" #{"Charlie"} 1000 {"Charlie" 1000} "Handsome Rob")
```

Each field's key is a keyword in the defining namespace, named after the field: `::name` here.
A field marked `^:optional` may be absent; every other field must be present. `->Heist` takes
every field, and `nil` counts as present.

Optional means absent, never `nil`. An optional field that holds `nil` is present, and its
shape check runs on `nil`: `(string? getaway)` fails. A caller that means "no getaway", or
"no change" in an update, leaves the key out.

A runnable version of this example is in `dev/recognitum/heist.clj`, with a fake `house`
source. `clojure -M:dev -m recognitum.demo` runs it end to end and prints each step.

## Checks

A clause after `rec/checks` is `(name [waits] "doc"? body...)`.

- A check passes when its body returns exactly `true`. Anything else it returns is its
  failure data.
- Its id is a keyword in the defining namespace: `::cuts-sum-to-take`.
- A check named after a field is that field's **shape check**. The shape check of an optional
  field passes where the field is absent.
- A check waits on the shape check of each field its body reads, `extant?` included. The wait
  vector names any other checks that must pass first; naming a shape check there changes
  nothing. If a check it waits on fails, this check does not run and is reported as blocked.
  Above, `cuts-sum-to-take` reads `take` and `cuts`, so it waits on their shape checks and
  never adds up a take that is not a number.

### What a body sees

Inside a body the fields are locals. A macro used in a body sees them in `&env` as compiler
locals, with their type hints. As with any local, a `deftype` that a body defines does not
see them in its methods: a field named there is refused when the type is defined. Pass the
field to the deftype as one of its own fields, or use `reify`, whose methods close over it.

- A read of an absent field throws, which fails the check with `{:absent key}`, the field's key.
  `(rec/extant? f)` asks whether `f` is present, and `(rec/when-extant f body...)` is
  `(if (rec/extant? f) (do body...) true)`.
- `(rec/fetch source k)` reads one key from a **source**, and `(rec/fetch-each source ks)`
  reads several in one call and returns a map from key to answer. A source is a function from a
  vector of distinct keys to a map from key to answer; a key it leaves out reads as `nil`. A
  read from a source that throws fails the check with `{:fetch-failed source :cause e}`, where
  `e` is what the source threw.
- A body may read a dynamic var; it sees the binding in force where the check runs. A draft
  remembers a pass as it remembers any other, so a check that passed under one binding is not
  run again under another until a write or `rec/recheck` asks for it.
- A body that runs, runs once, from start to end, in each run of the checks; a check assumed
  as passing or blocked does not run. An effect in a body therefore happens at most once per
  run.
- A body never throws on purpose. The absent read and the failed read are ordinary exceptions
  that a body may catch; uncaught, they fail the check. Anything else a body throws is a bug in
  the check, not a claim about the data. It propagates out of the constructor, write, `seal`,
  `check`, `recheck` or `check-all` that ran the check, and that call returns nothing for any
  value.

A body reads a source and calls functions through their vars, so a test stubs either with
`with-redefs`:

```clojure
(with-redefs [house/vetted (fn [ks] (zipmap ks (repeat true)))]
  (map->Heist job-entries))
```

The four body macros work only inside a check body, in call position, and outside any function
the body builds (`fn`, `#(...)`, `for`, `lazy-seq`, `delay`). Anywhere else they fail to
compile. This keeps a read where the engine can see it: a check engine may batch the reads of
many checks and values into fewer calls.

## Building values

`map->Heist`, `->Heist` and the tagged literal run every check, or throw:

```clojure
(map->Heist {::name "Job" ::crew #{"Charlie"} ::take 10 ::cuts {"Charlie" 9}})
;; ExceptionInfo: crew.heist/Heist failed 1 check
;;   :crew.heist/cuts-sum-to-take (the cuts add up to the take): false,
;;   which blocked :crew.heist/only-crew-get-cuts
```

A value prints as a tagged literal, `#crew.heist/Heist {...}`. For `read` to accept it, add a
line to the project's `data_readers.cljc`:

```clojure
{crew.heist/Heist crew.heist/map->Heist}
```

For `clojure.edn`, pass `(rec/data-readers)` as `:readers`.

The class's own JVM constructor is not a way in: `(Heist. ...)` throws
`SecurityException "no key"`, since it takes a key that only the library holds. Call
`map->Heist` or `->Heist`.

`(rec/sealed? x)` asks whether `x` is a value of the current definition of its type, and
`(rec/draft? x)` whether it is a draft.

Evaluating `defproven` again defines a new class. Values of the old class keep their entries
and still read and print as before, but they are no longer sealed: `rec/sealed?` answers false,
and every write that changes one and every method of the type's own protocols and interfaces
throws, `toString` excepted:

```clojure
(assoc old-heist ::take 12)
;; ExceptionInfo: crew.heist/Heist was redefined: this value is of an earlier definition, so it
;; is not sealed and refuses writes and its type's methods. To prove it under the current
;; definition: (rec/seal (rec/draft v))
```

`rec/draft` of an old value gives a draft of the new definition holding its entries, and
`rec/seal` of that draft runs the new checks and returns a value of the new class. Reading an
old value's printed form does the same, since the tag reads with the new `map->Heist`. A write
that changes nothing may return the old value unchanged, still not sealed: `assoc` of the
identical value or `dissoc` of an absent key does, while `into` and `assoc` of an equal value
that is not identical throw.

A constructor held from before the redefinition, as `(def make map->Heist)` holds it, makes
nothing: it throws, naming the current constructor.

```clojure
(make {::name "Ocean" ...})
;; ExceptionInfo: crew.heist/Heist was redefined: this constructor is of an earlier definition
;; and makes nothing. Call the current crew.heist/map->Heist
```

Code compiled before the redefinition names the old class. There, `(instance? Heist x)` is true
of an old value, which is not sealed; `rec/sealed?` answers false. A sink that may outlive a
redefinition at the REPL asks `rec/sealed?`.

A definition that fails leaves nothing: where a check or a method does not compile, or a
protocol or an interface is refused, the name reaches no class and no constructor, and an
earlier definition under the name stays current, its values sealed and its methods answering.
The checks and the methods compile as one form, before anything is defined, so a definition
works the same at the top level and inside a `let` or a `when`, and under a tool that wraps
or expands each top-level form, as `clojure.walk/macroexpand-all` or a coverage tool such as
cloverage does.

## Writing values

`assoc`, `dissoc`, `conj`, `merge` and `into` on a value run the checks that read a key the
write changes, and return a new value or throw. A write of several keys at once is one write,
checked on the result, so its order never matters:

```clojure
(merge job {::take 3000 ::cuts {"Charlie" 1500 "Stella" 1500}})   ; checked once
(assoc job ::getaway "Charlie")                                    ; throws: getaway-not-crew
```

Every type is closed: a value refuses a key its type does not declare. `empty` throws, since no
type with a required field has an empty value.

## Drafts

A **draft** is an unchecked map of a type, for a value built in steps. It accepts any write,
including keys the type does not declare, and remembers which checks pass on its entries.
`rec/seal` checks it and returns the value:

```clojure
(defn plan-from-form [partial-plan]
  (-> (rec/draft Heist) (into partial-plan) (rec/seal)))
```

- `(rec/draft Heist)` holds nothing and passes nothing. `(rec/draft job)` holds the value's
  entries and passes every check. A draft of a draft is that draft.
- A write returns a new draft that forgets the checks reading a key it wrote.
- `rec/seal` runs only the checks the draft does not remember as passing. A failed seal
  still remembers what passed, so after the draft is mended the next seal runs only the rest.
  A draft that holds an undeclared key does not seal; `rec/clean` drops such keys.
- `(rec/check d sel)` runs the selected checks, and what they wait on, that `d` does not
  remember, and remembers the result. `(rec/recheck d sel)` runs the selected checks even if
  `d` remembers them; use it when a source's answer may have changed.

The memory cannot be forged: marks are tied to the entries they were set for and can be set
only by the type's own checked code.

There is no `seal-all`. To seal many drafts at once, check them in one pass and then seal each;
the seals run nothing that `rec/check-all` found passing:

```clojure
(defn seal-all [drafts]
  (let [answers (rec/check-all drafts rec/every)]
    (if (every? true? answers)
      (mapv rec/seal drafts)
      (throw (ex-info "not every draft seals" {:answers answers})))))
```

## Bundles

A **bundle** is a group of checks over named inputs, defined once and bound into many types.

```clojure
(ns app.perms
  (:require [recognitum.core :as rec] [app.db :as db]))

(rec/defconcern permission "Checks on the acting user's rights.")

(rec/defchecks collection-write [user collection-id]
  rec/checks
  (collection-exists []
    "the collection is on file"
    (some? (rec/fetch db/collections collection-id)))

  ^::permission
  (may-write [collection-exists]
    "the acting user may write to the collection"
    (db/can-write? user (rec/fetch db/collections collection-id))))

(rec/defcopy current-collection-write collection-write
  {collection-exists current-collection-exists
   may-write         current-may-write})
```

- `defchecks` defines the bundle var and one var per check: `perms/may-write`.
- `defconcern` defines a concern. `^::permission` on a clause carries it; a concern that no
  `defconcern` defines is refused.
- `defcopy` copies a bundle under new names. Its map names every check of the bundle exactly
  once. The copy's ids are in the copying namespace, and its checks keep their concerns.

A type binds a bundle with the bundle's var followed by one `(input [] expression)` clause per
input, each an expression of the type's fields:

```clojure
(rec/defproven Card [id name collection-id archived
                     ^:optional acting-user ^:optional stored-collection-id]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)

  ^{:when-extant stored-collection-id}
  perms/current-collection-write
  (user          [] acting-user)
  (collection-id [] stored-collection-id)

  rec/checks
  (name [] (non-blank? name))
  ^::perms/permission
  (moves-only-where-allowed [perms/may-write perms/current-may-write] true))
```

- An input clause runs only when a check that reads its input runs, once for each such
  check. A check its waits block runs none.
- An input whose clause reads an absent field is absent: here, a card with no acting user
  fails `may-write` with `{:absent :user}`, the input's key. A throw from a clause propagates
  as a throw from a body does.
- A bundle check reads the fields that the clauses of its inputs read, so it waits on their
  shape checks.
- `^{:when-extant f}` on a binding skips its checks, as passing, while the optional field `f`
  is absent. A later write of `f` runs them.
- In a bundle's body, `rec/extant?` and `rec/when-extant` take an input as a type's body takes
  a field. In an input clause they take a field, with the same meaning: so
  `(user [] (rec/when-extant acting-user acting-user))` gives `true` where the field is absent.
  To pass an absent field on as an absent input, read it: `(user [] acting-user)`.
- A wait names an own check by its bare name, a bundle check by its qualified var, or every
  check of a bundle by the bundle's qualified var: `perms/may-write`, or `app.card/positives`
  for a bundle `positives` defined in the type's own namespace. A bare `positives` is read as
  an own check, and refused.
- `^:override (perms/may-write [waits] body)` in `rec/checks` replaces that check of a bound
  bundle with a body over the type's fields. It keeps the check's id and concerns, and its
  binding's `:when-extant`.

## Protocols

The full form is:

```clojure
(rec/defproven T [fields...]
  rec/checks clauses...          ; the checks sections, in any order:
  bundle input-clauses...        ;   rec/checks and the bundle bindings
  Protocol (method [this args...] body...)...
  Interface (method [this args...] body...)...
  Object (toString [this] body...)
  ...)
```

After the checks sections come the type's own protocols and interfaces, mixed, written as in
`defrecord`: a protocol or an interface, then its methods. In a method body the fields are
locals, nil where an optional field is absent, and the first parameter is the value. `recur`
goes to the method's head without the value.

```clojure
(defprotocol Describe (describe [x]))

(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (name [] (non-blank? name))
  ...
  Describe
  (describe [_] (str name ", " (count crew) " crew" (when getaway (str ", driven by " getaway)))))
```

- Values of the type are instances of each interface and of each protocol's interface,
  written values included, so a protocol call and a direct call of the interface's method
  (`(.describe v)`) dispatch on the class. Drafts are not; the drafts of every type share one
  class, `recognitum.record.Draft`.
- Where an interface has two methods of one arity, type hints on the parameters choose one:
  `(tag [this ^String s] ...)`. A method that more than one method matches is refused, and so
  is a method the interface does not declare.
- Of `Object`, a type gives only `toString`. `str` of a value is its `toString` where the type
  gives one, else its printed form; printing always shows the tagged map. `equals` and
  `hashCode` are refused: a value's equality and hash are its map's.
- A protocol method that a map already has, such as `size` or `count` with no argument or
  `valAt` with one, is refused when `defproven` expands, as `defrecord` refuses it. The message
  names the two remedies: rename the method, or extend the protocol to the type with
  `extend-protocol` after the definition. A method extended that way is not guarded (below).
  A protocol that the type lists with such a method at another arity defines, and a call of
  the method at the map's arity throws `AbstractMethodError`. An interface's method that a map
  already has, such as `count` of `clojure.lang.Counted`, is refused too: leave it out, and the
  map's method answers.
- Interfaces that cannot be implemented together are refused when `defproven` expands: two
  that give default bodies of one method the type does not give, an interface that the
  proof's drafts implement (`java.util.function.Predicate` and `BiConsumer`), or one whose
  `print-method` entry is preferred to the map's. Every refusal of the proof is made then, at
  compile time; the proof asks again when the type is defined, and a refusal there, where an
  interface or a print entry changed in between, leaves nothing.
- A checks section after a protocol or an interface is refused: the checks come first.
- In a method body `Heist` is the value's class, `(class this)`, as a value: `(instance? Heist
  x)` works. It takes no type hint, since the class does not exist while the methods compile.
  A method body may not call `map->Heist` or `->Heist`, as with `defrecord`.
- A protocol may also be extended to the type from outside, with `extend-protocol` or
  `extend-type`, as to any class. Such methods are not the type's own: values of an earlier
  definition still answer them.
- From another namespace, a type hint or an `extend-protocol` names the class by its full
  name, `crew.heist.Heist`, or by the name `(:import [crew.heist Heist])` gives it. An alias
  does not work: `heist/Heist` names a var, not the class.

## Selecting and asking

A **selection** names some of a type's checks:

| Selection | Selects |
|---|---|
| `::cuts-sum-to-take` | that check |
| `perms/permission`, `::perms/permission` | the checks carrying the concern |
| `perms/collection-write`, `perms/may-write` | a bundle's checks, or one of them |
| `rec/every`, `rec/shape`, `rec/pure` | every check, every shape check, every check that reads no source |
| `(rec/fetching house/vetted)` | the checks that read that source |
| `(rec/reading ::crew)` | the checks that read that field, directly or through an input |
| `(rec/except s ...)`, `(rec/union s ...)` | what `s` selects less the rest; what any selects |

A keyword names a concern or a bundle check through the var `defconcern` or `defchecks` made
for it; any other keyword is a type's own check id. A keyword that names no such var and no
check of a type its namespace defines is refused when `rec/except` or `rec/union` builds the
selection. An own check id that a type lacks is refused when the selection meets that type;
the message gives the type and the id. Otherwise, a selection that names nothing in a type
selects nothing there: a concern no check of the type carries, or a bundle the type does not
bind.

- `(rec/check-all xs sel)` runs what `rec/check` would run on each value of `xs`, which may be
  of several types, in one pass of the engine, so values that read the same source can share
  its calls. Each draft remembers what passed and forgets what failed, as under `rec/check`,
  so a later seal of each runs only the rest. It returns one answer per value, in order.
- `(rec/proves? x sel)` is true for every value of a type's current definition; for a draft it
  asks what the draft remembers, including the shape checks of every field it must or does
  hold.
- `(rec/proven-checks x)`, `(rec/proven-concerns x)` and `(rec/checks-of sel x-or-class)`
  return check ids and concern ids.

A selected check pulls in what it waits on, even a check that `rec/except` removed. The report
then explains, under `:pulled-back`, which check waited on it.

## Failure reports

`rec/check`, `rec/recheck` and each answer of `rec/check-all` are `true` or a report:

```clojure
{:failed  {:crew.heist/take {:data false
                             :doc "the take is a whole number"
                             :blocked #{:crew.heist/cuts-sum-to-take
                                        :crew.heist/only-crew-get-cuts}}}
 :blocked #{:crew.heist/cuts-sum-to-take :crew.heist/only-crew-get-cuts}}
```

Under each failed check, `:blocked` holds the checks its failure kept from running, directly or
through another blocked check. A constructor, a write or a seal that fails throws `ex-info`
whose data is this report with `:error :recognitum/check-failed` and `:type`. The other errors
are `:recognitum/undeclared-keys` and `:recognitum/missing-fields`, each with the `:keys`.

A missing required field fails each check that reads it with `{:absent key}`, its shape check
included. `:recognitum/missing-fields` comes only when every check passes and no check read the
missing field.

## Editor support

The library exports a clj-kondo configuration under `resources/clj-kondo.exports`. It teaches
clj-kondo and clojure-lsp the definition forms, and reports most mistakes the macros refuse:
misspelled waits, concerns, inputs and fields, reads outside a check body or inside a function
the body builds, the names the check engine reserves, incomplete `defcopy` maps, a bundle bound
twice in one type, waits that form a cycle, and a checks section after a protocol. It lints
method bodies as clj-kondo lints a `defrecord`'s. It reports nothing in a definition the macros
accept, and may miss a mistake they refuse. The cycles it reports are those of the waits the
forms list and the gates the bindings name; it does not read check bodies, so a cycle through
a field a body reads is reported only by the macros, when the namespace loads. It sees one form
at a time, so a cycle through the waits of a bundle defined in another namespace is also left
to the macros.

## Coming

- `rec/defproven-in`: a type whose constructors check nothing, for values that pass only some
  checks at a time, sealed against a selection.
- `rec/defcontext`, a named selection, and `(rec/seal x selection)`.

## Running it

```
clojure -X:test                     # the suite, including every file of test/corpus
clojure -M:dev -m recognitum.demo   # the Heist, end to end
clojure -M:dev -m recognitum.authz  # refund requests checked against a permission service
clojure -M:dev -m recognitum.orders # orders: shapes, invariants, an address bundle bound twice
bb test/corpus/corpus_check.bb      # clj-kondo over the corpus, with the exported hook
```
