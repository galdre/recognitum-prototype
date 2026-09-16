# validated-record

A record-like type that carries proof of its own validation.

If you are holding a `Heist`, the validation ran. Not "someone probably called `validate!`
upstream" — the type cannot exist otherwise. A bare map that happens to have the right keys is
not a `Heist`, and `assoc`-ing garbage into a `Heist` does not produce a `Heist`.

## Defining a type

```clojure
(require '[prototype.validated-record :refer [defvalidated revising draft seal clean]])

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(defvalidated Heist
  [^{:valid non-blank-string?} name
   ^{:valid crew-set?}         crew
   ^{:valid non-neg-int?}      take
   ^{:valid cuts-map?}         cuts]

  [{:doc  "the cuts add up to the take"
    :keys #{:take :cuts}
    :pred (fn [h] (= (:take h) (reduce + 0 (vals (:cuts h)))))}
   {:doc  "only crew get a cut"
    :keys #{:crew :cuts}
    :pred (fn [h] (every? (:crew h) (keys (:cuts h))))}])
```

Two kinds of check, and the difference matters:

- **Field predicates** (`^{:valid …}`) look at one value. They run on every write to that field
  of a validated value, and at `seal` for every field a draft touched.
- **Invariants** look at several fields at once. Each declares the keys it depends on, and runs
  whenever any of them changes.

Declare an invariant's `:keys` accurately. It is how the library knows when to re-check it, what
the error message reports, and — because an invariant none of whose declared keys were touched is
skipped — the assumption the whole guarantee rests on. See [Checking your `:keys`](#checking-your-keys).

## Reading

A validated value behaves like a map.

```clojure
(:take heist)                      ;=> 3000
(get heist :crew)                  ;=> #{:charlie :lyle :left-ear}
(let [{:keys [name take]} heist] …) ; destructuring works
(keys heist)                       ;=> (:name :crew :take :cuts)
(into {} heist)                    ;=> a plain map
```

## Writing

`assoc` validates the field and any invariant that depends on it:

```clojure
(assoc heist :name "The Sequel")     ;=> #Heist{…}
(assoc heist :take "lots")           ;=> throws :invalid-field
(assoc heist :getaway-car "mini")    ;=> throws :unknown-field
```

`merge` applies every entry and then checks invariants **once**, so a multi-key update is atomic:

```clojure
;; fine — take and cuts change together
(merge heist {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})

;; throws :invariant-violated — the cuts no longer add up
(assoc heist :take 4000)
```

`dissoc` of a declared field throws. A `Heist` without `:cuts` is not a `Heist`, and silently
degrading to a plain map is exactly the failure this type exists to prevent. Where a field's
predicate admits `nil`, `(assoc heist :cuts nil)` is how you spell absence — the error says so.

Undeclared keys are ordinary: `dissoc` removes one from a draft, and on a validated value — which
cannot be holding one — it is the usual no-op. `clean` is still how you strip them in bulk.

## Multi-step updates: `revising`

A single `assoc` can't express a change that breaks an invariant halfway through. `revising`
opens a **draft** — an unvalidated working copy — runs your body, and validates once at the end:

```clojure
(defn update-the-thing [heist]
  (revising [h heist]
    (-> h impl/do-a-thing impl/do-another)))
```

Inside the draft, writes are unchecked. Reaching the end of `revising` seals it: every field you
touched is validated, and every invariant those fields could break is re-run. If anything fails,
nothing is returned — you still have your original value, untouched.

```clojure
(revising [h heist]
  (reduce apply-changeset h changesets))   ; 500 writes, one validation
```

**`revising` is re-entrant.** Functions that take and return a draft can use `revising`
themselves and still be called from inside another one — only the outermost form seals. That
lets you write impl functions once and call them either way:

```clojure
(defn promote [h who bonus]
  (revising [x h]                 ; seals when called on a Heist
    (-> x                         ; passes through when called on a draft
        (assoc :cuts (update (:cuts x) who + bonus))
        (assoc :take (+ (:take x) bonus)))))
```

The convention that falls out: your public API takes and returns validated values, your impl
layer takes and returns drafts, and `revising` marks the boundary.

## Building a new value

Construction has the same problem as updating — the first field you set already violates the
invariants — so it uses the same tool. Pass the type instead of a value:

```clojure
(revising [h Heist]
  (merge h {:name "The Italian Job"
            :crew #{:charlie :lyle :left-ear}
            :take 3000
            :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}))
```

A field you never set is reported at seal as `:missing-field`. Note that `nil` is a legal value
if your predicate accepts it — "never written" and "written to nil" are different things.

## Undeclared keys

A draft accepts any key; sealing is where it is decided. By default an undeclared key is an
error:

```clojure
(revising [h heist]
  (assoc h :getaway-car "mini"))   ;=> throws :extraneous-keys
```

Call `clean` to strip them instead. It is an ordinary draft→draft step, so it goes wherever you
want in the pipeline:

```clojure
(revising [h heist]
  (-> h (assoc :getaway-car "mini") clean))   ;=> #Heist{…}
```

That gives you both policies — reject or strip — chosen per call site rather than baked into the
type. To drop one undeclared key rather than all of them, `dissoc` it.

## Errors

Everything throws `ex-info` with an `:error` key you can dispatch on:

| `:error` | when |
|---|---|
| `:invalid-field` | a value failed its field predicate |
| `:invariant-violated` | a cross-field invariant failed |
| `:unknown-field` | `assoc` of a key the type does not declare |
| `:extraneous-keys` | sealing a draft holding undeclared keys |
| `:missing-field` | sealing a value with a field never written |
| `:cannot-dissoc` | `dissoc` of a declared field — use `assoc … nil` if its predicate admits nil |
| `:undeclared-invariant-key` | under `*audit-invariant-keys*`, an invariant read a key it does not declare |

Invariant failures carry what you need to debug them:

```clojure
{:error      :invariant-violated
 :record     Heist
 :invariant  "only crew get a cut"
 :depends-on #{:crew :cuts}
 :values     {:crew #{:charlie :lyle :left-ear}
              :cuts {:charlie 1000 :lyle 1000 :left-ear 1000 :stranger 100}}}
```

## Checking your `:keys`

An invariant that reads a key it did not declare is skipped on the write that breaks it, and the
result seals anyway:

```clojure
(defvalidated Bad
  [^{:valid nb?} name ^{:valid int?} a ^{:valid int?} b]
  [{:doc "a matches b" :keys #{:a} :pred (fn [m] (= (:a m) (:b m)))}])  ; reads :b, declares only :a

(assoc ok :b 999)   ;=> #Bad{:a 1, :b 999, :name "n"}   — sealed, and wrong
```

Nothing declares `:b`, so nothing re-ran. Bind `*audit-invariant-keys*` to run every invariant
against a proxy that records its reads:

```clojure
(binding [v/*audit-invariant-keys* true]
  (revising [x Bad] (merge x {:name "n" :a 1 :b 1})))
;=> throws :undeclared-invariant-key
;   {:invariant "a matches b" :declared #{:a} :also-read #{:b}}
```

The audit only speaks for invariants that run, and a bad `:keys` is exactly what stops one from
running — so it cannot catch the mis-declaration on the write that exploits it. Construction
touches every field and so runs every invariant: **build one instance of each type under the
audit and every mis-declaration surfaces.** That is the thing to put in your test suite.

It is off by default and costs nothing when off. When on, each invariant run allocates a proxy
and reads go through it, so leave it out of production. A predicate that asks for the whole map
(`keys`, `vals`, `into {}`) cannot be attributed, and the audit abstains rather than guess.

## Running it

```
clojure -X:test                                        # the suite
clojure -M:dev -e "(require 'prototype.demo)(prototype.demo/run)"
clojure -M:bench -e "(require 'prototype.bench4)(prototype.bench4/run)"
```

## What it costs

Measured against a plain `defrecord` with the same predicates and invariants written by hand:

| | hand-rolled | this library |
|---|---|---|
| read a field | 4 ns | 5 ns |
| write, no invariants on the key | 17 ns | 33 ns |
| write, two invariants on the key | 302 ns | 362 ns |
| `merge` two keys | 865 ns | 578 ns |
| construct | 1215 ns | 1154 ns |
| 15 writes in one `revising` | 690 ns | 1231 ns |

Reads are effectively free. Where real validation happens the difference disappears, and `merge`
and construction come out ahead — the library only checks the invariants a change could actually
break, which is tedious enough to hand-roll that most code doesn't.

The worst case is a bare write to a field with nothing to validate: 16 ns of overhead. If you are
doing that in a tight loop, hold one draft open across the loop and seal once.
