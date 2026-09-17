# TODOs

The open queue. Closed items are gone; the rulings behind them are in the commit messages.

## Implementation sequence

Each step leaves the suite green and is scoped to be done on its own, tests first.

### Waiting on a go

- [ ] **I8. A primitive-hinted `definterface` method breaks the `loop` emission.** With
      `(definterface ITally (tally [^long n]))`, `deftype` compiles it and `rec/defrecord` does
      not: `recurrable-body` interleaves `(rest args)` verbatim, so the `^long` rides into the
      binding position and the compiler refuses "Can't type hint a local with a primitive
      initializer". Fix: strip metadata from the binding names only —
      `(interleave (map #(with-meta % nil) (rest args)) (rest args))`. Verified by the I7 agent in
      a throwaway patch: compiles, runs on both types, `recur` works. Test first with a
      `definterface` carrying `^long`, on both types, with and without `recur`. Do not edit
      TODOs.md; report.

### H — later, low priority

- [ ] **H2. Raise or name the limits.** The verdict mask is one `long`, so a type may declare
      at most 64 checks (`::too-many-checks` at macroexpansion, which names it). Fields are
      capped at 20 by Clojure's parameter limit on the generated `->Type`, with Clojure's own
      message "Can't specify more than 20 params", which does not name fields. Either widen
      the mask (two longs, or a `BitSet`) and give `->Type` a map-taking fallback past 20, or
      at least refuse a 21st field in the macro with a message that says so. Galdre: fix
      later.

- [ ] **H1. `recognitum.walk`.** `clojure.walk/walk` is a plain `cond`: on `IRecord` it does
      `(reduce conj form form)`, one write per entry, with no protocol or multimethod to
      intercept. So `postwalk` over an `:io-limited` sealed value throws `::io-limited`, and
      over any sealed value it runs checks per entry. Provide a namespace identical to
      `clojure.walk` in every way except that a sealed recognitum value is walked as a draft —
      opened, rebuilt, sealed once at the end — and a draft is walked as itself. Same public
      names (`walk`, `prewalk`, `postwalk`, `keywordize-keys`, `stringify-keys`,
      `prewalk-replace`, `postwalk-replace`, `macroexpand-all`), so a consumer swaps the require
      and nothing else. Nothing needs it yet.

## For discussion

- [ ] **`run-checks!` per-call overhead** (perf, later). Reported by the merge-fix agent: per
      validation it builds three phase vectors, a failure set, a lazy `remove` over the pure
      phase, and a `some` per check in the gate test — a fixed cost on every sealed write, not
      just `merge`. Alongside the existing note that `equiv`/`equals`/`hashCode` build a map per
      call. Galdre: perf work is later.
