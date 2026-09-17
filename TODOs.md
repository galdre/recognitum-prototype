# TODOs

The open queue. Closed items are gone; the rulings behind them are in the commit messages.

## Implementation sequence

Each step leaves the suite green and is scoped to be done on its own, tests first. Order is
the order listed. Every string a step writes is held to `PROSE.md`.

### Waiting on a go

- [ ] **I8. A primitive-hinted `definterface` method breaks the `loop` emission.** With
      `(definterface ITally (tally [^long n]))`, `deftype` compiles it and `rec/defrecord` does
      not: `recurrable-body` interleaves `(rest args)` verbatim, so the `^long` rides into the
      binding position and the compiler refuses "Can't type hint a local with a primitive
      initializer". Fix: strip metadata from the binding names only:
      `(interleave (map #(with-meta % nil) (rest args)) (rest args))`. Test first with a
      `definterface` carrying `^long`, on both types, with and without `recur`.

- [ ] **I10. The 20-field cap, by `defrecord`'s recipe.** Plain `defrecord` with 21 fields
      compiles: `build-positional-factory` takes the first 20 positionally and the rest as
      `& overage`, checks the count, and throws `ArityException` otherwise. Ours emits a flat
      arglist and fails with Clojure's "Can't specify more than 20 params", which does not name
      fields. Do what core does. Every other emission site constructs through `new`, which has
      no such limit. Tests: a 21-field and a 25-field type construct positionally and by map,
      round-trip through print and read, and `->Type` with the wrong count throws
      `ArityException`. The 64-check mask is a separate limit and stays in I12.

- [ ] **I11. Protocol methods are read-only by default; a method opts in to a draft receiver.**
      Ruled direction. Today every method installed on the sealed type opens a draft, pushes a
      thread binding, runs the body, pops, and seals, whether or not the body writes. A pure
      read such as `total-payout` pays all of that. New rule: on the sealed type an unmarked
      method receives the sealed value and its body runs as written, with no draft, no binding,
      no seal; a marked method gets today's behaviour. A body that writes without the mark
      still produces a valid value, because a write to a sealed value runs its checks, so the
      default is safe and only slower. Open points below under "Needs a ruling". Steps, each
      its own commit: (1) bench rows and a `cost-test` bound for a read-only method and a
      writing method on a sealed value against the hand-rolled twin, measured before the
      change; (2) the change; (3) re-measure and update the README table; (4) README protocol
      section and the `Heist` example marked accordingly.

- [ ] **I12. README and dev items from the walkthrough.** Small, batchable, one commit:
      - Delete `recognitum.core/check-id` and `owned-here?`: defined, never called.
        `ids-for` does `check-id`'s lookup inline with its own `:unknown-check` throw, and
        the `drafting` macro inlines `owned-here?`'s test. Reported by the prose-pass agents.
      - README re-entrancy section: one sentence that the ownership token is a dynamic
        binding, so a draft handed to a bare executor or thread pool has no owner there and a
        nested `drafting` on that thread seals it as its own. `future` conveys bindings; plain
        `ExecutorService` submission does not.
      - README writing section: state the asymmetry in one place. `assoc` refuses a bare key;
        `map->Type` and the reader qualify one. Cross-reference from both sections.
      - A dev-facing `deregister` (or `unregister!`) that drops a type's classes from the
        registries and its tag from the readers, for REPL redefinition. Not part of the
        published vocabulary; under `dev/` or marked so.

### Needs scoping before dispatch

- [ ] **I13. The 64-check mask.** A draft's verdict mask is one `long`, so a type declares at
      most 64 checks (`::too-many-checks`). A Metabase-width type with a shape validation per
      field exceeds it. Widening touches the emission: `all-pass`, `survives`, `shape-mask`,
      `gate-mask`, `:field-checks`, `held-in`, `passed-in`, `mask-indices`, `wants`,
      `plan-for`, and the mutable field itself. Options: two longs, a `long[]`, or a
      `java.util.BitSet`. Present real scope and a recommendation before an agent is
      dispatched; it is macro core.

### H — later

- [ ] **H1. `recognitum.walk`.** `clojure.walk/walk` is a plain `cond`: on `IRecord` it does
      `(reduce conj form form)`, one write per entry, with no protocol or multimethod to
      intercept. So `postwalk` over an `:io-limited` sealed value throws `::io-limited`, and
      over any sealed value it runs checks per entry. Provide a namespace identical to
      `clojure.walk` in every way except that a sealed recognitum value is walked as a draft,
      opened, rebuilt, sealed once at the end, and a draft is walked as itself. Same public
      names (`walk`, `prewalk`, `postwalk`, `keywordize-keys`, `stringify-keys`,
      `prewalk-replace`, `postwalk-replace`, `macroexpand-all`), so a consumer swaps the require
      and nothing else. Nothing needs it yet.

## Needs a ruling

- [ ] **Method-name reservation is wider than `defrecord`'s.** A protocol method is now
      refused when its name matches any of the 42 names the generated types carry, at any
      arity (12ba5d4). Plain `defrecord` refuses only an exact interface signature, so a
      same-name, different-arity method is legal there and not here. `values`, `remove`,
      `size`, `count`, `invoke`, and `seq` are the plausible domain names lost. Rule: keep
      by-name (simple, and named in the `defrecord` docstring), or narrow to name and arity
      to match the baseline. If by-name stays, the README's list of deliberate changes
      gains a line.

- [ ] **README facts that restate a docstring.** After the passes, the README's three io-dep
      fetch rules, its `unsealed` paragraph, and its constructor paragraph restate the
      `defrecord`, `unsealed`, and generated `map->` docstrings nearly word for word. The
      standard says each fact has one home. Rule whether the README keeps its own statement
      (it is the document a reader starts from) or defers to the docstring with a pointer.

- [ ] **I11 marker spelling.** Metadata on the method name, `(^:drafting add-member [h who cut] ...)`,
      is the recommendation: it names what the wrapper does (opens a draft) rather than what
      the body does (writes), and a read-only-marked body that writes is still correct. The
      alternative is a `:drafting-methods #{add-member ...}` section in the definition map,
      which keeps the protocol forms identical to `defrecord`'s.
- [ ] **I11 and `unsealed` in an unmarked method.** On the sealed type there is no draft to
      return, so `(unsealed (assoc h ...))` throws `::not-a-draft` at run time. Recommendation:
      accept that, document it in the `unsealed` docstring and the README, and do not scan
      bodies for the symbol.
- [ ] **Renames of private functions**, parked until galdre has read the code. The
      standard's vocabulary section says names carry the vocabulary furthest and renaming
      is part of the cleanup. Candidates, with what each does: `wants` (a mask plus its
      prerequisites), `established` (the constant result for nothing pending), `answered`
      and `insisted` with `insisted-one` (the non-throwing and throwing result shapes,
      matching `check` and `check!`), `opted-out` and `unmark` (two unwrappings of an
      `unsealed` marker), `open` (the draft a `drafting` starts from). `adopt`, `held-in`,
      and `passed-in` read fine against the published vocabulary. Public names stay.

## For discussion

- [ ] **A test that reads the model map's keys.** The comments on `:checks` and `:io-deps`
      inside the `defrecord` macro are the only statement of the model's shape, and nothing
      tests them. Reported by the prose-pass agent as the prose most likely to rot.
- [ ] **Three places the prose-pass agent thought warranted an explanatory docstring** and
      wrote the short form instead: `seal-draft` (why running only the checks the mask does
      not claim equals running all of them; now a four-line body comment), `run-group` in the
      macro (why a check whose gate did not return `true` may stand as `true`; sound only
      because the failed gate is in the same group and about to throw; a test would pin it
      better than prose), and `internal` (why generated code dereferences a var). Rule keep
      short, expand, or pin with a test.

- [ ] **Per-call overhead** (perf, later). `run-checks!` builds three phase vectors, a failure
      set, a lazy `remove` over the pure phase, and a `some` per check in the gate test, on
      every sealed write. `equiv` builds two maps per `=`, `hasheq` an entry proxy per key, and
      `.equals`/`.hashCode` a map per call. Galdre: perf work is later.
