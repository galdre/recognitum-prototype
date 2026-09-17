# TODOs

The open queue. Closed items are gone; the rulings behind them are in the commit messages.

## Implementation sequence

Each step leaves the suite green and is scoped to be done on its own, tests first. Order is
the order listed. Every string a step writes is held to `PROSE.md`.

### Waiting on a go

- [ ] **I15. A value is not callable; a definition may implement `IFn` itself.** Ruled. A
      plain `defrecord` is not a function, and the template's map-style `invoke` (from
      potemkin's `PotemkinFn` in `AbstractRecord`) is a deviation nobody asked for that
      also blocks a definition from implementing `clojure.lang.IFn` at one or two
      arguments. Remove `PotemkinFn` and its `invoke` methods from the template; `invoke`
      then leaves the derived reserved set on its own. Delete the README's deliberate-change
      lines "A value is callable as a function of its keys, as a map is" and "A definition
      may not implement `clojure.lang.IFn` at one or two arguments". Tests first: a
      definition implementing `clojure.lang.IFn` with `invoke` at one and two arguments
      installs on both types and is called; a value is not `ifn?` and calling one throws
      `ClassCastException` as calling a `defrecord` does, twinned against a plain
      `defrecord`; any existing assertion that a value is callable as a map is inverted,
      and every such assertion is listed in the report. `applyTo` is the author's business,
      as on a `defrecord`.

- [ ] **I15. A value is not callable; a definition may implement `IFn` itself.** Ruled. A
      plain `defrecord` is not a function, and the template's map-style `invoke` (from
      potemkin's `PotemkinFn` in `AbstractRecord`) is a deviation nobody asked for that
      also blocks a definition from implementing `clojure.lang.IFn` at one or two
      arguments. Remove `PotemkinFn` and its `invoke` methods from the template; `invoke`
      then leaves the derived reserved set on its own. Delete the README's deliberate-change
      lines "A value is callable as a function of its keys, as a map is" and "A definition
      may not implement `clojure.lang.IFn` at one or two arguments". Tests first: a
      definition implementing `clojure.lang.IFn` with `invoke` at one and two arguments
      installs on both types and is called; a value is not `ifn?` and calling one throws
      `ClassCastException` as calling a `defrecord` does, twinned against a plain
      `defrecord`; any existing assertion that a value is callable as a map is inverted,
      and every such assertion is listed in the report. `applyTo` is the author's business,
      as on a `defrecord`.

- [ ] **I16. Untouched-seal short-circuit and `seal`'s duplicated lookups.** Two tiny
      follow-ons to the runner step. (1) In `seal-draft`, compare the mask to the all-pass
      constant before reading anything else off the model; an untouched seal is 60 ns and
      most of it is keyword lookups on the model map. (2) `seal`'s one-arity unwraps the
      marker and looks up the model, then calls the two-arity, which does both again.
      Tests first: a cost bound on the untouched seal against opening a draft.

- [ ] **I17. A failing shape validation stops the checks behind it, everywhere, and the
      report says so.** Ruled: when a field fails its shape validation, the invariant and
      external validations reading that field do not run, in every entry point and every
      pass, a forced `recheck` on a sealed value included. Today that pass reads
      prerequisites off every-check-passing and runs them anyway. (1) The runner gates on
      what the current pass found on every path; `recheck`'s docstring changes from
      "prerequisites are read off the mask" to the rule. (2) One report shape everywhere,
      ruled: `check`, `recheck`, `check-all`, and `recheck-all` answer `true` or the same
      map the throwing forms put under `:failures`, keyed by check name in declaration
      order, each entry `{:doc :values :returned}` and, on a shape validation that stopped
      checks, `:stopped`, the names of the checks that did not run because it failed, in
      declaration order; absent from an entry that stopped nothing. The thrown message names
      the stopped checks too. Where a caller wrote `(get (rec/check d ::cuts) ::cuts)` it
      now writes `(:returned (::cuts (rec/check d ::cuts)))`; the callers are the tests,
      the README, and the demo. Tests first: a forced `recheck!` on a sealed value whose
      shape validation fails runs no invariant validation over that field and fetches
      nothing; the one shape and `:stopped` from every entry point, answering and throwing,
      single and bulk; the README tables and docstrings of the six answering forms.

- [ ] **I18. A shorthand check takes its `:doc` from the predicate's var.** `::cuts
      cuts-map?` parses with `:doc nil` because only the long form carries a docstring. At
      expansion, resolve the shorthand's symbol and use the var's `:doc` when it has one, so
      a documented predicate documents its check and the report carries it. Tests first: a
      shorthand over a documented predicate reports that docstring; over an undocumented one,
      `nil`; the long form's own docstring wins. Small.

- [ ] **I12. README and dev items from the walkthrough.** Small, batchable, one commit:
      - Tighten the two method-row bounds in `cost_test`: the write bound is 110x, set to
        pass the pre-change wrapper, and now guards nothing at a measured 0.8x; the read
        bound is 12x at a measured 1.0x. Bring both to the style of the existing rows,
        loose but real, about 4x and 3x.
      - Re-run the whole README cost table from one run and set its date line to that
        day; the two method rows were measured 2026-09-17 and the other nine 2026-09-16.
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

- [ ] **H3. A primitive return type with a hinted parameter.** `(^long total [^long n])`
      implemented as `(total [h ^long n] (+ n 1))` fails with "Mismatched return type:
      expected long, had Object": the method wrapper makes the body's static type `Object`,
      and with hinted parameters the compiler checks it against the primitive return. Plain
      `deftype` accepts the same body because it is a primitive expression, so this is a
      deviation. The unhinted implementation compiles and unboxes at run time. Fix: resolve
      the interface at macro time, read the matched method's return type, and wrap the
      wrapper's result in the primitive cast. Narrow and low priority.


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

- [ ] **Renames of private functions**, parked until galdre has read the code. The
      standard's vocabulary section says names carry the vocabulary furthest and renaming
      is part of the cleanup. Candidates, with what each does: `wants` (a mask plus its
      prerequisites), `established` (the constant result for nothing pending), `answered`
      and `insisted` with `insisted-one` (the non-throwing and throwing result shapes,
      matching `check` and `check!`), `opted-out` and `unmark` (two unwrappings of an
      `unsealed` marker), `open` (the draft a `drafting` starts from). `adopt`, `held-in`,
      and `passed-in` read fine against the published vocabulary. Public names stay.

## For discussion

- [ ] **Value-keyed lookups in class-keyed maps.** The runner agent found `model-of` and
      `open` asking the class-keyed registry about a value as a fallback for the type
      object; past eight types the registry is a hash map and the lookup hashes the whole
      value, 350 ns on `draft` with the suite loaded. Fixed at those two sites with a
      `class?` test (82c7db0). Nothing else was swept; anything that looks up a value in a
      map keyed by class has the same shape.
- [ ] **The thread binding around `drafting` and drafting methods.** After the seal
      short-circuit, `drafting` with an empty body is 97 ns and about 60 of it is the
      `push-thread-bindings`/`pop-thread-bindings` pair around `*owner*`. It is the largest
      single cost left on that path. The token exists so a nested `drafting` can tell a
      draft it owns from a free one; an alternative that avoids the binding frame would need
      the same re-entrancy guarantee. Reported by the short-circuit agent.
- [ ] **The model map as a type.** After the runner step, sixty of the 117 ns for a seal
      with one pending check is keyword lookups on the model, a hash map at any real
      type's width, and most of the rest is the `case` dispatch through `:run-draft` and the
      `record-verdict!` protocol call. Making the model a `deftype` with fields turns each
      lookup into a field read. Bigger than the runner step; the next lever if the seal
      needs to come down further.
- [ ] **The width ceiling is the class file's, and ours is lower than core's.** Measured
      2026-09-17: `clojure.core/defrecord` compiles up to 122 fields, ours up to 92; past
      each, "Method code too large!", the 64KB bytecode limit on one generated method; from
      254 both fail on the JVM's 255-parameter constructor limit. The large methods are the
      `case`-over-fields forms whose every branch rebuilds the value with every field, so
      bytecode grows with the square of the width; core's `assoc` has that shape, and ours
      has it in `assoc`, the merge loop, and the draft's `assoc`. Raising ours to core's
      means rebuilding through an array or a per-field setter rather than a full
      constructor call per branch. Not needed for any type under 92 fields.

- [ ] **Interface methods the template inherits but does not define.** `java.util.Map` and
      `Iterable` carry default methods `AbstractRecord` never writes (`merge`, `forEach`,
      `replace`, `getOrDefault`), and `java.util.Map/remove` has a `boolean` return, so a
      body may take those names and fail in the compiler ("Must hint overloaded method") or
      the verifier ("Bad type on operand stack"). Plain `defrecord` fails the same way, so
      this is baseline behaviour, recorded here so it is not mistaken for a gap in the
      reservation. Catching it means reflecting over the declared interfaces, which would
      also reserve `invoke` at every arity. Reported by the reservation agent.

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
