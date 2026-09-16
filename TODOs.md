# TODOs

Drift inventory taken 2026-09-15, before the project layout existed. Every observation below
was verified by running the code.

The four drift items have since been adjudicated by galdre; their rulings are recorded inline
below. In every case the code is right and the prose is wrong, which is worth noting: the docs
drifted off the code, not the reverse.

## Implementation sequence

`SPEC.md` is the target. Each step below leaves the suite green and is scoped to be done on its
own, tests first. Steps within a phase are ordered; phases after A are ordered.

### A — independent of the check redesign

- [x] **A1. Namespace-qualify field keys.** `:recognitum.heist/name` rather than `:name`, derived
      from the defining namespace. Touches the macro's key generation, the `IKeywordLookup`
      thunks, `map->Type`, the print form (which drops the namespace the tag carries) and the
      reader (which accepts bare or qualified keys). Mechanical but wide: every test, the demo,
      the bench, the README. Doing it first keeps it out of the way of everything else, and the
      `:checks` map needs it, since its keys are the field keys.

### B — the check model

- [x] **B1. The `:checks` map.** Named checks, arglist-as-dependency, the bare-function shorthand
      over a single field, and the optional docstring position. Exactly one arity. Replaces
      `:valid` metadata and the invariants vector. The field → checks index comes from arglists
      rather than declared `:keys`.
- [x] **B2. Compile-time resolution.** A symbol that is not a declared field, a keyword that is
      not a declared io-dep, a name used for both a check and an io-dep — each a compile error
      naming the offender. (Cycles wait for E1, when io-deps can name each other.)
- [x] **B3. The check contract.** `true` passed, anything else failure data. Exceptions
      propagate rather than becoming failures. Every entry point runs everything it was asked to
      and collects all failures, keyed by check name in declaration order, built with
      `array-map`.
- [x] **B4. Delete the audit.** `*audit-invariant-keys*`, `KeyAuditor`,
      `undeclared-invariant-key!`, `recognitum.audit-test`, `recognitum.audit-fixture`, the
      `:test-audit` alias, and the README section. Dependencies derived from arglists cannot be
      misdeclared, so there is nothing left to audit. Do this only once B1 lands — until then the
      audit is the only thing catching what B1 makes impossible.

### C — the definition form

- [x] **C0. `of-type?`.** True for either form of a registered type; false for anything
      unregistered, including the type object itself. Needed before C2, since a wrapped body
      receives a draft and `instance?` stops meaning what an author expects.
- [x] **C1. The definition map.** Wrap the checks in `{:options … :checks … :io-deps …
      :certificates …}`, all sections optional. Only `:checks` is populated at this point.
- [x] **C2. Protocol implementations.** One body installed on both types. The draft impl runs it
      as written; the sealed impl opens a draft, runs it, and seals the result only if the body
      returned a draft of this type — anything else passes through, which is most protocol
      methods. `unsealed` opts a return out of sealing, is unwrapped by both impls, and throws on
      a non-draft. `drafting` does not honour it.

### D — drafts and verdicts

- [x] **D1. The verdict mask.** An `^:unsynchronized-mutable` field on the draft. Set on pass,
      clear on failure. `assoc` seeds the new draft by `and`-ing the parent's mask against the
      invalidation mask for the written field. Snapshot, so versions diverge.
- [x] **D2. `check` / `check!`.** Return `true` or failure data; none return a draft. Running a
      check runs whatever it depends on that is not already passing.
- [x] **D3. Certificates.** The `:certificates` section, holding derived from the mask, and the
      built-in certificate covering every check. `certify` / `certify!`.
- [x] **D4. Uniform `meta`.** Library keys merged over the user's, same answer from drafts and
      sealed values.
- [x] **D5. `seal` consults the mask.** Run only what is not passing; collect every failure and
      throw once. Subsumes the current dirty-key logic.

### E — IO

- [x] **E1. `:io-deps`, singular form.** Fake the IO in `dev/` — an in-memory table standing in
      for a database is enough, and nothing here should need a real one to be exercised or
      tested. Arglists as everywhere else, io-deps nameable by other
      io-deps, topological fetch order, cycles rejected at compile time. The gate: an io-dep
      fetches once the pure checks over the fields it names have passed and every io-dep it names
      has fetched. Fetched values cached for one validation call and no longer. Argument tuples
      deduplicated.
- [x] **E2. Batched fetches.** `[[crew] :as rows]`, told from `[crew]` by the reader because it
      is not a legal arglist. Returns a map keyed by argument tuple.
- [x] **E3. `check-all` / `certify-all`.** Over a collection of drafts, results index-aligned.
      Partial success on the non-throwing forms only. Nothing bulk produces sealed values.
- [x] **E4. `:io-limited`.** Direct `assoc` and `merge` on a sealed value throw for any field an
      IO-bound check depends on; other fields unaffected.

### G — the baseline is `defrecord`

Both steps implement rulings recorded in `SPEC.md` after the first run: "Shape checks", "Unset
fields", and "The baseline is `defrecord`". Read those sections before anything else.

- [x] **G1. Shape checks gate the rest, and unset is `nil`.** A check named after a field is that
      field's shape check; it must name exactly that field, or the form does not compile. The
      model marks shape checks structurally. Every other pure check gets a gate mask — the
      shape-check bits of the fields it names — and runs only once those have passed; a check
      whose gate has not passed neither runs nor appears in any failure map. Run order and
      failure order become three phases: shape, other pure, IO, declaration order within each.
      The `unset` sentinel is removed: a draft opened from the type starts with every field
      `nil`, `(keys draft)` returns every declared key always, `map->Type` treats an omitted key
      as `nil`, and every check receives `nil` for an unwritten field. `:missing-field`,
      `:draft-missing`, `missing-values!`, and the pre-flight in `check-all` go, and the README
      error table loses the row. `dev/recognitum/heist.clj`'s two cross-field checks lose their
      `(and (non-neg-int? take) (cuts-map? cuts) …)` prefixes — they are the thing this fixes;
      the gate now guarantees what they were restating. Existing tests that pin `:missing-field`
      or partial `keys` on a draft are changed to the new behaviour, not deleted. The IO gate is
      unchanged.

- [x] **G2. A record template of our own.** Every defect in `SPEC.md`'s baseline table is one
      design decision in `potemkin.collections/AbstractMap`, which both generated types inherit:
      it implements `clojure.lang.MapEquivalence` (the marker `APersistentMap.equiv` consults, so
      a plain map equals us from *its* side and no `equiv` of ours can change that), its `equiv`
      is type-blind, its `hasheq` is the map's, its `empty` returns an empty instance, and it
      implements neither `IRecord` nor `Serializable`. Not a bug in potemkin — right for a
      map-like thing, wrong for a record.

      Write `AbstractRecord`, our own `potemkin.types/def-abstract-type` template on the same
      six-method contract the macro already targets (`get assoc dissoc keys meta with-meta`),
      and point both emissions at it in place of `AbstractMap`. Potemkin stays for the template
      mechanism; only the template is ours. In it: no `MapEquivalence`; `clojure.lang.IRecord`
      and `java.io.Serializable`; `equiv` and `equals` require the same class before comparing
      entries — never a plain map, never another type, and a draft never equals its sealed
      value; `hasheq` and `hashCode` mix the class in as `clojure.core/emit-defrecord` does
      (read it for the recipe); `empty` throws `UnsupportedOperationException` as a record's
      does. Everything else `AbstractMap` provides stays — `IPersistentMap`, `Associative`,
      `ILookup`, `Counted`, `Seqable`, `IFn`, `IObj`, `java.util.Map`, iteration — and the
      macro's own `IKeywordLookup` thunks and `print-method` are untouched.

      **The draft and the sealed type get identical treatment.** Whatever one becomes, the
      other becomes; the only differences between them are the contractual ones `SPEC.md`
      already names. "Record" here means the `IRecord` marker and record *behaviour*, not
      `clojure.core/defrecord`, whose write path cannot be overridden.

      Tests first, and the first test is a `defrecord` twin: pin every map-surface operation
      against a real `defrecord` holding the same fields — `get`, keyword lookup,
      destructuring, `keys`/`vals`/`seq`/`count`/`contains?`/`find`, `into {}`, `assoc`
      returning the same type, `meta`/`with-meta`, `=` in both directions against a map,
      another type and each other, `hash`, `record?`, `empty`, `Serializable`,
      `clojure.walk/postwalk` both as identity and editing a field, and the `java.util.Map`
      methods. Where the spec deliberately departs from `defrecord` (`dissoc` refused, an
      undeclared key refused on a sealed value), pin our behaviour and say so in the test name.
      Existing tests that leaned on map-equality between a draft and its sealed value, or
      between a value and a literal map, change to the new behaviour — not deleted.

      Acceptance: the six rows in `SPEC.md`'s defects table read as `defrecord`'s column;
      `(clojure.walk/postwalk identity heist)` returns a `Heist`; `(= (draft x) x)` is false.
      The sealed type's public Java constructor stays as it is — `deftype` offers no
      constructor hook, and it is the same interop-level standard `defrecord` accepts; anyone
      constructing one by hand has to work at it. Re-run the bench; if any row moves by more
      than the recorded spread, re-record the README table with provenance.

### I — the checking family, settled

- [x] **I1. One verb, an explicit re-run, and honest names.** Rulings of 2026-09-16, all
      galdre's. (a) `check` handles both a check name and a certificate name — a lone check is
      a certificate containing itself — and its answer is always a map keyed by check name
      (`true` when everything passed). `certify`, `certify!`, `certify-all`, `certify-all!` are
      removed. (b) `check` *establishes*: it skips what the verdict mask already says passes,
      and on a sealed value, where everything is established, returns `true` without running
      anything. The `:not-a-draft` refusal goes from every checking function; only `unsealed`
      keeps it. (c) `recheck`, `recheck!`, `recheck-all`, `recheck-all!` run what they name
      regardless of the mask: on a draft the result sets or clears bits, so a passed IO check
      that fails against today's world is un-passed; on a sealed value they run and answer.
      (d) `passing` → `passed`, `holding` → `certificates`, matching the `meta` keys, and both
      answer for a sealed value too (every check, every certificate). (e) `check-all` and
      `recheck-all` accept drafts of more than one type — one fetch pass per type, still
      batched within each; a name resolves per type; an unknown certificate fails the whole
      call, as a single `check` already does; `:mixed-types` goes. (f) Because every answer is
      keyed by check name, a check that was blocked by its gate shows the blocking check's
      entry, and the old `check-all` ambiguity (raw return vs gate-failure map) no longer
      exists. Update README's draft-level sections and the surface test's expected set.

- [ ] **I2. Two small fixes ruled on 2026-09-16.** (a) Protocol method bodies are emitted as
      `(loop [~@non-receiver-args] ~@body)` on both the sealed and draft types, so a user's
      `(recur …)` targets the loop with exactly the arity a `defrecord` method body has —
      receiver excluded — allocates nothing, and works inside the sealed side's `try`. Test
      with a `countdown` method that recurs, on both forms. (b) A certificate accepts any
      collection of check names and duplicates are removed at macroexpansion; documentation
      shows the set form everywhere. Test that `[::a ::a ::b]` and `#{::a ::b}` yield the same
      certificate. Do not edit TODOs.md or SPEC.md; report.

### H — later, low priority

- [ ] **H1. `recognitum.walk`.** `clojure.walk/walk` is a plain `cond`: on `IRecord` it does
      `(reduce conj form form)`, one write per entry, with no protocol or multimethod to
      intercept. So `postwalk` over an `:io-limited` sealed value throws `::io-limited`, and
      over any sealed value it runs checks per entry. Provide a namespace identical to
      `clojure.walk` in every way except that a sealed recognitum value is walked as a draft —
      opened, rebuilt, sealed once at the end — and a draft is walked as itself. Same public
      names (`walk`, `prewalk`, `postwalk`, `keywordize-keys`, `stringify-keys`,
      `prewalk-replace`, `postwalk-replace`, `macroexpand-all`), so a consumer swaps the require
      and nothing else. Nothing needs it yet.

### F — reconcile

- [x] **F1. README, demo, bench** to the new syntax. The demo should illustrate io-deps against
      the faked in-memory source from E1: a fetch resolving, the gate refusing to query on a
      malformed value, deduplication, and a check judging the result. The bench's hand-rolled side needs the same
      checks written out by hand again, or the comparison stops being apples-to-apples.
- [x] **F2. Re-measure and re-record the performance table.** Checks called with direct field
      access rather than keyword lookup is the change most likely to move it.

## For discussion

Design questions raised while walking the deferred list on 2026-09-16. Each carries the ruling
so far, or the options if it is still open. None is implemented.

- [x] **`check` versus `certify`, and an explicit re-run.** Settled 2026-09-16; see step I1. Proposal from galdre: `check` and
      `certify` *establish* — they skip whatever the verdict mask already says passes, keeping
      the fast path fast — and on a sealed value, where everything is established, they return
      `true` without running anything. New verbs `recheck` and `recertify` *run it now*
      regardless of the mask: on a draft the result sets or clears bits, so a passed IO check
      that fails against today's world is un-passed; on a sealed value they run and answer.
      This replaces the `:not-a-draft` refusal on every checking function, which galdre ruled
      wrong. Open: how many of the eight existing checking names (`check`, `certify`, their
      bangs, their `-all` forms, and those bangs) get a `re-` twin. Re-verifying a batch against
      the database is a real case; `recheck-all` for one check across a group is harder to
      picture.
      **Ruled 2026-09-16, first part: one verb handles both a check name and a certificate
      name** — a lone check is a certificate containing itself, the answer is always a map
      keyed by check name, and the separate `check`/`check!` verbs go. That also dissolves the
      `check-all` ambiguity below (a raw check return and a gate-failure map were
      indistinguishable; now every answer is keyed). Still open: which name survives
      (`check` or `certify`, and so `recheck` or `recertify`), and whether one draft and a
      group share the name.

- [x] **`passing` and `holding` are misnamed.** Settled; see step I1. Agents named them; galdre has not seen them.
      Present participles read as scoping forms — `drafting` already is one in this library —
      and these are queries returning sets. Options: rename to match the `meta` keys they
      duplicate (`passed`, `certificates`) and widen them to answer for a sealed value too (all
      checks, all certificates), which closes three deferred notes about whether they should;
      or remove them, since `(::recognitum.core/passed (meta d))` already answers and two
      spellings of one question is surface. **Ruled 2026-09-16: rename to match the `meta`
      keys — `passed` and `certificates` — and they answer for both forms.** Correction from
      galdre, same day: `meta` was never meant to be *the* uniform interface; the ruling was
      only that it must unforgeably carry the information. An agent extended that into
      "therefore the functions stay draft-only," which was never asked for.

- [x] **Groups of mixed types are allowed.** Settled; see step I1. Ruled by galdre: the contract is "recognitum
      types," and `check-all`/`certify-all` over drafts of more than one type should work. The
      `:mixed-types` refusal goes. Batched fetches are declared per type, so a mixed group is
      one fetch pass per type, still batched within each. A check or certificate name resolves
      per type. An unknown certificate fails the whole call — it is a programmer error, the
      same rule a single `certify` already applies, and burying it among per-value failures is
      how a typo hides.

- [ ] **How exceptions are documented.** The keyword tables in README and SPEC answer "what
      keywords exist"; the reader's question is "what can this call throw, and what is in it."
      Galdre: a table is the wrong mechanism. Direction: each function's docstring names what it
      throws and the ex-data keys, so `(doc check)` answers; a README section organised by
      operation rather than by keyword. Compile-time refusals and typo-catchers
      (`:unknown-check`) are not worth documenting in a table at all. Undecided in shape.

- [ ] **README gaps from the exception audit.** Once the above settle: `:malformed-batch` and
      `:incomplete-batch` (a batched fetch returned the wrong shape) get documented; whatever
      replaces `:not-a-draft` gets documented; `certify`'s docstring names
      `:unknown-certificate` as a possibility. `:io-limited` is already in README.

- [ ] **SPEC.md's future.** It began as the target for the rewrite and drifted into a design
      rationale document without that being decided. Galdre: make a list of what it holds that
      could move to README, and walk that list one item at a time — some of the rationale is
      not important in the grand scheme. Until then, no new rulings are written into it.

- [ ] **`run-checks!` per-call overhead** (perf, later). Reported by the merge-fix agent: per
      validation it builds three phase vectors, a failure set, a lazy `remove` over the pure
      phase, and a `some` per check in the gate test — a fixed cost on every sealed write, not
      just `merge`. Alongside the existing note that `equiv`/`equals`/`hashCode` build a map per
      call. Galdre: perf work is later.

## Deferred during implementation

- [x] **G2 — `postwalk` over an `:io-limited` sealed value throws where the step's
      acceptance line said it returns the value.** Superseded by H1 in the implementation
      sequence, which is the walk-of-our-own this asks about. `clojure.walk/walk` rebuilds a record
      as `(reduce (fn [r x] (conj r (inner x))) form form)` -- one `conj` per entry,
      each of which is a direct write to a sealed value. On `Heist`, which is the type
      `SPEC.md`'s example names and which is `:io-limited`, the first io-limited field
      written throws `::io-limited`. That is the option doing exactly what it exists to
      do: the alternative is a round of checks, and a fetch, per entry, silently. So the
      refusal was pinned rather than worked around, and `SPEC.md`'s baseline section now
      records it. What is left open is whether the library should offer a walk of its own
      -- one that opens a draft, walks, and seals -- so that a value of an `:io-limited`
      type can be rebuilt at all without the caller writing the `drafting` form by hand.
      Nothing needs it yet; `clojure.walk` over the draft works today.

- [x] **G2 — `equiv`, `equals` and `hashCode` build a map per call.** `AbstractRecord`
      is a template, so it cannot name the fields the way `emit-defrecord` does; it
      compares and hashes through `(into {} this)`, which allocates. `AbstractMap` did
      the same, so nothing regressed, and equality is on no benchmarked path. The fix,
      if one is ever wanted, is not in the template at all: the macro knows the fields
      and could emit a field-by-field `equiv`/`equals`/`hasheq` into each generated type,
      where it would override the template's the way `cons` already does. Folded into the perf item in "For discussion" 2026-09-16; galdre: perf later.

- [x] **G2 — `toString` is neither `defrecord`'s nor the print form.** potemkin's
      template answered with the entries, `str` of a `defrecord` is `Object`'s identity
      string, and `pr-str` of one of ours is `#recognitum.heist/Heist{...}`. The template
      kept potemkin's answer, which is the readable one of the three and what every
      existing test expected, but a value whose `str` drops the tag its printed form
      carries is a small lie. Ruled: `str` matches `defrecord` — the identity string —
      because `str` is what concatenation and logging call implicitly and the entries
      form would dump a collection field into every line, while the readable form is one
      `pr-str` away; the override is gone from the template and `SPEC.md`, "Printing and
      reading", records it.

- [x] **G1 — a check whose shape gate fails throws from `check`, where `SPEC.md` gives
      two outcomes.** The same position E1 took for a failing IO gate, and for the same
      reason: `SPEC.md` says `check` returns `true` or the failing check's data, and says
      separately that a check whose gate has not passed does not run. Where both apply the
      check asked about has no data of its own, so `check` throws the ordinary
      `:check-failed` carrying the gate's failures rather than inventing a verdict.
      `certify` needs no such rule -- it is shaped for several failures, so a failing gate
      simply appears among them -- and `check-all` answers that value with the gate's
      failures keyed by check name, as E3 already ruled for the IO gate. If the reading is
      wrong, `establish` is the only place it lives. Confirmed by galdre 2026-09-16: one rule for both gates.
      Superseded by I1: the answer is keyed now, so a failing gate is reported rather than
      thrown, from `check` as from `check-all`. `establish` is gone with it. The rule is
      still one rule for both gates, and it is still that a check the gate stopped has no
      verdict -- it simply has no entry rather than a throw of its own.

- [x] **G1 — an IO-bound check gets a shape gate too, which buys nothing observable
      yet.** `TODOs` scoped the gate to "every other pure check"; `SPEC.md`'s "Shape
      checks" says "every other check waits on the shape checks of the fields it names",
      and that is what was implemented -- every check not named after a field carries a
      `:shape-mask`, IO-bound ones included. It cannot currently be observed, because a
      value that failed any pure check fetches nothing at all, so an IO-bound check is
      already unreachable whenever its shape gate is unsatisfied. It was done this way so
      that the rule is one rule; if the coarse pure-phase gate is ever narrowed to the
      fetches a failure actually touches, this is already correct rather than a hole that
      opens at that moment. Acknowledged 2026-09-16. Verified: on a draft, `check` fetches for an IO-bound check even while an unrelated field is garbage (narrow, per spec); at seal no fetch runs if any pure check failed (coarse, per the spec's two-phase seal). No deviation.

- [x] **G1 — `::malformed-shape-check` has no row in `SPEC.md`'s error table.** Nor do
      `::unknown-dependency`, `::malformed-check` or any of the other compile-time
      refusals, so this is the existing shape of that table rather than a new gap: it
      lists what a caller can dispatch on at run time. Recorded because the table now
      omits one more thing, and because whoever gives the compile-time family a home --
      E4's deferral wants the same for `::io-limited` -- should take this with it. 2026-09-16: galdre ruled compile-time refusals are not worth a table row.

- [x] **G1 — the plan is computed in masks, and `cost-test` is why.** `plan-for` folds a
      check's prerequisites in with `bit-or` against the masks the macro computed, takes
      the closure to a fixed point, and answers `pending` unchanged where nothing was
      added -- which is the ordinary case, since a group asked for usually carries its own
      gates. The first version built a `sorted-set` per call instead, and
      `cost-test/a-three-field-update-inside-a-drafting-stays-within-its-multiple` read
      2.01x against a bound of 2.0. That is a bound doing its job. What is left undone is
      the same thing F2 deferred: `checks-for` still builds a `sorted-set` per write out
      of the field -> checks index, where the macro already computes a per-field mask, and
      `mask-indices` is now sitting there to expand the result.
      Done with F2 below: `checks-for` unions `:field-checks` with one `bit-or` per entry
      and expands the result through `mask-indices`, and the benchmark's `merge` row went
      from 1543ns to 962ns against a hand-rolled 860ns, same machine, same sitting.

- [x] **F2 — `merge` on a sealed value is the one row the library loses.** 1251ns against
      814ns hand-rolled, and the only row over 1.2x that is not a `drafting`. Nothing here
      regressed -- it read 1.6x in the table F1 inherited -- so F2 left it, having spent its
      fix on the drafting rows the deferral named. Where it goes: `checks-for` builds a
      `sorted-set` per write out of the field -> checks index, where the macro already
      computes a per-field mask for exactly this question (D1's invalidation masks), so the
      union could be one `bit-or` per key and an expansion over the set bits; and `cons`
      builds an intermediate map of the entries before constructing through `get` per field.
      The first is the cheaper win and touches only `checks-for` and its two callers.
      Both are fixed, and "nothing here regressed" was wrong: 933ns hand-rolled against 604ns
      before the rewrite, 844 against 1543 after, 860 against 962 now -- a tenth where it was
      most of a doubling -- with a 1.4x bound in `cost-test` so the next one fails rather than
      waits to be noticed. `checks-for` had one caller left by now, not two: the sealed `cons`.

- [x] **F2 — `recognitum.cost-test`'s bounds are loose, and one of them is very loose.**
      The rename row is held to 12x where the benchmark reads 3.5x, because the suite is a
      bad place to measure the library: by the time forty-odd types have been defined across
      the other files every call site into `recognitum.core` is megamorphic, which roughly
      doubles the library side and costs the hand-rolled side nothing. Inside the suite the
      row reads 7-8x where the benchmark reads 3.5x, and the regression the bound exists to
      catch read 15-17x. So it catches a fixed cost per call and nothing subtler. A tighter
      guard would have to run in its own JVM -- a `-X:cost` alias running only that
      namespace would read the benchmark's numbers and could be held to 5x -- which is a
      test-runner question rather than a library one. Folded into the perf item in "For discussion" 2026-09-16.

- [x] **B1 — the audit is inert but not yet gone.** `*audit-invariant-keys*`,
      `recognitum.audit-fixture`, the `:test-audit` alias and the README section all survive
      B1 and now do nothing: a check body cannot reach a field its arglist does not name, so
      there is no declaration left to disagree with the code. `recognitum.audit-test` was
      deleted with B1 rather than left broken, since every type it defines declares invariant
      `:keys`, which no longer exist; its coverage is replaced by
      `checks-test/a-check-body-cannot-read-a-field-it-did-not-name`, which shows the class of
      bug does not compile rather than that a proxy catches it. B4 removed the rest.
- [x] **B1 — declaration order is lost past eight checks.** The reader hands the macro a
      `PersistentHashMap` once a map literal passes eight pairs, so for a ninth check onward
      the order the macro sees is hash order, not source order. `SPEC.md`'s "the order the
      checks are written in the form, which the macro sees before a map literal would lose
      it" is true only up to eight. It is stable, just not the written order. B3 owns the
      ordered failure map and should decide whether that is acceptable or whether the section
      needs another shape. Ruled in B3: acceptable as it stands, see the B3 entry below.
- [x] **B1 — a cross-field check runs on fields its own type has already rejected.** With
      checks unified, `SPEC.md` says pure checks run ungated and exceptions propagate, so
      `::cuts-sum-to-take` over a `cuts` that is the string `"garbage"` would throw rather
      than report. B1 preserves the old behaviour — declaration order plus stop-at-first
      failure, with a throw read as a failure — so nothing is exposed yet. B3 takes both of
      those away at once and has to say what happens then. Ruled in B3: the check is at fault
      and must be total over what it names, since checks have no ordering among themselves.
      `recognitum.heist`'s two cross-field checks now restate the shapes they need.
- [ ] **B4 — the removal test forbids the word "audit" anywhere in the tree.**
      `recognitum.audit-removal-test` scans every `.clj` under `src/`, `test/`, `dev/` and
      `bench/`, plus `deps.edn` and `README.md`, for the audit's spellings, and two of its
      assertions are broader than that: the word itself must not appear in `deps.edn` or
      `README.md` at all. That is deliberate — the machinery survived B1 by being mentioned
      nowhere but its own definition — but it means F1, rewriting the README, cannot use
      "audit" in any sense without the suite failing. If a later document wants the word for
      something unrelated, narrow those two assertions to the token list the file already
      carries rather than deleting them. F1 rewrote the README end to end without wanting
      the word, so nothing was narrowed; the constraint stands for whoever does want it.

- [x] **D1 — `passing` answers for a draft only, and D4 will want it for both.**
      `SPEC.md`'s uniform `meta` says a sealed value reports the same
      `:recognitum.core/passed` key a draft does, holding every check, "a compile-time
      constant for a sealed type". `passing` throws `::not-a-draft` for anything but a
      draft, because in D1 the mask is the only thing that could answer and only a draft
      has one. D4 either widens `passing` to read the model's check names for a sealed
      value or builds that constant where it merges the metadata; either is fine, but the
      docstring's "a question about a draft and nothing else" has to move if the first is
      chosen. Ruled in D4: the second. `meta` is the uniform interface and answers for
      both forms; `passing` and `holding` stay questions about a mask, which only a draft
      has, and the sealed half of the answer is a constant the macro computes from the
      type and embeds in the sealed `meta` method. So a sealed value with no metadata of
      its own answers with that very map and allocates nothing, and there is one narrow
      set of draft-only fns rather than four fns disagreeing about what a sealed value is
      allowed to be asked. Folded into "For discussion" 2026-09-16: galdre ruled the `:not-a-draft` refusal wrong and `passing`/`holding` misnamed; see the check/certify and passing/holding items there. Closed by I1: all of them are wide. `check` answers `true` for a sealed value without running anything, `passed` and `certificates` answer for both forms and carry the same values `meta` does, and the `::not-a-draft` refusal is gone from the family -- what is left is `::not-a-value`, for something that is neither form of anything.

- [x] **D1 — the mask is written but barely read.** `run-draft-checks!` records what
      it ran and `seal` is its only caller; the inlined groups at `assoc`, `merge` and the two
      constructors work on sealed values, which have no mask, and `run-checks!` is what
      they still use. D2 landed `check`/`check!` alongside `run-draft-checks!` rather than
      on top of it -- one check answered rather than a group run to a throw is a different
      shape -- so a second writer of verdicts exists, and `establish` is the only reader of
      the mask so far. D5 makes `seal` select by the mask instead of by the dirty-key set;
      until then a check whose bit is already set is still re-run at seal, so work `check`
      did on a draft is not yet saved at the seal, only guaranteed to agree with it. Done
      in D5: `seal` runs exactly what `not-established` hands it, and the dirty-key
      machinery it replaced is gone -- the draft's written-field bitmask,
      `:draft-dirty-keys` and `:independent` are all deleted, so a write maintains the
      verdict mask and nothing else.

- [x] **D2 — `check` and `check!` refuse a sealed value, as `passing` does.** Both throw
      `::not-a-draft` for anything but a draft, which is `SPEC.md`'s framing ("Running
      checks on a draft") and matches `passing`. But a sealed value is one every check
      passed on, so `true` is an honest answer there too, and D4's uniform `meta` wants the
      same constant for the same reason. Whoever widens `passing` for D4 should widen these
      alongside it or leave all three narrow deliberately -- the one thing to avoid is
      `passing` answering for a sealed value while `check` throws on one. Ruled in D4:
      all of them stay narrow. `meta` is where a sealed value answers, and it answers
      from a constant rather than by running anything. Folded into "For discussion" 2026-09-16: galdre ruled the `:not-a-draft` refusal wrong and `passing`/`holding` misnamed; see the check/certify and passing/holding items there. Closed by I1: all of them are wide. `check` answers `true` for a sealed value without running anything, `passed` and `certificates` answer for both forms and carry the same values `meta` does, and the `::not-a-draft` refusal is gone from the family -- what is left is `::not-a-value`, for something that is neither form of anything.

- [x] **D2 — a check naming a field never written throws `::missing-field`, which
      `SPEC.md` does not say.** The spec's table gives `check` two outcomes, `true` or the
      failure data, and says nothing about an incomplete draft -- but running checks early
      on an incomplete value is the whole point of the facility, so the case is reachable
      by design. Running the check would hand its body `recognitum.core/unset`, an opaque
      `Object` no author can recognise, while the failure report would claim it was given
      `nil` (`dep-values` goes through `get`, which returns the default for an unwritten
      field). So it reports `:missing-field` naming the check and every field it is waiting
      on, in arglist order, before the body is called. `certify` (D3) and `check-all` (E3)
      have the same case and should answer the same way. Ruled in D3: `certify` answers
      the same way, over the whole group at once -- it pre-flights every check it is about
      to run and throws one `:missing-field` carrying `:waiting`, a map of check name ->
      the fields that check is waiting on, because nothing else here stops at the first
      anything either. `:check`, `:key` and `:keys` still describe the first, which is the
      whole of it wherever one check was asked about. Moot as of 2026-09-16: unset reads as nil and `:missing-field` no longer exists.

- [x] **D2 — `check` returns the check's own return value, not the failure report.**
      `SPEC.md` says "the failing check's data", and the check contract says what a check
      returns *is* the failure data, so `check` hands back exactly that. The richer report
      -- docstring, declared fields and their values, return value -- is what `check!`
      throws. D3 has to decide the same thing for `certify`, whose `{check-name
      failure-data}` could plausibly be either; making it match the `:failures` map of a
      thrown `:check-failed` would be the other reading. Ruled in D3: `certify` hands back
      the checks' own return values, so the two non-throwing forms read alike and the
      phrase "failure data" means one thing throughout. `certify!` throws the same report
      every other throwing entry point throws. Still true after I1, now in the one shape
      both scales use: what comes back is the checks' own return values keyed by check
      name, and the key is what says a check failed -- so one that returned `nil` or
      `false` is reported as faithfully as one that returned a map.

- [x] **D3 — the built-in certificate is called `:recognitum.core/every-check`.**
      `SPEC.md`'s "Open" section left the name to whoever needed it; D3 needed it, so it
      is settled there and the bullet is gone. `recognitum.core/every-check` is the var
      holding it, a type may not declare a certificate of that name
      (`::reserved-certificate`), and it is the last entry of every model's
      `:certificates`, so `holding` and `certify` need no special case for it. A type
      with no checks holds it vacuously, which is honest rather than a carve-out. Confirmed by galdre 2026-09-16.

- [x] **D3 — a certificate may be written as a vector as well as a set.** `SPEC.md` says
      "a set of check names", and a set is what the example writes. The macro accepts any
      set or sequential collection, because a vector of check names means exactly the same
      thing and refusing it would buy nothing: the order it is written in is discarded
      either way, since a certificate is read back in the type's own check order so that
      failures report in the order every other entry point reports in. A map, a keyword, a
      bare symbol and a number are refused as `::malformed-certificate`. Ruled 2026-09-16: accept any collection, remove duplicates at compile time, documentation shows the set form. Queued as step I2.

- [x] **D3 — `certify`, `certify!` and `holding` refuse a sealed value, as `check` and
      `passing` do.** Same note as D2's, now with two more functions in it: a sealed value
      passed every check, so `true` and "holds every certificate" are honest answers there,
      and D4's uniform `meta` wants both constants for a sealed type. Whoever widens
      `passing` for D4 should widen all five together or leave all five narrow; the thing
      to avoid is `meta` answering for a sealed value while the functions behind those keys
      throw on one. Folded into "For discussion" 2026-09-16: galdre ruled the `:not-a-draft` refusal wrong and `passing`/`holding` misnamed; see the check/certify and passing/holding items there. Closed by I1: all of them are wide. `check` answers `true` for a sealed value without running anything, `passed` and `certificates` answer for both forms and carry the same values `meta` does, and the `::not-a-draft` refusal is gone from the family -- what is left is `::not-a-value`, for something that is neither form of anything.

- [x] **D3 — `certify` does not save work at the seal yet.** Certifying a draft leaves
      every check it ran marked as passing, but `seal` still selects what to run by the
      dirty-key set rather than by the mask, so a fully certified draft re-runs everything
      when it seals. That is D5, and `SPEC.md`'s "bulk checking leaves the masks populated,
      so sealing each value afterwards finds its checks already passing and performs no IO"
      is not true until it lands. The guarantee is unaffected -- the checks agree, they are
      merely run twice. Landed in D5: a fully certified draft now seals without running
      anything, and the half of that sentence about IO waits only on E1.

- [x] **D3 — `README.md` says nothing about certificates.** F1 owns it. The draft-level
      half of the library -- `check`, `check!`, `certify`, `certify!`, `passing`, `holding`
      -- is documented only in docstrings and in `SPEC.md`. Done in F1: "What a draft has
      established" and "Certificates" carry all six, and `recognitum.readme-test` names
      each of them, so the section cannot be dropped again without the suite saying so.

- [x] **D2 — the README documents neither `check` nor `check!`.** F1 owns the README, and
      the draft-level facility is the half of the library it says least about. The four-way
      table C0 left for F1 is in the same position. Both landed in F1.

- [ ] **D1 — a type may declare at most 64 checks, and at most 20 fields.** The verdict
      mask is one long, so a 65th check would alias the first and a value could seal on a
      check that never ran; that is `::too-many-checks` at macroexpansion
      (`reject-too-many-checks!`). The written-field mask has the same width and cannot
      reach it, because a 21st field makes `->Type` a function of 21 parameters and
      Clojure refuses more than 20 -- with the message "Can't specify more than 20
      params", which says nothing about fields. If the field ceiling ever matters, refuse
      it in the macro with a message that names it.

- [x] **C1 — `:io-deps` and `:certificates` compile only empty.** The sections are part of
      the form before they are part of the library, so a populated one throws
      `:recognitum.core/unsupported-section` at macroexpansion rather than being accepted
      and ignored — accepting a declaration nothing reads is the silent mis-declaration
      this design is arranged to prevent. E1 and D3 each delete their half of the
      `(doseq [s [:io-deps :certificates]] ...)` guard in `parse-definition`, populate the
      model from the section, and drop the corresponding case from
      `definition-test/a-section-with-no-implementation-behind-it-is-accepted-only-empty`.
      D3 did its half: the guard is now a `when` over `:io-deps` alone, the model carries
      `:certificates`, and that test asserts a populated `:certificates` compiles. E1 did
      the other half: the guard is gone entirely, `::unsupported-section` with it, and
      `definition-test/a-section-with-no-implementation-behind-it-is-accepted-only-empty`
      is now `every-section-now-has-something-behind-it`.

- [x] **C1 — only `:options` was added to the model, and nothing reads it.** The model
      carries `:options` (an empty map where the section was left out) so that E4 has
      somewhere to look for `:io-limited`; `:io-deps` and `:certificates` are not on the
      model at all, because they can only be empty. Whoever lands E1/D3/E4 adds theirs the
      same way. D3 added `:certificates` -- certificate name -> `{:checks :ids :mask}`,
      emitted as `array-map` applied to the pairs so it stays ordered past eight. E1 added
      `:io-deps` (in fetch order), `:io-names`, `:run-io`, and three keys per check --
      `:io`, `:gate` and `:field-closure`. `known-options` is `#{:io-limited}` and an
      option outside it is a compile error, so a new option is declared there or it does
      not compile. E4 made `:options` read: `:io-limited` resolves at macroexpansion into
      `:io-limited-keys`, the fields a direct write to a sealed value is refused for, so
      nothing on the model is now carried and ignored. 2026-09-16: stale — the note itself records every later step adding its section; the option is read.

- [x] **C1 — the definition map is required, not optional.** `SPEC.md` says three
      positions and every *section* optional, so a type with no checks is written
      `(rec/defrecord T [a b] {})` rather than with the third position dropped. C2 adds
      trailing protocol implementations to the form and so has to touch the arity anyway;
      if an omitted definition map is wanted, that is where to decide it, since a map
      literal and a protocol name are trivially told apart. Ruled in C2: it stays
      required. `SPEC.md` says three positions, and a form that drops one of them puts a
      protocol name where a declaration goes — which `parse-definition` already reports as
      `::malformed-definition`, pinned by
      `protocol-test/the-definition-map-is-still-required-before-the-implementations`.
      Nothing is gained by letting a type be written two ways.

- [x] **C2 — `recur` to a method head does not work in either implementation.** Both
      wrap the body: the draft type in a call to `draft-impl-return`, the sealed type in
      a `let`/`binding` inside a call to `sealed-impl-return`. So a protocol method body
      written with a tail `recur` back to its own arglist is a compile error that a plain
      `deftype` would have accepted. Nothing in `SPEC.md` asks for it and no test wants
      it; a body needing a loop writes `loop` explicitly. Recorded because the error, if
      someone hits it, will not obviously be about this. Ruled 2026-09-16: fix. Emit the body inside `(loop [args…] …)` over the non-receiver arguments; `recur` then targets the loop with the same arity a defrecord method has, allocates nothing, and works inside the `try`. Verified on both emission shapes. Queued as step I2.

- [x] **C2 — the example model, the demo and the README carry no protocol
      implementation.** `SPEC.md`'s `Heist` implements `some.ns/SomeProtocol`, and
      `recognitum.heist/promote` is exactly the method its "Protocol implementations"
      section writes out — an impl fn that opens its own `drafting` because there is no
      wrapper to do it for it. F1 owns those files; moving `promote`, `rename` and
      `add-member` onto a protocol there would show the wrapper doing the work and the
      same body serving the impl layer, which is the whole claim. `recognitum.protocol-test`
      carries the coverage in the meantime. Done in F1: `recognitum.heist/Job` carries
      `rename`, `add-member`, `promote` and `total-payout`, none of which opens a
      `drafting` of its own any more; `apply-changeset` stays an ordinary fn, because it
      dispatches on a changeset rather than on a value.

- [x] **C0 — `README.md` does not mention `of-type?`, `draft?` or `sealed?` at all.** The
      four-way table in `SPEC.md` under "Telling values apart" has no counterpart in the
      README, which still leaves a reader with `instance?` and no warning that it is true of
      a sealed value and false of a draft of the same type. F1 owns the README; add the table
      there rather than growing a section this step would only have to rewrite. Done in F1,
      and the demo prints all four against the same two values.

- [x] **B1 — `README.md` still documents `^{:valid pred}` fields, the invariants vector, and
      the `:invalid-field` / `:invariant-violated` errors.** None of those exist. F1 owns the
      README; this is a note that it is now wrong rather than merely dated. C1 adds to the
      pile: the third position is now a definition map, so the README's `defrecord` example
      is wrong in its shape as well as in its contents. Rewritten in F1, and
      `recognitum.readme-test` forbids every one of those spellings by name.

- [x] **B3 — `SPEC.md` says failures come back "in declaration order", which is true only
      up to eight checks.** Ruled acceptable rather than fixed: the reader hands the macro a
      `PersistentHashMap` once a map literal passes eight pairs, so past that the order the
      macro sees is hash order. What B3 guarantees, and what `contract_test` pins, is the
      weaker invariant that holds at every size — the failures are reported in the order the
      model records its checks in, built with `array-map` so nothing promotes and reorders
      them, and that order is the written one up to eight. Fixing it properly means `:checks`
      taking a shape the reader preserves (a vector of pairs, say), which changes the
      definition form and belongs to whoever revisits C1, not here. Until then `SPEC.md`'s
      sentence overclaims by one clause. 2026-09-16: galdre — "not a big problem right now."

- [x] **B2 — the io-dep half of the resolution has no end-to-end path yet.** `resolve-deps`
      takes the set of declared io-deps and `reject-duplicate-names!` takes their names, but
      the macro passes `#{}` and `#{}` because there is no `:io-deps` section until E1. So the
      reachable macro-level behaviour is that every keyword in an arglist is unresolvable; a
      keyword resolving against a declared io-dep, and a name declared as both a check and an
      io-dep, are covered by calling those two fns directly in `recognitum.resolution-test`.
      E1 should replace the `io-deps #{}` binding in `defrecord` with the declared set, pass
      the io-dep names to `reject-duplicate-names!`, resolve each io-dep's own arglist through
      `resolve-deps` with `:what :io-dep`, and add the end-to-end cases. Done in E1; the
      end-to-end cases are in `recognitum.io-deps-test`, and `resolution-test` keeps the
      direct ones as the smallest statement of the same rules.

- [x] **A1 — the README performance table was not re-measured.** The bench runs and its
      numbers sit inside the run-to-run spread the table already warns about, but qualified
      keys are exactly the kind of change F2 exists to re-record. Left to F2, which owns the
      table. F1 relabelled its rows -- they said "invs" -- and said in the text that the
      numbers predate the check redesign, but touched no figure. It also ran the bench once
      without recording it, and the drafting rows have moved a long way in the wrong
      direction: N=1 renames read 2992ns against 260ns hand-rolled where the table says
      1220 against 862 for N=15. F2 should find out where that went rather than simply
      writing the new number down; nothing in F1 touched `src/`. Found in F2: it was not the
      checks, it was `establish-all`. E3 gave every entry point the group machinery, and a
      call with nothing pending still built a plan per value, asked what each was waiting on,
      and ran an empty pass over each phase -- about 2us of it, whatever the call had been
      asked to do. A call now answers before it enters a phase where nothing is pending, and
      the passes that remain are loops rather than `mapv` over several collections at once.
      A seal of an unwritten draft went 2286ns -> 279ns, N=1 renames 3060 -> 838 against 237
      hand-rolled, construct 5138 -> 1532 against 1358. Table re-recorded on the same
      machine, with every shape the bench measures now carrying a row.
- [x] **A1 — `CONSIDERATIONS.md` still shows bare field keys throughout.** It is scratch,
      kept but not maintained, and TODOs already says not to trust it. 2026-09-16: CONSIDERATIONS.md is unmaintained scratch by ruling; not fixing.

- [x] **D5 — `seal` believes the mask, so `record-verdict!` is a way past the checks.**
      The `Verdicts` protocol is public, because the draft type is generated in the
      caller's namespace and has to implement it there, so a caller can set the bit of a
      check that never ran and `seal` will skip it. Before D5 it could not: seal selected
      by the dirty-key set and re-ran whatever a write had touched whatever the mask said.
      That is the ordinary cost of caching a verdict, and the mask is where this design
      puts it; what is new is only that the cache is now believed. If it matters, the fix
      is to narrow the surface rather than the trust -- `verdict-mask` is wanted by tests
      and by nothing else outside `core`, and `record-verdict!` by nothing outside `core`
      at all, so both could be `alter-meta!`-ed private with `recognitum.verdict-test`
      reaching them through the protocol object. Not done here because D1 made them public
      deliberately and D5 is not the step that should reverse that. Done since, and wider
      than this entry proposed: the public surface of `recognitum.core` is now the
      twenty-one names `SPEC.md` and `README.md` use as API, and the other fifty-two are
      private. The premise above was wrong -- the protocol does not have to be public for
      the generated draft type to implement it, because `deftype` resolves a spec name
      through `ns-resolve`, which does not consult privacy. What the macro could not do is
      *call* a private var by qualified symbol from the consumer's namespace, so it emits
      `@#'recognitum.core/...` instead, which the compiler allows and a reader cannot
      mistake for API. `model-of` went with them, and so did the per-type model var a
      definition interns, since `:draft-seal` is the same back door reached by another
      name. `recognitum.surface-test` writes the surface out and fails on drift in either
      direction; `recognitum.internal`, under `test/`, is where the suite reaches around
      privacy, in one place and on purpose.

- [x] **D5 — `seal` is one phase, and `SPEC.md` describes two.** "The gate makes it
      two-phase: collect all pure failures and throw if there are any, otherwise fetch and
      collect the IO ones." Every check is pure until E1, so the second phase has nothing
      to hold and `seal` collects once over everything not already passing. E1 owns
      splitting what `not-established` hands back at the gate; the selection itself does
      not change, since a check already passing is run in neither phase. E1 split it:
      `establish-group` runs the pure phase, throws with everything it found if anything
      failed, and only then fetches and runs the IO-bound half. The selection is unchanged.

- [x] **E1 — the example model, the demo and the README have no io-dep in them.** F1 owns
      all three and its entry already says the demo should illustrate one. E1 left
      `recognitum.heist` exactly as it was and put the fake source beside it, in
      `dev/recognitum/roster.clj`: an in-memory table, a log of every call made to it, and
      a `fetch-roster` that throws on anything that is not a crew -- which is what makes a
      leaking gate show up as an exception rather than as a quietly wrong report.
      `recognitum.io-deps-test` uses it. Wiring it into `Heist` would have changed what
      every other suite's `Heist` does at the seal, which is not E1's to do. Done in F1:
      `Heist` declares `::roster` (batched) and `::vetted` (singular, naming the first),
      and one IO-bound check over the pair. It was arranged to cost the other suites as
      little as it could -- the check reads no field directly, so `::crew` is the only
      `:io-limited` key, and no test wrote that field on a sealed value -- and `:napster`
      was flipped to vetted in the table, so the three places using him as the happy-path
      new member stay happy-path. `:mole` is the unvetted one now.

- [x] **E1 — `check` throws where a check it is gated behind fails.** `SPEC.md` says
      `check` returns `true` or the failing check's data, and says separately that running
      a check runs whatever it depends on that is not already passing. Where a pure check
      gating an IO-bound check's fetch fails, the check asked about is never run, so there
      is no data of its own to return: E1 throws the ordinary `:check-failed` carrying the
      gate's failures, on the precedent of `:missing-field`, which D2 already throws from
      the non-throwing form for the same reason. `certify` needs no such rule -- it is
      already shaped for several failures, so a failing gate simply appears among them,
      even where the certificate does not name it. If the reading is wrong, the place to
      change it is `establish`. Confirmed by galdre 2026-09-16, same ruling as the shape-gate twin.
      Superseded by I1 alongside its shape-gate twin: `check` answers with the gate's
      failures keyed by check name instead of throwing them.

- [ ] **E1 — an io-dep nothing names compiles, and never fetches.** Every other declaration
      that would mean nothing is a compile error here, and a dead io-dep is arguably one of
      those. It is not refused, because unlike a certificate covering nothing it asserts
      nothing while it sits there: it is unreachable code rather than a false claim.
      `io-deps-test/an-io-dep-nothing-names-never-fetches` pins the behaviour either way.

- [x] **E1 — the batched form is refused as an unresolvable arglist entry.** `[[crew] :as
      rows]` is E2. Until then `[crew]` is an entry naming neither a field nor an io-dep and
      is reported as `::unknown-dependency`, which is true but says nothing about the form
      being a batched fetch written too early. E2 owns the message either way. 2026-09-16: moot — the batched form has existed since the first run.

- [x] **E2 — a batch of one is the only batch there is yet.** A validation call spans one
      value, so a batched fetch is handed exactly one tuple and the deduplication the form
      exists for -- a hundred values referencing five crews arriving as five tuples in one
      call -- has nothing to deduplicate. `fetch-one` is where E3 lands it: it builds the
      tuple out of the value, calls `:run-batch` with `[tuple]`, and reads the answer back
      out of the returned map by that key. E3 replaces that one-element vector with the
      distinct tuples of the whole group and distributes from the same map; nothing past
      `fetch-io` has to change, since everything downstream holds a value either way.
      Landed in E3: `fetch-one` is gone and `fetch-io-all` fetches a group in one pass per
      io-dep, with `fetch-io` a group of one through the same code.

- [x] **E2 — a batched fetch's return shape is policed and a singular one's is not.**
      `SPEC.md` gives the batched form a return contract ("a map keyed by the argument
      tuple") and gives the singular form none, on the grounds that a singular fetch's only
      consumers are the checks naming it. The library is the batched form's first consumer,
      because it has to distribute the result, so E2 refuses what it cannot distribute:
      `::malformed-batch` for a result that is not a map, `::incomplete-batch` for one that
      omits a tuple it was handed. A tuple answered `nil` is an answer and passes through;
      one left out is not, and reading it as `nil` would have a check judge a fetch that
      never happened. If that reading is wrong, `batch-answer!` is the only place it lives. 2026-09-16: consistent with galdre's ruling that a fetch returns what it returns and the checks decide; the batched form is policed only because the library must distribute it.

- [x] **E2 — the batched form appears in no example.** The demo, the README and the example
      model still have no io-dep in them at all (E1's deferral above), let alone a batched
      one. F1 owns all three; `recognitum.roster/fetch-rosters` is the batched twin of the
      fake E1 left beside them, and is there to be wired in when it does. Done in F1:
      `Heist`'s `::roster` is the batched one and `::vetted` the singular, so both
      spellings appear on one type and the demo shows what each is handed.

- [x] **E1 — nothing is deduplicated across values yet.** Within one validation call an
      io-dep is fetched once however many checks name it. Across values it is fetched once
      each, because there is no call that spans values until `check-all` / `certify-all`.
      E3 owns that, and `fetch-io` is where the argument tuple would become the cache key
      rather than the io-dep name. Landed in E3: the tuple is the cache key in
      `fetch-io-all`, so a singular fetch is called once per distinct tuple across the
      group and a batched one is handed all of them at once.

- [x] **E3 — what a group answers about a check it could not run is a map, and a check may
      return a map.** Where a pure check gating a fetch fails, `check` throws (E1's ruling,
      on the precedent of `:missing-field`): the check asked about never ran, so there is no
      verdict to answer with. A group call cannot throw for that -- the other values have
      answers of their own and a throw would discard every one of them -- so `check-all`
      answers that value with the gate's failures keyed by check name, which is the shape
      `certify` answers with. A check whose own failure data happens to be a map keyed by
      check names is indistinguishable from it. Nothing in `SPEC.md` decides this; if the
      reading is wrong, `check-all` is the only place it lives.
      Dissolved by I1: every answer is keyed by check name, at every scale, so there is no
      raw check return for a gate-failure map to be confused with. `check` no longer throws
      for a failing gate either -- the blocking check gets the entry and the check that
      never ran gets none, which says the same thing without inventing a verdict.

- [x] **E3 — a group is refused where a single draft would be accepted.** `check-all` takes
      drafts of one type: one fetch spans the group, the check name resolves against one
      model, and the tuples are built by one type's runner, so a mixed collection is
      `::mixed-types` and a sealed value in it is `::not-a-draft`. Both name the offender by
      index. `SPEC.md` says only "a collection of drafts", so neither error appears in its
      table; whether a mixed group should instead be partitioned by type and run per type is
      a question it does not ask. Folded into "For discussion" 2026-09-16: galdre ruled mixed-type groups are allowed.

- [x] **E3 — a group's throw is keyed by index, which `SPEC.md`'s error table does not
      describe.** `:check-failed` carries `:failures` keyed by check name everywhere else;
      from `check-all!` / `certify-all!` it is keyed by the value's index and holds the
      ordinary check-name map under each, with `:bulk true` saying which shape it is. The
      same goes for the `:missing-field` a group pre-flight throws, whose `:waiting` is
      index -> check -> fields. Keyed by index because two drafts holding identical data are
      `=`, and a handler that reads `:failures` without looking at `:bulk` would read a
      position as a check name. `bulk-failed!` and `missing-values!` are the only two places
      this lives. Folded into the exceptions-documentation discussion 2026-09-16.

- [x] **E4 — the refusal has no row in `SPEC.md`'s error table.** Every other throw here
      has one; the draft-only write is described in prose ("makes direct `assoc` and
      `merge` on a sealed value throw") and named nowhere. It is `::io-limited`, after the
      option that caused it, which is what a handler would want to dispatch on. Its
      ex-data carries `:key` (the first offender), `:keys` (all of them, in field
      declaration order, since a merge is one write and the order its entries arrived in
      means nothing), `:checks` (offending key -> the IO-bound checks reading it) and
      `:instead` (a `drafting` form). `io-limited!` is the only place this lives. 2026-09-16: `:io-limited` is in the README table; the spec table's future is in the exceptions-documentation discussion.

- [ ] **E4 — an option must be written as a literal boolean, which `SPEC.md` does not
      say.** `:options` is read while the macro runs, so `{:io-limited some-config}` or
      `{:io-limited (io-enabled?)}` would be on regardless of what it later evaluated to,
      `false` included -- a declaration meaning one thing to the macro and another to
      whoever reads it, which is the silent mis-declaration everything else here is
      arranged to prevent. `parse-definition` refuses anything but `true` or `false` as
      `::malformed-option`. If a future option wants a non-boolean value, that check is the
      thing to widen, not to delete.

- [x] **E4 — `:io-limited` appears in no example, no demo and no README.** The example
      model has no io-dep to limit (E1's deferral), so declaring the option there would say
      nothing; `SPEC.md`'s own `Heist` declares it. F1 owns all three and should declare it
      on `recognitum.heist` when it gives that type its io-deps, and show the refusal beside
      the `drafting` form that is the way the write is spelled instead. The README's error
      table wants a `:io-limited` row (and `::malformed-option` a mention) whenever it is
      rewritten -- it is still listing `:invalid-field` and `:invariant-violated`, which B1
      deleted. `recognitum.io-limited-test` is the worked example until then. Done in F1:
      `Heist` declares the option, the demo shows the refusal beside the `drafting` form
      its `:instead` proposes, and the README's error table has the row, with the
      compile-time family of errors named in the paragraph under it.

- [x] **E3 — bulk checking appears in no example and in no README.** F1 owns the README and
      the demo, and its entry already says the demo should illustrate an io-dep; a batched
      fetch over a group is the only place the batched form pays for itself, so that is what
      it should illustrate with. `recognitum.bulk-test` is the worked example until then.
      Done in F1: the demo runs `certify-all` over three drafts sharing two crews, prints
      what the fake source was handed -- one call, two tuples -- and then shows that
      sealing each of them afterwards fetches nothing at all.

- [ ] **F1 — the README's worked example and `dev/recognitum/heist.clj` are the same type
      written twice.** `recognitum.readme-test` evaluates every `defrecord` block in the
      README, so the example cannot rot into something that does not compile, and that is
      why it is self-contained: it carries its own four-name in-memory table rather than
      requiring `recognitum.roster`, because a README block requiring a dev-only namespace
      is not a thing a reader can type. Nothing keeps the two in step beyond both
      compiling, so if they diverge in substance the suite will not notice.

- [ ] **F1 — `unsealed` is documented but demonstrated nowhere.** The README's "Protocol
      implementations" section writes it out and `recognitum.protocol-test` covers it, but
      no method on `Heist` opts out of the seal, so the demo has nothing to print. A method
      handing back a free draft would be the example -- `begin-revision`, in the shape the
      README already gives.

- [ ] **F1 — the `:test` alias now carries `bench` and criterium.**
      `recognitum.example-test` runs the hand-rolled side of the benchmark against the
      library's and insists the two agree on the same values, throws included, which is the
      only mechanical statement of "apples to apples" available. That means loading
      `recognitum.bench`, which requires criterium. Nothing in the suite calls
      `quick-benchmark`, so the cost is one dependency on the test classpath and nothing at
      run time.

- [ ] **I1 — one verb leaves one refusal, so `::unknown-certificate` no longer exists.**
      `check` takes a check name or a certificate name and cannot tell which was meant, so a
      name the type declares neither of is one error rather than two: `::unknown-check`,
      carrying `:check`, `:checks` and now `:certificates`, so the message says what it
      could have been given of either kind. The step's brief named both keywords; there is
      no call site left that knows which one a caller intended, and two refusals for one
      question would have to guess. Recorded rather than assumed: if the keyword should be
      neutral -- `::unknown-name`, say -- `ids-for` is the only place it lives.

- [ ] **I1 — a mixed group's `:record` is a vector of type names.** A group of one type
      throws `:record 'Ledger` exactly as before. A group of several has no single type to
      speak for the rest, so `:record` is the names of all of them in first-appearance
      order and the message reads the same way. `group-name` is the only place this lives.
      A handler that dispatched on `:record` being a symbol now has a second shape to
      consider; nothing in the suite or the documents did.

- [ ] **I1 — `recheck` does not re-run the gates it waits on.** What was asked for is these
      checks, not their prerequisites, so a gate the mask already calls established is not
      run again -- which means a `recheck` of an IO-bound check re-fetches and re-judges but
      does not re-confirm the shape of the field the fetch names. That is the narrow reading
      and the cheap one; the alternative, taking the whole closure by force, would make
      `(recheck d some-io-check)` cost the same as `(recheck d every-check)` on most types.
      A gate that has *not* passed still stops the check behind it. `plan-for` is where the
      reading lives, in the one line that computes `pending`.

- [ ] **I1 — the deferred `README gaps from the exception audit` item is now partly stale.**
      It asks for `:unknown-certificate` to be named in `certify`'s docstring; neither the
      keyword nor the function exists. `:not-a-draft`'s replacement, which the same item
      asks about, is `:not-a-value` -- thrown by every checking and query function for
      something that is neither a draft nor a sealed value, and by the group forms with the
      offender's `:index`. `:malformed-batch` and `:incomplete-batch` are still undocumented.

## Blocking

- [x] `dev/recognitum/demo.clj:60` references `recognitum.core.Draft`, which does not
      exist — drafts are generated per model (`recognitum.heist.HeistDraft`). The namespace does
      not compile. It is the only file that fails to load.
  - [x] Once it loads, confirm the rest of the demo still says what it claims to say. It has not
        run since drafts became per-model, so nothing downstream of line 60 is verified.

## Drift: resolved, pending fix

Ground truth below is galdre's, given 2026-09-15. The code is correct in every case; the prose
is what needs to move.

### 1. Field predicates must not run on draft writes — SPEC

Code is correct. A draft is unvalidated by definition; `seal` is where validation happens.
A previous agent deviated from spec here, was made to fix the code, and left the prose behind.

- [x] `src/recognitum/core.clj:4-7` — ns docstring claimed predicates run on every
      write "including inside a draft". Delete the claim.
- [x] `README.md:35` — "They run on every write to that field." Scope it to validated values.
      `README.md:87` already states the rule correctly.
- [x] `dev/recognitum/demo.clj:50` — section header `--- per-field checks still fire INSIDE a
      draft ---`. The demo demonstrates the opposite of its own heading.
- [x] `CONSIDERATIONS.md:156-158` — **deleted the rationale.** It explains the deferral as a
      performance fix ("Deferring them to seal fixed the quadratic in a loop"). That is not why.
      The deferral is the spec; the quadratic is a side effect.
  - [ ] Deferred. `CONSIDERATIONS.md` is scratch for polishing later, not a deliverable — it is
        kept but not maintained, and other invented rationale likely remains in it. Do not trust
        it; re-derive anything you need from the code.

### 2. `seal` — BY DESIGN, and the contract is extensional

A value returned from `seal` is validated exactly as if every field predicate and every
invariant had been rerun. That equivalence is the guarantee. Checking only touched keys is an
implementation strategy that preserves it, not a weakening of it:

- an untouched field still holds the value it had in the validated source, which already
  satisfied its predicate;
- an invariant none of whose keys were touched reads only unchanged values, so its truth value
  cannot have moved.

Code is correct (`validated_record.clj:190-192`): field predicates for the dirty keys, then the
invariants depending on them. `:draft-dirty-keys` is the touched set.

- [x] `src/recognitum/core.clj:181-182` — the comment described the strategy
      ("values were checked by the write that set them") and never states the guarantee. It is
      also false on its own terms, since draft writes check nothing (see #1). Replace it with
      the equivalence.
- [x] The equivalence holds only if every invariant declares its `:keys` accurately. Ruled: add
      a check, off by default. `*audit-invariant-keys*` runs each invariant against a proxy
      recording its key reads and throws `:undeclared-invariant-key` on one it does not declare.
      Zero cost when off; `README.md` has a "Checking your `:keys`" section.
  - [x] `recognitum.audit-test` exercises it: the silent hole, the catch at construction, an
        honest type passing, a whole-map read abstaining, and the binding not leaking.
  - [x] `clojure -X:test-audit` runs the entire suite under the audit, so every type it builds
        is checked. Verified non-redundant: mis-declaring `Heist`'s "only crew get a cut" passes
        the plain suite with 0 failures and fails the audited one in 3 places.
- Not drift: `CONSIDERATIONS.md` §14's note that seal rechecks a key written back to its
  original value is a fair efficiency observation. The contract is extensional, so a cheaper
  sound strategy is permitted. (An earlier draft of this file called it inverted. It is not.)

### 3. `dissoc` — SETTLED

Declared fields stay permanent: `dissoc` refuses them and the error proposes the rewrite.
Undeclared keys dissoc normally. Refuse-vs-replace decided in favor of refuse — callers do the
rewrite themselves, and the message tells them how.

- [x] Declared field throws `:cannot-dissoc`, carrying *"use (assoc m :cuts nil) if its
      predicate admits nil"* and an `:instead` form in the ex-data (`no-dissoc!`).
- [x] Undeclared key removes from a draft's ext map, and is the ordinary map no-op on a
      validated value, which cannot be holding one. `clean` still strips in bulk.
- [x] `README.md` prose, the `:cannot-dissoc` row, the `clean` section, and the demo heading
      all match the behavior.

### 5. `defvalidated` renamed to `defrecord` — SETTLED

- [x] The macro generates the same three things `clojure.core/defrecord` does — the type,
      `->Type`, `map->Type` — under the same names, so it now carries the name too. The library
      excludes `clojure.core/defrecord`; consumers alias rather than refer, and both kinds
      coexist in one file (`bench/recognitum/bench.clj` has one of each).

### 4. Printing and reading — SETTLED

The old objection is void. `CONSIDERATIONS.md` §8 said omitting a reader literal "closes a
bypass"; that was true when `drafting` was the only validating constructor. `map->Type`
validates, so a literal routed through it is a validating entry point, not a bypass.

- [x] `#Heist{…}` was a non-namespaced tag — reserved for Clojure — that did not read. Now
      `#recognitum.heist/Heist{…}`, qualified by the defining namespace, which both escapes the
      reserved space and escapes the reader's record-literal path. (A tag whose *name* contains
      a dot goes to `RecordReader`, which calls a static `create` that only `defrecord`
      generates; `*data-readers*` is never consulted for those.)
- [x] `data-readers` returns `{tag -> validating constructor}` for `clojure.edn`. Reading is
      validating: a bad literal throws `:invalid-field` / `:invariant-violated` at read time.
- [x] No global registration, and not for lack of trying. `clojure.edn` never consults
      `*data-readers*`; and `clojure.main` thread-binds that var, so `alter-var-root` at load
      time is invisible to the loading thread and visible to every other one — measured, not
      assumed.
- [x] Drafts print `#<Heist draft {…}>`. `#<` is the reader's own unreadable marker.
- [x] Dropped `:ctor` from the model — unused, and an unvalidated constructor reachable through
      `(:ctor (model-of x))`. Third back door found and closed. `:unwrap` was dead too.

## Stale

- [x] Four benchmark files collapsed to one. `bench2` measured a draft implementation that no
      longer exists (dirty set, map backing); `bench3` duplicated `bench4`'s batch shapes with a
      baseline that did no validation; `bench.clj` compared against an unvalidated defrecord,
      which is not a question anyone is asking. Deleted. `bench4` renamed `recognitum.bench`.
- [x] `README.md` performance table regenerated on this machine, with CPU, JVM, OS, date, and
      the command recorded, plus a note that run-to-run spread is real.
- [x] Dropped README's closing advice to hold a draft open across a tight loop. Measured, it is
      backwards: `drafting` does not break even against direct `assoc` on a bare field until
      ~N=40 (N=5: 297 vs 625 ns; N=50: 2897 vs 2629 ns). And for a field carrying invariants the
      comparison does not exist — direct `assoc` throws, which is the whole reason drafts exist.

## Dead references

- [x] Dropped `clojure.set` and `clojure.string` from `src/recognitum/core.clj`.
- [x] Dropped the unused `draft`/`seal` refers from `demo.clj`, `heist.clj`, and `bench.clj`,
      and the unused `(:import (recognitum.heist Heist))` from `demo.clj`.

## Unverified claims

- [x] `clean` and `:extraneous-keys` are now covered by `recognitum.draft-test`, along with the
      `dissoc` rules and the unset-vs-nil distinction.
- [x] A `drafting` body returning a sealed value threw a bare `NullPointerException`. Ruled: it
      is not a mistake. `seal` is now idempotent — an already-sealed value passes through, so
      `seal` is a projection and a sealed value is a fixed point of it. Anything that is neither
      a draft nor sealed throws `:cannot-seal` with a real message. Added `sealed?` alongside
      `draft?`.
  - [ ] Open: whether there should also be a throwing variant of `seal` that refuses an
        already-sealed value. Argument for one: a body that ignored its draft binding and
        operated on the original — `(drafting [x job] (promote job ...))` — now returns a
        plausible-looking result with the draft's work silently discarded, and nothing says so.
        A dev-time warning was floated as an alternative. Undecided.

## Post-migration

- [x] `clojure.test` suite: 32 tests, 267 assertions, in three namespaces. Verified to have
      teeth by mutating the library — dropping seal's field checks, dropping invariant runs on
      writes, un-refusing extraneous-key detection, and silently disabling the audit are each
      caught. Now 32 tests / 270 assertions, and `clojure -X:test-audit` runs them all under the
      invariant `:keys` audit.
- [x] `dev/recognitum/ownership_test.clj` deleted. Everything it showed was already covered by
      `recognitum.draft-test` except construction from an explicit draft of the type — a case its
      own comments flagged as a past regression — which is now folded in.
- [ ] Namespace root is still `recognitum.*`. Decide the real name before anything depends on it.
- [x] Now a git repo. Nothing before the migration has history, so an agent reading the early
      state cold could not tell spec from accident, and reconstructed a rationale rather than
      admit it did not know. That is how the invented performance rationale got written.

- [x] **D4 — the metadata keys are documented only in docstrings.** `README.md` says
      nothing about `:recognitum.core/passed` or `:recognitum.core/certificates`, or about
      `meta` answering the same way from a draft and from a sealed value. F1 rewrites the
      README and should give them a section; `dev/recognitum/demo.clj` shows drafts and
      sealing without ever printing what a draft has established, which is the one place
      the uniformity is visible at a glance. Done in F1: "Metadata", under "What a draft
      has established", and the demo prints `meta` of a sealed value and of a draft one
      write old, one after the other.
