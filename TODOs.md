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

- [ ] **A1. Namespace-qualify field keys.** `:recognitum.heist/name` rather than `:name`, derived
      from the defining namespace. Touches the macro's key generation, the `IKeywordLookup`
      thunks, `map->Type`, the print form (which drops the namespace the tag carries) and the
      reader (which accepts bare or qualified keys). Mechanical but wide: every test, the demo,
      the bench, the README. Doing it first keeps it out of the way of everything else, and the
      `:checks` map needs it, since its keys are the field keys.

### B — the check model

- [ ] **B1. The `:checks` map.** Named checks, arglist-as-dependency, the bare-function shorthand
      over a single field, and the optional docstring position. Exactly one arity. Replaces
      `:valid` metadata and the invariants vector. The field → checks index comes from arglists
      rather than declared `:keys`.
- [ ] **B2. Compile-time resolution.** A symbol that is not a declared field, a keyword that is
      not a declared io-dep, a name used for both a check and an io-dep — each a compile error
      naming the offender. (Cycles wait for E1, when io-deps can name each other.)
- [ ] **B3. The check contract.** `true` passed, anything else failure data. Exceptions
      propagate rather than becoming failures. Every entry point runs everything it was asked to
      and collects all failures, keyed by check name in declaration order, built with
      `array-map`.
- [ ] **B4. Delete the audit.** `*audit-invariant-keys*`, `KeyAuditor`,
      `undeclared-invariant-key!`, `recognitum.audit-test`, `recognitum.audit-fixture`, the
      `:test-audit` alias, and the README section. Dependencies derived from arglists cannot be
      misdeclared, so there is nothing left to audit. Do this only once B1 lands — until then the
      audit is the only thing catching what B1 makes impossible.

### C — the definition form

- [ ] **C0. `of-type?`.** True for either form of a registered type; false for anything
      unregistered, including the type object itself. Needed before C2, since a wrapped body
      receives a draft and `instance?` stops meaning what an author expects.
- [ ] **C1. The definition map.** Wrap the checks in `{:options … :checks … :io-deps …
      :certificates …}`, all sections optional. Only `:checks` is populated at this point.
- [ ] **C2. Protocol implementations.** One body installed on both types. The draft impl runs it
      as written; the sealed impl opens a draft, runs it, and seals the result only if the body
      returned a draft of this type — anything else passes through, which is most protocol
      methods. `unsealed` opts a return out of sealing, is unwrapped by both impls, and throws on
      a non-draft. `drafting` does not honour it.

### D — drafts and verdicts

- [ ] **D1. The verdict mask.** An `^:unsynchronized-mutable` field on the draft. Set on pass,
      clear on failure. `assoc` seeds the new draft by `and`-ing the parent's mask against the
      invalidation mask for the written field. Snapshot, so versions diverge.
- [ ] **D2. `check` / `check!`.** Return `true` or failure data; none return a draft. Running a
      check runs whatever it depends on that is not already passing.
- [ ] **D3. Certificates.** The `:certificates` section, holding derived from the mask, and the
      built-in certificate covering every check. `certify` / `certify!`.
- [ ] **D4. Uniform `meta`.** Library keys merged over the user's, same answer from drafts and
      sealed values.
- [ ] **D5. `seal` consults the mask.** Run only what is not passing; collect every failure and
      throw once. Subsumes the current dirty-key logic.

### E — IO

- [ ] **E1. `:io-deps`, singular form.** Fake the IO in `dev/` — an in-memory table standing in
      for a database is enough, and nothing here should need a real one to be exercised or
      tested. Arglists as everywhere else, io-deps nameable by other
      io-deps, topological fetch order, cycles rejected at compile time. The gate: an io-dep
      fetches once the pure checks over the fields it names have passed and every io-dep it names
      has fetched. Fetched values cached for one validation call and no longer. Argument tuples
      deduplicated.
- [ ] **E2. Batched fetches.** `[[crew] :as rows]`, told from `[crew]` by the reader because it
      is not a legal arglist. Returns a map keyed by argument tuple.
- [ ] **E3. `check-all` / `certify-all`.** Over a collection of drafts, results index-aligned.
      Partial success on the non-throwing forms only. Nothing bulk produces sealed values.
- [ ] **E4. `:io-limited`.** Direct `assoc` and `merge` on a sealed value throw for any field an
      IO-bound check depends on; other fields unaffected.

### F — reconcile

- [ ] **F1. README, demo, bench** to the new syntax. The demo should illustrate io-deps against
      the faked in-memory source from E1: a fetch resolving, the gate refusing to query on a
      malformed value, deduplication, and a check judging the result. The bench's hand-rolled side needs the same
      checks written out by hand again, or the comparison stops being apples-to-apples.
- [ ] **F2. Re-measure and re-record the performance table.** Checks called with direct field
      access rather than keyword lookup is the change most likely to move it.

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
