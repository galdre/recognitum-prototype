# TODOs

Drift inventory taken 2026-09-15, before the project layout existed. Every observation below
was verified by running the code.

The four drift items have since been adjudicated by galdre; their rulings are recorded inline
below. In every case the code is right and the prose is wrong, which is worth noting: the docs
drifted off the code, not the reverse.

## Blocking

- [x] `dev/prototype/demo.clj:60` references `prototype.validated_record.Draft`, which does not
      exist — drafts are generated per model (`prototype.heist.HeistDraft`). The namespace does
      not compile. It is the only file that fails to load.
  - [x] Once it loads, confirm the rest of the demo still says what it claims to say. It has not
        run since drafts became per-model, so nothing downstream of line 60 is verified.

## Drift: resolved, pending fix

Ground truth below is galdre's, given 2026-09-15. The code is correct in every case; the prose
is what needs to move.

### 1. Field predicates must not run on draft writes — SPEC

Code is correct. A draft is unvalidated by definition; `seal` is where validation happens.
A previous agent deviated from spec here, was made to fix the code, and left the prose behind.

- [x] `src/prototype/validated_record.clj:4-7` — ns docstring claimed predicates run on every
      write "including inside a draft". Delete the claim.
- [x] `README.md:35` — "They run on every write to that field." Scope it to validated values.
      `README.md:87` already states the rule correctly.
- [x] `dev/prototype/demo.clj:50` — section header `--- per-field checks still fire INSIDE a
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

- [x] `src/prototype/validated_record.clj:181-182` — the comment described the strategy
      ("values were checked by the write that set them") and never states the guarantee. It is
      also false on its own terms, since draft writes check nothing (see #1). Replace it with
      the equivalence.
- [x] The equivalence holds only if every invariant declares its `:keys` accurately. Ruled: add
      a check, off by default. `*audit-invariant-keys*` runs each invariant against a proxy
      recording its key reads and throws `:undeclared-invariant-key` on one it does not declare.
      Zero cost when off; `README.md` has a "Checking your `:keys`" section.
  - [x] `prototype.audit-test` exercises it: the silent hole, the catch at construction, an
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

### 4. `#Heist{…}` prints like a readable literal but is not — FUTURE WORK

Ruled: acknowledged, deferred. `CONSIDERATIONS.md` §8 already records the absent reader literal
as deliberate.

- [ ] Future work, not drift: either a real reader literal, or a printed form that does not
      imply one.

## Stale

- [x] Four benchmark files collapsed to one. `bench2` measured a draft implementation that no
      longer exists (dirty set, map backing); `bench3` duplicated `bench4`'s batch shapes with a
      baseline that did no validation; `bench.clj` compared against an unvalidated defrecord,
      which is not a question anyone is asking. Deleted. `bench4` renamed `prototype.bench`.
- [x] `README.md` performance table regenerated on this machine, with CPU, JVM, OS, date, and
      the command recorded, plus a note that run-to-run spread is real.
- [x] Dropped README's closing advice to hold a draft open across a tight loop. Measured, it is
      backwards: `drafting` does not break even against direct `assoc` on a bare field until
      ~N=40 (N=5: 297 vs 625 ns; N=50: 2897 vs 2629 ns). And for a field carrying invariants the
      comparison does not exist — direct `assoc` throws, which is the whole reason drafts exist.

## Dead references

- [x] Dropped `clojure.set` and `clojure.string` from `src/prototype/validated_record.clj`.
- [x] Dropped the unused `draft`/`seal` refers from `demo.clj`, `heist.clj`, and `bench.clj`,
      and the unused `(:import (prototype.heist Heist))` from `demo.clj`.

## Unverified claims

- [x] `clean` and `:extraneous-keys` are now covered by `prototype.draft-test`, along with the
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
- [x] `dev/prototype/ownership_test.clj` deleted. Everything it showed was already covered by
      `prototype.draft-test` except construction from an explicit draft of the type — a case its
      own comments flagged as a past regression — which is now folded in.
- [ ] Namespace root is still `prototype.*`. Decide the real name before anything depends on it.
- [x] Now a git repo. Nothing before the migration has history, so an agent reading the early
      state cold could not tell spec from accident, and reconstructed a rationale rather than
      admit it did not know. That is how the invented performance rationale got written.
