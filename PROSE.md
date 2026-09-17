# Prose standard

Every human-readable string in this repository is held to this standard: docstrings, body
comments, exception messages, assertion messages, `testing` strings, section headers, and the
names of functions, bindings, keywords, and fixtures. The README is held to the general and
prose-slop sections.

## Writing Docstrings

A docstring is a contract for the *caller*, not a diary for the
implementer. It states what the function does, what it takes, returns,
throws, and the preconditions/invariants the caller must respect. Those
guarantees and requirements *belong* there — they are exactly what the
caller needs surfaced in the IDE.

When you find implementation context in a docstring, the default is to
**relocate it, not delete it** — move it to an inline comment at the
point in the body where it is actually relevant. That context is often
genuinely valuable; it is just in the wrong place (the caller should not
have to read it; the implementer standing at that line should). Delete
outright only when it is blather: self-congratulation, restating the
obvious, or documenting a property that is the expected default.

On that last case — narrating properties like "portable across all
supported appdbs" earns no sentence. If it were not portable, that is
either a bug, or it means callers must handle each case themselves — and
in *that* case it is the *absence* of the property that must be
documented. Document deviations from expectation, not conformance to it.

Heuristic — a sentence earns its place in the docstring only if it
passes both tests:

1. Rewrite test (necessary, not sufficient): if rewriting the body to a
   different implementation with an identical contract could make the
   sentence false, it describes the implementation, not the contract.
   Relocate it to an inline comment at the point in the body where it
   applies.

2. Ownership test: if the sentence states a fact about code this
   function does not own — a callee's behavior, a library's guarantee —
   it survives any rewrite trivially, which is exactly why the rewrite
   test alone is not enough. If this body *relies* on that fact, it is
   implementation context: relocate it to the call site. If nothing here
   relies on it, it is blather about someone else's code: delete it, and
   let the callee's own docstring answer for it.

Say each fact once. Duplicated prose is a stale comment in advance: when
the fact changes, one copy gets updated and the rest quietly rot. This
holds even within code you own — a namespace docstring that
re-summarizes its own multimethods, three sibling functions carrying the
same paragraph about retry behavior. Give the fact one home, and have
the other places name it rather than restate it.

Multi-line docstrings are not banned — a genuinely non-obvious constraint
the code had to deal with can be worth explaining. But be prudent; the
failure mode is far too much detail. When tempted to write a
multi-paragraph explanatory docstring, check with the user first. And
prefer a *test* to prose: if a future reader thinks "that's a silly way
to do it" and changes it, a test should fail and tell them why. If that
breakage keeps happening, *that* is the signal a comment was warranted.

## All text in code

These principles apply to **any human-readable text in the code**: docstrings,
body comments, exception messages, log lines, assertion messages, testing
strings, section headers, function and variable names, let-binding names,
keyword names, fixture names — anything a human reader will encounter.

### Audience awareness

Each piece of text has a different audience based on where it appears:

- **Body comments and inline notes**: maintainers reading the source of *this module*. Highest context — the surrounding code defines vocabulary.
- **Public-API docstrings (`defn`)**: callers from anywhere, possibly outside this module entirely. Should be self-contained — assume zero knowledge of internal structure.
- **Private-fn docstrings (`defn-`)**: maintainers of this module, but someone reading just the docstring should still understand the contract without spelunking.
- **Test descriptions and assertion messages**: developers debugging test failures. Medium context — the deftest name and surrounding test code give some grounding.
- **Operator-facing strings (logs, exception messages)**: zero context. These bubble up to log files, dashboards, and support tickets. Plain language only.

Stricter standards apply as the audience moves further from this module's
source.

### General principles

These apply across all surfaces.

- **Don't presume internal-jargon knowledge.** If text mentions a concept ("phase 4", "the initial_sync_status flip", "patch-on-match policy"), the reader needs to be able to understand it from context. If they can't, describe by structural condition or cross-reference an actual symbol.
- **Spell out the inference, don't just state the premise.** "Already correct on the matched row — it's part of the match key" works; "part of the match key" alone doesn't.
- **Don't over-compress.** The flip side: trimming "natural-key match already proves the matched row has the right parent" down to "part of the match key" loses the WHY. Restore the inferential step in tighter language, not the original prose.
- **No caller-leak.** A function (or other unit) shouldn't describe what its *caller* does. Each unit describes its own contract; how the caller responds is the caller's concern.
- **No conversation-leak.** No first-person ("we", "I"), no second-person ("you'll see"), no references to past discussions ("as agreed", "per the user's preference"). Text reads as a standalone artifact, not as a turn in a dialogue.
- **Verify the description still matches the code.** When a body changes, re-read the docstring end-to-end — don't just touch up the obviously affected sentence. Stale-but-confident descriptions are worse than missing ones: they mislead readers who trust the contract.

### Atemporal

Text in the code reads from now-forward. Strip:

- **Temporal references** (`2026-04-29`, `post-pivot`, `pre-pivot shape`). Exception: see below.
- **Personal names** (`John Doe's rework`).
- **Planning-doc references** (`§11c`, `METADATA_FILE_IMPORT_PLAN.md`).
- **Work-item references** (`item 6`, `item 13`, `Sub-project A/B`).
- **Pivot-history framing** ("Under the portable-id design", "no longer carries an integer :id").
- **"Considered, not pursued" content.** Multi-paragraph parked-implementation rationales belong in design docs or commit messages, not in code.

OK to keep:

- **Brief, atemporal TODOs and future-optimization notes.** "TODO: batched-grant fix." or "Future optimization: bulk-insert per depth level." are fine. In these cases and these cases only, inline dates should be used.

### Vocabulary (jargon out, structure in)

- **Drop project-specific labels** — "Convention A / Convention B / Conv-A / Conv-B" — everywhere. Describe by structural condition: `parent_id non-NULL`, `nfc_path non-empty`, `JSON-unfolded leaf`, `flat root`. The canonical place for labeled distinctions is the function whose branching defines them.
- **Drop policy labels** ("patch-on-match policy", "clobber-on-match policy"). Describe what happens: "On match, source description overwrites target's" / "Payload that overwrites a matched field row".
- **Drop "Phase N — " prefix from docstrings.** Function names and section headers can keep `Phase N` as navigation aids; docstrings shouldn't repeat it.
- **Drop "phase-N" inside text where structural language works** — and especially in operator-facing strings (logs, exceptions). Replace with the function/symbol name or a structural description.
- **Names propagate the vocabulary the furthest.** A function or binding named `conv-a-tree-rows` carries the label everywhere it's called. Renaming `(:conv-a-pct, :conv-b-pct)` → `(:nested-pct, :json-leaf-pct)`, `cb-leaf` → `leaf`, etc., is part of the cleanup, not deferred as architectural.

### Docstrings

- **Describe what the function does and its contract; never implementation detail.** Implementation goes in body comments, scattered next to the relevant code.
- **Drop weak cross-refs.** A double-bracket docstring cross-ref is fine when it pulls weight, but don't editorialize about what's there ("see that function's docstring for the three cases" — the function doesn't know how many cases it has).
- **Default to invariant-plus-crossref over verbose proofs.** State the invariant and cross-ref the function whose semantics make it true; trust the reader to verify by inspection. Multi-step proofs are OK only when the invariant alone wouldn't convince a careful reader — typically when correctness depends on several cases collapsing to the same form and the collapse isn't obvious from the code.

### Operator-facing strings (logs, exceptions, errors)

These bubble up to log files, error reports, and dashboards — where the
reader has zero context about the codebase.

- **Plain-language, grounded.** No internal jargon, no project-specific labels, no `:keyword/concepts` from the source. The string should describe the actual problem in terms that mean something to a reader outside the code.
- **Include the actionable data inline.** Whoever sees the message should be able to act on it without code-spelunking.
- **Same-strictness on log lines.** Log statements that fire in operator-visible channels are subject to the same standards as exception messages.

### Test files

- **Tighten testing strings.** Deftest names already do most of the work — testing strings should add brief context, not act as full specs.
- **Drop redundant assertion narration.** If the deftest name says "rejects integer db_id", the assertion message doesn't need to repeat the rejection target.
- **Test-file section headers can hold context — but only when it adds something the function definitions below don't.** Default to bare headers (`;;; ======== process-fields! ========`); add prose only when it points at something a reader couldn't infer from the deftests themselves.
- **Rename tests, fixtures, and vars that carry old vocabulary.**
  - `process-fields-convention-b-idempotent-test` → `process-fields-json-unfolded-leaf-idempotent-test`.
  - `fld-conv-b-db` → `fld-json-leaf-db`.
  - `cb-leaf` → `leaf`.

### Other code

- **Small style fixes are in scope when obvious.** `cond` with one guard + `:else` → `if-let`/`when`.
- **Section header comments can lose planning vocabulary** when it's not navigation-load-bearing. `;;; phase-3 single-pass with stubs ===` → `;;; fields (batch) ===`.
- **Parked / unused code is a separate decision.** Cleanup removes the *rationale prose* but doesn't decide whether the code stays. Flag parked variants and dead code paths as items requiring user judgment: keep with atemporal rationale ("alternative reference implementation showing X tradeoff"), delete entirely, or replace with a short pointer to git history.

## Deslopping: discovery and taxonomy

### Discovery

Find candidates before proposing anything:

- Read each changed namespace's **docstring** (the highest-value prose).
- `grep` the changed files for ALL-CAPS emphasis (`\b[A-Z]{3,}\b`, plus `NOT`,
  `AND`, `OR`, `NO`) in comment/`testing`/docstring lines — exclude genuine SQL
  keywords and acronyms (`NOT NULL`, `DROP TABLE`, `CSV`, `JSON`, `UUID`, …) and
  deliberately-malformed test SQL / string literals.
- For a thorough sweep, extract all comments + `testing` strings + docstrings to
  one scratch file and read it in a pass — cheaper than reading every full file,
  and it surfaces slop the grep misses.

### Candidate taxonomy

What to flag (each is a real find from past sessions):

- **Outward pointers** — a docstring/comment narrating other namespaces,
  callees, or the layer downstream: "see ns docstring", "the API layer maps
  these to HTTP", "consistent with X's taxonomy", caller name-drops
  ("e.g. used by sub-graph leaf detection"). Document the thing *itself*; the
  runtime error's metadata tells a developer the specifics.
- **Namespace docstring documenting a function** — an ns docstring re-stating a
  single function's error/cleanup/return contract. The ns orients (purpose,
  public surface, cross-cutting model); the function owns its contract. A
  single-public-fn namespace often reduces to a signpost.
- **Contract at the wrong altitude** — the return-shape/options block living in
  the ns docstring while the function points outward ("see namespace docstring
  for shape"), or vice versa. Move the contract onto the function; keep concepts
  in the ns.
- **Comment restating a callee's docstring** — at a call site. A comment earns
  its place only by a LOCAL "why" not readable from the code. If the names carry
  it, delete the comment.
- **Drift-bait / editorializing** — "by design", "kept in one place", literal
  values the code already shows, comparisons to other constants that go stale
  ("separately from the 240-minute timeout"). If single-sourcing matters, a test
  enforces it, not a comment.
- **ALL-CAPS shouting** — carry emphasis by word choice, not capitals
  ("does NOT throw" → "never throws").
- **Monologue cruft** — stream-of-consciousness "how do I test this"
  (`Easiest:`/`Simpler:`/`Strategy:`/`Simplest:` false starts) left in source.
  Trim to the one real reason.

### Slop classes in any prose, with examples

The taxonomy above is specific to code prose. These recur in every prose artifact — ticket drafts, review reports, READMEs, handoffs — and the test is always the same: does the sentence earn the reader's time?

**Every example in this section is invented.** Each imitates the shape of a real find without reproducing one; the endpoints, the numbers, and the quoted phrases are made up.

**The density target, calibrated.** State the mechanism in plain words before naming the component; cause before effect; one idea per sentence, each depending only on the one before it; citations support the sentence rather than replace it. Two versions of the same content, from an imagined ticket about a dashboard export endpoint:

> *Too much.* Proposed S1 / P2. In this project's scheme, S rates what the attacker must already hold, and P rates how reachable and how serious the result is. S1: the attacker needs an ordinary account, no elevated group membership, no API key — only the ability to open a shared dashboard. P2 rather than P1: the export includes the hidden columns only when the dashboard owner has enabled the "include all fields" option, described under Context. In the default configuration nothing new is disclosed. What the attacker receives is every column of the underlying table behind one of the dashboard's cards — the fields the card's own selection excludes — in the downloaded file.

> *Right.* Proposed S1/P2. The attacker needs an ordinary account. P2 because there is no exposure under default configuration. A viewer of a shared dashboard can export a card and receive every column of its underlying table, including columns the card does not display.

The second loses nothing the reader needs. What it drops: a rubric explained to someone who wrote it; a list of things the attacker *doesn't* need where "an ordinary account" says it; a rating suggested and rejected in the same sentence; and four words for every one.

**Presumed context.** Sentences that are true and unreadable because they assume the author's context. Four forms: project vocabulary used as if established ("S1/P2", "crosses the boundary", "the payload", "the calibration row"); forward references (a section that says "neither guard reaches this branch" before any branch has been introduced); mechanism as a chain of function names with no plain-language sentence saying what happens; pointers to project artifacts ("see the dive report"). Test: list every term the project coined; each is defined on first use, replaced with plain words, or the paragraph fails. Then check whether each sentence can be understood using only the sentences above it.

**Negatives and rejected alternatives.** Enumerating what is *not* the case — "no admin group, no API key, no signing secret, no session cookie" — when "an ordinary account" says it. Arguing in the body against a rating not chosen — "P2 rather than P1 because…" — when the body should state the choice and its one reason, and the alternatives belong in a note if anywhere. Both read as thoroughness and are padding.

**Fragments where sentences belong.** Telegraphic steps in which arrows, colons, labels, and code notation stand in for verbs:

> "Control, anonymous: `GET /api/export/42` → **403, 31 bytes**" · "Same dashboard, same card, `GET /api/export/42?all=true` → 200" · "`enable-dashboard-sharing true`. Publish a dashboard named X, one card over `orders`, `SELECT …`"

Test: does the step have a subject, a verb, and an object? Could someone follow it without knowing what the arrow means? Rewrite as "As an anonymous client, request `GET /api/export/42`. The server returns HTTP 403 with a 31-byte body: `{…}`. This is the control: the export refuses a caller with no session." Longer by a few words; readable by anyone.

**Emphasis with no budget.** Bold marks one thing as more important than its neighbours. Bold on every paragraph's lead sentence marks nothing, and a bolded forty-word sentence is a font change applied to a paragraph. Count the emphasis events in a section; more than roughly one per screen and the hierarchy is gone. Same for stacked treatments: bold lead-ins plus inline bold plus italic quotes in six lines.

**Private vocabulary.** A self-coined metaphor presented in the register of established jargon: "the export half", "the split seam", "a green guard", "the value domain", "the table graph". If the reader cannot look it up, define it inline or say it plainly. Watch for one word carrying two referents ("column" for a warehouse column, a markdown table column, and a value list inside fifty lines). The everyday form is the metaphorical verb: code that *hands*, *funnels*, *drops*, *collapses*, *survives*, *puts X on the caller*. Exotic coinages get caught; ordinary metaphors slip. Replace with what the code does: passes, calls, omits, returns, remains, states the caller is responsible.

> "its docstring puts selecting it on the caller" → "its docstring states the caller is responsible for selecting it" · "all three routes funnel through this path" → "all three routes call this function"

**Reader-instructions.** State the claim; do not direct the reading. "do not argue it upward" · "no rating should be argued past it" · "this is the first question a triager will ask".

**Pre-empting the reader's reaction.** Condescending even when accurate. "which a triager will find first" · "since a reviewer will reach for it".

**The "not X but Y" antithesis tic.** Six or seven instances in a 900-word draft reads as anxiety. Keep the positive half. "established rather than suggested" · "untested rather than chosen" · "stated rather than netted" · "recorded and not taken". Exemption: when it marks a distinction the surrounding standard defines, it is load-bearing.

**Sentences explaining why the sentence exists.** Cut with no information loss. "…so they are not re-derived" · "…and are kept in one ticket for that reason".

**Announcing an insight before showing it.** Show it. "**The export path discards that check, and the inversion is the point.**"

**Bolding the objection instead of the conclusion.** Actively dangerous: a skimmer reads the opposite of the finding. "**The dashboard UI already shows these columns, so this is not an API bypass.**" — bolded, then refuted in the next sentence.

**Abstract-noun pileup.** No verb doing work. "the payload is data-derived stored values crossing a boundary the deployment configured".

**Compression into opacity.** Shorthand only the author can expand. "configuration goes on P" · "which bounds the control column, not the claim" · "the two arrive through the same call".

**The punchline.** A short terminal sentence that sounds like a conclusion and states none — usually a definite article on a noun the text never defined, or a metaphorical verb standing in for the claim. It reads as decisive; that is the problem, because editors reward it. "these three routes are the gap" · "the inversion is the point" · "the disclosure belongs to the export". Test: can the reader write it as subject-verb-object without the surrounding paragraph? If not, it is drama. Fix by supplying the predicate: "two places in the tree refuse archived dashboards; these three routes do not."

**Prior rulings by paraphrase.** Give the identifier. A reader cannot check "the accepted verbose-error ruling".

**Harness terms leaking out.** "The coordinator", "driven", "the tool pass". Say what happened: "I ran these as anonymous HTTP requests against a local instance."

#### Two traps for the editor

**You share the author's context, so presumed context is invisible to you.** You know what "crosses the boundary" and "the calibration row" mean, so you read past them. One pass called a body "competent" when a reader outside the project could not have followed its mechanism section at all. The fix is mechanical: for each paragraph, write down the project-coined terms before judging the prose. If the list is not empty and the terms are not defined in the paragraph, the prose fails regardless of how it reads to you.

**Clean citations are not clean prose.** A draft whose every `file:line` resolves earns trust, and that trust leaks into how you read its sentences. One pass called a paragraph "clean" that held five slop classes in five sentences, because the citations around it were flawless. Verify the two separately. The same dulling happens across a long session: after several turns of structural review, the sentence-level ear degrades and you read for shape. Before the prose pass, reread this list, then read the body one sentence at a time. If a correction arrives after you have delivered, redo the whole pass rather than patching the one sentence named; a missed sentence is evidence the read was shallow.
