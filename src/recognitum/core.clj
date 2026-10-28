(ns recognitum.core
  "Types whose values carry proof that their checks passed.

  [[defproven]] defines a type from its fields and named checks. Its constructors run every
  check or throw, and so does every write to one of its values, so holding a value of the type
  is holding proof that the checks passed: `instance?` is the whole test. A **draft** is an
  unchecked map of a type, for values built in steps; [[seal]] checks it and returns the value.

  The vocabulary:

  - A **check** is a named body that passes by returning exactly `true`; anything else it
    returns is its failure data. Its id is a keyword in the namespace that defines it. A check
    named after a field is that field's **shape check**.
  - A check **waits** on the checks it names and on the shape check of each field it reads,
    and runs only once they have passed.
  - A **bundle** ([[defchecks]]) is a group of checks over named inputs, bound into a type by
    saying which field each input reads. [[defcopy]] copies one under new names.
  - A **concern** ([[defconcern]]) marks checks so that a selection can name them together.
  - A **selection** names some of a type's checks: a check id, a concern, a bundle, a bundle's
    check, or [[every]], [[shape]], [[pure]], [[fetching]], [[reading]], [[except]] and
    [[union]].
  - A **source** is a function from a vector of distinct keys to a map from key to answer.
    Check bodies read one with [[fetch]] and [[fetch-each]].

  The public API:

  - Define: [[defproven]], [[defchecks]], [[defcopy]], [[defconcern]], [[checks]].
  - Inside a check body: [[fetch]], [[fetch-each]], [[extant?]], [[when-extant]].
  - Build values: `map->T`, `->T` and the tagged literal `#ns/T {...}`, each generated per
    type; [[draft]], [[clean]], [[seal]].
  - Run checks: [[check]], [[recheck]], [[check-all]].
  - Ask: [[proves?]], [[proven-checks]], [[proven-concerns]], [[checks-of]], [[sealed?]],
    [[draft?]], [[data-readers]]."
  (:require
   [recognitum.body :as body]
   [recognitum.define :as define]
   [recognitum.engine :as engine]
   [recognitum.proof.key-class :as key-class]
   [recognitum.record :as record]
   [recognitum.select :as select]))

(def checks
  "Starts the section of a definition that holds its own checks."
  ::checks)

(def ^:private strategy
  "The proof strategy every type is defined with."
  key-class/strategy)

(def ^:private wiring
  {:strategy strategy :define #'engine/define :run-all #'engine/run-all})

(def ^:private expansion
  "What the defining macros give the record layer: the engine's compiler of bodies, the body
  macros by var as the kind of read each is, the var that starts a section of own checks, and
  the proof strategy, which answers a definition's refusal and its class's name."
  {:compile-body engine/body-form
   :read-macros `{fetch :fetch, fetch-each :fetch-each, extant? :extant?, when-extant :when-extant}
   :checks-marker `checks
   :strategy strategy})

;;; ------------------------------------------------------------------ definitions

(defmacro defproven
  "Defines the type `T`: its class, imported into this namespace, and `map->T` and `->T`,
  which run every check and return a value or throw.

      (rec/defproven Heist [name crew take cuts ^:optional getaway]
        rec/checks
        (name [] (non-blank? name))
        (cuts-sum-to-take [] (= take (reduce + (vals cuts))))
        (only-crew-get-cuts [cuts-sum-to-take] (every? crew (keys cuts))))

  Each field's key is a keyword in this namespace named after it. A field marked
  `^:optional` may be absent; every other field must be present.

  Sections follow the fields. `rec/checks` starts the type's own clauses, each
  `(name [waits] \"doc\"? body...)`. A bundle var starts a binding of that bundle: one
  `(input [] body)` clause per input of the bundle, each body an expression of the fields.
  `^{:when-extant field}` on the bundle var skips the binding's checks, as passing, where
  that optional field is absent.

  In a body the fields are locals. A read of an absent field throws, which fails the check;
  [[extant?]] and [[when-extant]] ask first. A failed read from a source throws too, and fails
  the check. A body may catch either. Anything else a body throws propagates out of the call
  that ran the check.

  A check waits on the shape check of each field its body reads, directly or through an
  input. A wait names any other check it waits on: an own check by its bare name, a bundle's
  check by its qualified var, or every check of a bundle by the bundle's qualified var, even
  where the bundle is defined in this namespace. A clause marked `^::c` carries the concern
  `c` that [[defconcern]] defined. A clause `^:override (bundle-ns/check [waits] body)`
  replaces that check of a bound bundle, keeping its id, its concerns and its binding's
  `:when-extant`.

  After the checks sections come the type's own protocols and interfaces, mixed, written as
  in `defrecord`: a protocol or an interface, then its methods, each `(method [this args*]
  body*)`. Of `Object`, only `toString`. In a method body the fields are locals, nil where
  absent, `this` is the value, and `T` is the value's class, `(class this)`, which takes no
  type hint. A method body may not call `map->T` or `->T`. Sealed values are instances of
  each interface and each protocol's interface, so a direct call of a method dispatches on
  the class; drafts are not. Type hints on the parameters choose among an interface's methods
  of one arity. Refused: a checks section after a method section, a method no interface of
  the section declares, a method more than one matches, `equals`, `hashCode`, and what the
  proof strategy refuses, such as a protocol method a map already has.

  The definition is one form: it defines at the top level and inside another form alike.
  Where a check or a method does not compile, or a section is refused, the definition is
  refused and leaves nothing: no class, no constructor, no var, and an earlier definition of
  `T` stays current.

  The shape check of an optional field passes where the field is absent. The tagged literal
  `#ns/T {...}` calls `map->T` once `ns/T` maps to it in `data_readers.cljc`.

  Evaluating the form again redefines `T` and displaces the earlier definition. Its values
  stay values of the earlier class, and they are not sealed: [[sealed?]] answers false, and a
  write that changes one, or a method of its protocols or interfaces other than `toString`
  called with one, throws, with `:error` `:recognitum/displaced`. A write that changes nothing may return the value unchanged.
  `(rec/seal (rec/draft v))` proves such a value under the current definition."
  {:arglists '([T [fields*] sections* method-sections*])}
  [tname fields & sections]
  (define/defproven &env tname fields sections
    (assoc expansion :wiring `@(var wiring))))

(defmacro defchecks
  "Defines a bundle: a var holding checks over `inputs`, and one var per check.

      (rec/defchecks collection-write [user collection-id]
        rec/checks
        (collection-exists [] (some? (rec/fetch db/collections collection-id)))
        (may-write [collection-exists] (db/can-write? user collection-id)))

  Clauses are those of [[defproven]], with the inputs in place of fields. Each check's id is a
  keyword in this namespace. Every input may be absent in a type that binds the bundle."
  {:arglists '([name [inputs*] rec/checks clauses*])}
  [bname inputs marker & clauses]
  (define/defchecks &env bname inputs marker clauses expansion))

(defmacro defcopy
  "Defines `new-name`, a copy of `bundle` whose checks are renamed by `names`, a map from each
  check of the bundle to its new name, which names every check exactly once. The copy's ids
  are keywords in this namespace; its checks keep their concerns."
  [new-name bundle names]
  (define/defcopy new-name bundle names))

(defmacro defconcern
  "Defines the concern `name`: a clause marked `^::name` carries it, and a selection names its
  checks by the var or by the keyword `::name`."
  [name doc]
  (define/defconcern name doc))

;;; ------------------------------------------------------------------ check bodies

(defn- outside! [form what]
  (throw (IllegalStateException.
          (str what " can be used only inside a check body of rec/defproven or rec/defchecks: "
               (pr-str form)))))

(defmacro fetch
  "Inside a check body only: reads `k` from `source` and returns its answer, or nil where the
  source leaves `k` out. A source that throws fails the check. Legal only in call position and
  outside any fn the body builds (fn, #(...), for, lazy-seq, delay) and outside `locking`.
  Throws at expansion anywhere else."
  [source k]
  (when-not (contains? &env body/marker) (outside! &form "rec/fetch"))
  (list '&fetch source k))

(defmacro fetch-each
  "Inside a check body only: reads every key of `ks` from `source` in one read, and returns a
  map from key to answer. The rules of [[fetch]] apply."
  [source ks]
  (when-not (contains? &env body/marker) (outside! &form "rec/fetch-each"))
  (list '&fetch-each source ks))

(defmacro extant?
  "Inside a check body only: whether the field `f` is present. Throws at expansion outside one."
  [f]
  (when-not (contains? &env body/marker) (outside! &form "rec/extant?"))
  (list '&extant? f))

(defmacro when-extant
  "Inside a check body only: `(if (rec/extant? f) (do body...) true)`."
  [f & body]
  (when-not (contains? &env body/marker) (outside! &form "rec/when-extant"))
  `(if (extant? ~f) (do ~@body) true))

;;; ------------------------------------------------------------------ selections

(def every "Selects every check." select/every)
(def shape "Selects every shape check." select/shape)
(def pure "Selects every check that reads no source." select/pure)

(defn fetching
  "Selects the checks that read `source`."
  [source]
  (select/fetching source))

(defn reading
  "Selects the checks that read the field whose key is `k`, directly or through an input."
  [k]
  (select/reading k))

(defn except
  "Selects what `s` selects, less what any of `xs` selects. Throws where a keyword among them
  names no concern, no bundle check and no check of a type its namespace defines."
  [s & xs]
  (record/refuse-unnamed! (apply select/except s xs)))

(defn union
  "Selects what any of `xs` selects. Throws where a keyword among them names no concern, no
  bundle check and no check of a type its namespace defines."
  [& xs]
  (record/refuse-unnamed! (apply select/union xs)))

;;; ------------------------------------------------------------------ values

(defn draft
  "A draft: given a type's class, one holding nothing and passing nothing; given a sealed value,
  one holding its entries and passing every check; given a value of a definition that a later
  one displaced, a draft of the later one holding its entries and passing nothing; given a
  draft, that draft. A draft is a map that accepts any write and remembers which checks pass
  on its entries; a write forgets the checks that read a key it writes."
  [x]
  (record/draft x))

(defn clean
  "Draft `d` without the keys its type does not declare."
  [d]
  (record/clean d))

(defn seal
  "The value of draft `d`, once every check `d` does not remember as passing has passed; a
  sealed value is returned as it is. A failed seal leaves `d` remembering the checks that
  passed, so the next seal runs only the rest. Throws ex-info whose `:error` is
  `:recognitum/undeclared-keys`, before any check runs, where `d` holds a key its type does
  not declare; `:recognitum/check-failed`, with the report of [[check]] under `:failed` and
  `:blocked`; or `:recognitum/missing-fields`."
  [d]
  (record/seal d))

(defn check
  "Runs the checks `sel` selects in `x`, and those they wait on, that `x` does not already pass;
  a draft remembers what passed and forgets what failed, and a sealed value runs nothing.
  Returns true, or a report: `{:failed {id {:data :doc :blocked}} :blocked #{id}}`, where
  `:blocked` under a failed check holds the checks its failure kept from running, and
  `:pulled-back` explains each failed check that an [[except]] removed and a wait ran anyway."
  [x sel]
  (record/check x sel))

(defn recheck
  "Runs the checks `sel` selects in `x` whether or not `x` passes them, and those they wait on
  that it does not. A draft remembers as [[check]] does; a sealed value stays as it is. Returns
  as [[check]] does."
  [x sel]
  (record/recheck x sel))

(defn check-all
  "Runs what [[check]] would run on each value of `xs`, which may be of several types, in one
  pass, so values that read the same source share its calls. Each draft remembers what
  passed and forgets what failed, as under [[check]]. Returns a vector of what [[check]]
  returns, one per value, in order."
  [xs sel]
  (record/check-all xs sel))

(defn proves?
  "Whether `x` passes the checks `sel` selects, those they wait on, and the shape check of each
  field it must or does hold. True for every sealed value of a type's current definition."
  [x sel]
  (record/proves? x sel))

(defn proven-checks
  "The ids of the checks `x` passes: all of its type's for a sealed value, those a draft
  remembers."
  [x]
  (record/proven-checks x))

(defn proven-concerns
  "The ids of the concerns whose checks `x` passes, as [[proves?]] judges each."
  [x]
  (record/proven-concerns x))

(defn checks-of
  "The ids of the checks `sel` selects in the type of `x`: a value, a draft or a type's class."
  [sel x]
  (record/checks-of sel x))

(defn sealed?
  "Whether `x` is a value of the current definition of a type."
  [x]
  (record/sealed? x))

(defn draft?
  "Whether `x` is a draft."
  [x]
  (record/draft? x))

(defn data-readers
  "A map from each type's tag to its `map->T`, for `clojure.edn/read` as `:readers`: one entry
  for each current type that its namespace maps its name to. An entry held across a
  redefinition of its type refuses, as a held `map->T` does."
  []
  (into {} (for [[n t] (record/current-types)]
             [n #(record/make t % "Read again with the current readers: (rec/data-readers)")])))
