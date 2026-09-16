(ns recognitum.core
  "A record-like type that carries proof of its own validation.

  A type declares named checks. A check names, in its arglist, exactly the fields it
  reads, and the body receives those and nothing else -- so the field -> checks index
  is derived from the form rather than declared alongside it, and cannot disagree with
  what the body actually does.

  A check returns `true` -- exactly `true` -- to pass; anything else is a failure, and what
  it returned is the failure data. An exception is neither: a throw is a bug in the check
  rather than a verdict about the data, so it propagates untouched. Nothing stops at the
  first failure either -- every entry point runs everything it was asked to run and throws
  once with all of it, keyed by check name in declaration order.

  A write to a validated value runs the checks naming the field written, so such a value
  is never observable in an invalid state. A draft is not a validated value: writes into
  it check nothing, which is what lets a multi-step update pass through states no single
  write could produce. [[seal]] closes the gap -- what it returns is validated exactly as
  if every check had been rerun.

  Fields become keys qualified by the namespace defining the type, so a bare keyword of
  the same name is a key the type does not declare.

  A protocol implementation is written once and installed on both generated types. The
  draft runs the body as written; the sealed value wraps it, so the body works on a draft
  and what it produces is sealed on the way out -- which is the same re-entrancy
  [[drafting]] provides, applied without anyone asking for it.

  A draft also records which of its checks currently pass. That record is a mutable field:
  the draft's data is immutable, but which checks have been run against that data is not
  part of the data. See [[passing]]. [[check]] and [[check!]] run one named check against a
  draft and answer the question rather than handing back a value -- the verdict goes in the
  mask, and a check the mask already says passes is not run again. [[seal]] reads the same
  mask: it runs whatever is not already passing, so work done a check at a time on a draft
  is not repeated when the draft closes.

  A certificate names a subset of those checks, so a draft can be run through a group of
  them before it is complete. [[certify]] and [[certify!]] establish one; whether a draft
  *holds* one is derived from the mask rather than recorded anywhere ([[holding]]), so
  overlapping certificates need no coordination and individual checks can complete one
  incidentally. Certificates are a draft-level facility and change nothing about what a
  sealed value guarantees: [[seal]] enforces every check on the type, certified or not --
  it merely finds a certified draft's share of them already established.

  `meta` answers the same way from either form. A value's own metadata comes back with
  `::passed` and `::certificates` merged over the top -- read off the mask for a draft, and
  a constant of the type for a sealed value, where every check passed and every certificate
  therefore holds. So nothing has to establish which form it is holding before asking, and
  because the library's keys are written last, nothing a caller sets can claim more than
  the value can show for itself."
  (:refer-clojure :exclude [defrecord])
  (:require
   [potemkin :as p]))

(defonce ^:private registry (atom {}))

(defn register! "Associate a class with its model map." [cls model] (swap! registry assoc cls model))

(defonce ^:private readers (atom {}))

(defn register-reader! "Associate a print tag with its validating map constructor." [tag ctor]
  (swap! readers assoc tag ctor))

(defn data-readers
  "`{tag -> validating constructor}` for every type defined so far, to hand `clojure.edn/read`
  or `clojure.edn/read-string` as `:readers`.

  There is no global registration, deliberately. `clojure.edn` never consults `*data-readers*`
  -- it takes `:readers` and nothing else -- and it is the reader you want for anything coming
  from outside the process. `alter-var-root` on `*data-readers*` is worse than useless besides:
  `clojure.main` thread-binds that var, so a registration made while loading a namespace is
  invisible to the very thread that loaded it and visible to every other one.

  Reading is validating. A literal whose contents fail a check throws at read time rather
  than producing a value that lies about itself.

      (edn/read-string {:readers (rec/data-readers)} s)"
  [] @readers)

;;; ------------------------------------------ Checks ------------------------------------------

(defn unknown-key! [model k]
  (throw (ex-info (format "%s has no field %s" (:name model) (pr-str k))
                  {:error ::unknown-field :record (:name model) :key k
                   :declared (:field-order model)})))

(defn declared-field!
  "Throw unless `k` is a field this type declares."
  [model k]
  (when-not (contains? (:field-set model) k) (unknown-key! model k)))

(defn dep-values
  "What a check was given, keyed by the dependency it was given for, in arglist order.
  Built only where a failure is about to be reported -- the passing path never has to say."
  [check x]
  (apply array-map (mapcat (fn [d] [d (get x d)]) (:deps check))))

(defn failure-entry
  "The `[check-name report]` pair for a check that did not return `true`. `ret` is what it
  returned, which is the failure data; `vals` is what it was given."
  [check ret vals]
  [(:name check) {:doc (:doc check) :values vals :returned ret}])

(defn- failure-str [[cname {:keys [doc values returned]}]]
  (format "%s%s: given %s it returned %s"
          cname (if doc (str " (" doc ")") "") (pr-str values) (pr-str returned)))

(defn checks-failed!
  "Throw one `:check-failed` carrying every failure in `fs`, which is [[failure-entry]] pairs
  in the order the checks ran -- declaration order, everywhere they are run from. `fs` may
  hold `nil` where a check passed, so a call site can emit one expression per check and let
  this work out which of them happened.

  The map is `array-map` applied to the pairs rather than conj-ed together one at a time:
  conj past the eighth entry would promote it to a hash map and lose the very order it is
  reporting in."
  [model fs]
  (let [fs (into [] (remove nil?) fs)]
    (throw (ex-info (str (:name model) " failed "
                         (when (next fs) (str (count fs) " checks -- "))
                         (apply str (interpose "; " (map failure-str fs))))
                    {:error ::check-failed
                     :record (:name model)
                     :failures (apply array-map (into [] cat fs))}))))

(defn checks-for
  "The indices of the checks naming any of `ks`, in declaration order."
  [model ks]
  (into (sorted-set) (mapcat (:index model)) ks))

(defn run-checks!
  "Run the checks at `idxs` against `x`, through the type's own generated runner -- which
  reads the fields each check needs directly rather than looking them up. Only the failure
  path pays for a lookup, to say what the check was given.

  Every one of them runs: a failure does not stop the pass, and what comes back is one throw
  carrying all of them. Only `true` passes, and nothing is caught -- a check that throws is a
  bug in the check rather than a verdict about the data, and \"we could not determine whether
  this is valid\" is a different claim from \"this is invalid\"."
  [model run x idxs]
  (let [cs (:checks model)
        fs (reduce (fn [acc i]
                     (let [ret (run i x)]
                       (if (true? ret)
                         acc
                         (let [c (nth cs i)]
                           (conj acc (failure-entry c ret (dep-values c x)))))))
                   [] idxs)]
    (when (seq fs) (checks-failed! model fs))))

(defn entries-of [o]
  (cond (map? o)                     (seq o)
        (instance? java.util.Map o)  (seq (into {} o))
        (and (vector? o) (= 2 (count o))) [o]
        :else (throw (ex-info "cannot conj this onto a validated value" {:value o}))))

(defn no-dissoc!
  "Declared fields are permanent, so `dissoc` refuses them; where a check admits nil,
  `(assoc m k nil)` is how absence is spelled. Undeclared keys dissoc normally."
  [model k]
  (throw (ex-info (format "cannot dissoc %s from %s: declared fields are permanent -- use (assoc m %s nil) if its checks admit nil"
                          (pr-str k) (:name model) (pr-str k))
                  {:error ::cannot-dissoc :record (:name model) :key k
                   :instead (list 'assoc 'm k nil)})))

(defn qualify-keys
  "Rewrite a bare keyword naming a declared field of `model` into the type's own namespace,
  and leave every other key exactly as it was written.

  That is what lets a printed literal read back: the tag already carries the namespace, so
  the printed map drops it, and the reader -- which is `map->Type` itself -- puts it back.
  Qualifying only what names a field is what keeps the error honest for what does not: a
  stray `:getaway-car` is reported as `:getaway-car` rather than as something the caller
  never wrote. Where a map holds both spellings of one field the qualified entry wins,
  so degenerate input resolves the same way every time rather than by seq order."
  [model m]
  (let [nsp    (:key-ns model)
        fields (:field-set model)
        bare   (reduce-kv (fn [acc k v]
                            (if (simple-keyword? k)
                              (let [q (keyword nsp (name k))]
                                (assoc acc (if (contains? fields q) q k) v))
                              acc))
                          {} m)
        given  (reduce-kv (fn [acc k v] (if (simple-keyword? k) acc (assoc acc k v))) {} m)]
    (conj bare given)))

(defn printable
  "The map a sealed value prints as. A sealed value holds nothing but its declared fields,
  every one of them qualified by the namespace the print tag already names, so repeating
  that namespace on every key would be noise."
  [x]
  (into (sorted-map) (map (fn [[k v]] [(keyword (name k)) v])) x))

;;; ------------------------------------------ Draft ------------------------------------------

(def unset
  "Marks a declared field a draft has never been written to, so `nil` stays a legal value."
  (Object.))

(defonce ^:private draft-registry (atom {}))

(defn register-draft! "Associate a draft class with its model map." [cls model]
  (swap! draft-registry assoc cls model))

(def ^:dynamic *owner*
  "Token identifying the outermost `drafting` currently on the stack, or nil outside one.
  A draft records the token in force when it was created, which is how a nested `drafting`
  tells a draft an enclosing form owns from a free one it must adopt and seal itself."
  nil)

(defn draft? [x] (contains? @draft-registry (class x)))

(defn draft-model [x] (@draft-registry (class x)))

(defprotocol Verdicts
  "The record a draft keeps of which of its checks currently pass.

  A mutable field rather than a value carried alongside, because a draft's *data* is
  immutable while which checks have been run against that data is not part of the data:
  running a check adds knowledge about a value without producing a different one. It is
  reached through methods rather than field access because a mutable field is private to
  the class declaring it, which is what keeps the mask out of reach of everything but the
  two operations entitled to touch it."
  (verdict-mask [d]
    "The mask of checks currently passing on `d` -- bit `i` for the check at index `i` of
    its type's `:checks`.")
  (record-verdict! [d i pass?]
    "Write down what the check at index `i` returned: set its bit if it passed, clear it if
    it did not. Answers with `pass?`, so a runner can record and branch in one step.

    Clearing is what makes re-running a check meaningful. A check that passed earlier and
    fails now must stop counting as passed, and without a clear there would be no way to
    un-pass anything."))

(defn draft-model!
  "The model for the draft `d`, or a throw naming the operation that wanted one. Everything
  that reads or writes a verdict wants a draft specifically: the mask is the only thing that
  could answer, and only a draft has one."
  [what d]
  (or (draft-model d)
      (throw (ex-info (format "%s expects a draft, and %s is not one" what (pr-str d))
                      {:error ::not-a-draft :value d}))))

(defn- passed-in
  "The names of the checks `mask` says are passing."
  [model mask]
  (into #{} (keep-indexed (fn [i c] (when (bit-test mask i) (:name c)))) (:checks model)))

(defn- held-in
  "The names of the certificates `mask` says are held -- one `and` each, since a certificate
  is held exactly when every check in it passes."
  [model mask]
  (into #{} (keep (fn [[cname {cm :mask}]] (when (= cm (bit-and mask cm)) cname)))
        (:certificates model)))

(defn passing
  "The set of check names currently passing on the draft `d`.

  A draft opened from a sealed value starts with all of them, since that is what being
  sealed means; one opened from the type starts with none; and a write clears exactly the
  checks naming the field written. So this is what has been established about `d` as it
  stands, never what was once true of something it came from.

  A question about a draft and nothing else: a sealed value has no mask, because it has no
  state to be in other than valid. Where the question is asked of a value that could be
  either, `(::passed (meta x))` is the same answer and both forms answer it."
  [d]
  (passed-in (draft-model! "passing" d) (verdict-mask d)))

(defn run-draft-checks!
  "Run the checks at `idxs` against the draft `d`, writing each verdict into its mask as it
  goes, and throw once with every failure.

  The recording is what separates this from [[run-checks!]]: a verdict is a fact about the
  checking rather than about the data, so it belongs to the draft the checking was done on
  and not to any value that comes out of it. Work done here is therefore not lost when this
  throws -- the checks that passed are marked as passing, and the one that failed is marked
  as not.

  A check that throws records nothing. It reached no verdict, so its bit says what it said
  before, and the throw propagates: a bug in a check is not a claim about the data."
  [model d idxs]
  (let [cs  (:checks model)
        run (:run-draft model)
        fs  (reduce (fn [acc i]
                      (let [ret (run i d)]
                        (if (record-verdict! d i (true? ret))
                          acc
                          (let [c (nth cs i)]
                            (conj acc (failure-entry c ret (dep-values c d)))))))
                    [] idxs)]
    (when (seq fs) (checks-failed! model fs))))

(defn check-id
  "The index of the check named `cname` on this type -- the bit it owns in a draft's mask.

  A name the type does not declare is refused rather than answered: a check that does not
  exist has no verdict, and returning one would be inventing an answer to a question about
  nothing."
  [model cname]
  (or (get (:check-ids model) cname)
      (throw (ex-info (format "%s declares no check named %s -- its checks are %s"
                              (:name model) (pr-str cname) (pr-str (mapv :name (:checks model))))
                      {:error ::unknown-check :record (:name model) :check cname
                       :checks (mapv :name (:checks model))}))))

(defn- waiting-on
  "check name -> the fields it names that `d` has never been written to, in arglist order,
  for every check at `idxs` that is waiting on one.

  Every check in the group is asked, rather than the search stopping at the first one that
  is waiting: nothing else here stops at the first anything, and a caller told about one
  unwritten field at a time would write one and be told about the next."
  [model d idxs]
  (let [missing (set ((:draft-missing model) d))
        cs      (:checks model)]
    (apply array-map
           (into [] (mapcat (fn [i]
                              (let [c  (nth cs i)
                                    ks (filterv missing (:deps c))]
                                (when (seq ks) [(:name c) ks]))))
                 idxs))))

(defn- missing-fields!
  "A check cannot judge a field nobody wrote. \"Never written\" and \"written to nil\" are
  different things everywhere else here, and the sentinel that tells them apart must not
  reach a check body -- a check handed it would be judging the library's internals, and a
  verdict recorded from that would be a verdict about nothing.

  `waiting` is [[waiting-on]]'s answer, never empty. `:waiting` carries all of it; `:check`,
  `:key` and `:keys` describe the first, which is the whole of it wherever one check was
  asked about."
  [model waiting]
  (let [[cname cks] (first waiting)
        ks          (into [] (distinct) (mapcat val waiting))]
    (throw (ex-info (if (next waiting)
                      (format "%s cannot run %d checks: %s"
                              (:name model) (count waiting)
                              (apply str
                                     (interpose
                                      "; "
                                      (map (fn [[c cks]]
                                             (format "%s names %s, which %s never written"
                                                     (pr-str c) (pr-str cks)
                                                     (if (next cks) "were" "was")))
                                           waiting))))
                      (format "%s cannot run %s: it names %s, which %s never written"
                              (:name model) (pr-str cname) (pr-str cks)
                              (if (next cks) "were" "was")))
                    {:error ::missing-field :record (:name model) :check cname
                     :key (first ks) :keys ks :waiting waiting}))))

(defn- run-check!
  "Call the check at `i` against `d`, write down what it returned, and answer with that.
  A check that throws records nothing: it reached no verdict, so its bit says what it said
  before."
  [model d i]
  (let [ret ((:run-draft model) i d)]
    (record-verdict! d i (true? ret))
    ret))

(defn- not-established
  "Those of `idxs` that `d`'s mask does not already say are passing, in the order given.

  The one question every entry point that runs a group of checks against a draft asks
  first. A set bit says that check passed against exactly this data -- a write cleared the
  bit of everything naming the field it wrote -- so there is nothing left to learn and the
  check is not called. A clear bit says nothing more than that: it does not distinguish
  \"failed\" from \"not yet run\", which is why anything not passing is run again."
  [d idxs]
  (let [m (verdict-mask d)]
    (filterv #(not (bit-test m %)) idxs)))

(defn- establish
  "Answer what the check at `i` says about `d`, running it only if its verdict does not
  already say it passes, and recording what it returned.

  `check` means *establish this*, not *run exactly this*. A set bit says this check passed
  against exactly this data -- a write cleared the bit of everything naming the field it
  wrote -- so there is nothing left to learn and the check is not called. A failure is not
  retained in the same way: nothing about a `false` bit distinguishes \"failed\" from \"not
  yet run\", which is why a later call runs it again and produces the failure again."
  [model d i]
  (if (bit-test (verdict-mask d) i)
    true
    (let [w (waiting-on model d [i])]
      (when (seq w) (missing-fields! model w))
      (run-check! model d i))))

(defn check
  "Run the check `cname` against the draft `d`: `true` if it passes, and whatever the check
  returned if it does not.

  Not a draft -- none of these return one. The verdict goes in `d`'s mask, which is what
  frees the return value to be the answer to the question that was asked, and `true` means
  passed here exactly as it does for a check itself.

  Running a check runs whatever it depends on that is not already passing, so this
  establishes the check rather than merely calling it: one already passing costs nothing,
  and one whose bit a write cleared runs again. (A pure check depends on fields, which are
  read rather than run; when an io-dep is what a check names, this is where the fetch it is
  gated behind happens.)

  Failures are not retained. What this returns is the failure; a later call re-runs anything
  not passing and produces it again."
  [d cname]
  (let [model (draft-model! "check" d)]
    (establish model d (check-id model cname))))

(defn check!
  "[[check]], insisting on the answer: `true`, or one `:check-failed` carrying the failure
  under the check's name, alongside its docstring, the fields it declared and their values,
  and what it returned -- the same report every other entry point throws.

  The work is the same work, so a check already passing costs nothing here either."
  [d cname]
  (let [model (draft-model! "check!" d)
        i     (check-id model cname)
        ret   (establish model d i)]
    (if (true? ret)
      true
      (let [c (nth (:checks model) i)]
        (checks-failed! model [(failure-entry c ret (dep-values c d))])))))

(def every-check
  "The certificate covering every check a type declares, which exists without anyone
  declaring it -- so a draft can be run through everything in one pass rather than leaving
  whatever a named certificate omitted to be run one check at a time.

  It is the one certificate name a type may not declare for itself."
  ::every-check)

(defn- certificate
  "What the certificate `cname` covers on this type, or a throw naming what it could have
  asked for. A certificate that does not exist covers nothing, and answering `true` for it
  would be asserting exactly that nothing."
  [model cname]
  (or (get (:certificates model) cname)
      (throw (ex-info (format "%s declares no certificate named %s -- its certificates are %s"
                              (:name model) (pr-str cname)
                              (pr-str (vec (keys (:certificates model)))))
                      {:error ::unknown-certificate :record (:name model) :certificate cname
                       :certificates (vec (keys (:certificates model)))}))))

(defn holding
  "The set of certificate names the draft `d` currently holds.

  Derived, never asserted: a draft holds a certificate exactly when every check in it is
  currently passing, which is one `and` against the certificate's mask. So overlapping
  certificates need no coordination with each other, running individual checks can complete
  a certificate incidentally, and there is nowhere to record holding one except by having
  actually passed its checks.

  A question about a draft, as [[passing]] is: a sealed value passed every check on its
  type, which is strictly more than any certificate claims. Where the question is asked of
  a value that could be either, `(::certificates (meta x))` is the same answer and both
  forms answer it."
  [d]
  (held-in (draft-model! "holding" d) (verdict-mask d)))

(defn merged-meta
  "`mta` with the library's own keys, `lib`, merged over the top.

  Over rather than under, deliberately. `with-meta` is public, so anything a caller could
  set a caller could forge; keys written last cannot be overridden by anything handed in,
  and what comes back is what the value can actually show for itself.

  A value carrying no metadata of its own answers with `lib` exactly -- for a sealed value
  that is the type's constant, allocated once when the type was defined."
  [mta lib]
  (if mta (conj mta lib) lib))

(defn draft-meta
  "What a draft answers [[clojure.core/meta]] with: its own metadata, with what its verdict
  mask says merged over the top.

  A sealed value answers the same two keys, from a constant its type computed when it was
  defined -- every check passed, so every certificate is held. That is what makes the
  question askable of a value without first establishing which form it is."
  [model mask mta]
  (merged-meta mta (array-map ::passed       (passed-in model mask)
                              ::certificates (held-in model mask))))

(defn- run-certificate!
  "Establish every check in `cert` against `d` and answer with `[i ret]` for each that did
  not return `true`, in the type's own check order.

  Only what is not already passing is run, so a certificate the draft already holds costs
  nothing and one sharing checks with another pays for them once. The fields are pre-flighted
  over that same set and in one pass, so a draft too incomplete to judge says so about every
  check that is waiting rather than one at a time."
  [model d cert]
  (let [pending (not-established d (:ids cert))
        w       (waiting-on model d pending)]
    (when (seq w) (missing-fields! model w))
    (reduce (fn [acc i]
              (let [ret (run-check! model d i)]
                (if (true? ret) acc (conj acc [i ret]))))
            [] pending)))

(defn certify
  "Run the certificate `cname` against the draft `d`: `true` if every check in it passes,
  and otherwise a map of check name -> what that check returned, for those that did not.

  What a failing check returned *is* the failure data, so that is what comes back here, as
  it does from [[check]]. The richer report -- the docstring, the fields the check declared
  and their values -- is what [[certify!]] throws.

  Every check in the certificate runs; none of this stops at the first failure. Failures
  come back keyed by check name in the type's own check declaration order, built with
  `array-map` so that order survives past the size at which a map literal would lose it.

  Not a draft: the verdicts go in `d`'s mask, which is what frees the return value to be
  the answer to the question asked. Nor is holding the certificate recorded anywhere --
  [[holding]] derives it from those verdicts, so this establishes checks and the
  certificate follows."
  [d cname]
  (let [model (draft-model! "certify" d)
        fs    (run-certificate! model d (certificate model cname))]
    (if (seq fs)
      (apply array-map
             (into [] (mapcat (fn [[i ret]] [(:name (nth (:checks model) i)) ret])) fs))
      true)))

(defn certify!
  "[[certify]], insisting on the answer: `true`, or one `:check-failed` carrying every
  failure in the certificate, keyed by check name in declaration order, each with the
  check's docstring, the fields it declared and their values, and what it returned.

  The work is the same work, so a certificate already held costs nothing here either."
  [d cname]
  (let [model (draft-model! "certify!" d)
        fs    (run-certificate! model d (certificate model cname))]
    (if (seq fs)
      (checks-failed! model (map (fn [[i ret]]
                                   (let [c (nth (:checks model) i)]
                                     (failure-entry c ret (dep-values c d))))
                                 fs))
      true)))

(defn model-of
  "The model for `x`, whether `x` is a validated instance or the type itself."
  [x]
  (let [r @registry] (or (r (class x)) (r x))))

(defn sealed?
  "Is `x` an instance of a registered validated type -- something [[seal]] has already produced?"
  [x] (some? (@registry (class x))))

(defn of-type?
  "Is `x` a value of type `t` -- either form of it?

  `defrecord` generates two classes, so `instance?` asks something narrower than what is
  usually meant by \"is this a Heist\": it holds of a sealed `Heist` and not of a draft of
  that same `Heist`. This is the question about the type. [[draft?]] and [[sealed?]] are the
  questions about the form, of any type, and \"a draft of Heist\" composes from one of each --
  which is the right amount of spelling out for something that specific.

  False for anything the library does not know, `t` itself included: a class is not a value
  of its type, however registered the type is. False, rather than a throw, when `t` names no
  registered type at all -- a predicate that throws is not a predicate.

  A function rather than a predicate generated per type, so a definition interns nothing into
  its namespace that could collide with a name written there by hand."
  [t x]
  (if-let [model (or (@registry t) (@draft-registry t))]
    (let [c (class x)]
      (or (identical? model (@registry c))
          (identical? model (@draft-registry c))))
    false))

(defn owner-of [d] ((:draft-owner (draft-model d)) d))

(defn adopt
  "Stamp `d` as owned by `token`, so nested `drafting` forms recognise it."
  ([d token] (adopt (draft-model d) d token))
  ([model d token] ((:draft-with-owner model) d token)))

(defn open
  "The draft a `drafting` works from, already stamped with `token`. A free draft is adopted; a
  validated value or a bare type is opened directly, which skips the extra allocation `adopt`
  would cost."
  [model x token already-draft?]
  (cond
    already-draft?                   (adopt model x token)
    (identical? model (@registry x)) ((:draft-empty model) token)
    :else                            ((:draft-from model) x token)))

(defn draft
  "A draft of `x`: a validated value, a registered type (construction from empty), or a draft.
  A draft created inside a `drafting` records that form as its owner."
  [x]
  (if (draft? x)
    x
    (if-let [model (model-of x)]
      (if (identical? model (@registry x))
        ((:draft-empty model) *owner*)
        ((:draft-from model) x *owner*))
      (throw (ex-info "not a validated value, draft, or registered type" {:value x})))))

(defn extraneous
  "The keys `d` holds that its type does not declare."
  [d]
  (seq (keys ((:draft-ext (draft-model d)) d))))

(defn clean
  "Drop the keys the type does not declare, so `d` can seal. A draft -> draft step: put it at
  the end of a `drafting` body to strip rather than reject."
  [d]
  (when-not (draft? d)
    (throw (ex-info "clean expects a draft" {:value d})))
  (if (extraneous d) ((:draft-clean (draft-model d)) d) d))

;;; ------------------------------ Opting out of the seal ------------------------------

(deftype Unsealed [wrapped])

(defn unsealed
  "Mark the draft `d` as a return value that should *not* be sealed, the way `reduced`
  marks a return value that should end a reduction.

  A protocol implementation written on a validated type is installed on both generated
  types, and on the sealed one it is wrapped: the body runs against a draft, and a draft
  of this type handed back is sealed on the way out. `unsealed` opts that return out. What
  the caller receives is a free draft, owned by nothing -- adoptable by a later [[drafting]]
  or sealable by hand.

  Both implementations unwrap it, so a body means the same thing wherever it runs; on the
  draft type, where nothing was going to be sealed, it is simply a no-op.

  [[drafting]] does not honour it, and neither does [[seal]]. The wrapper is implicit, so
  opting out of it is fair; a `drafting` form is the author asking to seal, and a marker
  that quietly defeated that would be a hole rather than an escape hatch.

  Anything that is not a draft throws: the marker asserts what is being returned, and a
  wrong assertion should say so where it is written rather than travel."
  [d]
  (if (draft? d)
    (Unsealed. d)
    (throw (ex-info (format "unsealed expects a draft, and %s is not one" (pr-str d))
                    {:error ::not-a-draft :value d}))))

(defn- unmark
  "The draft an [[unsealed]] marker holds, or `x` exactly as it came."
  [x]
  (if (instance? Unsealed x) (.-wrapped ^Unsealed x) x))

(defn- seal-draft [model d]
  ;; Only the checks the draft's mask does not already say are passing are actually run,
  ;; which is equivalent to running all of them: a bit is set only by a check that returned
  ;; `true`, and only inherited by a draft whose data is the data that check saw -- a draft
  ;; of a sealed value starts with every bit set because that is what being sealed means,
  ;; and a write clears exactly the checks naming the field it wrote. So a set bit is a
  ;; verdict about this data, and re-reaching it would learn nothing.
  ;;
  ;; A refusal comes first and costs no check at all. An undeclared key and a field nobody
  ;; wrote are facts about the draft rather than about any check's judgement, and a check
  ;; handed the unwritten-field sentinel would be judging the library's internals.
  (when-let [extra (extraneous d)]
    (throw (ex-info (format "%s has no field(s) %s -- use `clean` to strip them"
                            (:name model) (pr-str (vec extra)))
                    {:error ::extraneous-keys :record (:name model) :keys (vec extra)})))
  (when-let [missing (seq ((:draft-missing model) d))]
    (throw (ex-info (format "%s is missing field %s" (:name model) (first missing))
                    {:error ::missing-field :record (:name model) :key (first missing)})))
  ;; Every check on the type is exactly what the built-in certificate covers, so the vector
  ;; of indices to select from is one the macro already built.
  (run-draft-checks! model d (not-established d (:ids (get (:certificates model) every-check))))
  ((:draft-seal model) d))

(defn seal
  "Validate `d` and return the sealed value. What comes back is validated exactly as if
  every check had been rerun.

  What is actually run is whatever `d`'s verdict mask does not already say is passing, so
  work done incrementally on the draft is not repeated: a check established with [[check]],
  or a whole certificate established with [[certify]], costs nothing here, and a draft of a
  sealed value that has been written to pays only for the checks naming the fields written.
  The guarantee is extensional -- it is about what the returned value satisfies, not about
  which checks were called -- so a cheaper sound strategy is permitted, and the mask is one.

  A failure is not saved in the same way. Nothing about a cleared bit distinguishes
  \"failed\" from \"not yet run\", so a check that failed runs again at the next seal and
  fails again. Every failure is collected and thrown once, which costs nothing where
  everything passes, since every pending check runs in that case anyway.

  Idempotent: an already-sealed value passes straight through, so `seal` is a projection and a
  sealed value is a fixed point of it. That is what lets a `drafting` body end in a call that
  may or may not have sealed already, without the caller having to know which.

  An [[unsealed]] marker is unwrapped and the draft inside it sealed anyway. That marker
  opts out of the *implicit* seal a protocol method performs; asking to seal and being
  refused by a marker would be a hole rather than an escape hatch."
  ([d]
   (let [d (unmark d)]
     (cond
       (draft? d)  (seal (draft-model d) d)
       (sealed? d) d
       :else (throw (ex-info "seal expects a draft or an already-sealed value"
                             {:error ::cannot-seal :value d})))))
  ([model d]
   (let [d (unmark d)]
     (if-not (identical? model (draft-model d))
       (if (identical? model (model-of d))
         d
         (throw (ex-info (format "%s: expected a draft or an already-sealed %s, got %s"
                                 (:name model) (:name model) (pr-str (class d)))
                         {:error ::cannot-seal :record (:name model) :value d})))
       (seal-draft model d)))))

(defn- not-of-this-type!
  [model d]
  (throw (ex-info (format "%s: unsealed expects a draft of %s, and %s is a draft of %s"
                          (:name model) (:name model) (pr-str d) (:name (draft-model d)))
                  {:error ::not-a-draft :record (:name model) :value d})))

(defn- opted-out
  "The draft an [[unsealed]] marker holds, checked against the type whose method returned
  it. A method of one type cannot assert what a draft of another is."
  [model x]
  (let [d (.-wrapped ^Unsealed x)]
    (when-not (identical? model (draft-model d)) (not-of-this-type! model d))
    d))

(defn sealed-impl-return
  "What a protocol method installed on the *sealed* type hands back.

  The body ran against a draft, so a draft of this type is the value the method produced
  and is sealed here -- which is what makes a method on a sealed value unable to produce
  an invalid one whatever it did inside. Anything else is something else entirely -- a
  number, a string, an already-sealed value, a draft of some other type -- and passes
  through untouched. An [[unsealed]] marker is the opt-out: the draft comes back free,
  owned by nothing, so a later `drafting` adopts it rather than mistaking it for its own."
  [model ret]
  (cond
    (instance? Unsealed ret)             (adopt model (opted-out model ret) nil)
    (identical? model (draft-model ret)) (seal model ret)
    :else                                ret))

(defn draft-impl-return
  "What a protocol method installed on the *draft* type hands back: the body's value as
  written. Only the [[unsealed]] marker is unwrapped, so a body means the same thing
  wherever it runs; there was nothing to opt out of here, and the draft keeps whatever
  owner it had, since freeing one an enclosing `drafting` owns would break re-entrancy."
  [model ret]
  (if (instance? Unsealed ret) (opted-out model ret) ret))

(defn owned-here?
  "Is `x` a draft belonging to a `drafting` form already on the stack?"
  [x]
  (and (draft? x) (some? *owner*) (= *owner* (owner-of x))))

(defmacro drafting
  "Bind `sym` to a draft of `init`, evaluate `body`, and seal the result.

  `init` may be a validated value, a registered type (to build one from nothing), or a
  draft. A draft an enclosing `drafting` owns passes straight through unsealed, so only
  the outermost form validates — that is what lets an impl fn taking and returning a
  draft be called standalone or nested without knowing which. A draft made outside any
  `drafting` is adopted and sealed here."
  [[sym init] & body]
  `(let [init# ~init
         dm#   (draft-model init#)]
     (if (and dm# (some? *owner*) (= *owner* ((:draft-owner dm#) init#)))
       (let [~sym init#] ~@body)
       (let [model# (or dm# (model-of init#))
             token# (Object.)]
         (when-not model#
           (throw (ex-info "not a validated value, draft, or registered type" {:value init#})))
         (binding [*owner* token#]
           (let [~sym (open model# init# token# (some? dm#))]
             (seal model# (do ~@body))))))))

;;; ------------------------------------------ Macro ------------------------------------------

(defn unknown-dependency!
  "Report an arglist entry that names nothing this type declares."
  [{:keys [record what fields io-deps] cname :name} entry]
  (let [kind (name what)]
    (throw (ex-info (cond
                      (symbol? entry)
                      (format "%s %s %s names %s, which is not a field of %s -- its fields are %s"
                              record kind (pr-str cname) (pr-str entry) record
                              (pr-str (vec (keys fields))))

                      (keyword? entry)
                      (format "%s %s %s names %s, which is not an io-dep of %s -- %s"
                              record kind (pr-str cname) (pr-str entry) record
                              (if (seq io-deps)
                                (str "its io-deps are " (pr-str io-deps))
                                "it declares no io-deps"))

                      :else
                      (format (str "%s %s %s names %s: an arglist entry is a symbol naming a field "
                                   "or a keyword naming an io-dep, and nothing else")
                              record kind (pr-str cname) (pr-str entry)))
                    (assoc {:error ::unknown-dependency
                            :record record
                            :dependency entry
                            :fields (vec (keys fields))
                            :io-deps io-deps}
                           what cname)))))

(defn resolve-deps
  "Resolve one arglist into the dependency keys it names, in the order it names them.

  The arglist is the dependency declaration, so this is where a declaration either means
  something or the form stops compiling. An entry is a symbol naming a field of this type,
  which resolves to that field's key, or a keyword naming an io-dep of this type, which is
  already its own name. Anything else -- a destructuring form, a rest arg, a parameter named
  `_` -- would be a dependency the macro could not resolve to anything, and is refused with
  the offender named.

  `ctx` is `{:record :what :name :fields :io-deps}`: the type, `:check` or `:io-dep`, the
  name of the thing being resolved, an ordered map of field symbol -> field key, and the set
  of io-dep names. Checks and io-deps are resolved the same way, because they declare
  dependencies the same way."
  [{:keys [fields io-deps] :as ctx} args]
  (mapv (fn [entry]
          (cond
            (symbol? entry)  (or (get fields entry) (unknown-dependency! ctx entry))
            (keyword? entry) (if (contains? io-deps entry)
                               entry
                               (unknown-dependency! ctx entry))
            :else            (unknown-dependency! ctx entry)))
        args))

(defn reject-duplicate-names!
  "A keyword in an arglist means an io-dep and a check never appears in one, so the two
  namespaces only collide where a single name is declared in both places -- at which point
  the reader of an arglist could no longer tell what it is looking at. Refuse it."
  [tname check-names io-dep-names]
  (when-let [dup (some (set io-dep-names) check-names)]
    (throw (ex-info (format "%s declares %s as both a check and an io-dep"
                            tname (pr-str dup))
                    {:error ::duplicate-name :record tname :name dup}))))

(def definition-sections
  "The sections a definition map may hold, in the order `SPEC.md` writes them. Every one of
  them is optional; an absent section is an empty one."
  [:options :checks :io-deps :certificates])

(def known-options
  "What `:options` may hold. `:io-limited` is the only one so far -- the section exists so
  that there can be more."
  #{:io-limited})

(defn parse-definition
  "`defrecord`'s third position -> `{:options :checks :io-deps :certificates}`, with every
  section the form left out filled in empty, so nothing downstream has to ask whether it
  was written.

  Every key is resolved here, while the macro runs, for the same reason every arglist entry
  is: a section the macro does not recognise is a declaration that would mean nothing. A
  mistyped `:cheks` would quietly define a type with no checks at all -- a value asserting
  a validation that never ran, which is the one failure this library exists to make
  impossible -- so it is a compile error naming the offender instead. The same goes for a
  key inside `:options`, and for a bare map of checks written where the definition map now
  goes, whose check names arrive here as sections nobody declared.

  `:io-deps` is part of the form before it is part of the library. An empty one compiles,
  so a form may be written in its final shape; a populated one is refused rather than
  accepted and ignored, which would be the silent mis-declaration again, wearing a
  different hat."
  [tname definition]
  (when-not (map? definition)
    (throw (ex-info (format (str "%s: defrecord takes a name, a vector of fields and a "
                                 "definition map -- %s is not a map")
                            tname (pr-str definition))
                    {:error ::malformed-definition :record tname :definition definition})))
  (doseq [k (keys definition)]
    (when-not (some #{k} definition-sections)
      (throw (ex-info (format (str "%s declares %s, which is not a section of a definition "
                                   "map -- the sections are %s%s")
                              tname (pr-str k) (pr-str definition-sections)
                              (if (qualified-keyword? k)
                                (str " (a check goes under :checks, which holds the map this "
                                     "position used to take)")
                                ""))
                      {:error ::unknown-section :record tname :section k
                       :sections definition-sections}))))
  (let [sections (reduce (fn [acc s]
                           (let [v (get definition s {})]
                             (when-not (map? v)
                               (throw (ex-info (format "%s: %s is a map, and %s is not one"
                                                       tname s (pr-str v))
                                               {:error ::malformed-section :record tname
                                                :section s :value v})))
                             (assoc acc s v)))
                         {} definition-sections)]
    (doseq [o (keys (:options sections))]
      (when-not (contains? known-options o)
        (throw (ex-info (format (str "%s: :options holds %s, which is not an option -- the "
                                     "options are %s")
                                tname (pr-str o) (pr-str known-options))
                        {:error ::unknown-option :record tname :option o
                         :options known-options}))))
    (when (seq (:io-deps sections))
      (throw (ex-info (format (str "%s declares :io-deps, which the library does not implement "
                                   "yet -- the section is accepted only empty, so that nothing "
                                   "is declared and then ignored")
                              tname)
                      {:error ::unsupported-section :record tname :section :io-deps
                       :declared (vec (keys (:io-deps sections)))})))
    sections))

(def mask-width
  "How many checks a type may declare. A draft's verdict mask is one `long`, so a 65th check
  would alias the first: a passing check 0 would make check 64 count as passed, and a value
  could seal on a check that never ran. Refused where it is written rather than discovered
  by whatever the value goes on to do.

  The mask of written fields has the same width, and nothing can reach it: a type of 21
  fields does not compile, because its positional constructor would be a function of 21
  parameters and Clojure allows 20."
  64)

(defn reject-too-many-checks!
  "Refuse a type with more checks than one `long` has bits."
  [tname n]
  (when (> n mask-width)
    (throw (ex-info (format (str "%s declares %d checks, and %d is the limit -- a draft's "
                                 "verdict mask is one long, and a wider type would alias "
                                 "one check's bit onto another's")
                            tname n mask-width)
                    {:error ::too-many-checks :record tname :count n :limit mask-width}))))

(defn parse-certificates
  "The `:certificates` section -> certificate name -> `{:checks :ids :mask}`, with the
  built-in [[every-check]] appended. `check-names` is the type's checks in declaration
  order, which is the order a certificate is read back in whatever order it was written.

  A certificate is a declaration like any other here, so it means something or the form
  does not compile. A name it gives that is not a check of this type would be a claim about
  a check that does not exist; a certificate covering nothing would be held by every draft
  of the type, including one nothing has been run against, so `certify` would answer `true`
  while asserting precisely nothing; and a name already spelling a check, or spelling the
  built-in, would make the same word mean two things inside one type.

  `:mask` is the certificate's checks as bits of a draft's verdict mask, so holding one is
  a single `and` at run time rather than a walk over names."
  [tname certificates check-names]
  (let [known (vec check-names)
        ids   (zipmap known (range))
        bad!  (fn [cname why]
                (throw (ex-info (format "%s certificate %s %s" tname (pr-str cname) why)
                                {:error ::malformed-certificate :record tname
                                 :certificate cname})))
        one   (fn [cname names]
                (when-not (or (set? names) (sequential? names))
                  (bad! cname (format (str "is %s -- a certificate is the collection of check "
                                           "names it covers")
                                      (pr-str names))))
                (when (empty? names)
                  (bad! cname (str "covers no check -- a certificate covering nothing would be "
                                   "held by every draft of this type, including one nothing "
                                   "has been run against")))
                (doseq [c names]
                  (when-not (contains? ids c)
                    (throw (ex-info (format (str "%s certificate %s covers %s, which is not a "
                                                 "check of %s -- its checks are %s")
                                            tname (pr-str cname) (pr-str c) tname (pr-str known))
                                    {:error ::unknown-check :record tname :certificate cname
                                     :check c :checks known}))))
                (let [idxs (vec (sort (into #{} (map ids) names)))]
                  {:checks (mapv known idxs)
                   :ids    idxs
                   :mask   (reduce bit-set 0 idxs)}))]
    (doseq [cname (keys certificates)]
      (when (= cname every-check)
        (throw (ex-info (format (str "%s declares a certificate named %s, which is the built-in "
                                     "covering every check on the type")
                                tname (pr-str every-check))
                        {:error ::reserved-certificate :record tname :certificate cname})))
      (when (contains? ids cname)
        (throw (ex-info (format "%s declares %s as both a check and a certificate"
                                tname (pr-str cname))
                        {:error ::duplicate-name :record tname :name cname}))))
    (apply array-map
           (into [] cat
                 (concat (map (fn [[cname names]] [cname (one cname names)]) certificates)
                         [[every-check {:checks known
                                        :ids    (vec (range (count known)))
                                        :mask   (reduce bit-set 0 (range (count known)))}]])))))

(defn- parse-check
  "One entry of the `:checks` map -> `{:name :doc :args :fn}`.

  Three forms, told apart by what the value starts with: a string is a docstring, a vector
  is an arglist, and anything else is the shorthand -- a bare function over the single field
  the check's key names."
  [tname cname form]
  (let [bad! (fn [why]
               (throw (ex-info (format "%s check %s %s" tname (pr-str cname) why)
                               {:error ::malformed-check :record tname :check cname :form form})))]
    (cond
      (and (seq? form) (string? (first form)))
      (let [[doc args & body] form]
        (when-not (vector? args) (bad! "has a docstring but no arglist"))
        (when-not (seq body) (bad! "has an arglist but no body"))
        {:name cname :doc doc :args args :fn `(fn ~args ~@body)})

      (and (seq? form) (vector? (first form)))
      (let [[args & body] form]
        (when-not (seq body) (bad! "has an arglist but no body"))
        {:name cname :doc nil :args args :fn `(fn ~args ~@body)})

      (and (seq? form) (seq? (first form)) (vector? (ffirst form)))
      (bad! "gives more than one arity -- the arglist is the dependency list, so there is exactly one")

      :else
      {:name cname :doc nil :args [(symbol (name cname))] :fn form})))

(def reserved-methods
  "The method names the generated types already carry, and must keep. A protocol method
  spelled one of these would replace the implementation that makes a value behave as a map,
  or the one holding a draft's verdicts: the form would still compile, and the type would
  quietly stop being what it says it is."
  '#{get assoc dissoc keys meta with-meta empty cons getLookupThunk
     verdict-mask record-verdict!})

(defn parse-impls
  "`defrecord`'s trailing forms -> `[:type sym]` and `[:method form]` pairs, in the order
  they were written.

  One body is installed on two types, so what each form is has to be settled here, while
  the macro runs. A form that is neither a protocol or interface name nor a method
  implementation names nothing either type could carry; a method written before any such
  name belongs to nothing; and an arglist with no receiver leaves the sealed implementation
  nothing to open a draft of. Each is refused with the offender named, rather than handed
  to `deftype` to fail somewhere further down with the shape of the original form gone."
  [tname impls]
  (let [bad! (fn [form why]
               (throw (ex-info (format "%s: %s %s" tname (pr-str form) why)
                               {:error ::malformed-impl :record tname :form form})))]
    (first
     (reduce
      (fn [[acc named?] form]
        (cond
          (symbol? form) [(conj acc [:type form]) true]

          (seq? form)
          (do
            (when-not named?
              (bad! form (str "is a method implementation written before any protocol or "
                              "interface name, so there is nothing for it to implement")))
            (when-not (symbol? (first form))
              (bad! form "does not begin with a method name"))
            (when-not (vector? (second form))
              (bad! form (str "has no arglist -- a method implementation is a name, an "
                              "arglist and a body")))
            (when (empty? (second form))
              (bad! form (str "has an arglist with no receiver, and the receiver is what "
                              "the sealed implementation opens a draft of")))
            (when (contains? reserved-methods (first form))
              (throw (ex-info (format (str "%s implements %s, which is one of the methods "
                                           "that make a %s behave as a map -- a protocol "
                                           "method of that name would replace it")
                                      tname (first form) tname)
                              {:error ::reserved-method :record tname :method (first form)
                               :form form :reserved reserved-methods})))
            [(conj acc [:method form]) named?])

          :else
          (bad! form (str "is neither a protocol or interface name nor a method "
                          "implementation"))))
      [[] false] impls))))

(defmacro defrecord
  "Define a validated map-like type with a real field per declared key, so reads and writes
  are field access rather than map lookups.

  Three positions: the type name, the fields, and a definition map of named sections --

      (rec/defrecord Heist
        [name crew take cuts]
        {:options      {:io-limited true}
         :checks       {::name non-blank-string?}
         :io-deps      {}
         :certificates {::shape #{::name}}})

  Every section is optional and an absent one is an empty one, but a key that names no
  section at all is a compile error: a mistyped `:cheks` would define a type with no checks,
  and a value asserting a validation that never ran is the one thing here that must not be
  constructible. See [[parse-definition]]. `:io-deps` is accepted only empty until the
  library implements it.

  `:checks` maps a check name -- a namespace-qualified keyword, unique within the type, and
  what failures report -- to a check, written in one of three forms:

      ::name       non-blank-string?                      ; a bare fn over the field it names
      ::valid-name ([name] (non-blank-string? name))      ; arglist and body
      ::cuts-sum-to-take (\"The cuts add up to the take.\"  ; with a docstring
                          [take cuts] (= take (reduce + 0 (vals cuts))))

  The arglist is the dependency declaration. The body receives exactly what it names,
  positionally, in that order, and nothing else is in scope -- so nothing can depend on
  something it did not name, and the field -> checks index the macro derives from those
  arglists cannot disagree with the code. Exactly one arity: the arity is the dependency set.

  Every entry in an arglist is resolved here, while the macro runs: a symbol names a field
  of this type and a keyword names an io-dep of this type. One that names neither is a
  compile error naming the offender, so a dependency either means something or the form does
  not compile -- there is no way to write a check indexed under a key no write can produce.

  A check passes by returning `true` and nothing else; whatever else it returns is the
  failure data, reported under the check's name alongside its docstring and the fields it
  declared. Checks have no ordering among themselves and every one of them runs, so a
  check spanning two fields sees whatever those fields hold, including shapes the checks
  over them are rejecting in the same pass -- it restates what it needs rather than
  assuming an earlier check stopped anything. An exception is not caught: it is a bug in
  the check rather than a claim about the data, and \"we could not determine whether this
  is valid\" is not \"this is invalid\".

  `:certificates` maps a certificate name to the checks it covers -- a group a draft can be
  run through before it is complete, see [[certify]]. Holding one is derived from which
  checks are passing rather than recorded anywhere, so nothing coordinates between them,
  and a certificate changes nothing about what sealing demands. A name it covers that is
  not a check of this type, a certificate covering nothing, or one spelled like a check or
  like the built-in [[every-check]], is a compile error naming the offender. See
  [[parse-certificates]].

  A field becomes a key qualified by the namespace this form is written in -- a `Heist`
  defined in `my.app.heist` answers to `:my.app.heist/name`, which is `::name` here and
  `::h/name` wherever the namespace is aliased. A bare keyword is a key the type does not
  declare, so two types that happen to share a field name share nothing else.

  Trailing forms are protocol implementations, written once and installed on both generated
  types. On the draft type the body runs as written. On the sealed type it is wrapped: the
  method opens a draft of the receiver, runs the body against that, and seals the result --
  so a method called on a sealed value cannot produce an invalid one whatever it does
  inside, and the same body serves the impl layer unchanged. Only a draft of this type is
  sealed on the way out; a number, a string, an already-sealed value, a draft of some other
  type, all pass through untouched. The receiver is a draft in both cases, so a body asking
  `(instance? Heist h)` would surprise itself and wants [[of-type?]]. [[unsealed]] opts a
  return out of the seal."
  [tname fields definition & impls]
  (let [{:keys [options checks certificates]} (parse-definition tname definition)
        impl-forms (parse-impls tname impls)
        model-sym (symbol (str tname "-model"))
        fsyms     (mapv #(with-meta % nil) fields)
        ;; The defining namespace qualifies the keys, so a field named `name` is
        ;; `:this.ns/name` and two types sharing a field name share nothing else.
        key-ns    (str (ns-name *ns*))
        qkey      (fn [f] (keyword key-ns (str f)))
        ks        (mapv qkey fields)
        n         (count fsyms)
        key->sym  (zipmap ks fsyms)
        ;; every local is gensymed: a field could be named k, v, o, acc ...
        [ksym vsym dsym osym hsym essym ovsym accsym mtasym mta2sym msym thunksym tgtsym isym]
        (map gensym ["k" "v" "d" "o" "h" "es" "ov" "acc" "mta" "mta2" "m" "thunk" "target" "i"])
        tgthint   (with-meta tgtsym {:tag tname})
        dname     (symbol (str tname "Draft"))
        [extsym vdsym ownsym tokensym bitsym passym]
        (map gensym ["ext" "verdicts" "own" "token" "bit" "pass"])
        ;; The verdicts are the one thing on a draft that changes in place: running a check
        ;; establishes something about data that is already there rather than producing new
        ;; data. A mutable field is private to its class, so everything outside the type
        ;; reaches it through `verdict-mask`.
        vdfield   (with-meta vdsym {:unsynchronized-mutable true :tag 'long})
        [selfsym toksym] (map gensym ["self" "token"])
        dvsym     (with-meta (gensym "d") {:tag dname})
        svsym     (with-meta (gensym "s") {:tag tname})
        dfield    (fn [f] (list '. dvsym (symbol (str "-" f))))
        sfield    (fn [f] (list '. svsym (symbol (str "-" f))))
        dctor     (fn [vals ext verdicts own mta]
                    `(new ~dname ~@vals ~ext ~verdicts ~own ~mta))
        dfields   (mapv dfield fsyms)
        params    (conj fsyms mtasym)
        ;; (Heist. a b c mta) with position i replaced by `x`
        ctor-with (fn [i x] `(new ~tname ~@(assoc fsyms i x) ~mtasym))
        tagsym    (symbol key-ns (str tname))
        ;; ---- the checks ----
        ;; Ordered, so a rejected arglist can name the fields it could have named in the
        ;; order they were declared rather than in hash order. `array-map` built this way
        ;; stays one at any size; `into` an empty one would not.
        fieldmap  (apply array-map (interleave fsyms ks))
        ;; `:io-deps` arrives in E1. Until then a type declares none, so every keyword in an
        ;; arglist is unresolvable -- which is what it should be, and is reported as an
        ;; unknown io-dep rather than as a stray symbol.
        io-deps   #{}
        parsed    (mapv (fn [[cname form]]
                          (let [c (parse-check tname cname form)]
                            (assoc c :deps (resolve-deps {:record tname :what :check :name cname
                                                          :fields fieldmap :io-deps io-deps}
                                                         (:args c)))))
                        (seq checks))
        _         (reject-duplicate-names! tname (map :name parsed) io-deps)
        nchecks   (count parsed)
        _         (reject-too-many-checks! tname nchecks)
        ;; Every check passing, which is what a draft of a sealed value starts from. At the
        ;; full width the mask is every bit of the long, which `(dec (bit-shift-left 1 n))`
        ;; would not give -- the shift wraps.
        all-pass  (if (= nchecks mask-width) -1 (dec (bit-shift-left 1 nchecks)))
        ;; Resolved here for the same reason an arglist is: a certificate naming a check
        ;; that does not exist, or covering nothing at all, is a claim about a validation
        ;; nobody wrote. The built-in comes back on the end of this.
        certs     (parse-certificates tname certificates (mapv :name parsed))
        ;; What a sealed value adds to its own metadata: every check passed, so every
        ;; certificate is held. Both are facts about the type rather than about the value,
        ;; so this is computed once here and embedded as a constant -- a sealed value with
        ;; no metadata of its own answers with this very map, and allocates nothing.
        sealed-meta (array-map ::passed       (set (map :name parsed))
                               ::certificates (set (keys certs)))
        cvar      (fn [i] (symbol (str tname "--check-" i)))
        deps-of   (fn [i] (:deps (nth parsed i)))
        ;; Derived from the arglists, so it cannot be misdeclared: there is nowhere to
        ;; declare it. Sorted vectors, because a sorted set does not survive being embedded
        ;; in emitted code -- the compiler rebuilds a set literal as a hash set.
        index     (reduce (fn [idx i] (reduce #(update %1 %2 (fnil conj []) i) idx (deps-of i)))
                          {} (range nchecks))
        ;; What a write to `k` leaves standing: every verdict except the checks naming `k`.
        ;; Derived from the same index, so a field a check reads is a field that invalidates
        ;; it, and one bitwise `and` seeds the new draft. Computed here, so what the write
        ;; costs at run time is the `and` and nothing else.
        survives  (fn [k] (reduce bit-clear all-pass (get index k)))
        ;; ---- the protocol implementations ----
        ;; The draft type runs the body as written; only the `unsealed` marker is unwrapped,
        ;; so a body means the same thing wherever it runs.
        draft-impls (mapv (fn [[kind form]]
                            (if (= kind :type)
                              form
                              (let [[mname args & body] form]
                                `(~mname ~args (draft-impl-return ~model-sym (do ~@body))))))
                          impl-forms)
        ;; The sealed type wraps it: the receiver the body sees is a draft of the value,
        ;; owned by this call, so an enclosing-style `drafting` inside the body passes it
        ;; straight through and only the wrapper seals. What comes back is sealed only if
        ;; it is a draft of this type -- most methods return something else entirely.
        sealed-impls (mapv (fn [[kind form]]
                             (if (= kind :type)
                               form
                               (let [[mname args & body] form
                                     recv (first args)]
                                 `(~mname [~(with-meta selfsym (meta recv)) ~@(rest args)]
                                    (sealed-impl-return
                                     ~model-sym
                                     (let [~toksym (Object.)]
                                       (binding [*owner* ~toksym]
                                         (let [~recv ((:draft-from ~model-sym)
                                                      ~selfsym ~toksym)]
                                           ~@body))))))))
                           impl-forms)
        ;; Run the checks at `idxs` over the expressions `argvs-of` gives for each -- locals
        ;; or direct field reads, so evaluating one twice costs nothing. Every check in the
        ;; group is called before any verdict is read, because a failure does not stop the
        ;; pass; then every one that did not return `true` is reported together. Nothing is
        ;; caught, so a check that throws leaves the `let` before there is anything to
        ;; report, which is the contract: a throw is a bug rather than a verdict. The value
        ;; maps are built only inside the branch that is about to throw.
        run-group (fn [idxs argvs-of]
                    (when (seq idxs)
                      (let [rs (mapv #(gensym (str "r" % "-")) idxs)]
                        `(let [~@(mapcat (fn [i r] [r `(~(cvar i) ~@(argvs-of i))]) idxs rs)]
                           (when-not (and ~@(map (fn [r] `(true? ~r)) rs))
                             (checks-failed!
                              ~model-sym
                              [~@(map (fn [i r]
                                        `(when-not (true? ~r)
                                           (failure-entry (nth (:checks ~model-sym) ~i) ~r
                                                          (array-map
                                                           ~@(interleave (deps-of i)
                                                                         (argvs-of i))))))
                                      idxs rs)]))))))]
    `(do
       (declare ~model-sym)
       ~@(map (fn [i] `(def ~(with-meta (cvar i) {:private true}) ~(:fn (nth parsed i))))
              (range nchecks))

       (p/def-map-type ~tname ~params
         (~'get [_# ~ksym ~dsym]
           (case ~ksym ~@(interleave ks fsyms) ~dsym))

         ;; The checks naming this field are known here, so they are called with the new
         ;; value and the untouched fields directly -- no lookup, and no allocation unless
         ;; they all pass.
         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [j k]
                         [k `(do ~(run-group (get index k)
                                             (fn [i] (map (fn [d]
                                                            (if (= d k) vsym (key->sym d)))
                                                          (deps-of i))))
                                 ~(ctor-with j vsym))])
                       (range n) ks)
             (unknown-key! ~model-sym ~ksym)))

         ;; A validated value cannot hold an undeclared key, so dissoc of one is the
         ;; ordinary map no-op. Only the declared fields are permanent.
         (~'dissoc [this# ~ksym]
           (case ~ksym
             ~(apply list ks) (no-dissoc! ~model-sym ~ksym)
             this#))
         (~'keys [_#] ~ks)
         ;; The library's keys over the caller's, so nothing a caller writes can claim more
         ;; than the value can show for itself. Here what it shows is constant: a sealed
         ;; value is one every check passed on.
         (~'meta [_#] (merged-meta ~mtasym ~sealed-meta))
         (~'with-meta [_# ~mta2sym] (new ~tname ~@fsyms ~mta2sym))

         ;; one allocation for the whole merge, each applicable check once at the end
         (~'cons [_# ~osym]
           (let [~essym (entries-of ~osym)
                 ~ovsym (reduce (fn [~accsym [~ksym ~vsym]]
                                  (declared-field! ~model-sym ~ksym)
                                  (assoc ~accsym ~ksym ~vsym))
                                {} ~essym)
                 ~hsym  (new ~tname
                             ~@(map (fn [k f] `(get ~ovsym ~k ~f)) ks fsyms)
                             ~mtasym)]
             (run-checks! ~model-sym (:run-sealed ~model-sym) ~hsym
                          (checks-for ~model-sym (map first ~essym)))
             ~hsym))

         ;; A keyword call site caches one of these and reads the field directly, the way a
         ;; record does; without it `(:k x)` pays the full Keyword -> RT -> ILookup -> valAt walk.
         clojure.lang.IKeywordLookup
         (~'getLookupThunk [_# ~ksym]
           (case ~ksym
             ~@(mapcat (fn [[k f]]
                         [k `(reify clojure.lang.ILookupThunk
                               (~'get [~thunksym ~tgtsym]
                                 (if (identical? (class ~tgtsym) ~tname)
                                   (. ~tgthint ~(symbol (str "-" f)))
                                   ~thunksym)))])
                       (map vector ks fsyms))
             nil))

         ~@sealed-impls)

       ;; The draft: the same real fields, plus a map for keys the type does not declare
       ;; and a mutable bitmask of which checks currently pass. Which fields have been
       ;; written is read off the fields themselves, through the `unset` sentinel, and
       ;; which checks are still to run is read off the mask -- so there is nothing else
       ;; for a write to maintain.
       (p/def-map-type ~dname ~(into fsyms [extsym vdfield ownsym mtasym])
         (~'get [_# ~ksym ~dsym]
           (case ~ksym
             ~@(mapcat (fn [[k f]] [k `(if (identical? ~f unset) ~dsym ~f)]) (map vector ks fsyms))
             (get ~extsym ~ksym ~dsym)))
         ;; A write seeds the new draft's verdicts from this one's, clearing exactly the
         ;; checks naming the field written -- not the ones whose values moved, which is a
         ;; different and unknowable set. The seed is a snapshot: this draft keeps what it
         ;; had established, so two versions say nothing about each other.
         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [i k]
                         [k (dctor (assoc fsyms i vsym) extsym
                                   `(bit-and ~vdsym ~(survives k)) ownsym mtasym)])
                       (range n) ks)
             ~(dctor fsyms `(assoc ~extsym ~ksym ~vsym) vdsym ownsym mtasym)))
         (~'dissoc [this# ~ksym]
           (case ~ksym
             ~(apply list ks) (no-dissoc! ~model-sym ~ksym)
             (if (contains? ~extsym ~ksym)
               ~(dctor fsyms `(dissoc ~extsym ~ksym) vdsym ownsym mtasym)
               this#)))
         (~'keys [_#]
           (concat (cond-> [] ~@(mapcat (fn [[k f]] [`(not (identical? ~f unset)) `(conj ~k)])
                                        (map vector ks fsyms)))
                   (keys ~extsym)))
         ;; The same two keys a sealed value answers with, read off the mask rather than
         ;; fixed: on a draft they are what has actually been established. Merged over the
         ;; caller's metadata for the same reason, and the caller's own keys survive.
         (~'meta [_#] (draft-meta ~model-sym ~vdsym ~mtasym))
         (~'with-meta [_# ~mta2sym] ~(dctor fsyms extsym vdsym ownsym mta2sym))
         (~'cons [this# ~osym]
           (reduce (fn [~accsym [~ksym ~vsym]] (assoc ~accsym ~ksym ~vsym)) this# (entries-of ~osym)))

         ;; The only thing about a draft that changes in place, and the only two operations
         ;; allowed to change it.
         Verdicts
         (~'verdict-mask [_#] ~vdsym)
         (~'record-verdict! [_# ~bitsym ~passym]
           (set! ~vdsym (long (if ~passym (bit-set ~vdsym ~bitsym) (bit-clear ~vdsym ~bitsym))))
           ~passym)

         ~@draft-impls)

       (def ~model-sym
         {:name        '~tname
          :key-ns      ~key-ns
          :options     ~options
          :field-order ~ks
          :field-set   ~(set ks)
          :checks      ~(mapv (fn [i]
                                (let [{:keys [name doc deps]} (nth parsed i)]
                                  {:name name :doc doc :deps deps :pred (cvar i)}))
                              (range nchecks))
          ;; check name -> its index, which is its bit in a draft's verdict mask. Built
          ;; here because a check is named from outside by name and reached from inside by
          ;; index, and a scan of `:checks` per call would be the only alternative.
          :check-ids   ~(into {} (map-indexed (fn [i c] [(:name c) i])) parsed)
          :index       ~index
          ;; certificate name -> what it covers, as check names, indices, and the bits of
          ;; a draft's verdict mask they own -- so holding one is an `and`. `array-map`
          ;; applied to the pairs, rather than a literal, which would promote and reorder
          ;; past the eighth certificate.
          :certificates (array-map ~@(into [] cat certs))
          :tag         '~tagsym
          ;; One runner per type, so a check reached by index is still called with direct
          ;; field access rather than a lookup per dependency.
          :run-sealed  (fn [~isym ~svsym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i) ~@(map (comp sfield key->sym) (deps-of i)))])
                                     (range nchecks))
                           nil))
          :run-draft   (fn [~isym ~dvsym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i) ~@(map (comp dfield key->sym) (deps-of i)))])
                                     (range nchecks))
                           nil))
          ;; From the type, nothing is established; from a sealed value, everything is --
          ;; that is what being sealed means, and the draft inherits exactly that claim.
          :draft-empty      (fn [~ownsym] ~(dctor (repeat n `unset) {} 0 ownsym nil))
          ;; The metadata comes off the field rather than through `meta`, which would hand
          ;; back the library's keys merged in and store a snapshot of them as though the
          ;; caller had written it. What the draft inherits is what the caller actually set.
          :draft-from       (fn [~svsym ~ownsym]
                              ~(dctor (mapv sfield fsyms) {} all-pass ownsym (sfield mtasym)))
          :draft-owner      (fn [~dvsym] ~(dfield ownsym))
          :draft-ext        (fn [~dvsym] ~(dfield extsym))
          ;; Neither changes a field, so neither disturbs a verdict: the mask is copied
          ;; across as it stands. (Through the protocol, since a mutable field is private to
          ;; the class that declares it.)
          :draft-with-owner (fn [~dvsym ~tokensym]
                              ~(dctor dfields (dfield extsym)
                                      `(verdict-mask ~dvsym) tokensym (dfield mtasym)))
          :draft-clean      (fn [~dvsym]
                              ~(dctor dfields {} `(verdict-mask ~dvsym)
                                      (dfield ownsym) (dfield mtasym)))
          :draft-missing    (fn [~dvsym]
                              (cond-> [] ~@(mapcat (fn [[k f]] [`(identical? ~(dfield f) unset) `(conj ~k)])
                                                   (map vector ks fsyms))))
          :draft-seal       (fn [~dvsym] (new ~tname ~@dfields ~(dfield mtasym)))})
       ;; `deftype` interns a positional factory that checks nothing -- a public back door
       ;; straight through every guarantee the type exists to make. Replace it with one that
       ;; validates, and unmap the draft's, which nothing outside this file should be calling.
       (defn ~(symbol (str "->" tname))
         ~(str "Construct a validated " tname " from its fields, in declaration order.")
         [~@fsyms]
         ~(run-group (range nchecks) (fn [i] (map key->sym (deps-of i))))
         (new ~tname ~@fsyms nil))

       (defn ~(symbol (str "map->" tname))
         ~(str "Construct a validated " tname " from a map holding exactly its declared keys.\n"
               "  A key it does not declare is an error; a key it declares and the map omits is\n"
               "  reported as :missing-field, so `nil` and absent stay distinct.\n\n"
               "  Keys may be written qualified or bare: a bare keyword naming a declared field is\n"
               "  qualified into " key-ns ", which is what lets a printed literal -- whose tag\n"
               "  already carries the namespace -- read back through here and be validated.")
         [~msym]
         (seal ~model-sym (reduce-kv assoc ((:draft-empty ~model-sym) nil)
                                     (qualify-keys ~model-sym ~msym))))

       (register! ~tname ~model-sym)
       (register-draft! ~dname ~model-sym)
       (register-reader! '~tagsym ~(symbol (str "map->" tname)))

       (ns-unmap *ns* '~(symbol (str "->" dname)))
       ;; A draft is unvalidated working state. `#<` is the reader's own unreadable marker, so
       ;; the printed form says so rather than implying a round trip that should not exist.
       (defmethod print-method ~dname [x# ^java.io.Writer w#]
         (.write w# (str "#<" '~tname " draft " (into (sorted-map) x#) ">")))
       ;; The tag is namespace-qualified, so it escapes the reader's record-literal path and is
       ;; the exact symbol to hand `clojure.edn/read` as a key in `:readers`.
       (defmethod print-method ~tname [x# ^java.io.Writer w#]
         (.write w# (str "#" '~tagsym (printable x#))))
       ~tname)))
