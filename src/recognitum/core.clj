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
  once with all of it, keyed by check name.

  A check named after a field is that field's shape validation: the statement of what a
  well-formed value of that field is. It names exactly that field or the form does not
  compile, and every other check waits on the shape validations of the fields it names -- so a
  check spanning two fields receives well-formed input by construction and restates nothing,
  and one whose gate has not passed neither runs nor appears among the failures. That makes
  three phases, which is the order failures come back in: shape validations, then invariant
  validations, then external validations, in declaration order within each.

  A field nobody has written reads as `nil`, everywhere, exactly as it would on a
  `defrecord` whose field was never set. There is no sentinel and no state distinct from
  `nil`: a draft opened from the type starts with every field `nil`, and whether that will
  do is the shape validation's business and nobody else's.

  A write to a validated value runs the checks naming the field written, so such a value
  is never observable in an invalid state. A draft is not a validated value: writes into
  it check nothing, which is what lets a multi-step update pass through states no single
  write could produce. [[seal]] closes the gap -- what it returns is validated exactly as
  if every check had been rerun.

  Fields become keys qualified by the namespace defining the type, so a bare keyword of
  the same name is a key the type does not declare.

  An io-dep is a fetch declared the same way a check is, and named the same way: a keyword
  in an arglist. Shape gates it -- nothing reaches the outside until the fields it will
  carry there have passed the checks over them -- and it is fetched once per validation
  call however many checks name it, then forgotten. It may be written singular, `[crew]`, or
  batched, `[[crew] :as rows]`, which is handed the argument tuples and answers a map keyed
  by them; the declaration is the same either way and only the call differs. What it
  returned is nobody's business but the checks naming it: `nil` is neither a failure nor a
  success, and a throw is the database being unreachable rather than a verdict about the
  data.

  A protocol implementation is written once and installed on both generated types. The
  draft runs the body as written; the sealed value wraps it, so the body works on a draft
  and what it produces is sealed on the way out -- which is the same re-entrancy
  [[drafting]] provides, applied without anyone asking for it.

  A draft also records which of its checks currently pass. That record is a mutable field:
  the draft's data is immutable, but which checks have been run against that data is not
  part of the data. See [[passed]]. [[check]] and [[check!]] run what they name against a
  value and answer the question rather than handing back a value -- the verdict goes in the
  mask, and a check the mask already says passes is not run again. [[seal]] reads the same
  mask: it runs whatever is not already passing, so work done a check at a time on a draft
  is not repeated when the draft closes.

  One verb covers both scales of that question. A certificate names a subset of a type's
  checks, so a draft can be run through a group of them before it is complete, and a lone
  check is a certificate containing itself: [[check]] takes either name and answers in one
  shape, `true` or a map keyed by check name. Whether a value *holds* a certificate is
  derived from the mask rather than recorded anywhere ([[certificates]]), so overlapping
  certificates need no coordination and individual checks can complete one incidentally.
  Certificates change nothing about what a sealed value guarantees: [[seal]] enforces every
  check on the type, and merely finds an established draft's share already done.

  [[check]] *establishes* -- it skips whatever the mask already says passes, and a sealed
  value, where everything is established, answers `true` without running anything.
  [[recheck]] and its relatives run what they name regardless, which is the only way to ask
  whether a verdict about the outside world still holds: on a draft the answer is written
  back, so a check that passed yesterday and fails today is un-passed and the next seal
  runs it again.

  `meta` answers the same way from either form. A value's own metadata comes back with
  `::passed` and `::certificates` merged over the top -- read off the mask for a draft, and
  a constant of the type for a sealed value, where every check passed and every certificate
  therefore holds. So nothing has to establish which form it is holding before asking, and
  because the library's keys are written last, nothing a caller sets can claim more than
  the value can show for itself."
  (:refer-clojure :exclude [defrecord])
  (:require
   [recognitum.collections :as rc]))

(def ^:private core-ns
  "This namespace's own name, for the expansions below to spell an internal with. Read here
  rather than where it is used, because `*ns*` inside a macro is the namespace expanding it."
  (ns-name *ns*))

(defn- internal-var
  "`(var recognitum.core/nm)`, for generated code to reach an internal by."
  [nm]
  (list 'var (symbol (str core-ns) (name nm))))

(defn- internal
  "The form generated code refers to an internal of this namespace by: the var, dereferenced.

  Everything the public vocabulary does not name is private, and a macro expands into
  somebody else's namespace -- where a qualified symbol naming a private var does not
  compile. That refusal is the whole of what privacy is worth here: it is why
  `record-verdict!` being private closes the door on writing a draft's verdict mask, and why
  `model-of` being private closes the door on `:draft-seal`, which turns a draft into a
  sealed value and runs nothing.

  The var itself stays reachable, as every var does -- privacy in Clojure is advisory -- so
  the expansion reaches it, emitting `@#'recognitum.core/checks-failed!` where it would
  otherwise have written the name. What that costs is one `Var.deref` per call site against
  a `getRawRoot` for a public name, which is not what any of these calls are waiting on.

  The alternative is a public name for every helper a generated `assoc` happens to call --
  fifty-odd of them, two of which are a way past every check on the type."
  [nm]
  (list `deref (internal-var nm)))

(defonce ^:private registry (atom {}))

(defn- register! "Associate a class with its model map." [cls model] (swap! registry assoc cls model))

(defonce ^:private readers (atom {}))

(defn- register-reader! "Associate a print tag with its validating map constructor." [tag ctor]
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

(defn- unknown-key! [model k]
  (throw (ex-info (format "%s has no field %s" (:name model) (pr-str k))
                  {:error ::unknown-field :record (:name model) :key k
                   :declared (:field-order model)})))

(defn- dep-values
  "What a check was given, keyed by the dependency it was given for, in arglist order.
  Built only where a failure is about to be reported -- the passing path never has to say.

  An io-dep's value comes out of `cache`, the fetches this validation call made, rather
  than out of the value: what was fetched is not a key of anything, and reporting it as
  though the caller could look it up again would be naming a place instead of a thing."
  [model check x cache]
  (let [ios (:io-names model)]
    (apply array-map
           (mapcat (fn [d] [d (if (contains? ios d) (get cache d) (get x d))])
                   (:deps check)))))

(defn- failure-entry
  "The `[check-name report]` pair for a check that did not return `true`. `ret` is what it
  returned, which is the failure data; `vals` is what it was given."
  [check ret vals]
  [(:name check) {:doc (:doc check) :values vals :returned ret}])

(defn- failure-str [[cname {:keys [doc values returned]}]]
  (format "%s%s: given %s it returned %s"
          cname (if doc (str " (" doc ")") "") (pr-str values) (pr-str returned)))

(defn- checks-failed!
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

(defn- group-name
  "What a group call calls itself. One type is named as itself; a group of several is named
  as all of them, in the order they first appear, because there is no single type to speak
  for the rest and picking one would be a lie about the others."
  [models]
  (let [ns' (reduce (fn [acc m] (if (some #{(:name m)} acc) acc (conj acc (:name m))))
                    [] models)]
    (if (next ns') ns' (first ns'))))

(defn- bulk-failed!
  "Throw one `:check-failed` carrying every failure of every value in a group call, keyed by
  the value's index in the collection the caller handed over and, under that, by check name
  in declaration order. `fs` is `[index [[check-name report] ...]]` pairs, in index order.

  Keyed by index because two drafts holding identical data are `=`, so keying the report by
  the draft would collapse it exactly as keying the results would. `:bulk` says which of the
  two shapes this is, so nothing has to work out whether a key is a check name or a
  position.

  `array-map` applied to the pairs at both levels, for the reason [[checks-failed!]] builds
  its map that way: conj past the eighth entry would promote it and lose the order it is
  reporting in."
  [models fs]
  (let [nm (group-name models)
        fs (apply array-map
                  (into [] (mapcat (fn [[n es]] [n (apply array-map (into [] cat es))])) fs))
        n  (reduce + (map count (vals fs)))]
    (throw (ex-info (format "%s failed %d %s across %d of the values it was given -- %s"
                            (if (vector? nm) (apply str (interpose ", " nm)) nm)
                            n (if (= 1 n) "check" "checks") (count fs)
                            (apply str
                                   (interpose "; "
                                              (mapcat (fn [[i m]]
                                                        (map #(str i ": " (failure-str %)) m))
                                                      fs))))
                    {:error ::check-failed
                     :record nm
                     :bulk true
                     :indexes (vec (keys fs))
                     :failures fs}))))

(defn- phases
  "`[shape pure io]`: `idxs` split into the three phases anything here runs them in, each
  keeping the order it was given in -- which is declaration order everywhere these are
  built.

  Shape first, because a check named after a field is the statement of what that field is
  and everything else waits on it; the external validations last, because nothing reaches the
  outside until the values it will carry there are known to be well-formed. Within a phase
  there is no ordering to compute: shape validations depend on nothing, and every other check
  depends only on shape validations.

  One pass, because the usual answer is that nothing is IO-bound and most types have a
  shape validation for every field."
  [model idxs]
  (let [cs (:checks model)]
    (reduce (fn [[s p i] j]
              (let [c (nth cs j)]
                (cond
                  (:shape c)    [(conj s j) p i]
                  (seq (:io c)) [s p (conj i j)]
                  :else         [s (conj p j) i])))
            [[] [] []] idxs)))

(defn- gate-broken?
  "Did any shape validation gating the check at `i` fail in this pass? `failed` is the set of
  check indices that did.

  What a sealed value's paths ask, where there is no verdict mask to read: every check of
  the type passed when the value was made, so a gate that is not among the ones just run is
  a gate that passed."
  [model i failed]
  (boolean (some failed (:shape-gate (nth (:checks model) i)))))

(defn- batch-answer!
  "What a batched fetch returned, insisted on: a map holding an entry for every tuple it was
  handed, which is the one thing the batched form contracts to do.

  The singular form has no return contract, because its only consumers are the checks naming
  it. The batched form has one, because the library is the consumer first: it has to hand
  each value to the checks about the value it was fetched for, and the key is how it knows
  which is which. A result it cannot distribute is not a fetch that returned something odd;
  it is a fetch nobody can read.

  A missing tuple is refused rather than read as `nil` for the same reason. A tuple answered
  `nil` is an answer -- `nil` is whatever the checks naming it say it is -- and a tuple left
  out is not one. Passing the second off as the first would have a check judge a fetch that
  never happened."
  [model d res tuples]
  (when-not (or (map? res) (instance? java.util.Map res))
    (throw (ex-info (format (str "%s io-dep %s is batched, so it answers a map keyed by "
                                 "argument tuple -- it returned %s")
                            (:name model) (pr-str (:name d)) (pr-str res))
                    {:error ::malformed-batch :record (:name model) :io-dep (:name d)
                     :tuples tuples :returned res})))
  (let [missing (into [] (remove #(contains? res %)) tuples)]
    (when (seq missing)
      (throw (ex-info (format (str "%s io-dep %s answered nothing about %s -- a batched fetch "
                                   "answers every tuple it is handed, and a tuple answered nil "
                                   "is not a tuple left out")
                              (:name model) (pr-str (:name d))
                              (apply str (interpose ", " (map pr-str missing))))
                      {:error ::incomplete-batch :record (:name model) :io-dep (:name d)
                       :missing missing :tuples tuples :returned res}))))
  res)

(defn- answers-for
  "What the io-dep at `j` answers for `tuples`, keyed by tuple.

  Which form it was written in is settled here and nowhere else. A singular fetch is called
  with the elements of one tuple positionally and answers the value, so a group of tuples is
  a call each; a batched one is called with the whole collection and answers a map keyed by
  them. Everything downstream holds a value keyed by tuple either way, so nothing past this
  point has to know which was written.

  `tuples` is already distinct, and that is the deduplication: a hundred values referencing
  five crews are five tuples here, and each of them is fetched once whichever form was
  written. What the batched form adds is that those five arrive in one call."
  [model j tuples]
  (let [d (nth (:io-deps model) j)]
    (if (:batched d)
      (batch-answer! model d ((:run-batch model) j tuples) tuples)
      (into {} (map (fn [t] [t ((:call-io model) j t)])) tuples))))

(defn- fetch-io-all
  "Fetch, for every value in `xs`, every io-dep the checks at its own entry of `idxss` name,
  and answer with a vector of caches index-aligned with `xs` -- each one io-dep name -> what
  that io-dep returned for that value.

  One pass per io-dep rather than one pass per value, which is what lets a group share a
  fetch: the argument tuples of every value that needs it are built together, the distinct
  ones are fetched once, and the answer is distributed back to each value by the tuple it
  was asked about. Keying by the tuple rather than aligning positionally is what lets the
  library do that rather than the author.

  The order is the graph's: an io-dep may name another, and the model holds them already
  sorted so that a fetch is reached only after everything it names, which is why ascending
  index order is fetch order -- and why a tuple built out of what another fetch answered
  finds it in the cache by the time it is built.

  These maps are the whole of the sharing. Within one validation call an io-dep is fetched
  at most once per distinct tuple however many checks or values name it, and when the call
  returns they are garbage. Nothing carries them: a draft records which checks passed, never
  what they consulted to decide.

  An exception propagates. A fetch that throws is usually the database being unreachable,
  and \"we could not determine whether this is valid\" is a different claim from \"this is
  invalid\" -- folding one into the other would have the type assert what it does not know."
  [model xs idxss]
  (let [cs    (:checks model)
        xs    (vec xs)
        needs (mapv (fn [idxs] (into #{} (mapcat #(:io (nth cs %))) idxs)) idxss)]
    (reduce
     (fn [caches j]
       (let [nm (:name (nth (:io-deps model) j))
             vs (into [] (filter #(contains? (nth needs %) j)) (range (count xs)))
             ts (mapv (fn [n] ((:io-args model) j (nth xs n) (nth caches n))) vs)
             as (answers-for model j (into [] (distinct) ts))]
         (reduce (fn [caches [n t]] (update caches n assoc nm (get as t)))
                 caches
                 (map vector vs ts))))
     (vec (repeat (count xs) {}))
     (sort (reduce into #{} needs)))))

(defn- fetch-io
  "[[fetch-io-all]] for one value: the io-deps the checks at `idxs` name, fetched in the
  graph's order and answered as one cache. A group of one, so that a call spanning values
  and a call spanning one go through the same fetches."
  [model x idxs]
  (first (fetch-io-all model [x] [idxs])))

(defn- failures-of
  "`[i ret]` for those of `idxs` that did not return `true`, run through the type's own
  generated runner -- which reads the fields each check needs directly rather than looking
  them up. Only the failure path pays for a lookup, to say what it was given.

  Every one of them runs: a failure does not stop the pass. Only `true` passes, and nothing
  is caught -- a check that throws is a bug in the check rather than a verdict about the
  data."
  [run x idxs cache]
  (reduce (fn [acc i]
            (let [ret (run i x cache)]
              (if (true? ret) acc (conj acc [i ret]))))
          [] idxs))

(defn- failure-entries
  "The report for `fs`, which is [[failures-of]]'s answer: the check's docstring, the
  dependencies it declared with what it was handed for each, and what it returned."
  [model x fs cache]
  (map (fn [[i ret]]
         (let [c (nth (:checks model) i)]
           (failure-entry c ret (dep-values model c x cache))))
       fs))

(defn- run-group!
  "Run `idxs` and throw once carrying every failure among them."
  [model run x idxs cache]
  (let [fs (failures-of run x idxs cache)]
    (when (seq fs) (checks-failed! model (failure-entries model x fs cache)))))

(defn- run-io-checks!
  "Phase three for a value that is not a draft: fetch what the checks at `idxs` name and run
  them against it.

  Reached only once the pure checks have passed, which is the gate -- so the generated
  constructors and `assoc` call this after their own inline group and not before."
  [model x idxs]
  (run-group! model (:run-sealed model) x idxs (fetch-io model x idxs)))

(defn- run-checks!
  "Run the checks at `idxs` against `x` and throw once with every failure, in phase order.

  Three phases, because shape gates everything. The shape validations run first and ungated;
  then the other pure checks, each of them only if the shape validations of the fields it
  names came back `true` -- so a check spanning two fields is never handed a shape its own
  type rejected in the same pass, and a check the gate stopped is neither run nor reported,
  since \"cuts is not a map\" is the actionable failure and \"did not run because cuts is not
  a map\" adds nothing to it. If any of that failed the throw happens there and nothing is
  fetched, because the payload that would trigger a query is the one least worth trusting.
  Only when the pure phases are clean is anything fetched.

  A gate that is not among `idxs` is a gate that passed: a sealed value's checks all passed
  when it was made, so the only ones that can be in doubt are those naming a field being
  written -- and those are exactly the ones in `idxs`."
  [model run x idxs]
  (let [[shape pure io] (phases model idxs)
        f1     (failures-of run x shape {})
        failed (into #{} (map first) f1)
        f2     (failures-of run x (remove #(gate-broken? model % failed) pure) {})
        fs     (into f1 f2)]
    (when (seq fs) (checks-failed! model (failure-entries model x fs {})))
    (when (seq io) (run-group! model run x io (fetch-io model x io)))))

(defn- entries-of
  "The entries `o` contributes to a `conj` onto a value of this type: a map's, or the one a
  two-element vector is.

  Anything else is refused by name. A `conj` is a write, and a write that cannot be read as
  entries is not a write this type can run its checks over -- so it says which type refused
  it and what it was handed, as every other refusal here does."
  [model o]
  (cond (map? o)                     (seq o)
        (instance? java.util.Map o)  (seq (into {} o))
        (and (vector? o) (= 2 (count o))) [o]
        :else (throw (ex-info (format "cannot conj %s onto a %s -- a conj takes a map or a [k v]"
                                      (pr-str o) (:name model))
                              {:error ::cannot-conj :record (:name model) :value o}))))

(defn- no-dissoc!
  "Declared fields are permanent, so `dissoc` refuses them; where a check admits nil,
  `(assoc m k nil)` is how absence is spelled. Undeclared keys dissoc normally."
  [model k]
  (throw (ex-info (format "cannot dissoc %s from %s: declared fields are permanent -- use (assoc m %s nil) if its checks admit nil"
                          (pr-str k) (:name model) (pr-str k))
                  {:error ::cannot-dissoc :record (:name model) :key k
                   :instead (list 'assoc 'm k nil)})))

(defn- io-checks-on
  "The external validations that read `k`, by name and in declaration order."
  [model k]
  (into [] (comp (map (:checks model)) (filter (comp seq :io)) (map :name))
        (get (:index model) k)))

(defn- io-limited!
  "Refuse a direct write of `ks` to a sealed value of an `:io-limited` type.

  A check that queries a database, run on every bare `assoc`, means business logic makes a
  query per write. `:options {:io-limited true}` says so where the type is written: the
  fields an external validation reads are written under [[drafting]], where N writes cost one
  round of checks at the seal rather than N rounds on the way there.

  It refuses rather than defers. The caller asked for a sealed value, and a sealed value is
  one every check passed on -- there is nothing to defer to -- so nothing is fetched and no
  check runs. Fields no external validation reaches are unaffected, which is what declaring
  io-deps buys: the option narrows itself and the author never restates it."
  [model ks]
  (let [ks    (vec ks)
        by    (apply array-map (mapcat (fn [k] [k (io-checks-on model k)]) ks))
        names (fn [xs] (apply str (interpose ", " (map pr-str xs))))
        many? (boolean (next ks))]
    (throw (ex-info (format (str "%s is :io-limited and %s %s read by %s (%s) -- write %s "
                                 "inside `drafting` rather than by assoc or merge on a "
                                 "sealed value, so N writes cost one round of checks at "
                                 "the seal instead of N")
                            (:name model)
                            (names ks)
                            (if many? "are" "is")
                            (if many? "external validations" "an external validation")
                            (names (distinct (apply concat (vals by))))
                            (if many? "them" "it"))
                    {:error ::io-limited :record (:name model)
                     :key (first ks) :keys ks :checks by
                     :instead (if many?
                                (list `drafting '[x m] (list 'merge 'x 'other))
                                (list `drafting '[x m] (list 'assoc 'x (first ks) 'v)))}))))

(defn- io-limited-write!
  "Throw where a merge onto a sealed value writes a field its type keeps for [[drafting]].

  Every offending key at once, in field declaration order, because a merge is one write:
  refusing it a key at a time would report the order the entries happened to arrive in as
  though it meant something. Emitted only for a type that declares the option and has a
  field for it to reach, so a type without one pays nothing."
  [model ks]
  (let [lim     (:io-limited-keys model)
        written (set ks)]
    (when (some lim written)
      (io-limited! model (filterv #(and (lim %) (written %)) (:field-order model))))))

(defn- qualify-keys
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

(defn- printable
  "The map a sealed value prints as. A sealed value holds nothing but its declared fields,
  every one of them qualified by the namespace the print tag already names, so repeating
  that namespace on every key would be noise."
  [x]
  (into (sorted-map) (map (fn [[k v]] [(keyword (name k)) v])) x))

;;; ------------------------------------------ Draft ------------------------------------------

(defonce ^:private draft-registry (atom {}))

(defn- register-draft! "Associate a draft class with its model map." [cls model]
  (swap! draft-registry assoc cls model))

(def ^:private ^:dynamic *owner*
  "Token identifying the outermost `drafting` currently on the stack, or nil outside one.
  A draft records the token in force when it was created, which is how a nested `drafting`
  tells a draft an enclosing form owns from a free one it must adopt and seal itself."
  nil)

(defn draft?
  "Is `x` a draft -- of any type this library defines?

  The question about the form. [[of-type?]] is the question about the type, and \"a draft of
  Heist\" composes from one of each."
  [x] (contains? @draft-registry (class x)))

(defn- draft-model [x] (@draft-registry (class x)))

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

;; The protocol and both its methods are private. `seal` believes the mask, so a public
;; name that writes one is a way past every check the type declares -- which is the whole
;; of what narrowing this buys. The generated draft type still implements the protocol from
;; the consumer's namespace: `deftype` resolves a spec name through `ns-resolve`, which does
;; not consult privacy, and generated code reads the mask through [[internal]] as it reaches
;; everything else here.
(alter-meta! #'Verdicts assoc :private true)
(alter-meta! #'verdict-mask assoc :private true)
(alter-meta! #'record-verdict! assoc :private true)

(defn- value-model
  "The model for `x` -- a draft or a sealed value -- or `nil` where `x` is neither.

  Not the type object: a class is not a value of its type, so the bare `Heist` a definition
  interns answers `nil` here even though the registry knows it. The checking family asks
  about values."
  [x]
  (let [c (class x)] (or (@draft-registry c) (@registry c))))

(defn- value-model!
  "[[value-model]], or a throw naming the operation that wanted one.

  The refusal is of something that is not one of this library's values at all. It is not
  `::not-a-draft`: a sealed value is one every check passed on, so `true` is an honest
  answer about it, and refusing to answer was the mistake I1 corrects. Only [[unsealed]]
  still asserts a draft, because that marker is an assertion about what is being returned."
  [what x]
  (or (value-model x)
      (throw (ex-info (format "%s expects a draft or a sealed value, and %s is neither"
                              what (pr-str x))
                      {:error ::not-a-value :value x}))))

(defn- value-mask
  "What `x` has established, as a mask. A draft carries one; a sealed value is one every
  check passed on, so its mask is the type's constant with every bit set."
  [model x]
  (if (draft-model x) (verdict-mask x) (:all-pass model)))

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

(defn passed
  "The set of check names currently passing on `x`.

  A draft opened from a sealed value starts with all of them, since that is what being
  sealed means; one opened from the type starts with none; and a write clears exactly the
  checks naming the field written. So for a draft this is what has been established about it
  as it stands, never what was once true of something it came from.

  A sealed value answers with every check its type declares, because that is what being
  sealed means and there is no other state it could be in. The name matches the metadata key
  `::passed`, which carries the same answer for the same value; two spellings of one
  question, and they may not disagree.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`."
  [x]
  (let [model (value-model! "passed" x)]
    (passed-in model (value-mask model x))))

(defn- check-id
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

(defn- record-run
  "Run the checks at `idxs` against the draft `d` with `cache` in hand, writing each verdict
  into the mask as it goes, and answer with `[i ret]` for each that did not return `true`.

  The recording is what separates this from the sealed path: a verdict is a fact about the
  checking rather than about the data, so it belongs to the draft the checking was done on
  and not to any value that comes out of it. Work done here is therefore not lost when the
  caller throws -- the checks that passed are marked as passing, and the one that failed is
  marked as not.

  A check whose gate `d`'s mask does not satisfy records nothing either, and is not among
  the failures. It did not run, so there is no verdict about it to write down -- the shape
  failure that stopped it is the one the caller is owed, and a bit left clear is what makes
  a later call run it once the shape is fixed. Which is why the phases are separate passes:
  a gate is read off the mask, so what gates this pass has to have been written into the
  mask by the last one.

  A check that throws records nothing. It reached no verdict, so its bit says what it said
  before, and the throw propagates: a bug in a check is not a claim about the data.

  `draft?` says whether there is anywhere to record. A sealed value has no mask: its checks
  all passed when it was made, which is why [[check]] never runs one against it at all and
  only [[recheck]] gets here. Nothing is written down, because there is nothing a verdict
  about a sealed value could later save -- the value cannot change, so the next call would
  learn exactly this much again."
  [model draft? d idxs cache]
  (let [run  (if draft? (:run-draft model) (:run-sealed model))
        cs   (:checks model)
        mask (if draft? (verdict-mask d) (:all-pass model))]
    (reduce (fn [acc i]
              (let [g (:shape-mask (nth cs i))]
                (if-not (or (zero? g) (= g (bit-and mask g)))
                  acc
                  (let [ret  (run i d cache)
                        pass (true? ret)]
                    (when draft? (record-verdict! d i pass))
                    (if pass acc (conj acc [i ret]))))))
            [] idxs)))

(defn- mask-indices
  "The indices of the bits set in `m`, ascending -- which is declaration order, since a
  check's index is its bit.

  One iteration per set bit rather than one per check: a plan is usually a handful of
  checks out of a type's whole complement, and this is on the path of every call that
  establishes anything."
  [m]
  (loop [m (long m), acc (transient [])]
    (if (zero? m)
      (persistent! acc)
      (recur (bit-and m (dec m)) (conj! acc (Long/numberOfTrailingZeros m))))))

(defn- checks-for
  "The indices of the checks naming any of the fields `entries` writes, in declaration order.

  What a merge onto a sealed value has to run, asked once over the entries it was handed.
  The answer is a mask: the macro computed, per field, the bits of the checks that read it
  -- the same index by which a write to that field clears a draft's verdicts -- so the union
  is one `bit-or` per entry and [[mask-indices]] expands it in one iteration per check
  actually named. It read the field -> checks index and built a `sorted-set` per write
  before, which is a tree per merge to answer a question the type settled when it compiled."
  [model entries]
  (let [ms (:field-checks model)]
    (mask-indices (reduce (fn [m e] (bit-or (long m) (long (get ms (nth e 0) 0)))) 0 entries))))

(defn- wants
  "`idxs` plus everything that has to pass before any of them can run, as a mask.

  Two kinds of prerequisite. Anything that is not a shape validation waits on the shape
  validations of the fields it names; anything IO-bound waits, as well, on the pure checks
  over the fields its fetches name -- nothing reaches a database until the values it will
  carry there are established as well-formed. Both are masks the macro computed, so folding
  one in is a `bit-or`.

  The closure is taken to a fixed point, over whatever each round added rather than over
  everything again, because a prerequisite has prerequisites of its own: the pure checks
  gating a fetch are ordinary invariant validations, and each of them waits on the shape
  validations of the fields it names. It settles at once in practice -- what those add are
  shape validations, which wait on nothing -- and the loop is what makes that a fact about
  this type rather than an assumption about every type.

  The gate is narrow without anyone arranging it: an io-dep declares its own fields, so a
  fetch over `crew` waits on the checks covering `crew` and not on the whole type."
  [model idxs]
  (let [cs  (:checks model)
        add (fn [m i] (let [c (nth cs i)] (bit-or m (:shape-mask c) (:gate-mask c))))]
    (loop [m (reduce bit-set 0 idxs), fresh idxs]
      (let [m' (reduce add m fresh)]
        (if (= m' m)
          m
          (recur m' (mask-indices (bit-and-not m' m))))))))

(def ^:private established
  "The answer for a draft with nothing left to establish: nothing ran and nothing was
  fetched. Shared rather than rebuilt, since it is the answer most calls get."
  {:failures [] :cache {}})

(defn- plan-for
  "What is left to do about the checks at `idxs` for the draft `d`: the shape validations to
  run, the other pure checks, and the external validations -- each of them what the draft's
  mask does not already say is passing, together with whatever prerequisites those have.

  Prerequisites are pulled in here rather than discovered when a check turns out not to be
  runnable: [[wants]] takes the closure, and [[phases]] puts each of them where it belongs. A
  check that is not a shape validation waits on the shape validations of the fields it names,
  which land in the shape phase; anything IO-bound waits as well on the pure checks over the
  fields its fetches name, which land in the pure phase, and on their shape gates with them.

  `nil` where there is nothing to do at all, which is the case worth not paying for: a
  draft of a sealed value that has not been written to, a check an earlier call already
  established, a certificate already held. Every entry point asks this before it does
  anything, so what is skipped here is the whole of what a call with nothing pending
  costs. A sealed value's mask has every bit set, so that is also how `check` answers one
  without running anything.

  `force?` is what [[recheck]] passes: everything named is pending whatever the mask says,
  so the answer is never `nil` for a non-empty `idxs`. Prerequisites are still read off the
  mask -- a gate that is established is established, and re-running it would be a different
  request from the one that was made."
  [model mask idxs force?]
  (when (seq idxs)
    (let [pending (if force?
                    (vec idxs)
                    (filterv #(not (bit-test (long mask) (long %))) idxs))]
      (when (seq pending)
        ;; Masks rather than sets: a check's index is its bit, so the union of what is
        ;; wanted is a `bit-or` per check and what is left to do is one `bit-and-not`
        ;; against the verdicts. Where nothing was added -- the ordinary case, since a
        ;; group asked for usually carries its own gates -- `pending` is already the
        ;; answer and nothing is rebuilt.
        (let [pm   (reduce bit-set 0 pending)
              want (wants model pending)
              todo (if (= want pm)
                     pending
                     (mask-indices (bit-or pm (bit-and-not want (long mask)))))]
          (when (seq todo)
            (let [[shape pure io] (phases model todo)]
              {:shape shape :pure pure :io io})))))))

(defn- establish-all
  "Establish, against each value in `xs`, the checks at its own entry of `idxss`, and answer
  with `{:failures [[i ret] ...] :cache {io-dep-name value}}` for each of them, index-aligned
  with `xs`.

  What is actually run for a value is whatever its own mask does not already say is passing,
  together with whatever gates that -- so [[check]] means *establish this*, not *run exactly
  this*, and a satisfied prerequisite is free, value by value. Where nothing is pending for
  any of them, no phase is entered at all: a `seal` of a draft nothing has been written to,
  and a `check` of a check already established, are the calls this library is arranged to
  make cheap, so they are answered before anything is built rather than by running an empty
  pass over each phase in turn. A sealed value's mask is every bit set, which is the same
  arrangement answering `true` for it without running anything.

  `force?` is [[recheck]]: every check named is run whatever the mask says. A draft's mask
  is then written with what was found, so a verdict the world has taken back is cleared; a
  sealed value has no mask to write, and the answer is a report rather than a verdict.

  Three phases, because shape gates everything. The shape validations run first and ungated,
  writing their verdicts into the mask; the other pure checks follow, each running only if the
  mask now says the shape validations of the fields it names are passing, so a check the gate
  stopped is neither run nor reported. A value anything failed for fetches nothing, and its
  failures -- the ones asked for and the ones gating them alike -- are its answer. Only the
  values whose pure phases came back clean reach the fetch, and they reach it together: one
  pass per io-dep over the whole group, so a batched fetch sees every argument tuple in one
  call and distinct tuples are computed once however many values share them. The gate is per
  value, so one malformed draft holds up nothing but its own fetch. Where no value owes an
  external validation the fetch pass is not entered either."
  [model xs idxss force?]
  (let [xs     (vec xs)
        idxss  (vec idxss)
        n      (count xs)
        drafts (mapv #(some? (draft-model %)) xs)]
    (loop [i 0, plans (transient []), pending? false]
      (if (< i n)
        (let [p (plan-for model
                          (if (nth drafts i) (verdict-mask (nth xs i)) (:all-pass model))
                          (nth idxss i) force?)]
          (recur (inc i) (conj! plans p) (or pending? (some? p))))
        (if-not pending?
          (vec (repeat n established))
          (let [plans (persistent! plans)]
            (loop [i 0, fss (transient []), ios (transient []), fetching? false]
              (if (< i n)
                ;; A value whose shape failed carries nothing to the fetch, and one that
                ;; owes no external validation asks for none.
                (let [p  (nth plans i)
                      dr (nth drafts i)
                      fs (if p
                           (let [x (nth xs i)]
                             (into (record-run model dr x (:shape p) {})
                                   (record-run model dr x (:pure p) {})))
                           [])
                      io (if (or (nil? p) (seq fs)) [] (:io p))]
                  (recur (inc i) (conj! fss fs) (conj! ios io)
                         (or fetching? (boolean (seq io)))))
                (let [fss (persistent! fss)
                      ios (persistent! ios)]
                  (if-not fetching?
                    (mapv (fn [fs] (if (seq fs) {:failures fs :cache {}} established)) fss)
                    (let [caches (fetch-io-all model xs ios)]
                      (mapv (fn [i]
                              (let [fs (nth fss i)
                                    io (nth ios i)]
                                (cond
                                  (seq fs)    {:failures fs :cache {}}
                                  (empty? io) established
                                  :else       (let [cache (nth caches i)]
                                                {:failures (record-run model (nth drafts i)
                                                                       (nth xs i) io cache)
                                                 :cache    cache}))))
                            (range n)))))))))))))

(defn- establish-group
  "[[establish-all]] against one value: a group of one, so that a call spanning values and a
  call about one go through the same phases and the same fetches."
  [model x idxs force?]
  (first (establish-all model [x] [idxs] force?)))

(defn- run-draft-checks!
  "Establish the checks at `idxs` against the draft `d` and throw once with every failure,
  keyed by check name in declaration order."
  [model d idxs]
  (let [{:keys [failures cache]} (establish-group model d idxs false)]
    (when (seq failures)
      (checks-failed! model (failure-entries model d failures cache)))))

(def every-check
  "The certificate covering every check a type declares, which exists without anyone
  declaring it -- so a value can be run through everything in one pass rather than leaving
  whatever a named certificate omitted to be run one check at a time.

  It is the one certificate name a type may not declare for itself."
  ::every-check)

(defn- ids-for
  "The check indices `cname` names on this type: one, where it is a check's name; the
  certificate's, where it is a certificate's; a throw where it is neither.

  One verb over two kinds of name, because a lone check is a certificate containing itself
  and nothing downstream of here can tell which was asked for. So there is one refusal too:
  a name this type does not know is a programmer error whichever kind it was meant to be,
  and the message says what it could have been given of either kind. Answering `true` for a
  name that means nothing would be asserting exactly that nothing."
  [model cname]
  (if-let [i (get (:check-ids model) cname)]
    [i]
    (if-let [c (get (:certificates model) cname)]
      (:ids c)
      (throw (ex-info (format "%s declares no check or certificate named %s -- its checks are %s and its certificates are %s"
                              (:name model) (pr-str cname)
                              (pr-str (mapv :name (:checks model)))
                              (pr-str (vec (keys (:certificates model)))))
                      {:error ::unknown-check :record (:name model) :check cname
                       :checks (mapv :name (:checks model))
                       :certificates (vec (keys (:certificates model)))})))))

(defn certificates
  "The set of certificate names `x` currently holds.

  Derived, never asserted: a value holds a certificate exactly when every check in it is
  currently passing, which is one `and` against the certificate's mask. So overlapping
  certificates need no coordination with each other, running individual checks can complete
  a certificate incidentally, and there is nowhere to record holding one except by having
  actually passed its checks.

  A sealed value holds every certificate its type declares, the built-in among them, because
  every check on it passed -- which is strictly more than any certificate claims. The name
  matches the metadata key `::certificates`, which carries the same answer for the same
  value.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`."
  [x]
  (let [model (value-model! "certificates" x)]
    (held-in model (value-mask model x))))

(defn- merged-meta
  "`mta` with the library's own keys, `lib`, merged over the top.

  Over rather than under, deliberately. `with-meta` is public, so anything a caller could
  set a caller could forge; keys written last cannot be overridden by anything handed in,
  and what comes back is what the value can actually show for itself.

  A value carrying no metadata of its own answers with `lib` exactly -- for a sealed value
  that is the type's constant, allocated once when the type was defined."
  [mta lib]
  (if mta (conj mta lib) lib))

(defn- draft-meta
  "What a draft answers [[clojure.core/meta]] with: its own metadata, with what its verdict
  mask says merged over the top.

  A sealed value answers the same two keys, from a constant its type computed when it was
  defined -- every check passed, so every certificate is held. That is what makes the
  question askable of a value without first establishing which form it is."
  [model mask mta]
  (merged-meta mta (array-map ::passed       (passed-in model mask)
                              ::certificates (held-in model mask))))

(defn- returned-by-name
  "[[record-run]]'s answer as check name -> what that check returned, in the type's own
  check declaration order.

  What a failing check returned *is* the failure data, so that is the whole of what the
  answering forms hand back. The richer report -- the docstring, the fields the check
  declared and their values -- is what the insisting ones throw.

  `array-map` applied to the pairs, so the order survives past the size at which a map
  literal would lose it."
  [model fs]
  (apply array-map
         (into [] (mapcat (fn [[i ret]] [(:name (nth (:checks model) i)) ret])) fs)))

(defn- answered
  "`true`, or what each check that did not pass returned, keyed by check name. The shape
  every answering form here hands back."
  [model x idxs force?]
  (let [fs (:failures (establish-group model x idxs force?))]
    (if (seq fs) (returned-by-name model fs) true)))

(defn- insisted-one
  "`true`, or one `:check-failed` carrying the full report for every check that did not
  pass. The shape every insisting form here throws."
  [model x idxs force?]
  (let [{:keys [failures cache]} (establish-group model x idxs force?)]
    (if (seq failures)
      (checks-failed! model (failure-entries model x failures cache))
      true)))

(defn check
  "Run what `cname` names against `x`: `true` if everything named passes, and otherwise a
  map of check name -> what that check returned, for those that did not.

  `cname` is a check's name or a certificate's. A lone check is a certificate containing
  itself, so there is one verb and one answer, and nothing has to decide which kind of name
  it is holding before it asks. `every-check` is the certificate covering the whole type.

  What a failing check returned *is* the failure data, so that is what comes back, keyed by
  check name in the type's own declaration order and built with `array-map` so that order
  survives past the size at which a map literal would lose it. The richer report -- the
  docstring, the fields the check declared and their values -- is what [[check!]] throws.

  Every check named runs; none of this stops at the first failure. A check the certificate
  does not name can appear among the failures, and a check that *was* named can be missing
  from them: where a check's gate fails it never runs, so it has no verdict of its own and
  gets no entry, and the blocking check's entry is what says why.

  This *establishes* rather than merely calls. Whatever the mask already says is passing
  costs nothing -- no fetch either -- and whatever a write cleared runs again; the checks
  gating a fetch run first, and only if they pass does anything reach the outside. On a
  sealed value everything is established, so `true` comes back and nothing runs at all. Use
  [[recheck]] to ask the world again.

  Not a draft -- none of these return one. The verdict goes in `x`'s mask, which is what
  frees the return value to be the answer to the question that was asked. Nor is holding a
  certificate recorded anywhere: [[certificates]] derives it from those verdicts.

  Failures are not retained. What this returns is the failure; a later call re-runs anything
  not passing and produces it again, fetch included -- a verdict about an external validation
  is point-in-time, and what it consulted is not kept.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`;
  `:unknown-check` when the type declares no check or certificate of that name, carrying
  `:record`, `:check` and the names it could have been given; and `:malformed-batch` or
  `:incomplete-batch` when a batched fetch behind an external validation answers something
  other than a map keyed by every tuple it was handed, each carrying `:record`, `:io-dep`
  and what the fetch returned."
  [x cname]
  (let [model (value-model! "check" x)]
    (answered model x (ids-for model cname) false)))

(defn check!
  "[[check]], insisting on the answer: `true`, or one `:check-failed` carrying every failure
  keyed by check name in declaration order, each with the check's docstring, the fields it
  declared and their values, and what it returned -- the same report every other entry point
  throws.

  The work is the same work, so a check already passing costs nothing here either, and a
  sealed value answers `true` without running anything.

  Throws what [[check]] throws: `:not-a-value` when `x` is neither a draft nor a sealed
  value, `:unknown-check` when the type knows no name like that, and `:malformed-batch` or
  `:incomplete-batch` when a batched fetch misbehaves. It adds `:check-failed` where a check
  named did not pass, carrying `:record` and `:failures` -- the full report, keyed by check
  name in phase order."
  [x cname]
  (let [model (value-model! "check!" x)]
    (insisted-one model x (ids-for model cname) false)))

(defn recheck
  "[[check]], but run it now: every check `cname` names is run whether or not the mask
  already says it passes. Same argument, same keyed answer.

  It exists because a verdict about the outside world is a verdict at a point in time, and a
  set bit cannot tell one that still holds from one that has quietly stopped holding. `check`
  is right to believe the mask -- that is what makes a second call free and a seal after a
  bulk check perform no IO -- and wrong for the one question `recheck` asks.

  On a draft the answer is written back: a check that passed against yesterday's database and
  fails against today's is *un-passed*, so the draft stops claiming it and the next [[seal]]
  runs it again. A check that was failing and now passes has its bit set, the same way.

  On a sealed value there is no mask to write and the value cannot change, so this runs the
  checks and answers. What comes back is a report about the world rather than a verdict about
  the value -- a sealed value is one every check passed on, and [[passed]] goes on saying so.

  Prerequisites are still read off the mask: a gate already established is not re-run, because
  what was asked for was these checks and not their gates. A gate that has *not* passed still
  stops the check behind it, here as anywhere. Fetches are shared for the length of this one
  call and no longer, exactly as they are for `check`.

  Throws what [[check]] throws and in the same conditions: `:not-a-value`, `:unknown-check`,
  `:malformed-batch` and `:incomplete-batch`. The last two are likelier here than anywhere
  else, because this is the form that goes back out to the world on purpose."
  [x cname]
  (let [model (value-model! "recheck" x)]
    (answered model x (ids-for model cname) true)))

(defn recheck!
  "[[recheck]], insisting on the answer: `true`, or one `:check-failed` carrying every
  failure, keyed by check name in declaration order. Same work, and no partial answer to
  report.

  Throws what [[recheck]] throws -- `:not-a-value`, `:unknown-check`, `:malformed-batch`,
  `:incomplete-batch` -- and `:check-failed` where a check named did not pass, carrying
  `:record` and `:failures`, exactly as [[check!]] does."
  [x cname]
  (let [model (value-model! "recheck!" x)]
    (insisted-one model x (ids-for model cname) true)))

;;; --------------------------- Checking many values at once ---------------------------

(defn- group-models!
  "The model for each value in `xs`, index-aligned, or a throw naming by position the first
  thing that is not a value of this library at all.

  Position, because position is what a group call answers by -- two drafts holding identical
  data are `=`, so nothing else in a group identifies a member."
  [what xs]
  (mapv (fn [n x]
          (or (value-model x)
              (throw (ex-info (format (str "%s expects drafts or sealed values, and the "
                                           "value at index %d is neither: %s")
                                      what n (pr-str x))
                              {:error ::not-a-value :index n :value x}))))
        (range) xs))

(defn- partition-by-model
  "`[[model [index ...]] ...]` -- the positions of `xs` grouped by the type they belong to,
  types in the order they first appear and positions ascending within each.

  A group is checked per type because one fetch belongs to a type: the argument tuples are
  built by one type's runner and a batched fetch is declared on one type, so a mixed group is
  one pass per type, still batched within each. Identity rather than equality, because a
  model is a large map and two types' models are told apart by being different objects; and a
  scan rather than a hash, because the number of distinct types in one call is small."
  [models]
  (reduce (fn [acc [n m]]
            (let [i (first (keep-indexed (fn [k [m2]] (when (identical? m m2) k)) acc))]
              (if i
                (update-in acc [i 1] conj n)
                (conj acc [m [n]]))))
          []
          (map-indexed vector models)))

(defn- group-run
  "Establish `cname` against every value in `xs`, and answer `[models results]`, both
  index-aligned with `xs`, or `nil` where there is nothing to check.

  The one place a group call is assembled, so the answering and the insisting forms run
  exactly the same work and differ only in what they make of it.

  The name is resolved against every type in the group *before* anything runs. A type that
  does not know it fails the whole call -- it is a programmer error, the same rule a single
  [[check]] applies, and burying it among per-value failures is how a typo hides. Resolving
  first is also what keeps the refusal from leaving half the group established.

  An empty collection is answered rather than refused: nothing to check is a thing that
  happens to a bulk caller, and there is no value to resolve a name against, so the name is
  not resolved. A collection of one is a group of one and goes through all of this."
  [what xs cname force?]
  (let [xs (vec xs)]
    (when (seq xs)
      (let [models (group-models! what xs)
            groups (mapv (fn [[m is]] [m is (ids-for m cname)]) (partition-by-model models))
            out    (object-array (count xs))]
        (doseq [[m is idxs] groups]
          (let [vs (mapv #(nth xs %) is)
                rs (establish-all m vs (repeat (count is) idxs) force?)]
            (dotimes [k (count is)] (aset out (nth is k) (nth rs k)))))
        [models (vec out)]))))

(defn- answered-all
  "What a group's answering forms hand back: one answer per value, in the order the caller
  gave them -- `true`, or what each check that did not pass returned, keyed by check name."
  [models rs]
  (mapv (fn [m {:keys [failures]}]
          (if (seq failures) (returned-by-name m failures) true))
        models rs))

(defn- insisted
  "`true`, or one throw carrying every failure of every value in `rs`. What the bang forms
  make of a group run: partial success is a workspace notion, and a form that throws is not
  offering one."
  [models xs rs]
  (let [fs (into [] (keep-indexed (fn [n {:keys [failures cache]}]
                                    (when (seq failures)
                                      [n (failure-entries (nth models n) (nth xs n)
                                                          failures cache)])))
                 rs)]
    (if (seq fs) (bulk-failed! models fs) true)))

(defn check-all
  "Run what `cname` names against every value in `xs`, and answer with a vector index-aligned
  with `xs`: `true` where everything named passed, and otherwise a map of check name -> what
  that check returned, for those that did not.

  One call spanning values, which is what a batched fetch is written for: an io-dep behind a
  check is fetched once per distinct argument tuple across the whole group, and a batched one
  sees every tuple in a single call. Results are index-aligned rather than keyed by the value
  because two drafts holding identical data are `=`, and a map would collapse them into one.

  The group may hold values of more than one type, and sealed values alongside drafts.
  Batched fetches are declared per type, so a mixed group is one fetch pass per type and
  still batched within each; `cname` resolves per type, and a type that does not know it
  fails the whole call before anything runs.

  Per value, this is [[check]]: whatever the value's own mask already says is passing costs
  nothing, the checks gating a fetch run first, and a value whose gate failed carries nothing
  to the outside while the rest of the group still fetches. A sealed value has nothing left to
  establish and answers `true`.

  Each draft is left with its own verdicts recorded, which is what makes sealing afterwards
  free. Nothing here produces a sealed value: a guarantee does not come in partial.

  Throws `:not-a-value` when something in `xs` is neither a draft nor a sealed value,
  carrying `:index` and `:value`; `:unknown-check` when one of the types in the group
  declares no check or certificate of that name, carrying `:record`, `:check` and the names
  it could have been given; and `:malformed-batch` or `:incomplete-batch` when a batched
  fetch answers something other than a map keyed by every tuple it was handed, each carrying
  `:record`, `:io-dep` and what the fetch returned."
  [xs cname]
  (if-let [[models rs] (group-run "check-all" xs cname false)]
    (answered-all models rs)
    []))

(defn check-all!
  "[[check-all]], insisting on the answer: `true`, or one `:check-failed` carrying every
  failure across every value, keyed by the value's index and, under that, by check name.

  The work is the same work -- one fetch pass per type, nothing re-established -- and what
  differs is only that there is no partial success to report. Every value is run before
  anything is thrown, and each draft keeps the verdicts it earned. Where the group held more
  than one type, `:record` is the names of all of them rather than one speaking for the rest.

  Throws what [[check-all]] throws -- `:not-a-value`, `:unknown-check`, `:malformed-batch`
  and `:incomplete-batch` -- and `:check-failed` where a check did not pass for any value,
  carrying `:record`, `:bulk`, `:indexes`, and `:failures` keyed by the value's index and,
  under that, by check name."
  [xs cname]
  (if-let [[models rs] (group-run "check-all!" xs cname false)]
    (insisted models (vec xs) rs)
    true))

(defn recheck-all
  "[[check-all]], but run it now: every check `cname` names is run for every value, whether or
  not its mask already says it passes.

  This is the shape the verb is really for. Re-verifying a batch against the database is one
  call, one fetch pass per type, batched within each -- where the same work value by value is
  a query apiece. Each draft's mask is written with what was found, so a check the world has
  taken back is un-passed for exactly the values it was taken back for.

  Throws what [[check-all]] throws, and in the same conditions: `:not-a-value`,
  `:unknown-check`, `:malformed-batch` and `:incomplete-batch`."
  [xs cname]
  (if-let [[models rs] (group-run "recheck-all" xs cname true)]
    (answered-all models rs)
    []))

(defn recheck-all!
  "[[recheck-all]], insisting on the answer: `true`, or one `:check-failed` carrying every
  failure across every value, keyed by index and, under that, by check name.

  Throws what [[check-all!]] throws: `:not-a-value`, `:unknown-check`, `:malformed-batch`,
  `:incomplete-batch`, and `:check-failed` carrying `:record`, `:bulk`, `:indexes` and
  `:failures`."
  [xs cname]
  (if-let [[models rs] (group-run "recheck-all!" xs cname true)]
    (insisted models (vec xs) rs)
    true))

(defn- model-of
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

(defn- owner-of [d] ((:draft-owner (draft-model d)) d))

(defn- adopt
  "Stamp `d` as owned by `token`, so nested `drafting` forms recognise it."
  ([d token] (adopt (draft-model d) d token))
  ([model d token] ((:draft-with-owner model) d token)))

(defn- open
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
  A draft created inside a `drafting` records that form as its owner.

  Throws `:cannot-draft` when `x` is none of those three, carrying `:value`."
  [x]
  (if (draft? x)
    x
    (if-let [model (model-of x)]
      (if (identical? model (@registry x))
        ((:draft-empty model) *owner*)
        ((:draft-from model) x *owner*))
      (throw (ex-info (format (str "draft expects a validated value, a draft, or a registered "
                                   "type, and %s is none of them")
                              (pr-str x))
                      {:error ::cannot-draft :value x})))))

(defn- extraneous
  "The keys `d` holds that its type does not declare."
  [d]
  (seq (keys ((:draft-ext (draft-model d)) d))))

(defn clean
  "Drop the keys the type does not declare, so `d` can seal. A draft -> draft step: put it at
  the end of a `drafting` body to strip rather than reject.

  Throws `:not-a-draft` when `d` is not a draft, carrying `:value`."
  [d]
  (when-not (draft? d)
    (throw (ex-info (format "clean expects a draft, and %s is not one" (pr-str d))
                    {:error ::not-a-draft :value d})))
  (if (extraneous d) ((:draft-clean (draft-model d)) d) d))

;;; ------------------------------ Opting out of the seal ------------------------------

(deftype Unsealed [wrapped])

;; `deftype` interns a positional factory. [[unsealed]] is the constructor that asserts what
;; it is wrapping, and a marker holding something that is not a draft would travel rather
;; than fail where it was written.
(alter-meta! #'->Unsealed assoc :private true)

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
  wrong assertion should say so where it is written rather than travel.

  Throws `:not-a-draft` when `d` is not a draft, carrying `:value`."
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
  ;; The refusal comes first and costs no check at all. An undeclared key is a fact about
  ;; the draft rather than about any check's judgement: no check names it, so no check has
  ;; anything to say about it.
  ;;
  ;; A field nobody wrote is not refused here, and is not a separate notion. It holds nil, as
  ;; it would on a `defrecord` whose field was never set, and its shape validation says what
  ;; it thinks of nil -- which is a better answer than the library reporting that nobody wrote
  ;; anything, and leaves a field with no shape validation free to seal holding it.
  (when-let [extra (extraneous d)]
    (throw (ex-info (format "%s has no field(s) %s -- use `clean` to strip them"
                            (:name model) (pr-str (vec extra)))
                    {:error ::extraneous-keys :record (:name model) :keys (vec extra)})))
  ;; Every check on the type is exactly what the built-in certificate covers, so the vector
  ;; of indices to select from is one the macro already built.
  (run-draft-checks! model d (not-established d (:ids (get (:certificates model) every-check))))
  ((:draft-seal model) d))

(defn seal
  "Validate `d` and return the sealed value. What comes back is validated exactly as if
  every check had been rerun.

  What is actually run is whatever `d`'s verdict mask does not already say is passing, so
  work done incrementally on the draft is not repeated: anything established with [[check]],
  one check or a whole certificate, costs nothing here, and a draft of a sealed value that
  has been written to pays only for the checks naming the fields written.
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
  refused by a marker would be a hole rather than an escape hatch.

  Throws `:cannot-seal` when `d` is neither a draft nor an already-sealed value, carrying
  `:value` and, where a type was asking, `:record`; `:extraneous-keys` when the draft holds
  a key its type does not declare, carrying `:record` and `:keys`; `:check-failed` when a
  check does not pass, carrying `:record` and `:failures`, a map keyed by check name in
  phase order; and `:malformed-batch` or `:incomplete-batch` when a batched fetch behind an
  external validation answers something other than a map keyed by every tuple it was handed,
  each carrying `:record`, `:io-dep` and what the fetch returned."
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

(defn- sealed-impl-return
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

(defn- draft-impl-return
  "What a protocol method installed on the *draft* type hands back: the body's value as
  written. Only the [[unsealed]] marker is unwrapped, so a body means the same thing
  wherever it runs; there was nothing to opt out of here, and the draft keeps whatever
  owner it had, since freeing one an enclosing `drafting` owns would break re-entrancy."
  [model ret]
  (if (instance? Unsealed ret) (opted-out model ret) ret))

(defn- recurrable-body
  "A method body wrapped in a `loop` rebinding every argument but the receiver to itself, so
  that `recur` inside it means what it means in a `defrecord` method body.

  Neither emission puts the body in tail position of the method: on the draft type it is an
  argument to [[draft-impl-return]], and on the sealed type it is inside the `try` that pops
  the ownership frame -- and \"Cannot recur across try\" is not a fact about the body anyone
  wrote. A `loop` inside both is the recur target instead. The receiver is left out because
  a `defrecord` method body's recur leaves it out: the arity is the one the author would
  have written against `defrecord`, and rebinding the receiver would let a body swap the
  draft the wrapper owns for something else halfway through.

  It allocates nothing -- a loop with no recur compiles to the body -- and a body with no
  `recur` in it means exactly what it meant before."
  [args body]
  `(loop [~@(interleave (rest args) (rest args))] ~@body))

(defn- owned-here?
  "Is `x` a draft belonging to a `drafting` form already on the stack?"
  [x]
  (and (draft? x) (some? *owner*) (= *owner* (owner-of x))))

(defmacro drafting
  "Bind `sym` to a draft of `init`, evaluate `body`, and seal the result.

  `init` may be a validated value, a registered type (to build one from nothing), or a
  draft. A draft an enclosing `drafting` owns passes straight through unsealed, so only
  the outermost form validates — that is what lets an impl fn taking and returning a
  draft be called standalone or nested without knowing which. A draft made outside any
  `drafting` is adopted and sealed here.

  Throws `:cannot-draft` when `init` is none of those three, carrying `:value`, and then
  whatever the seal throws, since this seals: `:cannot-seal` where the body's value is
  neither a draft of that type nor an already-sealed one, `:extraneous-keys` where the draft
  holds a key the type does not declare, `:check-failed` where a check does not pass, and
  the batch-shape `:malformed-batch` and `:incomplete-batch`. See [[seal]] for what each of
  them carries."
  [[sym init] & body]
  ;; `binding` names its var by symbol and [[*owner*]] is private, so the frame is pushed
  ;; and popped here -- which is what `binding` expands to anyway.
  `(let [init# ~init
         dm#   (~(internal 'draft-model) init#)]
     (if (and dm# (some? ~(internal '*owner*))
              (= ~(internal '*owner*) ((:draft-owner dm#) init#)))
       (let [~sym init#] ~@body)
       (let [model# (or dm# (~(internal 'model-of) init#))
             token# (Object.)]
         (when-not model#
           (throw (ex-info (str "drafting expects a validated value, a draft, or a registered "
                                "type, and " (pr-str init#) " is none of them")
                           {:error ::cannot-draft :value init#})))
         (push-thread-bindings {~(internal-var '*owner*) token#})
         (try
           (let [~sym (~(internal 'open) model# init# token# (some? dm#))]
             (seal model# (do ~@body)))
           (finally (pop-thread-bindings)))))))

;;; ------------------------------------------ Macro ------------------------------------------

(defn- unknown-dependency!
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

(defn- resolve-deps
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

(defn- malformed-shape-check!
  "Refuse a check named after a field that does not name exactly that field.

  The naming convention is the declaration: `::name` is the check that says what a well-formed
  `name` is, and nothing else marks it. So `::name ([name crew] ...)` is not a shape
  validation written loosely -- it is two claims at once, that this is what a `name` is and
  that it depends on something other than `name`, and the second destroys what the first is
  for. Shape validations depending on nothing is exactly what makes the dependency structure
  two levels deep: every other check depends only on shape validations, so no ordering among
  checks ever has to be computed and a gate can be one bitwise `and`.

  A check that is not named after a field may name whatever it likes."
  [tname c field]
  (let [names (:deps c)]
    (throw (ex-info (format (str "%s check %s is named after the field %s, so it is that "
                                 "field's shape validation and must name exactly that "
                                 "field -- it names %s")
                            tname (pr-str (:name c)) (pr-str field)
                            (if (seq names)
                              (apply str (interpose ", " (map pr-str names)))
                              "nothing"))
                    {:error ::malformed-shape-check :record tname :check (:name c)
                     :field field :names names :args (:args c)}))))

(defn- reject-duplicate-names!
  "A keyword in an arglist means an io-dep and a check never appears in one, so the two
  namespaces only collide where a single name is declared in both places -- at which point
  the reader of an arglist could no longer tell what it is looking at. Refuse it."
  [tname check-names io-dep-names]
  (when-let [dup (some (set io-dep-names) check-names)]
    (throw (ex-info (format "%s declares %s as both a check and an io-dep"
                            tname (pr-str dup))
                    {:error ::duplicate-name :record tname :name dup}))))

(def ^:private definition-sections
  "The sections a definition map may hold, in the order the macro reads them. Every one of
  them is optional; an absent section is an empty one."
  [:options :checks :io-deps :certificates])

(def ^:private known-options
  "What `:options` may hold. `:io-limited` is the only one so far -- the section exists so
  that there can be more.

  The set is the whole of what is general about an option: a key written here means
  something, and a key written anywhere else means nothing. What an option's *value* may be
  is the option's own business, decided by [[option-value]], because a rule covering options
  in general could only be the rule of whichever one happened to be written first.

  `:io-limited` makes direct `assoc` and `merge` on a *sealed* value throw for any field an
  external validation reads, so those writes go through `drafting` instead. See
  [[io-limited!]]."
  #{:io-limited})

(defn- io-limited-option
  "What `:options {:io-limited v}` declares: `v` where it is a boolean, or the boolean held
  by the var `v` names.

  The option is read while the macro runs, so it has to mean something then. A literal says
  what it says. A symbol is resolved here, in the namespace the form is being compiled in
  and honouring a qualified name, and the var's value is read on the spot -- which is what
  lets one flag decide a family of types without being retyped at each of them. What is
  read is what the type is compiled with: a later `alter-var-root` moves the var and leaves
  every type already compiled against it alone, exactly as a rebound `def` leaves a compiled
  call site alone.

  Everything else is refused where it is written, and the refusal says which case it was --
  a symbol naming nothing, a symbol naming something that is not a var, a var holding
  something that is not a boolean, or a form that is neither. A call like `(io-enabled?)` is
  the tempting one: nothing here would evaluate it, so it would be read as the list it is
  and the type would be limited because a non-empty list is truthy. Refusing it is the
  difference between a declaration and a decoration."
  [tname v]
  (let [refuse! (fn [why extra]
                  (throw (ex-info (format (str "%s: :options :io-limited takes true, false, "
                                               "or a symbol naming a var that holds one -- %s")
                                          tname why)
                                  (merge {:error ::malformed-option :record tname
                                          :option :io-limited :value v}
                                         extra))))]
    (cond
      (boolean? v) v

      (symbol? v)
      (let [r (try (resolve v) (catch Exception _ nil))]
        (cond
          (nil? r)
          (refuse! (format "%s does not resolve in %s" v (ns-name *ns*)) {:symbol v})

          (not (var? r))
          (refuse! (format "%s resolves to %s, which is not a var" v (pr-str r))
                   {:symbol v :resolved r})

          (not (boolean? @r))
          (refuse! (format "%s holds %s, which is not a boolean" v (pr-str @r))
                   {:symbol v :var r :var-value @r})

          :else @r))

      :else (refuse! (format "%s is neither a boolean nor a symbol" (pr-str v)) {}))))

(defn- option-value
  "What the option `o` written as `v` declares, decided by `o` itself.

  One option, one rule, and no rule above them: an option is a name for a decision, and two
  decisions need not be spelled alike. `o` is known by the time this is called, so the
  `case` covers everything that can reach it."
  [tname o v]
  (case o
    :io-limited (io-limited-option tname v)))

(defn- parse-definition
  "`defrecord`'s third position -> `{:options :checks :io-deps :certificates}`, with every
  section the form left out filled in empty, so nothing downstream has to ask whether it
  was written.

  Every key is resolved here, while the macro runs, for the same reason every arglist entry
  is: a section the macro does not recognise is a declaration that would mean nothing. A
  mistyped `:cheks` would quietly define a type with no checks at all -- a value asserting
  a validation that never ran, which is the one failure this library exists to make
  impossible -- so it is a compile error naming the offender instead. The same goes for a
  key inside `:options` and for what that key holds -- an option is read here, while the
  macro runs, so a form that means nothing then is refused rather than taken for whatever
  it happened to evaluate to; see [[option-value]] -- and for a bare map of checks written
  where the definition map now goes, whose check names arrive here as sections nobody
  declared.

  Every section now means something, so nothing here is accepted and then ignored. What a
  section holds is resolved further down: an arglist by [[resolve-deps]], a certificate by
  [[parse-certificates]], and the io-dep graph by [[order-io-deps!]]."
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
    ;; An option key means something or it means nothing, and a key nobody declared is the
    ;; second -- so it is refused here rather than ignored. What the key holds is read here
    ;; too, by the option itself, and what the sections carry onward is what it read: the
    ;; model records the option's value, not the form somebody wrote it as.
    (assoc sections :options
           (reduce-kv (fn [acc o v]
                        (when-not (contains? known-options o)
                          (throw (ex-info (format (str "%s: :options holds %s, which is not "
                                                       "an option -- the options are %s")
                                                  tname (pr-str o) (pr-str known-options))
                                          {:error ::unknown-option :record tname :option o
                                           :options known-options})))
                        (assoc acc o (option-value tname o v)))
                      {} (:options sections)))))

(def ^:private mask-width
  "How many checks a type may declare. A draft's verdict mask is one `long`, so a 65th check
  would alias the first: a passing check 0 would make check 64 count as passed, and a value
  could seal on a check that never ran. Refused where it is written rather than discovered
  by whatever the value goes on to do.

  The mask of written fields has the same width, and nothing can reach it: a type of 21
  fields does not compile, because its positional constructor would be a function of 21
  parameters and Clojure allows 20."
  64)

(defn- reject-too-many-checks!
  "Refuse a type with more checks than one `long` has bits."
  [tname n]
  (when (> n mask-width)
    (throw (ex-info (format (str "%s declares %d checks, and %d is the limit -- a draft's "
                                 "verdict mask is one long, and a wider type would alias "
                                 "one check's bit onto another's")
                            tname n mask-width)
                    {:error ::too-many-checks :record tname :count n :limit mask-width}))))

(defn- parse-certificates
  "The `:certificates` section -> certificate name -> `{:checks :ids :mask}`, with the
  built-in [[every-check]] appended. `check-names` is the type's checks in declaration
  order, which is the order a certificate is read back in whatever order it was written.

  A certificate is a declaration like any other here, so it means something or the form
  does not compile. A name it gives that is not a check of this type would be a claim about
  a check that does not exist; a certificate covering nothing would be held by every draft
  of the type, including one nothing has been run against, so `check` would answer `true`
  while asserting precisely nothing; and a name already spelling a check, or spelling the
  built-in, would make the same word mean two things inside one type.

  `:mask` is the certificate's checks as bits of a draft's verdict mask, so holding one is
  a single `and` at run time rather than a walk over names.

  A certificate is a set of check names however it was written, so any collection will do
  and a repeat in one is one name given twice: the names are resolved to indices through a
  set here, and `[::a ::a ::b]` compiles to exactly what `#{::a ::b}` compiles to, mask
  included. Nothing downstream could tell them apart in any case -- a check owns one bit."
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

(defn- binding-syms
  "An arglist as the body's parameters: a keyword names an io-dep, and what the body sees of
  it is a symbol of the same name -- `[crew cuts ::roster]` binds `crew`, `cuts`, `roster`.

  Anything that is neither is left exactly as written, so that [[resolve-deps]] refuses it
  with the offender still spelled the way its author spelled it."
  [args]
  (mapv (fn [e] (if (keyword? e) (symbol (name e)) e)) args))

(defn- reject-duplicate-bindings!
  "Two entries of one arglist that bind the same name would leave the body reading the
  second where it named the first -- silently, since a repeated parameter is legal Clojure.
  A dependency that is declared and then shadowed is a dependency that does not mean what it
  says."
  [tname what dname params]
  (when-let [dup (first (for [[p n] (frequencies params) :when (> n 1)] p))]
    (throw (ex-info (format "%s %s %s binds %s twice -- an arglist entry that shadows another is a dependency the body cannot read"
                            tname (name what) (pr-str dname) (pr-str dup))
                    {:error ::duplicate-dependency :record tname what dname
                     :dependency dup}))))

(defn- reaching-for-a-batch?
  "Is this arglist reaching for the batched form?

  `[[crew] :as rows]` is not a legal function arglist, which is how the reader tells the two
  forms apart -- but a form that reaches and misses is not a legal one either, and reporting
  it as an entry that resolves to nothing would answer a question nobody asked. A leading
  tuple or a stray `:as` is the reach; whether it landed is settled next."
  [args]
  (boolean (or (vector? (first args)) (some #{:as} args))))

(defn- batched-shape
  "`[[crew] :as rows]` -> `[[crew] rows]`: the tuple, which is the dependency declaration,
  and the name the collection of tuples is bound to. `nil` where the arglist is not exactly
  that.

  The `:as` is required because `[[crew]]` alone *is* a legal arglist -- a singular fetch
  over a destructured argument -- so without it there would be nothing to tell the two forms
  apart by. `:as` is unqualified and io-dep names are namespaced, so it cannot be mistaken
  for a dependency."
  [args]
  (let [[inner as sym] args]
    (when (and (= 3 (count args)) (vector? inner) (= :as as) (simple-symbol? sym))
      [inner sym])))

(defn- batched-check!
  "A check judges one value, so there is no batched form for one to be written in."
  [tname dname args]
  (throw (ex-info (format (str "%s check %s is written in the batched form -- a check judges "
                               "one value, so only an io-dep may be written [[crew] :as rows]")
                          tname (pr-str dname))
                  {:error ::batched-check :record tname :check dname :args args})))

(defn- parse-dep
  "One entry of the `:checks` or `:io-deps` map -> `{:name :doc :args :fn}`. The two are
  written the same way, so they are read the same way; `what` is `:check` or `:io-dep` and
  says which is being read, so a refusal names the kind it was.

  Three forms, told apart by what the value starts with: a string is a docstring, a vector
  is an arglist, and anything else is the shorthand -- a bare function over the single field
  the key names."
  [tname what dname form]
  (let [label (name what)
        bad!  (fn [why]
                (throw (ex-info (format "%s %s %s %s" tname label (pr-str dname) why)
                                {:error (if (= what :check) ::malformed-check ::malformed-io-dep)
                                 :record tname what dname :form form})))
        made  (fn [doc args body]
                (cond
                  ;; A check judges one value, so a tuple bound with `:as` is a form it has
                  ;; nothing to mean. An arglist that merely destructures is left alone --
                  ;; there is no batched form for it to have been reaching for, so it is
                  ;; refused where every other unresolvable entry is.
                  (and (= what :check) (vector? (first args)) (some #{:as} args))
                  (batched-check! tname dname args)

                  (and (= what :io-dep) (reaching-for-a-batch? args))
                  (let [[inner as]
                        (or (batched-shape args)
                            (bad! (str "is neither a legal arglist nor the batched form -- a "
                                       "batched fetch is written [[crew] :as rows]: one tuple "
                                       "of dependencies, bound as a collection with :as, and "
                                       "nothing else")))]
                    (reject-duplicate-bindings! tname what dname (binding-syms inner))
                    ;; `:args` is the tuple, so everything downstream resolves a batched
                    ;; arglist exactly as it resolves a singular one. What differs is what
                    ;; the body is handed: there are many tuples, so there is nothing for
                    ;; the names inside one to be bound to, and the collection is all the
                    ;; body sees.
                    {:name dname :doc doc :args inner :batched true
                     :fn `(fn [~as] ~@body)})

                  :else
                  (let [params (binding-syms args)]
                    (reject-duplicate-bindings! tname what dname params)
                    {:name dname :doc doc :args args :fn `(fn ~params ~@body)})))]
    (cond
      (and (seq? form) (string? (first form)))
      (let [[doc args & body] form]
        (when-not (vector? args) (bad! "has a docstring but no arglist"))
        (when-not (seq body) (bad! "has an arglist but no body"))
        (made doc args body))

      (and (seq? form) (vector? (first form)))
      (let [[args & body] form]
        (when-not (seq body) (bad! "has an arglist but no body"))
        (made nil args body))

      (and (seq? form) (seq? (first form)) (vector? (ffirst form)))
      (bad! "gives more than one arity -- the arglist is the dependency list, so there is exactly one")

      :else
      {:name dname :doc nil :args [(symbol (name dname))] :fn form})))

(defn- reject-field-collisions!
  "An io-dep may not be spelled like a field key. Both are keywords, both end up in the same
  resolved dependency list, and a fetched value is looked up somewhere a field is not -- so
  one name for the two would make an arglist entry mean two things and a failure report
  collapse them into one."
  [tname ks io-names]
  (when-let [dup (some (set ks) io-names)]
    (throw (ex-info (format "%s declares %s as both a field and an io-dep" tname (pr-str dup))
                    {:error ::duplicate-name :record tname :name dup}))))

(defn- order-io-deps!
  "The io-deps in fetch order: everything an io-dep names comes before it, so ascending
  index is an order the fetches can actually run in.

  An io-dep may name another, so fetches form a graph and the order is the graph's rather
  than the form's -- which is why this exists at all, and why a type may write its io-deps
  in whatever order reads best. A cycle is a fetch waiting on itself: not a slow program but
  an impossible one, so it is refused while the macro runs, with the ring named in the order
  it closes."
  [tname deps io-names]
  (let [by-name (into {} (map (juxt :name identity)) deps)
        order   (volatile! [])
        marks   (volatile! {})
        visit   (fn visit [nm path]
                  (case (get @marks nm)
                    :done nil
                    :open (let [ring (conj (vec (drop-while #(not= nm %) path)) nm)]
                            (throw (ex-info (format "%s io-deps form a cycle: %s -- a fetch cannot wait on itself"
                                                    tname (apply str (interpose " -> " (map pr-str ring))))
                                            {:error ::io-dep-cycle :record tname :cycle ring})))
                    (do (vswap! marks assoc nm :open)
                        (doseq [d (:deps (by-name nm))]
                          (when (contains? io-names d) (visit d (conj path nm))))
                        (vswap! marks assoc nm :done)
                        (vswap! order conj (by-name nm)))))]
    (doseq [d deps] (visit (:name d) []))
    @order))

(def ^:private reserved-methods
  "The method names the generated types already carry, and must keep. A protocol method
  spelled one of these would replace the implementation that makes a value behave as a map,
  or the one holding a draft's verdicts: the form would still compile, and the type would
  quietly stop being what it says it is."
  '#{get assoc dissoc keys meta with-meta empty cons getLookupThunk
     verdict-mask record-verdict!})

(defn- parse-impls
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
  "Define a validated record with a real field per declared key, so reads and writes are
  field access rather than map lookups.

  Both generated types are records rather than maps with a record's shape: `record?` holds,
  `=` compares the class before it compares an entry, `hash` is type-aware, `empty` refuses,
  and they serialize. See [[recognitum.collections/AbstractRecord]], which is the template
  they are built on.

  Three positions: the type name, the fields, and a definition map of named sections --

      (rec/defrecord Heist
        [name crew take cuts]
        {:options      {:io-limited true}
         :checks       {::name non-blank-string?}
         :io-deps      {::roster ([crew] (fetch-roster crew))}
         :certificates {::shape #{::name}}})

  Every section is optional and an absent one is an empty one, but a key that names no
  section at all is a compile error: a mistyped `:cheks` would define a type with no checks,
  and a value asserting a validation that never ran is the one thing here that must not be
  constructible. See [[parse-definition]].

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
  declared. An exception is not caught: it is a bug in the check rather than a claim about
  the data, and \"we could not determine whether this is valid\" is not \"this is invalid\".

  A check *named after a field* is that field's shape validation -- the statement of what a
  well-formed value of that field is. The naming convention is the whole of the declaration,
  and such a check names exactly its own field or the form does not compile. Every other check
  waits on the shape validations of the fields it names: `::cuts-sum-to-take [take cuts]` runs
  once `::take` and `::cuts` have passed, so it receives well-formed input by construction and
  restates nothing, and a throw out of it is therefore the bug the contract above says a throw
  is. A check whose gate has not passed neither runs nor appears among the failures -- the
  shape failure is the actionable one. A field with no shape validation has no declared shape,
  so the checks naming it run against whatever it holds and have to be total over anything:
  the author's choice, made loudly.

  That is three phases, and it is the order failures come back in: the shape validations, then
  the other pure checks, then the external validations, in declaration order within each.

  `:io-deps` names functions that fetch something from outside the value -- a database row,
  an upstream response. They are written exactly as checks are, arglist and all, and follow
  the same rule, so a keyword in an arglist names an io-dep and an io-dep may name another:
  fetches form a graph, run in its order, and a cycle among them is a compile error naming
  the ring. Anything is IO-bound exactly when its arglist holds a keyword, which is visible
  where it is written without consulting anything else.

  A fetch may be written singular or batched, and the reader tells them apart:

      ::roster ([crew] (fetch-roster crew))               ; singular
      ::roster ([[crew] :as rows] (fetch-rosters rows))   ; batched

  `[[crew] :as rows]` is not a legal function arglist, so the macro knows at expansion time
  which it has. The tuple is the dependency declaration, exactly as a singular arglist is,
  and everything derived from it -- the gate, the fetch order, the field -> checks index --
  comes out the same. What differs is the call: a batched fetch is handed the collection of
  argument tuples, bound by the `:as`, and answers a map keyed by them, `{[crew] result}`.
  Keying by the tuple rather than aligning positionally is what lets the library do the
  deduplication rather than the author. Checks are never batched; a check judges one value.

  A fetch does not decide validity. It produces a value, and the checks naming it decide
  what that value means -- `nil` is neither a failure nor a success. An exception from a
  fetch propagates rather than becoming a failure: a fetch that throws is usually the
  database being unreachable, and \"we could not determine whether this is valid\" is a
  different claim from \"this is invalid\". An author who wants it treated as a failure
  catches it inside the fetch, which loses nothing; the reverse is not recoverable.

  Shape gates IO. Pure checks run first and ungated; an io-dep fetches once every pure check
  over the fields *it* names has passed and every io-dep it names has fetched; an external
  validation runs once its io-deps have fetched. Nothing reaches a database until the values
  it will carry there are established as well-formed, because the payload that triggers the
  query is the one least worth trusting -- and because an io-dep declares its own fields, the
  gate is narrow: a malformed field no fetch touches holds up nothing.

  A fetch is shared for one validation call and no longer. Within a single [[check]],
  [[recheck]] or [[seal]] an io-dep is fetched at most once however many checks name it,
  and when the call returns the value is gone. So a draft carries no IO state: what it
  carries is which checks passed, which is a fact about the checking rather than about the
  world.

  `:certificates` maps a certificate name to the checks it covers -- a group a draft can be
  run through before it is complete, which [[check]] takes by name exactly as it takes a
  check's. Holding one is derived from which
  checks are passing rather than recorded anywhere, so nothing coordinates between them,
  and a certificate changes nothing about what sealing demands. A name it covers that is
  not a check of this type, a certificate covering nothing, or one spelled like a check or
  like the built-in [[every-check]], is a compile error naming the offender. See
  [[parse-certificates]].

  `:options` holds `:io-limited`, which takes `true`, `false`, or a symbol naming a var
  holding one -- read where the form is written, so the type is compiled with what the var
  held then and a later `alter-var-root` leaves it alone.
  `:io-limited` makes direct `assoc` and `merge` on a sealed value throw for any field an
  external validation reads -- its own fields and its fetches' alike -- so those writes go
  through [[drafting]], where N of them cost one round of checks at the seal rather than a
  query apiece on the way there. Fields no external validation reaches are written as before,
  and the set is computed from the arglists rather than declared. See [[io-limited!]].

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
  (let [{:keys [options checks io-deps certificates]} (parse-definition tname definition)
        impl-forms (parse-impls tname impls)
        ;; Private: the model carries `:draft-seal`, which turns a draft into a sealed
        ;; value and runs nothing, so a public name for it in the defining namespace is the
        ;; same back door `model-of` would be.
        model-sym (with-meta (symbol (str tname "-model")) {:private true})
        fsyms     (mapv #(with-meta % nil) fields)
        ;; The defining namespace qualifies the keys, so a field named `name` is
        ;; `:this.ns/name` and two types sharing a field name share nothing else.
        key-ns    (str (ns-name *ns*))
        qkey      (fn [f] (keyword key-ns (str f)))
        ks        (mapv qkey fields)
        n         (count fsyms)
        key->sym  (zipmap ks fsyms)
        ;; every local is gensymed: a field could be named k, v, o, acc ...
        [ksym vsym dsym osym hsym essym cursym entsym accsym mtasym mta2sym msym thunksym
         tgtsym isym cachesym xsym newsym tuplessym tupsym]
        (map gensym ["k" "v" "d" "o" "h" "es" "cur" "ent" "acc" "mta" "mta2" "m" "thunk"
                     "target" "i" "cache" "x" "new" "tuples" "tuple"])
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
        ;; ---- the io-deps ----
        ;; A keyword in an arglist means an io-dep, so the set of them has to be known
        ;; before any arglist can be resolved -- including an io-dep's own, since one may
        ;; name another. The name is the only thing needed for that; the graph comes after.
        io-names  (set (keys io-deps))
        _         (reject-field-collisions! tname ks io-names)
        io-parsed (mapv (fn [[dep form]]
                          (let [d (parse-dep tname :io-dep dep form)]
                            (assoc d :deps (resolve-deps {:record tname :what :io-dep :name dep
                                                          :fields fieldmap :io-deps io-names}
                                                         (:args d)))))
                        (seq io-deps))
        ;; Fetch order, and the one place a cycle can be caught: a fetch that waits on
        ;; itself never runs, and a form that describes one never compiles.
        io-parsed (order-io-deps! tname io-parsed io-names)
        nio       (count io-parsed)
        ;; Whether the generated constructors can meet a batched fetch that misbehaves, which
        ;; is what decides whether their docstrings have anything to say about one. A type
        ;; with no batched io-dep cannot reach either of those throws, and a docstring that
        ;; mentioned them would be describing a different type.
        batched-io? (boolean (some :batched io-parsed))
        batch-doc (if batched-io?
                    (str ";\n  and `:malformed-batch` or `:incomplete-batch` when one of this "
                         "type's batched fetches\n  answers something other than a map keyed by "
                         "every tuple it was handed, each\n  carrying `:record`, `:io-dep` and "
                         "what the fetch returned")
                    "")
        io?       (fn [d] (contains? io-names d))
        batched?  (fn [j] (boolean (:batched (nth io-parsed j))))
        ;; What a fetch reads for one dependency, whichever form it was written in: a field
        ;; out of the value, an io-dep out of what this call has already fetched.
        io-arg    (fn [x cache]
                    (fn [d] (if (io? d) `(get ~cache ~d) `(get ~x ~d))))
        ;; io-dep name -> what reaching it costs: `:fields` are the fields it names itself,
        ;; which is what gates it; `:io` are the io-dep indices needed to fetch it, itself
        ;; included, in fetch order; `:field-closure` is every field it reads, directly or
        ;; through another. Folded forward over the ordered io-deps, so what a dependency
        ;; needs is already computed by the time anything names it.
        io-info   (first
                   (reduce (fn [[acc i] d]
                             (let [own-io (filterv io? (:deps d))
                                   own-f  (filterv (complement io?) (:deps d))]
                               [(assoc acc (:name d)
                                       {:idx i
                                        :fields own-f
                                        :io (vec (sort (into #{i} (mapcat #(:io (acc %))) own-io)))
                                        :field-closure
                                        (into [] (distinct)
                                              (concat own-f (mapcat #(:field-closure (acc %))
                                                                    own-io)))})
                                (inc i)]))
                           [{} 0] io-parsed))
        ;; ---- the checks ----
        parsed    (mapv (fn [[cname form]]
                          (let [c (parse-dep tname :check cname form)]
                            (assoc c :deps (resolve-deps {:record tname :what :check :name cname
                                                          :fields fieldmap :io-deps io-names}
                                                         (:args c)))))
                        (seq checks))
        _         (reject-duplicate-names! tname (map :name parsed) io-names)
        ;; A check named after a field is that field's shape validation -- the check that says
        ;; what a well-formed value of that field is. The naming convention is the whole of
        ;; the declaration, and it is marked here so that nothing downstream re-derives it. It
        ;; names exactly its own field or the form does not compile, which is what keeps the
        ;; dependency structure two levels deep: shape validations depend on nothing, every
        ;; other check depends only on shape validations, and no ordering among checks ever
        ;; has to be computed.
        field-key? (set ks)
        parsed    (mapv (fn [c]
                          (let [sh (when (contains? field-key? (:name c)) (:name c))]
                            (when (and sh (not= [sh] (:deps c)))
                              (malformed-shape-check! tname c sh))
                            (assoc c :shape sh)))
                        parsed)
        ;; What a check reaches: the io-deps behind it in fetch order, and every field it
        ;; reads -- its own, plus whatever the fetches it names read. A check is IO-bound
        ;; exactly when the first of those is non-empty.
        parsed    (mapv (fn [c]
                          (let [own-io (filterv io? (:deps c))]
                            (assoc c
                                   :io (vec (sort (into #{} (mapcat #(:io (io-info %))) own-io)))
                                   :field-closure
                                   (into [] (distinct)
                                         (concat (filterv (complement io?) (:deps c))
                                                 (mapcat #(:field-closure (io-info %)) own-io))))))
                        parsed)
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
        ivar      (fn [j] (symbol (str tname "--io-" j)))
        deps-of   (fn [i] (:deps (nth parsed i)))
        pure?     (fn [i] (empty? (:io (nth parsed i))))
        shape?    (fn [i] (some? (:shape (nth parsed i))))
        ;; field key -> the index of that field's shape validation, where it has one. A field
        ;; with none has no declared shape: the checks naming it run against whatever it
        ;; holds and have to be total over anything, which is the author's choice made
        ;; loudly rather than a hole.
        shape-id  (into {} (keep-indexed (fn [i c] (when (:shape c) [(:shape c) i]))) parsed)
        ;; Every other check waits on the shape validations of the fields it names, so it
        ;; receives well-formed input by construction and restates nothing. A throw from it
        ;; is therefore a genuine bug in the check, which is what the check contract already
        ;; says a throw is. Sorted, so a phase reports in declaration order; as a mask too,
        ;; because a draft tests it against its verdicts in one `and`.
        parsed    (mapv (fn [c]
                          (let [g (if (:shape c)
                                    []
                                    (vec (sort (into #{}
                                                     (keep shape-id)
                                                     (remove io? (:deps c))))))]
                            (assoc c :shape-gate g :shape-mask (reduce bit-set 0 g))))
                        parsed)
        ;; Derived from the arglists, so it cannot be misdeclared: there is nowhere to
        ;; declare it. Sorted vectors, because a sorted set does not survive being embedded
        ;; in emitted code -- the compiler rebuilds a set literal as a hash set.
        ;;
        ;; Over what a check *reads* rather than what it names: a check naming a fetch reads
        ;; the fields that fetch names, so a write to one of them makes its verdict stale in
        ;; exactly the way a write to a field it named itself would. A stale verdict is no
        ;; verdict, and this index is what clears it.
        index     (reduce (fn [idx i]
                            (reduce #(update %1 %2 (fnil conj []) i) idx
                                    (:field-closure (nth parsed i))))
                          {} (range nchecks))
        ;; What must pass before a check's fetches may run: the pure checks over the fields
        ;; those fetches name. Nothing reaches a database until the values it will carry
        ;; there are established as well-formed, and because an io-dep declares its own
        ;; fields the gate is narrow without anyone narrowing it.
        parsed    (mapv (fn [c]
                          (let [g (let [fs (into [] (comp (mapcat #(:fields (io-info (:name (nth io-parsed %)))))
                                                          (distinct))
                                                 (:io c))]
                                    (vec (sort (into #{} (comp (mapcat #(get index %))
                                                               (filter pure?))
                                                     fs))))]
                            (assoc c :gate g :gate-mask (reduce bit-set 0 g))))
                        parsed)
        pure-idxs (fn [idxs] (filterv pure? idxs))
        io-idxs   (fn [idxs] (filterv (complement pure?) idxs))
        ;; What a write to `k` leaves standing: every verdict except the checks naming `k`.
        ;; Derived from the same index, so a field a check reads is a field that invalidates
        ;; it, and one bitwise `and` seeds the new draft. Computed here, so what the write
        ;; costs at run time is the `and` and nothing else.
        survives  (fn [k] (reduce bit-clear all-pass (get index k)))
        ;; ---- :io-limited ----
        ;; Which fields a direct write to a sealed value is refused for: exactly those an
        ;; external validation reads, computed from the same index rather than declared
        ;; anywhere. A check reads the fields it names and the fields its fetches name, so
        ;; a field behind a fetch is reached without anyone saying so, and a field only
        ;; pure checks read -- or no check at all -- is written exactly as before. The
        ;; option narrows itself; declaring io-deps is what makes that computable.
        limited   (if (:io-limited options)
                    (into #{} (filter (fn [k] (seq (io-idxs (get index k))))) ks)
                    #{})
        limited?  (fn [k] (contains? limited k))
        ;; ---- the protocol implementations ----
        ;; The draft type runs the body as written; only the `unsealed` marker is unwrapped,
        ;; so a body means the same thing wherever it runs.
        draft-impls (mapv (fn [[kind form]]
                            (if (= kind :type)
                              form
                              (let [[mname args & body] form]
                                `(~mname ~args
                                   (~(internal 'draft-impl-return)
                                    ~model-sym
                                    ~(recurrable-body args body))))))
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
                                    (~(internal 'sealed-impl-return)
                                     ~model-sym
                                     (let [~toksym (Object.)]
                                       (push-thread-bindings
                                        {~(internal-var '*owner*) ~toksym})
                                       (try
                                         (let [~recv ((:draft-from ~model-sym)
                                                      ~selfsym ~toksym)]
                                           ~(recurrable-body args body))
                                         (finally (pop-thread-bindings)))))))))
                           impl-forms)
        ;; Run the checks at `idxs` over the expressions `argvs-of` gives for each -- locals
        ;; or direct field reads, so evaluating one twice costs nothing. The shape validations
        ;; are called first and every other one after, each of those guarded by the shape
        ;; validations of the fields it names: a check whose gate did not come back `true` is
        ;; not called and stands as `true`, which is exactly what it means for it not to be
        ;; reported. That is sound here and only here, because a gate that failed is itself in
        ;; this group and about to throw -- nothing a skipped check stands for escapes.
        ;;
        ;; A gate not among `idxs` is a gate that passed: this is the sealed path, and a
        ;; sealed value is one every check passed on, so the only verdicts in doubt are
        ;; those over a field being written.
        ;;
        ;; Every check that is going to run is called before any verdict is read, because a
        ;; failure does not stop the pass; then every one that did not return `true` is
        ;; reported together, shape failures first. Nothing is caught, so a check that
        ;; throws leaves the `let` before there is anything to report, which is the
        ;; contract: a throw is a bug rather than a verdict. The value maps are built only
        ;; inside the branch that is about to throw.
        run-group (fn [idxs argvs-of]
                    (let [idxs (pure-idxs idxs)]
                      (when (seq idxs)
                        (let [in    (set idxs)
                              order (into (filterv shape? idxs) (remove shape? idxs))
                              rs    (into {} (map (fn [i] [i (gensym (str "r" i "-"))])) order)
                              gate  (fn [i] (filterv in (:shape-gate (nth parsed i))))]
                          `(let [~@(mapcat
                                    (fn [i]
                                      (let [call `(~(cvar i) ~@(argvs-of i))
                                            g    (gate i)]
                                        [(rs i)
                                         (if (seq g)
                                           `(if (and ~@(map (fn [j] `(true? ~(rs j))) g))
                                              ~call
                                              true)
                                           call)]))
                                    order)]
                             (when-not (and ~@(map (fn [i] `(true? ~(rs i))) order))
                               (~(internal 'checks-failed!)
                                ~model-sym
                                [~@(map (fn [i]
                                          `(when-not (true? ~(rs i))
                                             (~(internal 'failure-entry)
                                              (nth (:checks ~model-sym) ~i) ~(rs i)
                                              (array-map
                                               ~@(interleave (deps-of i)
                                                             (argvs-of i))))))
                                        order)])))))))
        ;; Phase three, for the paths that produce a sealed value: the external validations
        ;; among `idxs`, run against the value `xform` builds once the inline group above has
        ;; passed. Not inlined, because what a fetch is handed has to be computed in the
        ;; graph's order and shared across the checks naming it -- and because the value has
        ;; to exist before it can be asked for the fields the fetches read. It escapes only if
        ;; the checks pass, as it does through `merge` already.
        with-io   (fn [idxs xform]
                    (let [io (io-idxs idxs)]
                      (if (seq io)
                        `(let [~newsym ~xform]
                           (~(internal 'run-io-checks!) ~model-sym ~newsym ~io)
                           ~newsym)
                        xform)))]
    `(do
       (declare ~model-sym)
       ~@(map (fn [i] `(def ~(with-meta (cvar i) {:private true}) ~(:fn (nth parsed i))))
              (range nchecks))
       ~@(map (fn [j] `(def ~(with-meta (ivar j) {:private true}) ~(:fn (nth io-parsed j))))
              (range nio))

       (rc/def-record-type ~tname ~params
         (~'get [_# ~ksym ~dsym]
           (case ~ksym ~@(interleave ks fsyms) ~dsym))

         ;; The checks naming this field are known here, so the pure ones are called with the
         ;; new value and the untouched fields directly -- no lookup, and no allocation unless
         ;; they all pass. The external validations run after, against the value they are
         ;; about to admit: shape gates IO, so a write that fails a check over the field
         ;; itself never reaches a fetch. Nothing else has to gate them, because every other
         ;; check on a sealed value passed when it was made and only this field has moved.
         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [j k]
                         [k (if (limited? k)
                              ;; Refused before anything runs: the value the caller asked
                              ;; for is one every check passed on, so there is nothing to
                              ;; defer and nothing worth fetching for a write that is not
                              ;; going to happen.
                              `(~(internal 'io-limited!) ~model-sym [~k])
                              `(do ~(run-group (get index k)
                                               (fn [i] (map (fn [d]
                                                              (if (= d k) vsym (key->sym d)))
                                                            (deps-of i))))
                                   ~(with-io (get index k) (ctor-with j vsym))))])
                       (range n) ks)
             (~(internal 'unknown-key!) ~model-sym ~ksym)))

         ;; A validated value cannot hold an undeclared key, so dissoc of one is the
         ;; ordinary map no-op. Only the declared fields are permanent.
         (~'dissoc [this# ~ksym]
           (case ~ksym
             ~(apply list ks) (~(internal 'no-dissoc!) ~model-sym ~ksym)
             this#))
         (~'keys [_#] ~ks)
         ;; The library's keys over the caller's, so nothing a caller writes can claim more
         ;; than the value can show for itself. Here what it shows is constant: a sealed
         ;; value is one every check passed on.
         (~'meta [_#] (~(internal 'merged-meta) ~mtasym ~sealed-meta))
         (~'with-meta [_# ~mta2sym] (new ~tname ~@fsyms ~mta2sym))

         ;; one allocation for the whole merge, each applicable check once at the end
         (~'cons [_# ~osym]
           ;; The entries are walked once, into the fields themselves: this type's fields
           ;; are locals here, so an entry is a rebinding of one of them and the value is
           ;; constructed out of what the loop carries. There is no map in the middle of
           ;; it -- what a merge allocates is the value it returns.
           (let [~essym (~(internal 'entries-of) ~model-sym ~osym)
                 ~hsym  (loop [~cursym (seq ~essym)
                               ~@(interleave fsyms fsyms)]
                          (if ~cursym
                            (let [~entsym (first ~cursym)
                                  ~ksym   (nth ~entsym 0)
                                  ~vsym   (nth ~entsym 1)]
                              (case ~ksym
                                ~@(mapcat (fn [i k]
                                            [k `(recur (next ~cursym)
                                                       ~@(assoc fsyms i vsym))])
                                          (range n) ks)
                                ;; A key the type does not declare is refused here rather
                                ;; than admitted and caught at the seal: `drafting` would
                                ;; not take it either.
                                (~(internal 'unknown-key!) ~model-sym ~ksym)))
                            (new ~tname ~@fsyms ~mtasym)))]
             ;; After the walk, so every key is known to be declared before this speaks,
             ;; and before anything runs. Only where the option reaches something, so a
             ;; type without one emits nothing here.
             ~@(when (seq limited)
                 [`(~(internal 'io-limited-write!) ~model-sym (map first ~essym))])
             (~(internal 'run-checks!) ~model-sym (:run-sealed ~model-sym) ~hsym
              (~(internal 'checks-for) ~model-sym ~essym))
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
       ;; and a mutable bitmask of which checks currently pass. A field nobody has written
       ;; holds nil, as it would on a `defrecord` whose field was never set, so there is no
       ;; "never written" state to maintain and nothing to keep away from a check body;
       ;; which checks are still to run is read off the mask, and that is the whole of what
       ;; a write has to seed.
       (rc/def-record-type ~dname ~(into fsyms [extsym vdfield ownsym mtasym])
         ;; A declared field is present whatever it holds, so `get` never reaches its
         ;; default for one -- which is what a record does.
         (~'get [_# ~ksym ~dsym]
           (case ~ksym
             ~@(interleave ks fsyms)
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
             ~(apply list ks) (~(internal 'no-dissoc!) ~model-sym ~ksym)
             (if (contains? ~extsym ~ksym)
               ~(dctor fsyms `(dissoc ~extsym ~ksym) vdsym ownsym mtasym)
               this#)))
         ;; Every declared key, always. A draft is a record with a map beside it, not a map
         ;; that accumulates keys as they are written.
         (~'keys [_#]
           (if (seq ~extsym) (concat ~ks (keys ~extsym)) ~ks))
         ;; The same two keys a sealed value answers with, read off the mask rather than
         ;; fixed: on a draft they are what has actually been established. Merged over the
         ;; caller's metadata for the same reason, and the caller's own keys survive.
         (~'meta [_#] (~(internal 'draft-meta) ~model-sym ~vdsym ~mtasym))
         (~'with-meta [_# ~mta2sym] ~(dctor fsyms extsym vdsym ownsym mta2sym))
         (~'cons [this# ~osym]
           (reduce (fn [~accsym [~ksym ~vsym]] (assoc ~accsym ~ksym ~vsym))
                   this# (~(internal 'entries-of) ~model-sym ~osym)))

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
          ;; What the option resolves to, rather than what was written: `:io-limited-keys`
          ;; is the fields a direct write to a sealed value is refused for, and is empty
          ;; both where the option was not declared and where the type reaches no IO.
          :io-limited  ~(boolean (:io-limited options))
          :io-limited-keys ~limited
          :field-order ~ks
          :field-set   ~(set ks)
          ;; `:deps` is what the check named, in arglist order, and is what a failure reports.
          ;; `:shape` is the field this check is the shape validation of, or nil, which is
          ;; what puts it in the first phase and what makes it gate anything; `:shape-gate`
          ;; and `:shape-mask` are the shape validations it waits on, as indices and as bits
          ;; of a draft's verdict mask. `:io` is what it reaches through the names it gave --
          ;; the io-deps behind it in fetch order, empty exactly when it is pure; `:gate` and
          ;; `:gate-mask` the pure checks that must pass before any of them fetches;
          ;; `:field-closure` every field it reads, its own and its fetches' alike, which is
          ;; what a write invalidates it by.
          :checks      ~(mapv (fn [i]
                                (let [{:keys [name doc deps shape shape-gate shape-mask
                                              io gate gate-mask field-closure]}
                                      (nth parsed i)]
                                  {:name name :doc doc :deps deps :pred (cvar i)
                                   :shape shape :shape-gate shape-gate :shape-mask shape-mask
                                   :io io :gate gate :gate-mask gate-mask
                                   :field-closure field-closure}))
                              (range nchecks))
          ;; The io-deps in fetch order, so that ascending index is an order they can run
          ;; in, and the names of them, so that a resolved dependency can be told from a
          ;; field key without asking the value.
          ;; `:batched` is how it was written, and the only thing anything at run time has
          ;; to know about that: a batched fetch is handed the tuples and answers a map
          ;; keyed by them, a singular one is handed its dependencies and answers the
          ;; value. What it names is `:deps` either way.
          :io-deps     ~(mapv (fn [j]
                                (let [{:keys [name doc deps batched]} (nth io-parsed j)]
                                  {:name name :doc doc :deps deps :batched (boolean batched)
                                   :fields (:fields (io-info name))
                                   :fetch (ivar j)}))
                              (range nio))
          :io-names    ~io-names
          ;; check name -> its index, which is its bit in a draft's verdict mask. Built
          ;; here because a check is named from outside by name and reached from inside by
          ;; index, and a scan of `:checks` per call would be the only alternative.
          :check-ids   ~(into {} (map-indexed (fn [i c] [(:name c) i])) parsed)
          ;; Every check of this type passing, as a mask. What a draft of a sealed value
          ;; starts from, and what a sealed value answers with wherever a mask is read:
          ;; being sealed is exactly every check having passed, so the two are one constant.
          :all-pass    ~all-pass
          :index       ~index
          ;; field -> the bits of the checks naming it, which is `:index` said in the shape
          ;; a write can union in one `bit-or`. The same derivation as the invalidation
          ;; masks a write to a draft seeds with, and for the same reason: what a merge
          ;; costs at run time should be the `or` and nothing else.
          :field-checks ~(into {} (map (fn [k] [k (reduce bit-set 0 (get index k))])) ks)
          ;; certificate name -> what it covers, as check names, indices, and the bits of
          ;; a draft's verdict mask they own -- so holding one is an `and`. `array-map`
          ;; applied to the pairs, rather than a literal, which would promote and reorder
          ;; past the eighth certificate.
          :certificates (array-map ~@(into [] cat certs))
          :tag         '~tagsym
          ;; One runner per type, so a check reached by index is still called with direct
          ;; field access rather than a lookup per dependency. What it names that is not a
          ;; field is read out of `cache`, the fetches this validation call has made --
          ;; which is the only place a fetched value ever lives.
          :run-sealed  (fn [~isym ~svsym ~cachesym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i)
                                            ~@(map (fn [d]
                                                     (if (io? d)
                                                       `(get ~cachesym ~d)
                                                       (sfield (key->sym d))))
                                                   (deps-of i)))])
                                     (range nchecks))
                           nil))
          :run-draft   (fn [~isym ~dvsym ~cachesym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i)
                                            ~@(map (fn [d]
                                                     (if (io? d)
                                                       `(get ~cachesym ~d)
                                                       (dfield (key->sym d))))
                                                   (deps-of i)))])
                                     (range nchecks))
                           nil))
          ;; Every fetch is keyed by its argument tuple, whichever form it was written in:
          ;; the tuple is what tells one value's fetch from another's, so it is what a
          ;; result is distributed by and what two values sharing a fetch share. It is
          ;; built here, out of the same reads a singular fetch once took positionally. A
          ;; field is read through `get` rather than directly, because one runner serves
          ;; both forms of the value and a fetch is about to do IO -- what a lookup costs
          ;; here is not what anything waits on.
          :io-args     (fn [~isym ~xsym ~cachesym]
                         (case (int ~isym)
                           ~@(mapcat (fn [j]
                                       [j (vec (map (io-arg xsym cachesym)
                                                    (:deps (nth io-parsed j))))])
                                     (range nio))
                           nil))
          ;; The singular fetches: called with the elements of one tuple, positionally, as
          ;; the arglist they were written with says. A batched one is handed the whole
          ;; collection of tuples instead, which is the only difference between the two
          ;; once the tuples are built.
          :call-io     (fn [~isym ~tupsym]
                         (case (int ~isym)
                           ~@(mapcat (fn [j]
                                       [j `(~(ivar j)
                                            ~@(map-indexed (fn [p _] `(nth ~tupsym ~p))
                                                           (:deps (nth io-parsed j))))])
                                     (remove batched? (range nio)))
                           nil))
          :run-batch   (fn [~isym ~tuplessym]
                         (case (int ~isym)
                           ~@(mapcat (fn [j] [j `(~(ivar j) ~tuplessym)])
                                     (filter batched? (range nio)))
                           nil))
          ;; From the type, nothing is established and every field is nil -- which is what
          ;; a `defrecord` none of whose fields were set holds, and what the shape validations
          ;; will be asked about. From a sealed value everything is established, because
          ;; that is what being sealed means, and the draft inherits exactly that claim.
          :draft-empty      (fn [~ownsym]
                              ~(dctor (repeat n nil) {} 0 ownsym nil))
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
                                      `(~(internal 'verdict-mask) ~dvsym)
                                      tokensym (dfield mtasym)))
          :draft-clean      (fn [~dvsym]
                              ~(dctor dfields {} `(~(internal 'verdict-mask) ~dvsym)
                                      (dfield ownsym) (dfield mtasym)))
          :draft-seal       (fn [~dvsym] (new ~tname ~@dfields ~(dfield mtasym)))})
       ;; `deftype` interns a positional factory that checks nothing -- a public back door
       ;; straight through every guarantee the type exists to make. Replace it with one that
       ;; validates, and unmap the draft's, which nothing outside this file should be calling.
       (defn ~(symbol (str "->" tname))
         ~(str "Construct a validated " tname " from its fields, in declaration order.\n\n"
               "  Throws `:check-failed` when a check does not pass, carrying `:record` and\n"
               "  `:failures` -- a map keyed by check name in phase order, each entry holding the\n"
               "  check's docstring, what it was given and what it returned"
               batch-doc ".")
         [~@fsyms]
         ~(run-group (range nchecks) (fn [i] (map key->sym (deps-of i))))
         ~(with-io (range nchecks) `(new ~tname ~@fsyms nil)))

       (defn ~(symbol (str "map->" tname))
         ~(str "Construct a validated " tname " from a map of its declared keys.\n"
               "  A key it does not declare is an error; a key it declares and the map omits\n"
               "  is nil, as it would be on a defrecord, and that field's shape validation\n"
               "  says whether nil is acceptable.\n\n"
               "  Keys may be written qualified or bare: a bare keyword naming a declared field is\n"
               "  qualified into " key-ns ", which is what lets a printed literal -- whose tag\n"
               "  already carries the namespace -- read back through here and be validated.\n\n"
               "  Throws `:extraneous-keys` when the map holds a key this type does not declare,\n"
               "  carrying `:record` and `:keys`; `:check-failed` when a check does not pass,\n"
               "  carrying `:record` and `:failures`, a map keyed by check name in phase order"
               batch-doc ".")
         [~msym]
         (seal ~model-sym (reduce-kv assoc ((:draft-empty ~model-sym) nil)
                                     (~(internal 'qualify-keys) ~model-sym ~msym))))

       (~(internal 'register!) ~tname ~model-sym)
       (~(internal 'register-draft!) ~dname ~model-sym)
       (~(internal 'register-reader!) '~tagsym ~(symbol (str "map->" tname)))

       (ns-unmap *ns* '~(symbol (str "->" dname)))
       ;; A draft is unvalidated working state. `#<` is the reader's own unreadable marker, so
       ;; the printed form says so rather than implying a round trip that should not exist.
       (defmethod print-method ~dname [x# ^java.io.Writer w#]
         (.write w# (str "#<" '~tname " draft " (into (sorted-map) x#) ">")))
       ;; The tag is namespace-qualified, so it escapes the reader's record-literal path and is
       ;; the exact symbol to hand `clojure.edn/read` as a key in `:readers`.
       (defmethod print-method ~tname [x# ^java.io.Writer w#]
         (.write w# (str "#" '~tagsym (~(internal 'printable) x#))))
       ~tname)))
