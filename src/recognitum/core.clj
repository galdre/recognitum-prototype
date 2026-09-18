(ns recognitum.core
  "A record-like type that carries proof of its own validation.

  [[defrecord]] defines a type from a vector of fields and a map of named checks, and
  generates two types from it: a **sealed value**, which every check declared on the type has
  passed, and a **draft**, an unvalidated working copy of one that records which of its
  checks currently pass. No constructor skips the checks, so holding a sealed value is
  holding proof that they passed.

  The library's vocabulary, defined here and used without redefinition elsewhere:

  - A **check** is named by a namespace-qualified keyword and declares what it reads in its
    arglist. A **shape validation** states the shape of a single field: what a well-formed
    value of that field is, taken on its own. It is named after the field and reads only that
    field. An **invariant validation** states a relational invariant across the fields: a
    relationship among them that holds of every valid value. An **external validation**
    states a relational invariant between the value and an external resource, read through
    an io-dep; it holds as of when it ran.
  - An **io-dep** is a function that fetches something from outside the value. It is declared
    the way a check is, and named in an arglist by a keyword.
  - A **certificate** names a subset of a type's checks, so a group of them can be run
    against a draft, under one name, before the value is complete.
  - To **seal** a draft is to validate it and return the sealed value. [[drafting]] opens a
    draft, runs a body against it, and seals the result. [[unsealed]] marks a value returned
    from a protocol method as one that is not to be sealed.
  - A **drafting method** is a protocol method marked `^:drafting`, declaring that it returns
    a modified receiver; on the sealed type it is handed a draft and its result is sealed.

  The public surface:

  - Define a type: [[defrecord]].
  - Open and close a draft: [[draft]], [[drafting]], [[clean]], [[seal]], [[unsealed]].
  - Run checks against one value: [[check]], [[check!]], [[recheck]], [[recheck!]].
  - Run checks across a collection: [[check-all]], [[check-all!]], [[recheck-all]],
    [[recheck-all!]].
  - Ask which checks a value passes: [[passed]], [[certificates]], [[every-check]].
  - Tell values apart: [[of-type?]], [[draft?]], [[sealed?]].
  - Read printed values: [[data-readers]]."
  (:refer-clojure :exclude [defrecord])
  (:require
   [recognitum.collections :as rc]))

(def ^:private core-ns
  "This namespace's own name. Bound here rather than read where it is used, because `*ns*`
  inside a macro is the namespace the macro is expanding into."
  (ns-name *ns*))

(defn- internal-var
  "`(var recognitum.core/nm)`, for generated code to name a private var of this namespace."
  [nm]
  (list 'var (symbol (str core-ns) (name nm))))

(defn- internal
  "The form generated code names a private var of this namespace by: the var, dereferenced.
  A macro expands into another namespace, where a qualified symbol naming a private var does
  not compile, and the expansion emits `@#'recognitum.core/checks-failed!` instead."
  [nm]
  (list `deref (internal-var nm)))

(defonce ^:private registry (atom {}))

(defn- register! "Associate a class with its model map." [cls model] (swap! registry assoc cls model))

(defonce ^:private readers (atom {}))

(defn- register-reader! "Associate a print tag with its validating map constructor." [tag ctor]
  (swap! readers assoc tag ctor))

(defn data-readers
  "`{tag -> validating constructor}` for every type defined so far, to pass
  `clojure.edn/read` or `clojure.edn/read-string` as `:readers`.

      (edn/read-string {:readers (rec/data-readers)} s)

  No tag is registered globally: this library never alters `*data-readers*`, so plain `read`
  and `read-string` reach a type's tag only through a `data_readers.clj` on the classpath.

  The constructors validate, so a literal whose contents fail a check throws at read time."
  [] @readers)

;;; ------------------------------------------ Checks ------------------------------------------

(defn- unknown-key! [model k]
  (throw (ex-info (format "%s has no field %s -- its fields are %s"
                          (:name model) (pr-str k) (pr-str (:field-order model)))
                  {:error ::unknown-field :record (:name model) :key k
                   :declared (:field-order model)})))

(defn- dep-values
  "What a check was given, keyed by the dependency it was given for, in arglist order. A
  field's value is read out of `x`, an io-dep's out of `cache`, the fetches this validation
  call made."
  [model check x cache]
  (let [ios (:io-names model)]
    (apply array-map
           (mapcat (fn [d] [d (if (contains? ios d) (get cache d) (get x d))])
                   (:deps check)))))

(defn- failure-entry
  "The `[check-name report]` pair for a check that did not return `true`. `ret` is what it
  returned, which is the failure data; `vals` is what it was given; `stopped` the names of
  the checks that did not run because this one failed, left out where it stopped none."
  [check ret vals stopped]
  [(:name check)
   (cond-> {:doc (:doc check) :values vals :returned ret}
     (seq stopped) (assoc :stopped stopped))])

(defn- name-list
  "`xs` as an English list: `a`, then `a and b`, then `a, b and c`."
  [xs]
  (let [ss (map pr-str xs)]
    (if (next ss)
      (str (apply str (interpose ", " (butlast ss))) " and " (last ss))
      (str (first ss)))))

(defn- failure-str [[cname {:keys [doc values returned stopped]}]]
  (str (format "%s%s: given %s it returned %s"
               cname (if doc (str " (" doc ")") "") (pr-str values) (pr-str returned))
       (when (seq stopped)
         (format ", so %s did not run" (name-list stopped)))))

(defn- checks-failed!
  "Throw one `:check-failed` carrying every failure in `fs`, [[failure-entry]] pairs in the
  order they are to be reported. `fs` may hold `nil` where a check passed, so a call site can
  emit one expression per check."
  [model fs]
  (let [fs (into [] (remove nil?) fs)]
    ;; `array-map` applied to the pairs: `conj` past the eighth entry would promote to a hash
    ;; map and lose the reporting order.
    (throw (ex-info (str (:name model) " failed "
                         (when (next fs) (str (count fs) " checks -- "))
                         (apply str (interpose "; " (map failure-str fs))))
                    {:error ::check-failed
                     :record (:name model)
                     :failures (apply array-map (into [] cat fs))}))))

(defn- group-name
  "What a group call reports as its `:record`: the one type's name, or, where the group spans
  several, a vector of all of them in the order they first appear."
  [models]
  (let [ns' (reduce (fn [acc m] (if (some #{(:name m)} acc) acc (conj acc (:name m))))
                    [] models)]
    (if (next ns') ns' (first ns'))))

(defn- bulk-failed!
  "Throw one `:check-failed` carrying every failure of every value in a group call, keyed by
  the value's index in the collection passed to it and, under that, by check name in the
  order given. `fs` is `[index [[check-name report] ...]]` pairs, in index order.

  Keyed by index because two drafts holding identical data are `=`. The thrown data carries
  `:bulk true`, which tells this shape from [[checks-failed!]]'s."
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

(defn- batch-answer!
  "`res`, what the batched fetch `d` returned, once it is known to be a map holding an entry
  for every tuple in `tuples`.

  Throws `:malformed-batch` where `res` is not a map, and `:incomplete-batch` where a tuple
  has no entry -- an entry holding `nil` counts as present. Both carry `:record`, `:io-dep`,
  `:tuples` and `:returned`."
  [model d res tuples]
  (when-not (or (map? res) (instance? java.util.Map res))
    (throw (ex-info (format (str "%s io-dep %s is batched and must return a map keyed by "
                                 "argument tuple -- it returned %s")
                            (:name model) (pr-str (:name d)) (pr-str res))
                    {:error ::malformed-batch :record (:name model) :io-dep (:name d)
                     :tuples tuples :returned res})))
  (let [missing (into [] (remove #(contains? res %)) tuples)]
    (when (seq missing)
      (throw (ex-info (format (str "%s io-dep %s returned no entry for %s -- a batched fetch "
                                   "must return an entry for every tuple it is given, even "
                                   "if the entry holds nil")
                              (:name model) (pr-str (:name d))
                              (apply str (interpose ", " (map pr-str missing))))
                      {:error ::incomplete-batch :record (:name model) :io-dep (:name d)
                       :missing missing :tuples tuples :returned res}))))
  res)

(defn- answers-for
  "What the io-dep at `j` returns for each of `tuples`, keyed by tuple. A singular fetch is
  called once per tuple with the tuple's elements as its arguments; a batched one is called
  once with the whole collection. Nothing downstream distinguishes the two forms.

  `tuples` must already be distinct: each one is fetched exactly once."
  [model j tuples]
  (let [d (nth (:io-deps model) j)]
    (if (:batched d)
      (batch-answer! model d ((:run-batch model) j tuples) tuples)
      (into {} (map (fn [t] [t ((:call-io model) j t)])) tuples))))

(defn- fetch-io-all
  "Fetch, for every value in `xs`, every io-dep the checks at its own entry of `idxss` name,
  and return a vector index-aligned with `xs` of maps io-dep name -> what it returned for
  that value.

  Each io-dep is fetched once per distinct argument tuple across the whole of `xs`, and a
  batched one is called once with all of them. An exception out of a fetch propagates."
  [model xs idxss]
  (let [cs    (:checks model)
        xs    (vec xs)
        needs (mapv (fn [idxs] (into #{} (mapcat #(:io (nth cs %))) idxs)) idxss)]
    ;; Ascending index is fetch order: the model holds the io-deps sorted so that a fetch
    ;; comes after everything it names, so a tuple built out of what another fetch answered
    ;; finds that answer already in the cache.
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
  "[[fetch-io-all]] for one value: the io-deps the checks at `idxs` name, as one map io-dep
  name -> what it returned."
  [model x idxs]
  (first (fetch-io-all model [x] [idxs])))

(defn- failures-of
  "`[i ret]` for those of `idxs` that did not return `true`, run through `run`, the type's
  own generated runner. Every one of them runs; only `true` passes, and nothing is caught."
  [run x idxs cache]
  (reduce (fn [acc i]
            (let [ret (run i x cache)]
              (if (true? ret) acc (conj acc [i ret]))))
          [] idxs))

(defn- stopped-by
  "The names of the checks `plan` names that did not run because the check at index `i`
  failed: those waiting on it, directly or through another check that waits on it, in
  declaration order."
  [model plan i]
  (let [^longs closures (:closures model)]
    (into []
          (keep-indexed (fn [j c]
                          (when (and (bit-test (long plan) j)
                                     (bit-test (aget closures j) i))
                            (:name c))))
          (:checks model))))

(defn- failure-entries
  "The report for `fs`, which is [[failures-of]]'s answer: the check's docstring, the
  dependencies it declared with what it was given for each, what it returned, and which of
  the checks `plan` names its failure stopped."
  [model x fs cache plan]
  (map (fn [[i ret]]
         (let [c (nth (:checks model) i)]
           (failure-entry c ret (dep-values model c x cache) (stopped-by model plan i))))
       fs))

(defn- failure-report
  "[[failure-entries]] as one map, check name -> that check's entry, in the order the
  failures are to be reported: what an answering form answers with, and what a throwing one
  carries under `:failures`."
  [model x fs cache plan]
  ;; `array-map` applied to the pairs: `conj` past the eighth entry would promote to a hash
  ;; map and lose the reporting order.
  (apply array-map (into [] cat (failure-entries model x fs cache plan))))

(defn- run-group!
  "Run `idxs` and throw once carrying every failure among them. `idxs` is the whole of what
  this call had to run, so it is also what a failure among them can have stopped."
  [model run x idxs cache]
  (let [fs (failures-of run x idxs cache)]
    (when (seq fs)
      (checks-failed! model (failure-entries model x fs cache (reduce bit-set 0 idxs))))))

(defn- run-io-checks!
  "Fetch what the external validations at `idxs` name and run them against the sealed value
  `x`, throwing once with every failure. Every check those wait on must have passed before
  this runs."
  [model x idxs]
  (run-group! model (:run-sealed model) x idxs (fetch-io model x idxs)))

(defn- io-entries
  "The report for the external validations `idxs` names, run against the sealed value `x`
  with what they name fetched for them, and nothing where `idxs` names none or they all pass.

  `idxs` may hold `nil` where an external validation waits on something this pass failed, so
  a call site can emit one expression per external validation."
  [model x idxs]
  (let [idxs (into [] (remove nil?) idxs)]
    (when (seq idxs)
      (let [cache (fetch-io model x idxs)]
        (failure-entries model x (failures-of (:run-sealed model) x idxs cache)
                         cache (reduce bit-set 0 idxs))))))

(defn- checks-failed-with-io!
  "Throw one `:check-failed` carrying `fs`, the failures a compiled group of pure checks
  found, together with those of the external validations `idxs` names, which run before the
  throw: a throwing entry point runs everything it was asked to run, and that includes every
  external validation nothing it waits on failed."
  [model x fs idxs]
  (checks-failed! model (concat fs (io-entries model x idxs))))

(defn- entries-of
  "The entries `o` contributes to a `conj` onto a value of this type: a map's, or the one a
  two-element vector is.

  Throws `:cannot-conj` for anything else, carrying `:record` and `:value`."
  [model o]
  (cond (map? o)                     (seq o)
        (instance? java.util.Map o)  (seq (into {} o))
        (and (vector? o) (= 2 (count o))) [o]
        :else (throw (ex-info (format "cannot conj %s onto a %s -- conj takes a map or a [k v] pair"
                                      (pr-str o) (:name model))
                              {:error ::cannot-conj :record (:name model) :value o}))))

(defn- io-checks-on
  "The external validations that read `k`, by name and in declaration order."
  [model k]
  (into [] (comp (map (:checks model)) (filter (comp seq :io)) (map :name))
        (get (:index model) k)))

(defn- io-limited!
  "Throw `:io-limited` for a direct write of `ks` to a sealed value of a type declaring
  `:options {:io-limited true}`. Nothing is fetched and no check runs.

  Carries `:record`, `:key`, `:keys`, `:checks` -- the external validations reading each --
  and an `:instead` form spelling the same write under [[drafting]]."
  [model ks]
  (let [ks    (vec ks)
        by    (apply array-map (mapcat (fn [k] [k (io-checks-on model k)]) ks))
        names (fn [xs] (apply str (interpose ", " (map pr-str xs))))
        many? (boolean (next ks))]
    (throw (ex-info (format (str "%s declares :io-limited, and %s %s read by %s (%s) -- "
                                 "write %s inside drafting rather than by assoc or merge "
                                 "on a sealed value")
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
  "Throw `:io-limited` where `ks`, the keys a merge onto a sealed value writes, include a
  field an external validation reads (`:io-limited-keys`), naming every such key in field
  declaration order."
  [model ks]
  (let [lim     (:io-limited-keys model)
        written (set ks)]
    (when (some lim written)
      (io-limited! model (filterv #(and (lim %) (written %)) (:field-order model))))))

(defn- qualify-keys
  "Rewrite a bare keyword naming a declared field of `model` into the type's own namespace,
  and leave every other key exactly as it was written -- so a key naming no field is
  reported as the caller spelled it. Where a map holds both spellings of one field, the
  qualified entry wins."
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
  "The map a sealed value prints as: its declared fields, sorted, with the namespace the
  print tag already carries dropped from every key."
  [x]
  (into (sorted-map) (map (fn [[k v]] [(keyword (name k)) v])) x))

;;; ------------------------------------------ Draft ------------------------------------------

(defonce ^:private draft-registry (atom {}))

(defn- register-draft! "Associate a draft class with its model map." [cls model]
  (swap! draft-registry assoc cls model))

(def ^:private ^:dynamic *owner*
  "Token identifying the outermost `drafting` currently on the stack, or `nil` outside one. A
  draft records the token in force when it was created, which is how a nested `drafting`
  tells a draft an enclosing form owns from one no form owns, which it adopts."
  nil)

(defn draft?
  "Is `x` a draft of any type this library defines? [[of-type?]] tests the type rather than
  the form; `(and (draft? x) (of-type? Heist x))` is a draft of `Heist`."
  [x] (contains? @draft-registry (class x)))

(defn- draft-model [x] (@draft-registry (class x)))

(defprotocol Verdicts
  "The record a draft keeps of which of its checks currently pass: one bit per check, held in
  a mutable field. A draft's data is immutable; which checks have been run against that data
  is not part of the data. A mutable field is private to the class declaring it, so these two
  methods are how anything else reads or writes it."
  (verdict-mask [d]
    "The mask of checks currently passing on `d` -- bit `i` for the check at index `i` of
    its type's `:checks`.")
  (record-verdict! [d i pass?]
    "Set the bit of the check at index `i` where `pass?`, clear it otherwise, and return
    `pass?`."))

;; The protocol and both its methods are private: `seal` runs only the checks the mask does
;; not mark passing, so a public name that set a bit would be a way past every check the type
;; declares. The generated
;; draft type still implements the protocol from the namespace it is defined in --
;; `deftype` resolves a spec name through `ns-resolve`, which does not consult privacy.
(alter-meta! #'Verdicts assoc :private true)
(alter-meta! #'verdict-mask assoc :private true)
(alter-meta! #'record-verdict! assoc :private true)

(defn- value-model
  "The model for `x` -- a draft or a sealed value -- or `nil` where `x` is neither, the type
  object included: a class is not a value of its type."
  [x]
  (let [c (class x)] (or (@draft-registry c) (@registry c))))

(defn- value-model!
  "[[value-model]], or a throw of `:not-a-value` naming `what`, the operation that wanted one,
  and carrying `:value`. A sealed value is as acceptable here as a draft."
  [what x]
  (or (value-model x)
      (throw (ex-info (format "%s expects a draft or a sealed value, and %s is neither"
                              what (pr-str x))
                      {:error ::not-a-value :value x}))))

(defn- live-mask
  "The mask a check's prerequisites are read off: a draft's own record of which checks pass,
  or, for a sealed value, the type's constant with every bit set."
  [model draft? x]
  (if draft? (verdict-mask x) (:all-pass model)))

(defn- value-mask
  "The mask of checks passing on `x`: a draft's own, or, for a sealed value, the type's
  constant with every bit set."
  [model x]
  (live-mask model (some? (draft-model x)) x))

(defn- passed-in
  "The names of the checks `mask` says are passing."
  [model mask]
  (into #{} (keep-indexed (fn [i c] (when (bit-test mask i) (:name c)))) (:checks model)))

(defn- held-in
  "The names of the certificates `mask` says are held: those all of whose checks are
  passing."
  [model mask]
  (into #{} (keep (fn [[cname {cm :mask}]] (when (= cm (bit-and mask cm)) cname)))
        (:certificates model)))

(defn passed
  "The set of check names currently passing on `x`.

  A draft opened from a sealed value starts with all of them, one opened from the type starts
  with none, and a write clears exactly the checks that read the field written, directly or
  through an io-dep. For a sealed value, every check its type declares. The metadata key
  `::passed` carries the same answer for the same value.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`."
  [x]
  (let [model (value-model! "passed" x)]
    (passed-in model (value-mask model x))))

(defn- check-id
  "The index of the check named `cname` on this type, which is its bit in a draft's verdict
  mask.

  Throws `:unknown-check` for a name the type does not declare, carrying `:record`, `:check`
  and the type's check names."
  [model cname]
  (or (get (:check-ids model) cname)
      (throw (ex-info (format "%s declares no check named %s -- its checks are %s"
                              (:name model) (pr-str cname) (pr-str (mapv :name (:checks model))))
                      {:error ::unknown-check :record (:name model) :check cname
                       :checks (mapv :name (:checks model))}))))

;;; ----------------------------------------- Runner -----------------------------------------

;; Work is named by a mask: bit `i` for the check at index `i` of the type's `:checks`, which
;; is also that check's bit in a draft's verdict mask. Every mask the phases and the
;; prerequisites are read out of is a constant the macro computed, so what a call derives is
;; the one thing that varies -- which checks the value already records as passing.

(defn- mask-indices
  "The indices of the bits set in `m`, ascending -- which is check declaration order, since a
  check's index is its bit."
  [m]
  (loop [m (long m), acc (transient [])]
    (if (zero? m)
      (persistent! acc)
      (recur (bit-and m (dec m)) (conj! acc (Long/numberOfTrailingZeros m))))))

(defn- checks-for
  "The checks reading any of the fields `entries` writes, as a mask: what a merge onto a
  sealed value has to run."
  [model entries]
  (let [ms (:field-checks model)]
    (reduce (fn [m e] (bit-or (long m) (long (get ms (nth e 0) 0)))) 0 entries)))

(defn- closure-of
  "Everything the checks `m` names wait on, transitively, as a mask. Each check's own closure
  is a constant of the type, so this is their union."
  [^longs closures m]
  (loop [m (long m), acc 0]
    (if (zero? m)
      acc
      (recur (bit-and m (dec m))
             (bit-or acc (aget closures (Long/numberOfTrailingZeros m)))))))

(defn- plan-mask
  "What is left to do about `requested` for a value whose verdict mask is `mask`, as a mask:
  what the mask does not already say is passing, together with whatever that waits on. `0`
  where there is nothing left to do, as for a sealed value's mask.

  `closure` is everything `requested` waits on, which the type carries per check name and per
  certificate name. `force?` puts everything `requested` names into the answer whatever the
  mask says; prerequisites are still read off the mask."
  [model mask requested closure force?]
  (if force?
    (bit-or (long requested) (bit-and-not (long closure) (long mask)))
    (let [pending (bit-and-not (long requested) (long mask))]
      (if (zero? pending)
        0
        ;; What is pending decides what its prerequisites are, so the closure the type
        ;; carries for the whole request is too much here: a check already passing drags
        ;; nothing in.
        (bit-and-not (bit-or pending (closure-of (:closures model) pending)) (long mask))))))

(def ^:private established
  "The result for a value on which nothing needed to run: no failures and no fetches."
  {:failures [] :cache {} :plan 0})

(defn- established-mask
  "What holds of `x` once the pure checks of this pass have run: its record of which checks
  pass, less the `fs` this pass just failed. A draft's own record already has those cleared
  out of it; a sealed value carries no record, so they are cleared out of the type's
  every-check-passing constant here."
  [model draft? x fs]
  (reduce (fn [m e] (bit-clear (long m) (long (nth e 0)))) (live-mask model draft? x) fs))

(defn- run-bits
  "Run the checks `m` names against `x`, ascending -- which is declaration order -- reading
  io-dep values from `cache`, and return `acc` with `[i ret]` added for each that did not
  return `true`. `acc` starts as `nil`, and the vector is allocated at the first failure.

  A check whose `:shape-mask` `live` does not cover does not run, records nothing, and is not
  among the failures, so the shape validations must have run before the checks waiting on
  them. Where `draft?`, each result is written into `x`'s verdict mask; a sealed value has no
  mask. A check that throws records nothing, and the throw propagates."
  [model draft? x cache live m acc]
  (let [^longs gates (:shape-masks model)
        run          (if draft? (:run-draft model) (:run-sealed model))]
    (loop [m (long m), acc acc]
      (if (zero? m)
        acc
        (let [i  (Long/numberOfTrailingZeros m)
              g  (aget gates i)
              m' (bit-and m (dec m))]
          (if (and (not (zero? g)) (not= g (bit-and (long live) g)))
            (recur m' acc)
            (let [ret  (run i x cache)
                  pass (true? ret)]
              (when draft? (record-verdict! x i pass))
              (recur m' (if pass acc (conj (or acc []) [i ret]))))))))))

(defn- run-pure
  "Run the shape validations `plan` names against `x`, then the other pure checks it names,
  and return `[i ret]` for each that did not return `true` -- shape validations first,
  declaration order within each -- or `nil` where none did. Where `draft?`, each result is
  written into `x`'s verdict mask.

  The second phase reads its prerequisites off what this pass has established, so a check
  whose shape validations did not all pass neither runs nor reports."
  [model draft? x plan]
  ;; A shape validation waits on nothing, so the first phase is handed a `live` of 0 and
  ;; never reads it.
  (let [f1 (run-bits model draft? x nil 0 (bit-and (long plan) (long (:shape-bits model))) nil)
        p  (bit-and (long plan) (long (:pure-bits model)))]
    (if (zero? p)
      f1
      (run-bits model draft? x nil (established-mask model draft? x f1) p f1))))

(defn- runnable
  "Those of the checks `m` names that have everything they wait on: every check in one's
  closure is passing on `live`. The rest do not run, and the failure of whatever in the
  closure is not passing is what reports them."
  [model live m]
  (let [^longs closures (:closures model)]
    (loop [m (long m), acc 0]
      (if (zero? m)
        acc
        (let [i (Long/numberOfTrailingZeros m)
              c (aget closures i)]
          (recur (bit-and m (dec m))
                 (if (= c (bit-and (long live) c)) (bit-set acc i) acc)))))))

(defn- run-io
  "Run the external validations `m` names against `x`, reading what their io-deps returned
  out of `cache`, and add what they failed to `acc`, the pure phase's failures."
  [model draft? x live m cache acc]
  (run-bits model draft? x cache live m acc))

(defn- establish-one
  "Run the checks `plan` names against `x` and return
  `{:failures [[i ret] ...] :cache {io-dep-name value} :plan plan}`, the plan being what a
  report reads to say which checks a failure stopped. Where `draft?`, each result is written
  into `x`'s verdict mask.

  The external validations that run are those the pure phase left with everything they wait
  on passing, and only what those name is fetched; the rest neither fetch nor run."
  [model draft? x plan]
  (if (zero? (long plan))
    established
    (let [iob (bit-and (long plan) (long (:io-bits model)))
          fs  (run-pure model draft? x plan)]
      (if (zero? iob)
        (if (seq fs) {:failures fs :cache {} :plan plan} established)
        (let [live (established-mask model draft? x fs)
              io   (runnable model live iob)]
          (if (zero? (long io))
            (if (seq fs) {:failures fs :cache {} :plan plan} established)
            (let [idxs  (mask-indices io)
                  cache (fetch-io model x idxs)]
              {:failures (run-io model draft? x live io cache fs)
               :cache    cache
               :plan     plan})))))))

(defn- run-checks!
  "Run the checks `m` names against the sealed value `x` and throw one `:check-failed` with
  every failure: shape validations first, then the other pure checks, then external
  validations. A check is neither run nor reported where something it waits on failed in
  this pass, and only what the external validations that do run name is fetched.

  A check `m` does not name counts as passing, so `x` must be a value every check outside
  `m` has already passed on."
  [model x m]
  (let [iob (bit-and (long m) (long (:io-bits model)))
        fs  (run-pure model false x m)]
    (if (zero? iob)
      (when (seq fs) (checks-failed! model (failure-entries model x fs {} m)))
      (let [live (established-mask model false x fs)
            io   (runnable model live iob)]
        (if (zero? (long io))
          (when (seq fs) (checks-failed! model (failure-entries model x fs {} m)))
          (let [idxs  (mask-indices io)
                cache (fetch-io model x idxs)
                fs    (run-io model false x live io cache fs)]
            (when (seq fs) (checks-failed! model (failure-entries model x fs cache m)))))))))

(defn- establish-all
  "[[establish-one]] against each value in `xs`, with `req` asked of every one of them, and
  one result per value index-aligned with `xs`.

  Every plan is computed before anything runs, and the io-deps of the values that reach the
  fetch phase are fetched together, one pass per io-dep over the whole group, so a batched
  fetch sees every argument tuple in one call."
  [model xs req force?]
  (let [xs        (vec xs)
        n         (count xs)
        requested (:mask req)
        closure   (:closure req)
        drafts    (boolean-array n)
        plans     (long-array n)
        ;; Every plan first, off masks none of the running has touched yet, so one draft
        ;; appearing twice in the group is asked the same question twice.
        pending?  (loop [i 0, any? false]
                    (if (= i n)
                      any?
                      (let [x  (nth xs i)
                            dr (some? (draft-model x))
                            p  (long (plan-mask model (live-mask model dr x)
                                                requested closure force?))]
                        (aset drafts i dr)
                        (aset plans i p)
                        (recur (inc i) (or any? (not (zero? p)))))))]
    (if-not pending?
      (vec (repeat n established))
      (let [iob   (long (:io-bits model))
            fss   (object-array n)
            ios   (long-array n)
            lives (long-array n)
            ;; Then every value's pure checks, in order. A value owes a fetch only for the
            ;; external validations its own pure phase left with everything they wait on
            ;; passing.
            fetching?
            (loop [i 0, any? false]
              (if (= i n)
                any?
                (let [p  (aget plans i)
                      dr (aget drafts i)
                      fs (when-not (zero? p) (run-pure model dr (nth xs i) p))
                      lv (if (zero? (bit-and p iob))
                           0
                           (long (established-mask model dr (nth xs i) fs)))
                      io (if (zero? (bit-and p iob))
                           0
                           (long (runnable model lv (bit-and p iob))))]
                  (aset fss i fs)
                  (aset lives i lv)
                  (aset ios i (long io))
                  (recur (inc i) (or any? (not (zero? (long io))))))))
            ;; The fetches are the one thing the group does together rather than per value.
            caches (when fetching?
                     (fetch-io-all model xs (mapv mask-indices (vec ios))))]
        (loop [i 0, acc (transient [])]
          (if (= i n)
            (persistent! acc)
            (let [fs (aget fss i)
                  io (aget ios i)]
              (recur (inc i)
                     (conj! acc
                            (cond
                              (not (zero? io))
                              (let [cache (nth caches i)]
                                {:failures (run-io model (aget drafts i) (nth xs i)
                                                   (aget lives i) io cache fs)
                                 :cache    cache
                                 :plan     (aget plans i)})

                              (seq fs) {:failures fs :cache {} :plan (aget plans i)}
                              :else    established))))))))))

(defn- establish-group
  "[[establish-one]] against one value, with the plan computed from its own record of which
  checks pass. `req` is what a name asked for: [[request-for]]'s answer."
  [model x req force?]
  (let [draft? (some? (draft-model x))]
    (establish-one model draft? x
                   (plan-mask model (live-mask model draft? x)
                              (:mask req) (:closure req) force?))))

(defn- run-draft-checks!
  "Run the checks `plan` names against the draft `d` as [[establish-one]] does, and throw one
  `:check-failed` with every failure."
  [model d plan]
  (let [{:keys [failures cache]} (establish-one model true d plan)]
    (when (seq failures)
      (checks-failed! model (failure-entries model d failures cache plan)))))


(def every-check
  "The certificate covering every check a type declares. It exists on every type without
  being declared, and is the one certificate name a type may not declare for itself."
  ::every-check)

(defn- request-for
  "What `cname` asks for on this type: `{:mask :closure}`, the checks it names as bits of a
  draft's verdict mask, and, transitively, everything those wait on. `cname` is a check's
  name or a certificate's; a lone check is a certificate containing itself.

  Throws `:unknown-check` where it is neither, carrying `:record`, `:check`, and the check
  and certificate names it could have been given."
  [model cname]
  (or (get (:requests model) cname)
      (throw (ex-info (format (str "%s declares no check or certificate named %s -- its "
                                   "checks are %s and its certificates are %s")
                              (:name model) (pr-str cname)
                              (pr-str (mapv :name (:checks model)))
                              (pr-str (vec (keys (:certificates model)))))
                      {:error ::unknown-check :record (:name model) :check cname
                       :checks (mapv :name (:checks model))
                       :certificates (vec (keys (:certificates model)))}))))

(defn certificates
  "The set of certificate names `x` currently holds: those every check of which is currently
  passing on `x`. A sealed value holds every certificate its type declares, the built-in
  [[every-check]] among them. The metadata key `::certificates` carries the same answer for
  the same value.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`."
  [x]
  (let [model (value-model! "certificates" x)]
    (held-in model (value-mask model x))))

(defn- merged-meta
  "`mta` with the library's own keys, `lib`, merged over the top, so a key a caller set
  cannot override one the library writes. `lib` exactly, allocating nothing, where `mta` is
  `nil`."
  [mta lib]
  (if mta (conj mta lib) lib))

(defn- draft-meta
  "What a draft answers [[clojure.core/meta]] with: its own metadata `mta`, with `::passed`
  and `::certificates` read off `mask` merged over the top. A sealed value answers the same
  two keys from a constant of its type."
  [model mask mta]
  (merged-meta mta (array-map ::passed       (passed-in model mask)
                              ::certificates (held-in model mask))))

(defn- answered
  "`true`, or the report for every check that did not pass."
  [model x req force?]
  (let [{:keys [failures cache plan]} (establish-group model x req force?)]
    (if (seq failures)
      (failure-report model x failures cache plan)
      true)))

(defn- insisted-one
  "`true`, or one `:check-failed` carrying the report for every check that did not pass."
  [model x req force?]
  (let [{:keys [failures cache plan]} (establish-group model x req force?)]
    (if (seq failures)
      (checks-failed! model (failure-entries model x failures cache plan))
      true)))

(defn check
  "Run what `cname` names against `x`: `true` if everything named passes, and otherwise a
  report of those that did not, keyed by check name in the type's declaration order -- shape
  validations first, then invariant validations, then external validations.

  Each entry holds `:doc`, the check's own docstring; `:values`, what it was given, keyed by
  the dependency it was given for; `:returned`, what it returned; and, where its failure kept
  other checks from running, `:stopped`, their names in declaration order. An entry that
  stopped nothing has no `:stopped`, and a check two failures stopped is named under both.
  [[check!]] throws this same report.

  `cname` is a check's name or a certificate's; a lone check is a certificate containing
  itself, and [[every-check]] is the certificate covering the whole type.

  Every check named runs; nothing stops at the first failure. A check does not run while one
  it waits on is failing -- a shape validation of a field it names, or, for an external
  validation, a pure check over a field its io-deps name -- so it has no entry of its own,
  and the `:stopped` of the check that failed is what names it. Nothing is fetched for an
  external validation that will not run, and a failure nothing was waiting on holds up no
  fetch. A check the certificate does not name can appear in the report, since what a named
  check waits on runs first.

  A check `x` already records as passing does not run again, and no io-dep behind it is
  fetched. A sealed value passes every check, so `true` is returned without anything running.
  [[recheck]] runs what `cname` names whether or not it is recorded as passing.

  No draft is returned: each result is written into `x`'s record of which checks pass, which
  [[passed]] and [[certificates]] read. A failure is not recorded, so a later call runs the
  failing check again, fetch included.

  Throws `:not-a-value` when `x` is neither a draft nor a sealed value, carrying `:value`;
  `:unknown-check` when the type declares no check or certificate of that name, carrying
  `:record`, `:check` and the names it could have been given; and `:malformed-batch` or
  `:incomplete-batch` when a batched fetch behind an external validation answers something
  other than a map keyed by every tuple it was given, each carrying `:record`, `:io-dep` and
  what the fetch returned."
  [x cname]
  (let [model (value-model! "check" x)]
    (answered model x (request-for model cname) false)))

(defn check!
  "[[check]], throwing on failure: `true`, or one `:check-failed` carrying under `:failures`
  the report [[check]] answers with.

  Throws what [[check]] throws -- `:not-a-value`, `:unknown-check`, `:malformed-batch` and
  `:incomplete-batch`, in the same conditions -- and `:check-failed` where a check named did
  not pass, carrying `:record` and `:failures`."
  [x cname]
  (let [model (value-model! "check!" x)]
    (insisted-one model x (request-for model cname) false)))

(defn recheck
  "[[check]], with every check `cname` names run whether or not `x` already records it as
  passing. Same argument, same answer shape. An external validation is true as of when it
  ran; this is how to run it again.

  On a draft each result is written back: a check that passed before and fails now is no
  longer recorded as passing, so the next [[seal]] runs it again; one that failed before and
  passes now is recorded as passing. A sealed value has no record to write and cannot change,
  so the answer reports the current result and [[passed]] still names every check.

  What a named check waits on is not re-run where `x` already records it as passing; what is
  not recorded as passing runs first. Either way a check does not run when something it waits
  on failed in this pass, a sealed value included, and the failing check's `:stopped` names
  it. An io-dep is fetched at most once within this one call, and what it returned is not
  kept after the call returns.

  Throws what [[check]] throws and in the same conditions: `:not-a-value`, `:unknown-check`,
  `:malformed-batch` and `:incomplete-batch`."
  [x cname]
  (let [model (value-model! "recheck" x)]
    (answered model x (request-for model cname) true)))

(defn recheck!
  "[[recheck]], throwing on failure: `true`, or one `:check-failed` carrying the same report
  [[check!]] carries.

  Throws what [[recheck]] throws -- `:not-a-value`, `:unknown-check`, `:malformed-batch` and
  `:incomplete-batch` -- and `:check-failed` where a check named did not pass, carrying
  `:record` and `:failures`, exactly as [[check!]] does."
  [x cname]
  (let [model (value-model! "recheck!" x)]
    (insisted-one model x (request-for model cname) true)))

;;; --------------------------- Checking many values at once ---------------------------

(defn- group-models!
  "The model for each value in `xs`, index-aligned, or a throw of `:not-a-value` naming `what`
  and the position of the first thing that is neither a draft nor a sealed value, carrying
  `:index` and `:value`."
  [what xs]
  (let [n (count xs)]
    (loop [i 0, acc (transient [])]
      (if (= i n)
        (persistent! acc)
        (let [x (nth xs i)]
          (recur (inc i)
                 (conj! acc
                        (or (value-model x)
                            (throw (ex-info (format (str "%s expects drafts or sealed values, "
                                                         "and the value at index %d is "
                                                         "neither: %s")
                                                    what i (pr-str x))
                                            {:error ::not-a-value :index i :value x}))))))))))

(defn- partition-by-model
  "`[[model [index ...]] ...]` -- the positions of `xs` grouped by the type they belong to,
  types in the order they first appear and positions ascending within each. A fetch belongs
  to one type, so a mixed group is one pass per type. Models are compared by identity."
  [models]
  (reduce (fn [acc [n m]]
            (let [i (first (keep-indexed (fn [k [m2]] (when (identical? m m2) k)) acc))]
              (if i
                (update-in acc [i 1] conj n)
                (conj acc [m [n]]))))
          []
          (map-indexed vector models)))

(defn- group-run
  "Run what `cname` names against every value in `xs` as [[establish-all]] does, and return
  `[models results]`, both index-aligned with `xs`, or `nil` for an empty `xs`, before any
  name is resolved.

  `cname` is resolved against every type in the group before anything runs, so a type that
  does not declare it fails the whole call before any value is run."
  [what xs cname force?]
  (let [xs (vec xs)
        n  (count xs)]
    (when (pos? n)
      (let [models (group-models! what xs)
            m0     (nth models 0)]
        ;; One type is the ordinary case and needs no partition: the values are already in
        ;; the order the answer is wanted in.
        (if (loop [i 1] (cond (= i n)                             true
                              (identical? m0 (nth models i))      (recur (inc i))
                              :else                               false))
          [models (establish-all m0 xs (request-for m0 cname) force?)]
          (let [groups (mapv (fn [[m is]] [m is (request-for m cname)])
                             (partition-by-model models))
                out    (object-array n)]
            (doseq [[m is req] groups]
              (let [vs (mapv #(nth xs %) is)
                    rs (establish-all m vs req force?)]
                (dotimes [k (count is)] (aset out (nth is k) (nth rs k)))))
            [models (vec out)]))))))

(defn- answered-all
  "One answer per value, index-aligned with the group: `true`, or the report for every check
  that did not pass."
  [models xs rs]
  (let [n (count rs)]
    (loop [i 0, acc (transient [])]
      (if (= i n)
        (persistent! acc)
        (let [{:keys [failures cache plan]} (nth rs i)]
          (recur (inc i)
                 (conj! acc (if (seq failures)
                              (failure-report (nth models i) (nth xs i) failures cache plan)
                              true))))))))

(defn- insisted
  "`true`, or one throw carrying every failure of every value in `rs`."
  [models xs rs]
  (let [fs (into [] (keep-indexed (fn [n {:keys [failures cache plan]}]
                                    (when (seq failures)
                                      [n (failure-entries (nth models n) (nth xs n)
                                                          failures cache plan)])))
                 rs)]
    (if (seq fs) (bulk-failed! models fs) true)))

(defn check-all
  "Run what `cname` names against every value in `xs`, and return a vector index-aligned
  with `xs`: `true` where everything named passed, and otherwise the report [[check]] answers
  with. Results are index-aligned rather than keyed by the value, since two drafts holding
  identical data are `=`.

  An io-dep behind a check is fetched once per distinct argument tuple across the whole
  group, and a batched one is called once with every tuple.

  The group may hold values of more than one type, and sealed values alongside drafts.
  Fetches are declared per type, so a mixed group is one fetch pass per type, batched within
  each; `cname` resolves per type, and a type that does not declare it fails the whole call
  before anything runs.

  Per value, this is [[check]]: a check the value already records as passing does not run, a
  check waits on the same prerequisites, and a value with a failing prerequisite fetches
  nothing while the rest of the group still does. A sealed value passes every check and
  answers `true`. Each draft's record of which checks pass is written with its results, and
  no sealed value is produced.

  Throws `:not-a-value` when something in `xs` is neither a draft nor a sealed value,
  carrying `:index` and `:value`; `:unknown-check` when one of the types in the group
  declares no check or certificate of that name, carrying `:record`, `:check` and the names
  it could have been given; and `:malformed-batch` or `:incomplete-batch` when a batched
  fetch answers something other than a map keyed by every tuple it was given, each carrying
  `:record`, `:io-dep` and what the fetch returned."
  [xs cname]
  (if-let [[models rs] (group-run "check-all" xs cname false)]
    (answered-all models (vec xs) rs)
    []))

(defn check-all!
  "[[check-all]], throwing on failure: `true`, or one `:check-failed` whose `:failures` is
  keyed by the value's index and holds that value's report under it.

  Every value is run before anything is thrown, and each draft's record of which checks pass
  is written with its results. Where the group held more than one type, `:record` is a
  vector of all of their names.

  Throws what [[check-all]] throws -- `:not-a-value`, `:unknown-check`, `:malformed-batch`
  and `:incomplete-batch` -- and `:check-failed` where a check did not pass for any value,
  carrying `:record`, `:bulk`, `:indexes`, and `:failures` keyed by the value's index and,
  under that, by check name."
  [xs cname]
  (if-let [[models rs] (group-run "check-all!" xs cname false)]
    (insisted models (vec xs) rs)
    true))

(defn recheck-all
  "[[check-all]], with every check `cname` names run for every value whether or not the value
  already records it as passing, so re-verifying a batch against a database is one call and
  one fetch pass per type. Each draft's record is written with its results, so a check that
  no longer holds is cleared for exactly the values it no longer holds for.

  Throws what [[check-all]] throws, and in the same conditions: `:not-a-value`,
  `:unknown-check`, `:malformed-batch` and `:incomplete-batch`."
  [xs cname]
  (if-let [[models rs] (group-run "recheck-all" xs cname true)]
    (answered-all models (vec xs) rs)
    []))

(defn recheck-all!
  "[[recheck-all]], throwing on failure: `true`, or one `:check-failed` carrying what
  [[check-all!]] carries.

  Throws what [[check-all!]] throws: `:not-a-value`, `:unknown-check`, `:malformed-batch`,
  `:incomplete-batch`, and `:check-failed` carrying `:record`, `:bulk`, `:indexes` and
  `:failures`."
  [xs cname]
  (if-let [[models rs] (group-run "recheck-all!" xs cname true)]
    (insisted models (vec xs) rs)
    true))

(defn- model-of
  "The model for `x`, whether `x` is a sealed value or the type itself."
  [x]
  ;; The registry is keyed by class, so only a class can be found under `x` itself. Asking
  ;; for a value would hash it, and a value of this library's is a record over whatever its
  ;; fields hold.
  (let [r @registry] (or (r (class x)) (when (class? x) (r x)))))

(defn sealed?
  "Is `x` a sealed value of any type this library defines?"
  [x] (some? (@registry (class x))))

(defn of-type?
  "Is `x` a value of type `t` -- a draft of it or a sealed one?

  `defrecord` generates two classes, so `instance?` holds of a sealed `Heist` and not of a
  draft of that same `Heist`. [[draft?]] and [[sealed?]] test the form, of any type;
  `(and (draft? x) (of-type? Heist x))` is a draft of `Heist`.

  False for anything that is not a value of a registered type, `t` itself included, and
  false rather than a throw where `t` is not a registered type."
  [t x]
  (if-let [model (or (@registry t) (@draft-registry t))]
    (let [c (class x)]
      (or (identical? model (@registry c))
          (identical? model (@draft-registry c))))
    false))

(defn- owner-of [d] ((:draft-owner (draft-model d)) d))

(defn- adopt
  "`d` with `token` as its owner, so a nested `drafting` passes it through unsealed."
  ([d token] (adopt (draft-model d) d token))
  ([model d token] ((:draft-with-owner model) d token)))

(defn- open
  "The draft a `drafting` binds, owned by `token`: `x` adopted where `already-draft?`, an
  empty draft where `x` is the type, and otherwise a draft of the sealed value `x`."
  [model x token already-draft?]
  (cond
    already-draft? (adopt model x token)
    (class? x)     ((:draft-empty model) token)
    :else          ((:draft-from model) x token)))

(defn draft
  "A draft of `x`: `x` itself where it is a draft, an empty draft where `x` is a registered
  type, and otherwise a draft holding the fields of the sealed value `x`. A draft created
  inside a [[drafting]] belongs to that form, so a nested `drafting` passes it through
  unsealed.

  Throws `:cannot-draft` when `x` is none of those three, carrying `:value`."
  [x]
  (if (draft? x)
    x
    (if-let [model (model-of x)]
      (if (class? x)
        ((:draft-empty model) *owner*)
        ((:draft-from model) x *owner*))
      (throw (ex-info (format (str "draft expects a sealed value, a draft, or a registered "
                                   "type, and %s is none of them")
                              (pr-str x))
                      {:error ::cannot-draft :value x})))))

(defn- extraneous
  "The keys `d` holds that its type does not declare; `model` is `d`'s."
  [model d]
  (seq (keys ((:draft-ext model) d))))

(defn clean
  "`d` without the keys its type does not declare, which [[seal]] would otherwise refuse.
  Returns a draft.

  Throws `:not-a-draft` when `d` is not a draft, carrying `:value`."
  [d]
  (if-let [model (draft-model d)]
    (if (extraneous model d) ((:draft-clean model) d) d)
    (throw (ex-info (format "clean expects a draft, and %s is not one" (pr-str d))
                    {:error ::not-a-draft :value d}))))

;;; ------------------------------ Opting out of the seal ------------------------------

(deftype Unsealed [wrapped])

;; `deftype` interns a positional factory; [[unsealed]] is the constructor that asserts what
;; it wraps is a draft.
(alter-meta! #'->Unsealed assoc :private true)

(defn unsealed
  "Mark the draft `d` as a return value a protocol method is not to seal, as `reduced` marks
  a value that ends a reduction.

  A drafting method installed on the sealed type seals a draft of its type that it returns
  (see [[defrecord]]); one returning `(unsealed d)` returns `d` itself, owned by no
  `drafting`, so a later one adopts it. On the draft type the marker is unwrapped and nothing
  else changes, so a body means the same thing on either type. [[drafting]] and [[seal]]
  unwrap the marker and seal the draft inside it.

  An unmarked method on the sealed type is handed the sealed value, so a write in its body
  produces a sealed value and `unsealed` of that throws.

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
  ;; Running only the checks the mask does not already say are passing is equivalent to
  ;; running all of them: a bit is set only by a check that returned `true` against exactly
  ;; this data, since a draft of a sealed value starts with every bit set and a write clears
  ;; the bits of every check reading the field it wrote.
  ;;
  ;; An undeclared key is refused before any check runs, so a draft that will be refused
  ;; anyway fetches nothing, and a draft with nothing left to run is refused too.
  (let [;; Every check on the type that the draft's record does not already say is passing.
        ;; What those wait on is among them already, so no closure is read.
        pending (bit-and-not (long (:all-pass model)) (long (verdict-mask d)))]
    (when-let [extra (extraneous model d)]
      (throw (ex-info (format "%s does not declare %s -- clean removes undeclared keys from a draft"
                              (:name model) (pr-str (vec extra)))
                      {:error ::extraneous-keys :record (:name model) :keys (vec extra)})))
    (when-not (zero? pending)
      (run-draft-checks! model d pending))
    ((:draft-seal model) d)))

(defn seal
  "Validate `d` and return the sealed value, which satisfies every check its type declares.

  A check `d` already records as passing does not run again, so work done on the draft with
  [[check]] is not repeated, and a draft of a sealed value runs only the checks reading the
  fields written since. A failed check is not recorded, so it runs again at the next seal.
  Every failure is collected and thrown once.

  Idempotent: an already-sealed value is returned as it is, so a `drafting` body may end in a
  call that has already sealed. An [[unsealed]] marker is unwrapped and the draft inside it
  sealed.

  Throws `:cannot-seal` when `d` is neither a draft nor an already-sealed value, carrying
  `:value`, and `:record` where the type was known; `:extraneous-keys` when the draft holds
  a key its type does not declare, carrying `:record` and `:keys`; `:check-failed` when a
  check does not pass, carrying `:record` and `:failures`, the report [[check]] answers with;
  and `:malformed-batch` or `:incomplete-batch` when a batched fetch behind an external
  validation returns something other than a map keyed by every tuple it was given, each
  carrying `:record`, `:io-dep` and what the fetch returned."
  ([d]
   (let [d (unmark d)]
     (if-let [model (draft-model d)]
       (seal-draft model d)
       (if (sealed? d)
         d
         (throw (ex-info "seal expects a draft or an already-sealed value"
                         {:error ::cannot-seal :value d}))))))
  ([model d]
   (let [d (unmark d)]
     (if (identical? model (draft-model d))
       (seal-draft model d)
       (if (identical? model (model-of d))
         d
         (throw (ex-info (format "seal expects a draft or an already-sealed %s, and was given a %s"
                                 (:name model) (pr-str (class d)))
                         {:error ::cannot-seal :record (:name model) :value d})))))))

(defn- not-of-this-type!
  [model d]
  (throw (ex-info (format "%s: unsealed expects a draft of %s, and %s is a draft of %s"
                          (:name model) (:name model) (pr-str d) (:name (draft-model d)))
                  {:error ::not-a-draft :record (:name model) :value d})))

(defn- opted-out
  "The draft an [[unsealed]] marker holds, or a throw of `:not-a-draft` where it is a draft
  of a type other than `model`'s."
  [model x]
  (let [d (.-wrapped ^Unsealed x)]
    (when-not (identical? model (draft-model d)) (not-of-this-type! model d))
    d))

(defn- sealed-impl-return
  "What a protocol method installed on the sealed type returns, given `ret`, the body's
  value: a draft of this type is sealed; an [[unsealed]] marker is unwrapped and the draft
  inside it returned with no owner, so a later `drafting` adopts it; anything else is
  returned as it is."
  [model ret]
  (cond
    (instance? Unsealed ret)             (adopt model (opted-out model ret) nil)
    (identical? model (draft-model ret)) (seal model ret)
    :else                                ret))

(defn- draft-impl-return
  "What a protocol method installed on the draft type returns, given `ret`, the body's
  value: an [[unsealed]] marker is unwrapped, and anything else is returned as it is. The
  draft keeps its owner."
  [model ret]
  (if (instance? Unsealed ret) (opted-out model ret) ret))

(defn- recurrable-body
  "A method body wrapped in a `loop` rebinding every argument but the receiver to itself, so
  that `recur` inside it means what it means in a `defrecord` method body. No emission puts
  the body in tail position of the method, so the `loop` is the recur target instead. The
  receiver is left out because a `defrecord` method body's recur leaves it out."
  [args body]
  ;; The binding names are stripped: the compiler refuses a primitive hint in a binding position.
  `(loop [~@(interleave (map #(with-meta % nil) (rest args)) (rest args))] ~@body))

(defn- impl-as-written
  "The emission of a method whose body runs against the value it was called on: the receiver
  is bound as the author wrote it, and the return goes through [[draft-impl-return]]."
  [model-sym [mname args & body]]
  `(~mname ~args
     (~(internal 'draft-impl-return) ~model-sym ~(recurrable-body args body))))

(defn- owned-here?
  "Is `x` a draft belonging to a `drafting` form already on the stack?"
  [x]
  (and (draft? x) (some? *owner*) (= *owner* (owner-of x))))

(defmacro drafting
  "Bind `sym` to a draft of `init`, evaluate `body`, and seal the result.

  `init` may be a sealed value, a registered type (to build one from nothing), or a draft. A
  draft an enclosing `drafting` owns passes straight through unsealed, so only the outermost
  form seals, and a function taking and returning a draft can be called standalone or
  nested. Any other draft is sealed here.

  Throws `:cannot-draft` when `init` is none of those three, carrying `:value`, and then
  whatever the seal throws: `:cannot-seal` where the body's value is neither a draft of that
  type nor an already-sealed one, `:extraneous-keys` where the draft holds a key the type
  does not declare, `:check-failed` where a check does not pass, and `:malformed-batch` or
  `:incomplete-batch` where a batched fetch misbehaves. See [[seal]] for what each of them
  carries."
  [[sym init] & body]
  ;; `binding` names its var by symbol and `*owner*` is private, so the frame is pushed and
  ;; popped by hand.
  `(let [init# ~init
         dm#   (~(internal 'draft-model) init#)]
     (if (and dm# (some? ~(internal '*owner*))
              (= ~(internal '*owner*) ((:draft-owner dm#) init#)))
       (let [~sym init#] ~@body)
       (let [model# (or dm# (~(internal 'model-of) init#))
             token# (Object.)]
         (when-not model#
           (throw (ex-info (str "drafting expects a sealed value, a draft, or a registered "
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
                      (format (str "%s %s %s names %s, which is not a field of %s -- its "
                                   "fields are %s")
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
  "Resolve one arglist into the dependency keys it names, in the order it names them: a symbol
  naming a field of this type resolves to that field's key, and a keyword naming an io-dep of
  this type is already its own name. Anything else -- a destructuring form, a rest arg, a
  parameter named `_` -- is refused with the offender named.

  `ctx` is `{:record :what :name :fields :io-deps}`: the type, `:check` or `:io-dep`, the name
  of the thing being resolved, an ordered map of field symbol -> field key, and the set of
  io-dep names."
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

  The rule keeps the dependency structure two levels deep -- a shape validation depends on
  nothing, and every other check depends on shape validations -- so no order among checks has
  to be computed, and whether a check may run is one bitwise `and`."
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
  "Refuse a name declared both as a check and as an io-dep, so that an arglist entry and a
  failure report each name one thing."
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
  "What `:options` may hold; a key written anywhere else means nothing. [[option-value]]
  decides what each option's value may be.

  `:io-limited` makes direct `assoc` and `merge` on a sealed value throw for any field an
  external validation reads, so those writes go through `drafting` instead. See
  [[io-limited!]]."
  #{:io-limited})

(defn- io-limited-option
  "What `:options {:io-limited v}` declares: `v` where it is a boolean, or the boolean held by
  the var a symbol `v` names, resolved in the namespace being compiled and read on the spot.
  What is read is what the type is compiled with, so a later `alter-var-root` leaves every
  type already compiled against that var alone.

  Throws `:malformed-option` for anything else -- a symbol naming nothing, a symbol naming
  something that is not a var, a var holding something that is not a boolean, or a form that
  is neither a boolean nor a symbol -- carrying `:record`, `:option` and `:value`."
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
  "What the option `o` written as `v` declares, decided by `o` itself. `o` is known by the
  time this is called, so the `case` covers everything that can reach it."
  [tname o v]
  (case o
    :io-limited (io-limited-option tname v)))

(defn- parse-definition
  "`defrecord`'s third position -> `{:options :checks :io-deps :certificates}`, with every
  section the form left out filled in empty, so nothing downstream has to ask whether it was
  written.

  A key that names no section, a section that is not a map, a key inside `:options` that
  names no option, and a value that option refuses are each a compile error naming the
  offender: a mistyped `:cheks` would otherwise define a type with no checks at all. What an
  option holds is read here by [[option-value]], so what the sections carry onward is the
  value rather than the form it was written as.

  What a section holds is resolved further down: an arglist by [[resolve-deps]], a
  certificate by [[parse-certificates]], and the io-dep graph by [[order-io-deps!]]."
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
                              (if (qualified-keyword? k) " (a check goes under :checks)" ""))
                      {:error ::unknown-section :record tname :section k
                       :sections definition-sections}))))
  (let [sections (reduce (fn [acc s]
                           (let [v (get definition s {})]
                             (when-not (map? v)
                               (throw (ex-info (format "%s: %s takes a map, and %s is not one"
                                                       tname s (pr-str v))
                                               {:error ::malformed-section :record tname
                                                :section s :value v})))
                             (assoc acc s v)))
                         {} definition-sections)]
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
  could seal on a check that never ran."
  64)

(defn- reject-too-many-checks!
  "Refuse a type with more checks than one `long` has bits."
  [tname n]
  (when (> n mask-width)
    (throw (ex-info (format "%s declares %d checks, and the limit is %d" tname n mask-width)
                    {:error ::too-many-checks :record tname :count n :limit mask-width}))))

(defn- parse-certificates
  "The `:certificates` section -> certificate name -> `{:checks :ids :mask}`, with the
  built-in [[every-check]] appended. `check-names` is the type's checks in declaration order,
  which is the order a certificate is read back in whatever order it was written. `:mask` is
  the certificate's checks as bits of a draft's verdict mask, so holding one is a single
  `and` at run time.

  A certificate is the set of check names it covers, however that collection was written, so
  `[::a ::a ::b]` compiles to exactly what `#{::a ::b}` compiles to. A name that is not a
  check of this type, a certificate covering nothing, and a certificate spelled like a check
  or like the built-in are each a compile error naming the offender."
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
                  (bad! cname "covers no check -- a certificate must name at least one"))
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
        (throw (ex-info (format (str "%s declares a certificate named %s, which is the name of "
                                     "the built-in certificate covering every check")
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
  Anything that is neither is left exactly as written, so that [[resolve-deps]] refuses it as
  its author spelled it."
  [args]
  (mapv (fn [e] (if (keyword? e) (symbol (name e)) e)) args))

(defn- reject-duplicate-bindings!
  "Refuse two entries of one arglist that bind the same name. A repeated parameter is legal
  Clojure, so the body would silently read the second where it named the first."
  [tname what dname params]
  (when-let [dup (first (for [[p n] (frequencies params) :when (> n 1)] p))]
    (throw (ex-info (format "%s %s %s binds %s twice -- an arglist names each dependency once"
                            tname (name what) (pr-str dname) (pr-str dup))
                    {:error ::duplicate-dependency :record tname what dname
                     :dependency dup}))))

(defn- reaching-for-a-batch?
  "Does this arglist look like the batched form -- a leading tuple, or an `:as` anywhere in
  it? [[batched-shape]] settles whether it is exactly that form, so one that looks batched and
  is not is refused as a malformed batch rather than as an entry resolving to nothing."
  [args]
  (boolean (or (vector? (first args)) (some #{:as} args))))

(defn- batched-shape
  "`[[crew] :as rows]` -> `[[crew] rows]`: the tuple, which is the dependency declaration, and
  the name the collection of tuples is bound to. `nil` where the arglist is not exactly that.

  The `:as` is required because `[[crew]]` alone is a legal arglist -- a singular fetch over a
  destructured argument. `:as` is unqualified and io-dep names are namespaced, so it cannot
  be mistaken for a dependency."
  [args]
  (let [[inner as sym] args]
    (when (and (= 3 (count args)) (vector? inner) (= :as as) (simple-symbol? sym))
      [inner sym])))

(defn- batched-check!
  "Refuse a check written in the batched form: a check judges one value."
  [tname dname args]
  (throw (ex-info (format (str "%s check %s is written in the batched form -- only an io-dep "
                               "may be written [[crew] :as rows], and a check runs against "
                               "one value")
                          tname (pr-str dname))
                  {:error ::batched-check :record tname :check dname :args args})))

(defn- parse-dep
  "One entry of the `:checks` or `:io-deps` map -> `{:name :doc :args :fn}`. The two are
  written the same way, so they are read the same way; `what` is `:check` or `:io-dep`, and
  is the kind a refusal names.

  Three forms, told apart by what the value starts with: a string is a docstring, a vector is
  an arglist, and anything else is the shorthand -- a bare function over the single field the
  key names."
  [tname what dname form]
  (let [label (name what)
        bad!  (fn [why]
                (throw (ex-info (format "%s %s %s %s" tname label (pr-str dname) why)
                                {:error (if (= what :check) ::malformed-check ::malformed-io-dep)
                                 :record tname what dname :form form})))
        made  (fn [doc args body]
                (cond
                  ;; An arglist that merely destructures is left alone, and refused where
                  ;; every other unresolvable entry is.
                  (and (= what :check) (vector? (first args)) (some #{:as} args))
                  (batched-check! tname dname args)

                  (and (= what :io-dep) (reaching-for-a-batch? args))
                  (let [[inner as]
                        (or (batched-shape args)
                            (bad! (str "is neither a plain arglist nor the batched form -- a "
                                       "batched fetch is written [[crew] :as rows]: one tuple "
                                       "of dependencies, then :as, then a name for the "
                                       "collection")))]
                    (reject-duplicate-bindings! tname what dname (binding-syms inner))
                    ;; `:args` is the tuple, so everything downstream resolves a batched
                    ;; arglist as it resolves a singular one. The body sees only the
                    ;; collection: there are many tuples, so there is nothing for the names
                    ;; inside one to be bound to.
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
      (bad! "gives more than one arity -- a check or io-dep takes exactly one arglist")

      :else
      {:name dname :doc nil :args [(symbol (name dname))] :fn form})))

(defn- reject-field-collisions!
  "Refuse an io-dep spelled like a field key. Both are keywords and both end up in the same
  resolved dependency list, so one name for the two would make an arglist entry mean two
  things and a failure report collapse them into one."
  [tname ks io-names]
  (when-let [dup (some (set ks) io-names)]
    (throw (ex-info (format "%s declares %s as both a field and an io-dep" tname (pr-str dup))
                    {:error ::duplicate-name :record tname :name dup}))))

(defn- order-io-deps!
  "The io-deps in fetch order: everything an io-dep names comes before it, so ascending index
  is an order the fetches can run in, whatever order the type wrote them in.

  A cycle is a fetch waiting on itself, and is a compile error naming the ring in the order
  it closes."
  [tname deps io-names]
  (let [by-name (into {} (map (juxt :name identity)) deps)
        order   (volatile! [])
        marks   (volatile! {})
        visit   (fn visit [nm path]
                  (case (get @marks nm)
                    :done nil
                    :open (let [ring (conj (vec (drop-while #(not= nm %) path)) nm)]
                            (throw (ex-info (format (str "%s io-deps form a cycle: %s -- a fetch "
                                                         "cannot wait on itself")
                                                    tname
                                                    (apply str (interpose " -> "
                                                                          (map pr-str ring))))
                                            {:error ::io-dep-cycle :record tname :cycle ring})))
                    (do (vswap! marks assoc nm :open)
                        (doseq [d (:deps (by-name nm))]
                          (when (contains? io-names d) (visit d (conj path nm))))
                        (vswap! marks assoc nm :done)
                        (vswap! order conj (by-name nm)))))]
    (doseq [d deps] (visit (:name d) []))
    @order))

(defn- refuse-reserved-methods!
  "Throw where any of `impl-forms` is a method the generated types already carry: one whose
  name and parameter count are a pair in `reserved`, or one whose name is in `names`, which
  no method may take at any arity."
  [tname reserved names impl-forms]
  ;; `deftype+` merges a body into the template by method name and parameter type hints, so a
  ;; method whose name and parameter count match one already there replaces it and the type
  ;; still compiles, while another parameter count is added beside it. The names in `names`
  ;; are the exception: `def-record-type` renames a body's method of one of those into the
  ;; map operation it stands for whatever arity it was written at, and an arity that
  ;; operation does not have belongs to no interface the type implements, so `deftype`
  ;; refuses it with a message that does not name the cause.
  (doseq [[kind form] impl-forms
          :when (= kind :method)
          :let  [mname    (first form)
                 arity    (count (second form))
                 by-name? (contains? names mname)]
          :when (or by-name? (contains? reserved [mname arity]))]
    (throw (ex-info (if by-name?
                      (format (str "%s implements %s, which is one of the methods that make "
                                   "a %s behave as a map -- a method of that name replaces "
                                   "it whatever parameters it takes")
                              tname mname tname)
                      (format (str "%s implements %s with %d parameters, which is one of the "
                                   "methods that make a %s behave as a map or a record -- a "
                                   "method of that name and parameter count replaces it "
                                   "rather than adding to it. The same name with a different "
                                   "number of parameters is a method of its own.")
                              tname mname arity tname))
                    {:error ::reserved-method :record tname :method mname :arity arity
                     :form form :reserved reserved :reserved-names names}))))

(defn- marked-drafting?
  "Is `form` marked `^:drafting`, written on the method form or on the method name?"
  [form]
  (boolean (or (:drafting (meta form)) (:drafting (meta (first form))))))

(defn- unmark-drafting
  "`form` with the `:drafting` mark gone from both places it may be written, so that nothing
  downstream of here sees it."
  [[mname & more :as form]]
  (with-meta (cons (vary-meta mname dissoc :drafting) more)
    (not-empty (dissoc (meta form) :drafting))))

(defn- parse-impls
  "`defrecord`'s trailing forms -> `[:type sym]` and `[:method form drafting?]` tuples, in
  the order they were written. `drafting?` is whether the method was marked `^:drafting`,
  and `form` is the method with the mark stripped.

  A form that is neither a protocol or interface name nor a method implementation, a method
  written before any such name, and an arglist with no receiver are each refused here with
  the offender named. A method's name is not among what this refuses; that is
  [[refuse-reserved-methods!]]'s question."
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
              (bad! form (str "has an empty arglist -- a method implementation's first "
                              "parameter is the receiver")))
            [(conj acc [:method (unmark-drafting form) (marked-drafting? form)]) named?])

          :else
          (bad! form (str "is neither a protocol or interface name nor a method "
                          "implementation"))))
      [[] false] impls))))

(defmacro defrecord
  "Define a validated record from a name, a vector of fields, a definition map of named
  sections, and trailing protocol implementations.

      (rec/defrecord Heist
        [name crew take cuts]
        {:options      {:io-limited true}
         :checks       {::name non-blank-string?}
         :io-deps      {::roster ([crew] (fetch-roster crew))}
         :certificates {::shape #{::name}}})

  Both generated types, the sealed value and its draft, are records rather than maps with a
  record's shape: `record?` holds, `=` compares the class before it compares an entry, `hash`
  is type-aware, `empty` refuses, and they serialize. See
  [[recognitum.collections/AbstractRecord]], the template they are built on.

  Each field becomes a real field of both types, so reads and writes are field access rather
  than map lookups, and a key qualified by the namespace this form is written in: a `Heist`
  defined in `my.app.heist` answers to `:my.app.heist/name`, which is `::name` there and
  `::h/name` wherever the namespace is aliased. A bare keyword is a key the type does not
  declare, so two types that happen to share a field name share nothing else.

  A declared field is present whatever it holds, so there is no value of the type without
  one: `dissoc` of a declared field is `(assoc x k nil)`, checks and all, and the field
  reads as `nil` afterwards.

  Every section is optional and an absent one is empty. A key that names no section is a
  compile error, as is every other declaration this macro cannot resolve while it runs: a
  mistyped `:cheks` would otherwise define a type with no checks.

  `:checks` maps a check name -- a namespace-qualified keyword, unique within the type, and
  what failures report under -- to a check, written in one of three forms:

      ::name       non-blank-string?                      ; a bare fn over the field it names
      ::valid-name ([name] (non-blank-string? name))      ; arglist and body
      ::cuts-sum-to-take (\"The cuts add up to the take.\"  ; with a docstring
                          [take cuts] (= take (reduce + 0 (vals cuts))))

  The arglist is the dependency declaration: a symbol names a field of this type, a keyword
  names an io-dep of this type, and the body receives exactly those, positionally, in that
  order, with nothing else in scope. Exactly one arity -- the arity is the dependency set --
  and an entry naming neither a field nor an io-dep is a compile error naming the offender.

  A check passes by returning `true` and nothing else; whatever else it returns is the
  failure data, reported under the check's name alongside its docstring and the dependencies
  it declared. An exception is not caught: a throw is a bug in the check rather than a claim
  about the data.

  A shape validation names exactly its own field, or the form does not compile. Every other
  check waits on the shape validations of the fields it names -- `::cuts-sum-to-take [take
  cuts]` runs once `::take` and `::cuts` have passed -- so it receives well-formed input, and
  one whose shape validations have not passed neither runs nor gets an entry of its own; the
  shape validation's entry names it. Failures come back in that order: the shape validations,
  then the invariant validations, then the external validations, in declaration order within
  each. A field with no shape validation has no declared shape, so the checks naming it run
  against whatever it holds and have to be total over anything.

  `:io-deps` maps an io-dep name to a fetch, written exactly as a check is, arglist and all,
  so an io-dep may name another: fetches run in dependency order, and a cycle among them is a
  compile error naming the ring. A fetch may be written singular or batched:

      ::roster ([crew] (fetch-roster crew))               ; singular
      ::roster ([[crew] :as rows] (fetch-rosters rows))   ; batched

  The inner vector of the batched form is the dependency declaration, as the singular
  arglist is, and `rows` is bound to the collection of argument tuples. A batched fetch
  returns a map keyed by tuple, `{[crew] result}`, and distinct tuples are fetched once.
  Checks are never batched; a check judges one value.

  A fetch does not decide validity. It produces a value, the checks naming it decide what
  that value means, and `nil` is neither a failure nor a success. An exception from a fetch
  propagates rather than becoming a failure; an author who wants it treated as a failure
  catches it inside the fetch.

  Pure checks run before any fetch. An io-dep is fetched once every pure check over the
  fields it names has passed and every io-dep it names has been fetched; an external
  validation runs once every io-dep it names has been fetched. An io-dep declares its own
  fields, so a malformed field no fetch reads holds up no fetch.

  Within a single [[check]], [[recheck]] or [[seal]] an io-dep is fetched at most once
  however many checks name it, and what it returned is not kept after the call returns, so a
  draft carries no IO state.

  `:certificates` maps a certificate name to the checks it covers, which [[check]] takes by
  name exactly as it takes a check's. A draft holds a certificate when every check in it is
  currently passing, and a certificate changes nothing about what sealing requires. A name it
  covers that is not a check of this type, a certificate covering nothing, or one spelled
  like a check or like the built-in [[every-check]], is a compile error naming the offender.

  `:options` holds `:io-limited`, which takes `true`, `false`, or a symbol naming a var
  holding one -- read where the form is written, so the type is compiled with what the var
  held then and a later `alter-var-root` leaves it alone. `:io-limited` makes a direct write
  to a sealed value -- `assoc`, `dissoc` or `merge` -- throw `:io-limited` for any field an
  external validation reads, directly or through an io-dep, so those writes go through
  [[drafting]]. Other fields are unaffected.

  Trailing forms are protocol implementations, written once and installed on both generated
  types. On the draft type the body runs as written, whatever the method is marked.

  A method that returns a modified receiver is a drafting method, and is marked `^:drafting`
  either on the method form or on the method name:

      ^:drafting (add-member [h who cut] ...)     ; either spelling
      (^:drafting add-member [h who cut] ...)

  On the sealed type a drafting method is wrapped: it opens a draft of the receiver, runs the
  body against that, and seals the result, so it cannot produce an invalid value whatever it
  does inside, and the receiver is a draft, which is why `(instance? Heist h)` is false in
  such a body and [[of-type?]] is the question that was meant. An unmarked method receives
  the sealed value itself and its body runs as written; a write it makes is an ordinary write
  to a sealed value, so it runs that write's checks and is refused for an `:io-limited`
  field. [[unsealed]] there is handed a sealed value and throws `:not-a-draft`.

  Either way, only a draft of this type is sealed on the way out; a number, a string, an
  already-sealed value, a draft of some other type, all pass through untouched, and
  [[unsealed]] opts a returned draft out of the seal.

  A method may not take both the name and the arity of one the generated types already carry
  -- `(seq [this])`, `(count [this])`, `(valAt [this k])` and the rest of the map and record
  interfaces -- since it would replace that one rather than add to it. The same name at
  another arity is a method of its own: `(count [this unit])` installs beside the `count`
  that counts the fields, as it does on a `defrecord`. `get`, `assoc`, `dissoc`, `keys`,
  `empty`, `with-meta` and `meta` are reserved at every arity, since a method of one of those
  names is taken as the map operation itself whatever its arity. `toString` is not reserved,
  and neither is any arity of `invoke`: the template declares no `clojure.lang.IFn`, so a
  definition may implement it.

  Both constructors this generates, `->Type` and `map->Type`, validate; there is no
  unvalidated one."
  [tname fields definition & impls]
  (let [{:keys [options checks io-deps certificates]} (parse-definition tname definition)
        impl-forms (parse-impls tname impls)
        ;; Private: the model carries `:draft-seal`, which turns a draft into a sealed value
        ;; and runs no check.
        model-sym (with-meta (symbol (str tname "-model")) {:private true})
        fsyms     (mapv #(with-meta % nil) fields)
        key-ns    (str (ns-name *ns*))
        qkey      (fn [f] (keyword key-ns (str f)))
        ks        (mapv qkey fields)
        n         (count fsyms)
        key->sym  (zipmap ks fsyms)
        ;; every local is gensymed: a field could be named k, v, o, acc ...
        [ksym vsym dsym osym hsym essym cursym entsym accsym mtasym mta2sym msym thunksym
         tgtsym isym cachesym xsym newsym tuplessym tupsym oversym]
        (map gensym ["k" "v" "d" "o" "h" "es" "cur" "ent" "acc" "mta" "mta2" "m" "thunk"
                     "target" "i" "cache" "x" "new" "tuples" "tuple" "overage"])
        tgthint   (with-meta tgtsym {:tag tname})
        dname     (symbol (str tname "Draft"))
        [extsym vdsym ownsym tokensym bitsym passym]
        (map gensym ["ext" "verdicts" "own" "token" "bit" "pass"])
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
        ;; ---- the fields ----
        ;; Ordered, so a refusal names the fields in declaration order. `array-map` applied
        ;; to the pairs stays one at any size; `into` an empty one would not.
        fieldmap  (apply array-map (interleave fsyms ks))
        ;; ---- the io-deps ----
        ;; A keyword in an arglist means an io-dep, so the names have to be known before any
        ;; arglist can be resolved -- including an io-dep's own, since one may name another.
        io-names  (set (keys io-deps))
        _         (reject-field-collisions! tname ks io-names)
        io-parsed (mapv (fn [[dep form]]
                          (let [d (parse-dep tname :io-dep dep form)]
                            (assoc d :deps (resolve-deps {:record tname :what :io-dep :name dep
                                                          :fields fieldmap :io-deps io-names}
                                                         (:args d)))))
                        (seq io-deps))
        io-parsed (order-io-deps! tname io-parsed io-names)
        nio       (count io-parsed)
        ;; A type with no batched io-dep cannot reach the batch-shape throws, so its
        ;; constructors' docstrings say nothing about them.
        batched-io? (boolean (some :batched io-parsed))
        batch-doc (if batched-io?
                    (str ";\n  and `:malformed-batch` or `:incomplete-batch` when one of this "
                         "type's batched fetches\n  answers something other than a map keyed by "
                         "every tuple it was given, each\n  carrying `:record`, `:io-dep` and "
                         "what the fetch returned")
                    "")
        io?       (fn [d] (contains? io-names d))
        batched?  (fn [j] (boolean (:batched (nth io-parsed j))))
        ;; What a fetch reads for one dependency, whichever form it was written in: a field
        ;; out of the value, an io-dep out of what this call has already fetched.
        io-arg    (fn [x cache]
                    (fn [d] (if (io? d) `(get ~cache ~d) `(get ~x ~d))))
        ;; io-dep name -> what fetching it needs: `:fields` are the fields it names itself,
        ;; whose pure checks must pass first; `:io` are the io-dep indices needed to fetch
        ;; it, itself included, in fetch order; `:field-closure` is every field it reads, its
        ;; own and those of the fetches it names. Folded forward over the ordered io-deps, so
        ;; what a dependency needs is computed by the time anything names it.
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
        ;; A check named after a field is that field's shape validation, marked here so that
        ;; nothing downstream re-derives it from the name.
        field-key? (set ks)
        parsed    (mapv (fn [c]
                          (let [sh (when (contains? field-key? (:name c)) (:name c))]
                            (when (and sh (not= [sh] (:deps c)))
                              (malformed-shape-check! tname c sh))
                            (assoc c :shape sh)))
                        parsed)
        ;; What a check depends on: the io-deps behind it in fetch order, and every field it
        ;; reads -- its own, plus whatever the fetches it names read. `:io` is empty exactly
        ;; when the check is pure.
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
        ;; The built-in certificate comes back on the end of this.
        certs     (parse-certificates tname certificates (mapv :name parsed))
        ;; What a sealed value adds to its own metadata: every check passed, so every
        ;; certificate is held. Both are facts about the type, so this is computed once and
        ;; embedded as a constant.
        sealed-meta (array-map ::passed       (set (map :name parsed))
                               ::certificates (set (keys certs)))
        cvar      (fn [i] (symbol (str tname "--check-" i)))
        ivar      (fn [j] (symbol (str tname "--io-" j)))
        deps-of   (fn [i] (:deps (nth parsed i)))
        pure?     (fn [i] (empty? (:io (nth parsed i))))
        shape?    (fn [i] (some? (:shape (nth parsed i))))
        ;; field key -> the index of that field's shape validation, where it has one.
        shape-id  (into {} (keep-indexed (fn [i c] (when (:shape c) [(:shape c) i]))) parsed)
        ;; The shape validations each check waits on. Sorted, so a phase reports in
        ;; declaration order, and as a mask, which a draft tests against its verdicts in one
        ;; `and`.
        parsed    (mapv (fn [c]
                          (let [g (if (:shape c)
                                    []
                                    (vec (sort (into #{}
                                                     (keep shape-id)
                                                     (remove io? (:deps c))))))]
                            (assoc c :shape-gate g :shape-mask (reduce bit-set 0 g))))
                        parsed)
        ;; field key -> the indices of the checks that *read* it, its own and its fetches'
        ;; alike, so a write to a field behind a fetch invalidates what depends on it.
        ;; Sorted vectors, because a sorted set does not survive being embedded in emitted
        ;; code -- the compiler rebuilds a set literal as a hash set.
        index     (reduce (fn [idx i]
                            (reduce #(update %1 %2 (fnil conj []) i) idx
                                    (:field-closure (nth parsed i))))
                          {} (range nchecks))
        ;; What must pass before a check's fetches may run: the pure checks over the fields
        ;; those fetches name.
        parsed    (mapv (fn [c]
                          (let [g (let [fs (into [] (comp (mapcat #(:fields (io-info (:name (nth io-parsed %)))))
                                                          (distinct))
                                                 (:io c))]
                                    (vec (sort (into #{} (comp (mapcat #(get index %))
                                                               (filter pure?))
                                                     fs))))]
                            (assoc c :gate g :gate-mask (reduce bit-set 0 g))))
                        parsed)
        ;; The three phases as masks, so splitting a plan into them is three `and`s.
        shape-bits (reduce (fn [m i] (if (shape? i) (bit-set m i) m)) 0 (range nchecks))
        pure-bits  (reduce (fn [m i] (if (and (pure? i) (not (shape? i))) (bit-set m i) m))
                           0 (range nchecks))
        io-bits    (reduce (fn [m i] (if (pure? i) m (bit-set m i))) 0 (range nchecks))
        ;; What each check waits on, taken to a fixed point here rather than at every call: a
        ;; prerequisite has prerequisites of its own, and an external validation reaches the
        ;; shape validations of the fields its fetches name through the pure checks over
        ;; them. There is no cycle to fall into -- a shape validation waits on nothing.
        direct    (mapv (fn [c] (bit-or (long (:shape-mask c)) (long (:gate-mask c)))) parsed)
        closure   (fn [m]
                    (loop [acc (long m)]
                      (let [acc' (reduce (fn [a i] (bit-or (long a) (long (nth direct i))))
                                         acc (mask-indices acc))]
                        (if (= acc' acc) acc (recur (long acc'))))))
        closures  (mapv closure direct)
        ;; check or certificate name -> the bits it names and what those wait on, so a call
        ;; resolves its name into a plan without walking anything.
        requests  (into {}
                        (concat (map-indexed (fn [i c]
                                               [(:name c) {:mask    (bit-set 0 i)
                                                           :closure (nth closures i)}])
                                             parsed)
                                (map (fn [[cname {:keys [ids mask]}]]
                                       [cname {:mask mask
                                               :closure (reduce (fn [a i]
                                                                  (bit-or (long a)
                                                                          (long (nth closures i))))
                                                                0 ids)}])
                                     certs)))
        pure-idxs (fn [idxs] (filterv pure? idxs))
        io-idxs   (fn [idxs] (filterv (complement pure?) idxs))
        ;; What a write to `k` leaves standing: every verdict but those of the checks reading
        ;; `k`. Computed here, so what a write costs at run time is one `and`.
        survives  (fn [k] (reduce bit-clear all-pass (get index k)))
        ;; ---- :io-limited ----
        ;; Which fields a direct write to a sealed value is refused for: exactly those an
        ;; external validation reads, computed from the same index. A field behind a fetch is
        ;; among them; a field only pure checks read, or no check at all, is not.
        limited   (if (:io-limited options)
                    (into #{} (filter (fn [k] (seq (io-idxs (get index k))))) ks)
                    #{})
        limited?  (fn [k] (contains? limited k))
        ;; ---- the protocol implementations ----
        ;; The draft type runs every body as written, marked or not, unwrapping only an
        ;; `unsealed` marker.
        draft-impls (mapv (fn [[kind form]]
                            (if (= kind :type) form (impl-as-written model-sym form)))
                          impl-forms)
        ;; The sealed type wraps a drafting method: the receiver the body sees is a draft of
        ;; the value, owned by this call, so a `drafting` inside the body passes it straight
        ;; through and only the wrapper seals. An unmarked method is emitted as the draft
        ;; type's is, so its receiver is the sealed value itself.
        sealed-impls (mapv (fn [[kind form drafting?]]
                             (cond
                               (= kind :type) form
                               (not drafting?) (impl-as-written model-sym form)
                               :else
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
        ;; Run the checks at `plan` over the expressions `argvs-of` gives for each -- locals
        ;; or direct field reads, so evaluating one twice costs nothing -- and answer with
        ;; the value `xform` builds, or throw.
        ;;
        ;; The pure checks are called first, the shape validations among them ahead of the
        ;; rest, and every other check is guarded by the shape validations of the fields it
        ;; names: one whose shape validations did not all return `true` is not called and
        ;; stands as `true`. That is sound only because the failed shape validation is itself
        ;; in this group and about to throw.
        ;;
        ;; A check not among `plan` is one that passed: this is the sealed path, so the only
        ;; verdicts that can have changed are those over a field being written.
        ;;
        ;; An external validation is guarded the same way, by whatever it waits on that is in
        ;; this group, and runs whether or not something else failed. The value has to exist
        ;; before a fetch can read fields off it, so it is built ahead of the throw; it
        ;; escapes only where nothing failed.
        ;;
        ;; Every check runs before any verdict is read, since a failure does not stop the
        ;; pass. Nothing is caught, and the value maps are built only inside the branch that
        ;; is about to throw.
        run-checked
        (fn [plan argvs-of xform]
          (let [idxs    (pure-idxs plan)
                io      (io-idxs plan)
                in      (set idxs)
                order   (into (filterv shape? idxs) (remove shape? idxs))
                rs      (into {} (map (fn [i] [i (gensym (str "r" i "-"))])) order)
                passed  (fn [is] (map (fn [i] `(true? ~(rs i))) is))
                ;; What a check waits on that this group is running; the rest passed before
                ;; this call.
                gate    (fn [is] (filterv in is))
                ;; The checks this call was going to run that wait on the one that failed: a
                ;; literal, since both the plan and what each check waits on are known here.
                stopped (fn [i]
                          (into []
                                (comp (filter #(bit-test (nth closures %) i))
                                      (map #(:name (nth parsed %))))
                                plan))
                binds   (mapcat
                         (fn [i]
                           (let [call `(~(cvar i) ~@(argvs-of i))
                                 g    (gate (:shape-gate (nth parsed i)))]
                             [(rs i)
                              (if (seq g)
                                `(if (and ~@(passed g)) ~call true)
                                call)]))
                         order)
                entries `[~@(map (fn [i]
                                   `(when-not (true? ~(rs i))
                                      (~(internal 'failure-entry)
                                       (nth (:checks ~model-sym) ~i) ~(rs i)
                                       (array-map ~@(interleave (deps-of i) (argvs-of i)))
                                       ~(stopped i))))
                                 order)]
                ;; Which external validations still have everything they wait on, decided
                ;; from the results in hand.
                live-io `[~@(map (fn [j]
                                   (let [g (gate (mask-indices (nth closures j)))]
                                     (if (seq g) `(when (and ~@(passed g)) ~j) j)))
                                 io)]
                checked (fn [v] (if (seq io)
                                  `(do (~(internal 'run-io-checks!) ~model-sym ~v ~io) ~v)
                                  v))]
            (cond
              (empty? order) (if (seq io) `(let [~newsym ~xform] ~(checked newsym)) xform)

              (empty? io)    `(do (let [~@binds]
                                    (when-not (and ~@(passed order))
                                      (~(internal 'checks-failed!) ~model-sym ~entries)))
                                  ~xform)

              :else          `(let [~@binds
                                    ~newsym ~xform]
                                (if (and ~@(passed order))
                                  ~(checked newsym)
                                  (~(internal 'checks-failed-with-io!)
                                   ~model-sym ~newsym ~entries ~live-io))))))
        ;; The two generated bodies. Built here rather than written inline in the expansion
        ;; below, so that the names they define are known before a method the caller wrote is
        ;; allowed to take one.
        sealed-body
        `[(~'get [_# ~ksym ~dsym]
            (case ~ksym ~@(interleave ks fsyms) ~dsym))

          ;; The checks naming this field are known here, so the pure ones are called with the
          ;; new value and the untouched fields directly -- no lookup, and no allocation
          ;; unless they all pass. The external validations run against the value they are
          ;; about to admit. Nothing but their own prerequisites gates them: every other
          ;; check on a sealed value passed when it was made, and only this field has moved.
          (~'assoc [_# ~ksym ~vsym]
            (case ~ksym
              ~@(mapcat (fn [j k]
                          [k (if (limited? k)
                               `(~(internal 'io-limited!) ~model-sym [~k])
                               (run-checked (get index k)
                                            (fn [i] (map (fn [d]
                                                           (if (= d k) vsym (key->sym d)))
                                                         (deps-of i)))
                                            (ctor-with j vsym)))])
                        (range n) ks)
              (~(internal 'unknown-key!) ~model-sym ~ksym)))

          ;; A declared field is always present, so dissoc of one is the write above with
          ;; `nil`, dispatched there rather than repeated here. A sealed value cannot hold an
          ;; undeclared key, so dissoc of one is the ordinary map no-op.
          (~'dissoc [this# ~ksym]
            (case ~ksym
              ~(apply list ks) (assoc this# ~ksym nil)
              this#))
          (~'keys [_#] ~ks)
          ;; The library's keys over the caller's. Here they are constant: a sealed value is
          ;; one every check passed on.
          (~'meta [_#] (~(internal 'merged-meta) ~mtasym ~sealed-meta))
          (~'with-meta [_# ~mta2sym] (new ~tname ~@fsyms ~mta2sym))

          (~'cons [_# ~osym]
            ;; The entries are walked once, into the fields themselves: this type's fields are
            ;; locals here, so an entry is a rebinding of one of them and the value is built
            ;; out of what the loop carries. What a merge allocates is the value it returns.
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
                                 (~(internal 'unknown-key!) ~model-sym ~ksym)))
                             (new ~tname ~@fsyms ~mtasym)))]
              ;; After the walk, so every key is known to be declared before this runs.
              ;; Emitted only where the option reaches a field.
              ~@(when (seq limited)
                  [`(~(internal 'io-limited-write!) ~model-sym (map first ~essym))])
              (~(internal 'run-checks!) ~model-sym ~hsym
               (~(internal 'checks-for) ~model-sym ~essym))
              ~hsym))

          ;; A keyword call site caches one of these and reads the field directly, as it does
          ;; on a record; without it `(:k x)` pays the whole Keyword -> RT -> ILookup -> valAt
          ;; walk.
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
              nil))]
        draft-body
        `[;; A declared field is present whatever it holds, so `get` never reaches its default
          ;; for one, as on a record.
          (~'get [_# ~ksym ~dsym]
            (case ~ksym
              ~@(interleave ks fsyms)
              (get ~extsym ~ksym ~dsym)))
          ;; A write seeds the new draft's verdicts from this one's, clearing exactly the
          ;; checks reading the field written. The seed is a snapshot: this draft keeps what
          ;; it had established, so two versions say nothing about each other.
          (~'assoc [_# ~ksym ~vsym]
            (case ~ksym
              ~@(mapcat (fn [i k]
                          [k (dctor (assoc fsyms i vsym) extsym
                                    `(bit-and ~vdsym ~(survives k)) ownsym mtasym)])
                        (range n) ks)
              ~(dctor fsyms `(assoc ~extsym ~ksym ~vsym) vdsym ownsym mtasym)))
          ;; A declared field is always present, so dissoc of one is the write above with
          ;; `nil`, verdicts and all; an undeclared key is removed from the map beside the
          ;; fields, where a write moved no verdict either.
          (~'dissoc [this# ~ksym]
            (case ~ksym
              ~(apply list ks) (assoc this# ~ksym nil)
              (if (contains? ~extsym ~ksym)
                ~(dctor fsyms `(dissoc ~extsym ~ksym) vdsym ownsym mtasym)
                this#)))
          ;; Every declared key, always: a draft is a record with a map beside it.
          (~'keys [_#]
            (if (seq ~extsym) (concat ~ks (keys ~extsym)) ~ks))
          ;; The same two keys a sealed value answers with, read off the mask rather than
          ;; fixed, and merged over the caller's metadata as they are there.
          (~'meta [_#] (~(internal 'draft-meta) ~model-sym ~vdsym ~mtasym))
          (~'with-meta [_# ~mta2sym] ~(dctor fsyms extsym vdsym ownsym mta2sym))
          (~'cons [this# ~osym]
            (reduce (fn [~accsym [~ksym ~vsym]] (assoc ~accsym ~ksym ~vsym))
                    this# (~(internal 'entries-of) ~model-sym ~osym)))

          ;; The only thing about a draft that changes in place, and the only two operations
          ;; that may change it.
          Verdicts
          (~'verdict-mask [_#] ~vdsym)
          (~'record-verdict! [_# ~bitsym ~passym]
            (set! ~vdsym (long (if ~passym (bit-set ~vdsym ~bitsym) (bit-clear ~vdsym ~bitsym))))
            ~passym)]
        ;; Every name and arity a method the caller wrote may not take: what the template
        ;; already answers, and what the two bodies above define.
        reserved  (into rc/template-methods
                        (comp (filter seq?) (map (fn [[nm args]] [nm (count args)])))
                        (concat sealed-body draft-body))
        _         (refuse-reserved-methods! tname reserved rc/renamed-methods impl-forms)]
    `(do
       (declare ~model-sym)
       ~@(map (fn [i] `(def ~(with-meta (cvar i) {:private true}) ~(:fn (nth parsed i))))
              (range nchecks))
       ~@(map (fn [j] `(def ~(with-meta (ivar j) {:private true}) ~(:fn (nth io-parsed j))))
              (range nio))

       (rc/def-record-type ~tname ~params
         ~@sealed-body
         ~@sealed-impls)

       ;; The draft: the same real fields, plus a map for keys the type does not declare and
       ;; a mutable bitmask of which checks currently pass. A field nobody has written holds
       ;; nil, as it would on a `defrecord` whose field was never set, so there is no "never
       ;; written" state to maintain.
       (rc/def-record-type ~dname ~(into fsyms [extsym vdfield ownsym mtasym])
         ~@draft-body
         ~@draft-impls)

       (def ~model-sym
         {:name        '~tname
          :key-ns      ~key-ns
          :options     ~options
          ;; What the option resolved to, rather than what was written. `:io-limited-keys` is
          ;; the fields a direct write to a sealed value is refused for, and is empty both
          ;; where the option was not declared and where the type reaches no IO.
          :io-limited  ~(boolean (:io-limited options))
          :io-limited-keys ~limited
          :field-order ~ks
          :field-set   ~(set ks)
          ;; `:deps` is what the check named, in arglist order, and is what a failure
          ;; reports. `:shape` is the field this check is the shape validation of, or nil,
          ;; which is what puts it in the first phase and what makes it gate anything;
          ;; `:shape-gate` and `:shape-mask` are the shape validations it waits on, as
          ;; indices and as bits of a draft's verdict mask. `:io` is the io-deps behind it in
          ;; fetch order, empty exactly when it is pure; `:gate` and `:gate-mask` the pure
          ;; checks that must pass before any of them fetches; `:field-closure` every field
          ;; it reads, its own and its fetches' alike, which is what a write invalidates it
          ;; by.
          :checks      ~(mapv (fn [i]
                                (let [{:keys [name doc deps shape shape-gate shape-mask
                                              io gate gate-mask field-closure]}
                                      (nth parsed i)]
                                  {:name name :doc doc :deps deps :pred (cvar i)
                                   :shape shape :shape-gate shape-gate :shape-mask shape-mask
                                   :io io :gate gate :gate-mask gate-mask
                                   :field-closure field-closure}))
                              (range nchecks))
          ;; The io-deps in fetch order, so that ascending index is an order they can run in,
          ;; and their names, so that a resolved dependency can be told from a field key
          ;; without asking the value. `:batched` is the only thing run time has to know
          ;; about which form was written: what it names is `:deps` either way.
          :io-deps     ~(mapv (fn [j]
                                (let [{:keys [name doc deps batched]} (nth io-parsed j)]
                                  {:name name :doc doc :deps deps :batched (boolean batched)
                                   :fields (:fields (io-info name))
                                   :fetch (ivar j)}))
                              (range nio))
          :io-names    ~io-names
          ;; check name -> its index, which is its bit in a draft's verdict mask. A check is
          ;; named from outside by name and reached from inside by index.
          :check-ids   ~(into {} (map-indexed (fn [i c] [(:name c) i])) parsed)
          ;; Every check of this type passing, as a mask: what a draft of a sealed value
          ;; starts from, and what a sealed value answers with wherever a mask is read.
          :all-pass    ~all-pass
          ;; The three phases a validation call runs in, as masks over the same bits, so
          ;; splitting a plan into them is three `and`s and each phase is walked bit by bit
          ;; in ascending order, which is declaration order.
          :shape-bits  ~shape-bits
          :pure-bits   ~pure-bits
          :io-bits     ~io-bits
          ;; Two arrays indexed by check index: the shape validations each check waits on,
          ;; which is its `:shape-mask` where an array read is cheaper than the entry in
          ;; `:checks`; and everything it waits on transitively, which is what a call unions
          ;; to find what has to run before what it was asked for.
          :shape-masks (long-array ~(mapv :shape-mask parsed))
          :closures    (long-array ~closures)
          ;; check or certificate name -> `{:mask :closure}`: the checks that name covers and
          ;; what those wait on, so resolving a name is one lookup and planning a call is
          ;; three bitwise operations.
          :requests    ~requests
          :index       ~index
          ;; field -> the bits of the checks reading it, which is `:index` in the shape a
          ;; merge can union in one `bit-or` per entry.
          :field-checks ~(into {} (map (fn [k] [k (reduce bit-set 0 (get index k))])) ks)
          ;; certificate name -> what it covers, as check names, indices, and the bits of a
          ;; draft's verdict mask they own, so holding one is an `and`. `array-map` applied
          ;; to the pairs, since a literal would promote and reorder past the eighth
          ;; certificate.
          :certificates (array-map ~@(into [] cat certs))
          :tag         '~tagsym
          ;; One runner per type, so a check reached by index is still called with direct
          ;; field access rather than a lookup per dependency. What it names that is not a
          ;; field is read out of `cache`, the fetches this validation call has made.
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
          ;; the tuple is what tells one value's fetch from another's, so it is what a result
          ;; is distributed by. A field is read through `get` rather than directly, because
          ;; one runner serves both forms of the value.
          :io-args     (fn [~isym ~xsym ~cachesym]
                         (case (int ~isym)
                           ~@(mapcat (fn [j]
                                       [j (vec (map (io-arg xsym cachesym)
                                                    (:deps (nth io-parsed j))))])
                                     (range nio))
                           nil))
          ;; The singular fetches: called with the elements of one tuple, positionally, as
          ;; the arglist they were written with says.
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
          ;; From the type: no check passing, and every field nil, which is what the shape
          ;; validations will be run against. From a sealed value: every check passing.
          :draft-empty      (fn [~ownsym]
                              ~(dctor (repeat n nil) {} 0 ownsym nil))
          ;; The metadata comes off the field rather than through `meta`, which would return
          ;; the library's keys merged in and store a snapshot of them as though the caller
          ;; had written them. What the draft inherits is what the caller set.
          :draft-from       (fn [~svsym ~ownsym]
                              ~(dctor (mapv sfield fsyms) {} all-pass ownsym (sfield mtasym)))
          :draft-owner      (fn [~dvsym] ~(dfield ownsym))
          :draft-ext        (fn [~dvsym] ~(dfield extsym))
          ;; Neither changes a field, so neither disturbs a verdict: the mask is copied
          ;; across as it stands, through the protocol, since a mutable field is private to
          ;; the class declaring it.
          :draft-with-owner (fn [~dvsym ~tokensym]
                              ~(dctor dfields (dfield extsym)
                                      `(~(internal 'verdict-mask) ~dvsym)
                                      tokensym (dfield mtasym)))
          :draft-clean      (fn [~dvsym]
                              ~(dctor dfields {} `(~(internal 'verdict-mask) ~dvsym)
                                      (dfield ownsym) (dfield mtasym)))
          :draft-seal       (fn [~dvsym] (new ~tname ~@dfields ~(dfield mtasym)))})
       ;; `deftype` interns a positional factory that checks nothing. Replace it with one
       ;; that validates, and unmap the draft's.
       ~(let [ctor-name (symbol (str "->" tname))
              ;; A function takes at most 20 parameters, so past that width the factory
              ;; takes the rest of its arguments in one, as `clojure.core/defrecord`'s own
              ;; does: the same count check, and the same `ArityException` when it fails.
              [fixed over] (split-at 20 fsyms)
              body [(run-checked (range nchecks) (fn [i] (map key->sym (deps-of i)))
                                 `(new ~tname ~@fsyms nil))]]
          `(defn ~ctor-name
             ~(str "Construct a validated " tname " from its fields, in declaration order.\n\n"
                   "  Throws `:check-failed` when a check does not pass, carrying `:record` and\n"
                   "  `:failures`, the report [[recognitum.core/check]] answers with: what each\n"
                   "  failing check was given, what it returned, and which checks it stopped"
                   batch-doc ".")
             [~@fixed ~@(when (seq over) ['& oversym])]
             ~@(if (seq over)
                 ;; The checks below read the fields as locals, so the arguments past the
                 ;; twentieth are bound back to their field names before the body runs.
                 [`(if (= (count ~oversym) ~(count over))
                     (let [~@(mapcat (fn [i f] [f `(nth ~oversym ~i)]) (range) over)]
                       ~@body)
                     (throw (clojure.lang.ArityException.
                             (+ ~(count fixed) (count ~oversym)) (name '~ctor-name))))]
                 body)))

       (defn ~(symbol (str "map->" tname))
         ~(str "Construct a validated " tname " from a map of its declared keys.\n"
               "  A key it does not declare is an error; a key it declares and the map omits\n"
               "  is nil, as it would be on a defrecord, and that field's shape validation\n"
               "  says whether nil is acceptable.\n\n"
               "  Keys may be written qualified or bare: a bare keyword naming a declared field\n"
               "  is qualified into " key-ns ", so a printed literal, whose tag already carries\n"
               "  the namespace, reads back through here.\n\n"
               "  Throws `:extraneous-keys` when the map holds a key this type does not declare,\n"
               "  carrying `:record` and `:keys`; `:check-failed` when a check does not pass,\n"
               "  carrying `:record` and `:failures`, the report [[recognitum.core/check]]\n"
               "  answers with"
               batch-doc ".")
         [~msym]
         (seal ~model-sym (reduce-kv assoc ((:draft-empty ~model-sym) nil)
                                     (~(internal 'qualify-keys) ~model-sym ~msym))))

       (~(internal 'register!) ~tname ~model-sym)
       (~(internal 'register-draft!) ~dname ~model-sym)
       (~(internal 'register-reader!) '~tagsym ~(symbol (str "map->" tname)))

       (ns-unmap *ns* '~(symbol (str "->" dname)))
       ;; A draft is unvalidated working state, and `#<` is the reader's own unreadable
       ;; marker, so the printed form does not imply a round trip.
       (defmethod print-method ~dname [x# ^java.io.Writer w#]
         (.write w# (str "#<" '~tname " draft " (into (sorted-map) x#) ">")))
       ;; The tag is namespace-qualified, so it escapes the reader's record-literal path and
       ;; is the exact symbol `clojure.edn/read` takes as a key in `:readers`.
       (defmethod print-method ~tname [x# ^java.io.Writer w#]
         (.write w# (str "#" '~tagsym (~(internal 'printable) x#))))
       ~tname)))
