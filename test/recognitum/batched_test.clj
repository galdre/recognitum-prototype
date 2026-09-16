(ns recognitum.batched-test
  "Batched fetches: `[[crew] :as rows]`.

  An io-dep may be written either way, and the two are told apart by the reader rather than
  by convention -- `[[crew] :as rows]` is not a legal function arglist, so the macro knows at
  expansion time which form it has. (`[[crew]]` alone *is* legal, which is why the `:as` is
  required.)

  What the inner vector holds is the dependency declaration, exactly as a singular arglist
  is: a symbol names a field, a keyword names another io-dep, and everything downstream --
  the gate, the fetch order, the field -> checks index -- is derived from it without knowing
  which form was written. What the `:as` binds is the collection of argument tuples, and it
  is the only thing the body sees: there are many tuples, so there is nothing for the inner
  names to be bound to.

  The return shapes differ accordingly. A singular fetch returns the value; a batched fetch
  returns a map keyed by the argument tuple, `{[crew] result}`. Keying by the tuple rather
  than aligning positionally is what lets the library deduplicate rather than the author,
  and it is what makes a tuple the batch did not answer distinguishable from one it answered
  with `nil`.

  Checks are never batched. A check judges one value."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec]
   [recognitum.roster :as roster]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that is
  not an ExceptionInfo propagates -- which is where a fetch body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which interns
  ;; vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.batched-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

(defn- msg
  "The message of the ExceptionInfo `f` throws, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

;;; ------------------------------------- the log -------------------------------------

(def ^:private log (atom []))
(defn- note! [k] (swap! log conj k) nil)
(defn- ran [k] (count (filter #(= k %) @log)))
(defn- order [& ks] (into [] (filter (set ks)) @log))

;; What a batched fetch was handed, verbatim, call by call. The shape of that argument is
;; the whole of the batched contract on the way in.
(def ^:private handed (atom []))
(defn- saw! [rows] (swap! handed conj rows) rows)

(defn- reset-log! [] (reset! log []) (reset! handed []))

(use-fixtures :each (fn [f] (reset-log!) (roster/reset-calls!) (f)))

;;; ------------------------------------- the types -------------------------------------

;; The shape SPEC.md writes, with the fetch batched. `::roster` names `crew` and nothing
;; else, so the gate in front of it is the checks over `crew` alone, and two checks name it,
;; so one call is what naming them is for.
(rec/defrecord Job
  [name crew cuts]
  {:checks
   {::name ([name] (do (note! ::name) (string? name)))
    ::crew ([crew] (do (note! ::crew) (and (set? crew) (every? keyword? crew))))
    ::cuts ([cuts] (do (note! ::cuts) (and (map? cuts) (every? keyword? (keys cuts)))))

    ::only-roster-gets-a-cut
    ("only people the house knows about get a cut"
     [cuts ::roster] (do (note! ::only-roster-gets-a-cut) (every? roster (keys cuts))))

    ::somebody-is-known
    ([::roster] (do (note! ::somebody-is-known) (boolean (seq roster))))}

   :io-deps
   {::roster ("who the house knows about"
              [[crew] :as rows] (do (note! ::roster)
                                    (roster/fetch-rosters (saw! rows))))}})

(def ^:private job (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1}))

(defn- whole
  "A draft of the type with every field written and nothing established about it."
  []
  (-> (rec/draft Job)
      (assoc ::name "The Italian Job")
      (assoc ::crew #{:charlie :lyle})
      (assoc ::cuts {:charlie 1 :lyle 1})))

;; The same io-dep, written singular, so the two forms can be compared rather than described.
(rec/defrecord Singular
  [crew]
  {:checks
   {::crew   ([crew] (and (set? crew) (every? keyword? crew)))
    ::known  ([::roster] (boolean (seq roster)))}
   :io-deps
   {::roster ([crew] (roster/fetch-roster crew))}})

;; Two fields in the tuple, and a check that can only pass if the fetched value arrived
;; where it was named.
(rec/defrecord Pair
  [a b]
  {:checks
   {::a   ([a] (int? a))
    ::b   ([b] (int? b))
    ::sum ([a ::total] (= total (* 2 a)))}
   :io-deps
   {::total ([[a b] :as rows]
             (into {} (map (fn [[x y :as t]] [t (+ x y)])) (saw! rows)))}})

;; A batched io-dep naming another io-dep. Declared in the wrong order on purpose: the
;; order is the graph's and not the form's.
(rec/defrecord Chain
  [a]
  {:checks
   {::a  ([a] (int? a))
    ::ok ([::two] (= 4 two))}
   :io-deps
   {::two ([[::one] :as rows]
           (do (note! ::two) (into {} (map (fn [[o :as t]] [t (inc o)])) (saw! rows))))
    ::one ([a] (do (note! ::one) (inc a)))}})

;; A batched fetch over nothing at all: one tuple, and it is empty.
(rec/defrecord Nullary
  [a]
  {:checks
   {::a     ([a] (int? a))
    ::const ([::k] (= :v k))}
   :io-deps
   {::k ([[] :as rows] (into {} (map (fn [t] [t :v])) (saw! rows)))}})

;; A batched fetch that answers with a tuple present and `nil` -- which is neither a failure
;; nor a success, but whatever the check naming it says it is.
(rec/defrecord NilEntry
  [a]
  {:checks
   {::a           ([a] (int? a))
    ::nil-is-fine ([::row] (nil? row))}
   :io-deps
   {::row ([[a] :as rows] (into {} (map (fn [t] [t nil])) rows))}})

;; A batched fetch that answers with something that is not a map at all.
(rec/defrecord NotAMap
  [a]
  {:checks
   {::a       ([a] (int? a))
    ::has-row ([::row] (some? row))}
   :io-deps
   {::row ([[a] :as rows] (do (note! ::row-fetch) :nope))}})

;; A batched fetch that answers a map, but not about the tuple it was asked about.
(rec/defrecord Incomplete
  [a]
  {:checks
   {::a       ([a] (int? a))
    ::has-row ([::row] (some? row))}
   :io-deps
   {::row ([[a] :as rows] {[:somebody-else] :surprise})}})

;; A batched fetch that cannot answer at all.
(rec/defrecord Flaky
  [a]
  {:checks
   {::a       ([a] (int? a))
    ::has-row ([::row] (some? row))}
   :io-deps
   {::row ([[a] :as rows] (throw (ex-info "the database is unreachable" {:down true})))}})

;;; ----------------------------- the form, as the macro reads it -----------------------------

(deftest a-batched-io-dep-compiles-and-says-it-is-batched
  (let [d (first (:io-deps (rec/model-of Job)))]
    (is (= ::roster (:name d)))
    (is (true? (boolean (:batched d))))
    (is (= "who the house knows about" (:doc d))
        "a batched io-dep takes a docstring where a singular one does")
    (is (= [::crew] (:deps d))
        "the inner vector is the dependency declaration, resolved to field keys")
    (is (= [::crew] (:fields d))
        "and those fields are what gates the fetch")
    (is (= #{::roster} (:io-names (rec/model-of Job))))))

(deftest the-two-forms-are-told-apart-by-the-reader
  (is (not (:batched (first (:io-deps (rec/model-of Singular)))))
      "`[crew]` is a legal arglist, so it is the singular form")
  (is (:batched (first (:io-deps (rec/model-of Job))))
      "`[[crew] :as rows]` is not, so it is the batched one")
  (testing "and they resolve to the same dependency either way"
    (is (= [::crew] (:deps (first (:io-deps (rec/model-of Singular))))))))

(deftest a-check-naming-a-batched-io-dep-names-it-as-any-other
  (let [c (first (filter #(= ::only-roster-gets-a-cut (:name %))
                         (:checks (rec/model-of Job))))]
    (is (= [::cuts ::roster] (:deps c)) "in arglist order")
    (is (= [::cuts ::crew] (:field-closure c))
        "and it reads the fields the batch names, which is what a write invalidates it by")))

;;; ------------------------------ what the fetch is handed ------------------------------

(deftest the-body-sees-the-collection-of-tuples-and-only-that
  (is (rec/sealed? (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1})))
  (is (= [[[#{:charlie :lyle}]]] @handed)
      "one call, handed a collection of argument tuples -- one tuple, of one argument"))

(deftest the-tuple-holds-what-the-inner-vector-named-in-the-order-it-named-it
  (is (rec/sealed? (->Pair 1 1)))
  (is (= [[[1 1]]] @handed))
  (testing "and the value comes back keyed by that tuple"
    (is (= ::rec/check-failed (:error (err #(->Pair 1 5))))
        "1 + 5 is not 2 * 1, so the relation is not vacuous")))

(deftest a-batched-fetch-over-nothing-is-handed-one-empty-tuple
  (is (rec/sealed? (->Nullary 1)))
  (is (= [[[]]] @handed)))

(deftest the-result-is-distributed-by-tuple
  ;; `::sum` passes exactly when the fetched total is twice `a`, which is true only if the
  ;; value it was handed came back keyed by its own tuple.
  (is (rec/sealed? (->Pair 3 3)))
  (is (= ::rec/check-failed (:error (err #(->Pair 3 4))))
      "and 3 + 4 is not twice 3, so the relation is not vacuous"))

;;; ------------------------------------ how often ------------------------------------

(deftest a-batched-io-dep-fetches-once-however-many-checks-name-it
  (is (rec/sealed? (->Job "Job" #{:charlie} {:charlie 1})))
  (is (= 1 (ran ::roster)) "two checks name it, and one call answered both")
  (is (= 1 (count @handed)))
  (is (= 1 (roster/call-count :fetch-rosters)))
  (is (= 1 (ran ::only-roster-gets-a-cut)))
  (is (= 1 (ran ::somebody-is-known))))

(deftest a-batched-fetch-is-shared-for-one-call-and-no-longer
  (let [s (->Job "Job" #{:charlie} {:charlie 1})]
    (is (= 1 (ran ::roster)))
    (reset-log!)
    (is (rec/sealed? (assoc s ::cuts {:charlie 2})))
    (is (= 1 (ran ::roster)) "a later call fetches again -- the value was not retained")))

(deftest a-draft-carries-no-batched-io-state
  (let [d (whole)]
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (ran ::roster)))
    (is (nil? (get d ::roster)) "the fetched value is not a key of the draft")
    (is (= #{::name ::crew ::cuts} (set (keys d))))
    (testing "and nothing about the draft holds it for the next call"
      (reset-log!)
      (let [d' (assoc d ::cuts {:lyle 1})]
        (is (true? (rec/check d' ::only-roster-gets-a-cut)))
        (is (= 1 (ran ::roster)))))))

;;; ------------------------------------ the gate ------------------------------------

(deftest a-batched-fetch-waits-on-the-checks-over-the-fields-it-names
  ;; `roster/fetch-rosters` throws on anything that is not a crew, so a fetch that ran here
  ;; would be an exception rather than a report. The gate is what makes it a report.
  (let [d (-> (whole) (assoc ::crew "not a crew"))
        e (err #(rec/seal d))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::crew] (vec (keys (:failures e)))))
    (is (= 0 (ran ::roster)) "nothing reached the fetch")
    (is (= 0 (roster/call-count)))
    (is (= 0 (ran ::only-roster-gets-a-cut)))))

(deftest the-gate-in-front-of-a-batch-is-narrow
  (let [d (-> (whole) (assoc ::name 42))]
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (ran ::roster)))
    (is (= 1 (ran ::crew)) "the gate ran")
    (is (= 0 (ran ::name)) "and nothing else did")))

(deftest a-field-a-batched-io-dep-names-must-have-been-written
  (let [d (assoc (rec/draft Job) ::cuts {:charlie 1})
        e (err #(rec/check d ::only-roster-gets-a-cut))]
    (is (= ::rec/missing-field (:error e)))
    (is (= [::crew] (:keys e)) "named through the batch that names it")
    (is (= 0 (ran ::roster)))))

;;; ------------------------------ what a batched fetch returns ------------------------------

(deftest a-batched-fetch-does-not-decide-validity
  (testing "a tuple answered nil is not a failure"
    (is (rec/sealed? (->NilEntry 1))))
  (testing "and the check naming it is what reads it"
    (let [e (err #(->Job "Job" #{:charlie :stranger} {:stranger 1}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::only-roster-gets-a-cut] (vec (keys (:failures e)))))
      (let [f (get (:failures e) ::only-roster-gets-a-cut)]
        (is (= [::cuts ::roster] (vec (keys (:values f)))))
        (is (= #{:charlie} (get (:values f) ::roster))
            "reported as the value distributed to this tuple, not as the whole batch")
        (is (false? (:returned f)))))))

(deftest a-batched-fetch-that-throws-propagates
  (let [e (try (->Flaky 1) nil (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e))
    (is (= {:down true} (ex-data e)))
    (is (nil? (:error (ex-data e))) "not a :check-failed")))

(deftest a-batched-fetch-that-does-not-answer-with-a-map-is-refused
  ;; The singular form has no return contract, because its only consumers are the checks
  ;; naming it. The batched form does: the library has to distribute the result, and it
  ;; distributes it by tuple. Something that is not a map keyed by tuple cannot be
  ;; distributed at all, and handing the checks a `nil` from it would have them judge a
  ;; fetch that never happened.
  (let [e (err #(->NotAMap 1))]
    (is (= ::rec/malformed-batch (:error e)))
    (is (= 'NotAMap (:record e)))
    (is (= ::row (:io-dep e)))
    (is (= :nope (:returned e)))
    (is (= 1 (ran ::row-fetch)) "the fetch did run; what came back is the problem")))

(deftest a-batched-fetch-that-omits-a-tuple-is-refused
  ;; A tuple answered `nil` and a tuple not answered at all are different things, and the
  ;; keying is what tells them apart. "We did not fetch this" is not a value for a check to
  ;; judge.
  (let [e (err #(->Incomplete 1))]
    (is (= ::rec/incomplete-batch (:error e)))
    (is (= ::row (:io-dep e)))
    (is (= [[1]] (:missing e)))
    (is (re-find #"\[1\]" (msg #(->Incomplete 1)))
        "and the message names the tuple nothing came back for")))

;;; ------------------------------ order, and naming another ------------------------------

(deftest a-batched-io-dep-may-name-another-io-dep
  (is (rec/sealed? (->Chain 2)))
  (is (= [::one ::two] (order ::one ::two))
      "declared two-then-one, fetched one-then-two: the order is the graph's")
  (is (= [[[3]]] @handed) "and the tuple holds what the io-dep it named returned"))

;;; ------------------------------ the mask and what it saves ------------------------------

(deftest a-write-to-a-field-a-batched-fetch-names-clears-the-checks-behind-it
  (let [d (rec/draft job)]
    (is (= #{::name ::crew ::cuts ::only-roster-gets-a-cut ::somebody-is-known}
           (rec/passing d)))
    (let [d' (assoc d ::crew #{:charlie :lyle :mole})]
      (is (= #{::name ::cuts} (rec/passing d'))
          "the checks behind the batch go stale with the field the batch names"))))

(deftest a-verdict-already-established-costs-no-batched-fetch-at-the-seal
  (let [d (whole)]
    (is (true? (rec/certify d rec/every-check)))
    (is (= 1 (ran ::roster)))
    (reset-log!)
    (is (rec/sealed? (rec/seal d)))
    (is (= 0 (ran ::roster)))))

;;; ------------------------- every entry point fetches the same way -------------------------

(deftest assoc-merge-and-the-constructors-all-reach-the-batch
  (testing "assoc"
    (is (rec/sealed? (assoc job ::cuts {:charlie 2})))
    (is (= 1 (ran ::roster))))
  (testing "merge, which is one round of checks and so one fetch"
    (reset-log!)
    (is (rec/sealed? (merge job {::crew #{:charlie :lyle :mole} ::cuts {:mole 1}})))
    (is (= 1 (ran ::roster))))
  (testing "map->Type"
    (reset-log!)
    (is (rec/sealed? (map->Job {::name "Job" ::crew #{:charlie} ::cuts {:charlie 1}})))
    (is (= 1 (ran ::roster))))
  (testing "and drafting, which seals once at the end"
    (reset-log!)
    (is (rec/sealed? (rec/drafting [d job]
                       (-> d (assoc ::crew #{:charlie}) (assoc ::cuts {:charlie 1})))))
    (is (= 1 (ran ::roster)))))

;;; ------------------------------- what does not compile -------------------------------

(deftest the-inner-vector-is-resolved-like-any-other-arglist
  (testing "a symbol naming no field"
    (let [e (compile-data '(recognitum.core/defrecord Stray
                             [a]
                             {:io-deps {:scratch/x ([[nope] :as rows] rows)}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= :scratch/x (:io-dep e)) "reported under its own kind")
      (is (= 'nope (:dependency e)))))
  (testing "a keyword naming no io-dep"
    (let [e (compile-data '(recognitum.core/defrecord Stray2
                             [a]
                             {:io-deps {:scratch/x ([[:scratch/nonesuch] :as rows] rows)}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= :scratch/nonesuch (:dependency e)))))
  (testing "and one name twice is one name twice, batched or not"
    (let [e (compile-data '(recognitum.core/defrecord Twice
                             [a]
                             {:io-deps {:scratch/x ([[a a] :as rows] rows)}}))]
      (is (= ::rec/duplicate-dependency (:error e)))
      (is (= 'a (:dependency e))))))

(deftest a-cycle-through-a-batched-io-dep-does-not-compile
  (let [e (compile-data '(recognitum.core/defrecord Ring
                           [a]
                           {:io-deps {:scratch/x ([[:scratch/y] :as rows] rows)
                                      :scratch/y ([:scratch/x] x)}}))]
    (is (= ::rec/io-dep-cycle (:error e)))
    (is (= [:scratch/x :scratch/y :scratch/x] (:cycle e)))))

(deftest the-as-is-required-and-so-is-what-it-binds
  ;; `[[crew]]` alone is a legal arglist -- a singular fetch over a destructured argument --
  ;; which is exactly why the `:as` is what says "batched". A form that is neither one thing
  ;; nor the other is refused rather than read as whichever is nearer.
  (testing "no :as at all"
    (let [e (compile-data '(recognitum.core/defrecord NoAs
                             [a]
                             {:io-deps {:scratch/x ([[a]] :v)}}))]
      (is (= ::rec/malformed-io-dep (:error e)))
      (is (= :scratch/x (:io-dep e)))
      (is (re-find #":as" (compile-message
                           '(recognitum.core/defrecord NoAs2
                              [a]
                              {:io-deps {:scratch/x ([[a]] :v)}}))))))
  (testing ":as with nothing after it"
    (is (= ::rec/malformed-io-dep
           (:error (compile-data '(recognitum.core/defrecord DanglingAs
                                    [a]
                                    {:io-deps {:scratch/x ([[a] :as] :v)}}))))))
  (testing ":as binding something that is not a symbol"
    (is (= ::rec/malformed-io-dep
           (:error (compile-data '(recognitum.core/defrecord KeywordAs
                                    [a]
                                    {:io-deps {:scratch/x ([[a] :as :rows] :v)}}))))))
  (testing "and anything after the binding"
    (is (= ::rec/malformed-io-dep
           (:error (compile-data '(recognitum.core/defrecord Trailing
                                    [a]
                                    {:io-deps {:scratch/x ([[a] :as rows extra] rows)}})))))))

(deftest a-tuple-written-anywhere-but-first-is-refused
  ;; `[crew [x] :as rows]` is nobody's arglist: not a singular one, since `[x]` is a
  ;; destructuring form rather than a dependency, and not a batched one, since the tuple is
  ;; the whole of what a batch is over.
  (let [e (compile-data '(recognitum.core/defrecord Mixed
                           [a b]
                           {:io-deps {:scratch/x ([a [b] :as rows] rows)}}))]
    (is (= ::rec/malformed-io-dep (:error e)))
    (is (= :scratch/x (:io-dep e)))))

(deftest a-check-may-not-be-batched
  ;; A check judges one value. There is nothing for a collection of tuples to mean where the
  ;; answer is a verdict about a single one of them.
  (let [e (compile-data '(recognitum.core/defrecord BatchedCheck
                           [a]
                           {:checks {:scratch/c ([[a] :as rows] (every? int? rows))}}))]
    (is (= ::rec/batched-check (:error e)))
    (is (= 'BatchedCheck (:record e)))
    (is (= :scratch/c (:check e)))
    (is (re-find #"one value" (compile-message
                               '(recognitum.core/defrecord BatchedCheck2
                                  [a]
                                  {:checks {:scratch/c ([[a] :as rows] (every? int? rows))}})))))
  (testing "while a check arglist that merely destructures is refused as it always was"
    ;; `[[a]]` is a legal arglist and there is no batched form for a check to have been
    ;; reaching for, so it stays what it was: an entry naming nothing.
    (let [e (compile-data '(recognitum.core/defrecord Destructuring
                             [a]
                             {:checks {:scratch/c ([[a]] true)}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= '[a] (:dependency e))))))

(deftest a-rejected-batched-io-dep-defines-nothing
  (is (some? (compiles '(recognitum.core/defrecord Refused
                          [a]
                          {:io-deps {:scratch/x ([[a] :as] :v)}}))))
  (is (nil? (ns-resolve @scratch '->Refused)))
  (is (nil? (ns-resolve @scratch 'Refused-model))))

(deftest a-batched-io-dep-with-a-docstring-and-one-arity-only
  (is (nil? (compiles '(recognitum.core/defrecord Documented
                         [a]
                         {:checks  {:scratch/c ([:scratch/x] (some? x))}
                          :io-deps {:scratch/x ("the doc" [[a] :as rows]
                                                (zipmap rows (repeat :v)))}}))))
  (testing "while two arities is two arities"
    (let [e (compile-data '(recognitum.core/defrecord Arities
                             [a]
                             {:io-deps {:scratch/x (([[a] :as rows] rows) ([a] a))}}))]
      (is (= ::rec/malformed-io-dep (:error e))))))
