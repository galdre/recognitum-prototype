(ns recognitum.io-deps-test
  "`:io-deps`: the singular form.

  An io-dep is written as a check is, arglist and all: a symbol names a field, a keyword
  names another io-dep, and the body receives exactly those. A fetch produces a value and
  does not decide validity: `nil` is whatever the checks naming it say it is, and an
  exception propagates. An io-dep is fetched once every pure check over the fields it names
  has passed and every io-dep it names has been fetched, and an external validation runs
  once its io-deps have been fetched. Within one validation call an io-dep is fetched at
  most once, and what it returned is not kept after the call returns."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]
   [recognitum.roster :as roster]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates -- which is where a fetch or a check body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which
  ;; interns vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.io-deps-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

;;; ------------------------------------- the log -------------------------------------

;; Every check and every fetch logs itself, so a check recorded as passing can be told
;; from one that ran, and a fetch that did not happen can be seen.
(def ^:private log (atom []))

(defn- note! [k] (swap! log conj k) nil)

(defn- reset-log! [] (reset! log []))

(defn- ran [k] (count (filter #(= k %) @log)))

(defn- order [& ks] (into [] (filter (set ks)) @log))

(use-fixtures :each (fn [f] (reset-log!) (roster/reset-calls!) (f)))

;;; ------------------------------------- the types -------------------------------------

;; A crew, cuts, and a roster fetched from outside. `::roster` names `crew` and nothing
;; else, so it waits on the checks over `crew` alone; a malformed `name`, which no fetch
;; reads, holds up nothing.
(rec/defrecord Job
  [name crew cuts]
  {:checks
   {::name ([name] (do (note! ::name) (string? name)))
    ::crew ([crew] (do (note! ::crew) (and (set? crew) (every? keyword? crew))))
    ::cuts ([cuts] (do (note! ::cuts) (and (map? cuts) (every? keyword? (keys cuts)))))

    ::only-roster-gets-a-cut
    ("only people the house knows about get a cut"
     [cuts ::roster] (do (note! ::only-roster-gets-a-cut)
                         (every? roster (keys cuts))))}

   :io-deps
   {::roster ("who the house knows about"
              [crew] (do (note! ::roster) (roster/fetch-roster crew)))}

   :certificates
   {::shape  #{::name ::crew ::cuts}
    ::payout #{::cuts ::only-roster-gets-a-cut}}})

(def ^:private job (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1}))

(defn- whole
  "A draft of the type with every field written and nothing recorded as passing."
  []
  (-> (rec/draft Job)
      (assoc ::name "The Italian Job")
      (assoc ::crew #{:charlie :lyle})
      (assoc ::cuts {:charlie 1 :lyle 1})))

;; Two checks over one io-dep, plus an io-dep nothing names.
(rec/defrecord Shared
  [a]
  {:checks
   {::a       ([a] (do (note! ::a) (int? a)))
    ::d-pos   ([::d] (do (note! ::d-pos) (pos? d)))
    ::d-fits  ([a ::d] (do (note! ::d-fits) (= d (* 2 a))))}
   :io-deps
   {::d      ([a] (do (note! ::d) (* 2 a)))
    ::unused ([a] (do (note! ::unused) :nobody-asked))}})

;; An io-dep naming another. Written in the wrong order on purpose: `::two` is declared
;; first and must fetch second, because the order is the graph's and not the form's.
(rec/defrecord Chain
  [a]
  {:checks
   {::a   ([a] (do (note! ::a) (int? a)))
    ::sum ([::two] (do (note! ::sum) (= 4 two)))}
   :io-deps
   {::two ([::one] (do (note! ::two) (inc one)))
    ::one ([a] (do (note! ::one) (inc a)))}})

;; A fetch that cannot answer. Not a failure of the data: a throw propagates.
(rec/defrecord Flaky
  [a]
  {:checks
   {::a    ([a] (int? a))
    ::seen ([::row] (some? row))}
   :io-deps
   {::row ([a] (throw (ex-info "the database is unreachable" {:down true :for a})))}})

;; A fetch that answers `nil`, which the checks are free to read either way.
(rec/defrecord Absent
  [a]
  {:checks
   {::a           ([a] (int? a))
    ::nil-is-fine ([::row] (nil? row))}
   :io-deps
   {::row ([a] nil)}})

(rec/defrecord Demanding
  [a]
  {:checks
   {::a            ([a] (int? a))
    ::needs-a-row  ("there has to be a row" [::row] (some? row))}
   :io-deps
   {::row ([a] nil)}})

;;; ---------------------------- the section compiles and means something ----------------------------

(deftest a-populated-io-deps-section-compiles
  (let [model (internal/model-of Job)]
    (is (= [::roster] (mapv :name (:io-deps model))))
    (is (= "who the house knows about" (:doc (first (:io-deps model))))
        "an io-dep takes a docstring where a check does")
    (is (= [::crew] (:deps (first (:io-deps model))))
        "and its arglist resolves to field keys, exactly as a check's does")
    (is (= #{::roster} (:io-names model)))))

(deftest a-keyword-in-a-check-arglist-resolves-to-a-declared-io-dep
  (let [model (internal/model-of Job)
        c     (first (filter #(= ::only-roster-gets-a-cut (:name %)) (:checks model)))]
    (is (= [::cuts ::roster] (:deps c))
        "in arglist order, with the io-dep named as itself and the field as its key")))

(deftest the-body-receives-what-it-named-positionally
  ;; `::d-fits` names `a` then `::d` and asserts the relation between them, so it can only
  ;; pass if the fetched value arrived in the second position and the field in the first.
  (is (rec/sealed? (->Shared 3)))
  (is (= 1 (ran ::d-fits)))
  (is (= ::rec/check-failed (:error (err #(->Shared 0))))
      "and 0 fails ::d-pos, so the relation is not vacuous"))

;;; ------------------------------------ what a fetch is ------------------------------------

(deftest the-fetched-value-is-what-the-check-sees
  (let [s (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1})]
    (is (rec/sealed? s))
    (is (= 1 (roster/call-count :fetch-roster))))
  (testing "and a cut to somebody the house does not know about fails the check"
    (let [e (err #(->Job "Job" #{:charlie :stranger} {:stranger 1}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::only-roster-gets-a-cut] (vec (keys (:failures e)))))
      (let [f (get (:failures e) ::only-roster-gets-a-cut)]
        (is (= "only people the house knows about get a cut" (:doc f)))
        (is (= [::cuts ::roster] (vec (keys (:values f))))
            "the fields it declared, in arglist order -- the io-dep among them")
        (is (= #{:charlie} (get (:values f) ::roster))
            "reported as what was actually fetched, not as a key nobody could look up")
        (is (false? (:returned f)))))))

(deftest a-fetch-does-not-decide-validity
  (testing "nil is not a failure"
    (is (rec/sealed? (->Absent 1))))
  (testing "nor a success -- the check naming it decides"
    (let [e (err #(->Demanding 1))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::needs-a-row] (vec (keys (:failures e)))))
      (is (= {::row nil} (:values (get (:failures e) ::needs-a-row)))))))

(deftest a-fetch-that-throws-propagates
  ;; A check that throws is a bug in the check; a fetch that throws is usually the database
  ;; being unreachable. Neither is a verdict about the data, and reporting either as one
  ;; would have the type assert something it does not know.
  (let [e (try (->Flaky 1) nil (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e))
    (is (= {:down true :for 1} (ex-data e)))
    (is (nil? (:error (ex-data e))) "not a :check-failed"))
  (testing "and it propagates from a seal too"
    (let [d (assoc (rec/draft Flaky) ::a 1)]
      (is (= {:down true :for 1} (ex-data (try (rec/seal d) nil
                                               (catch clojure.lang.ExceptionInfo ex ex)))))
      (is (= #{::a} (rec/passed d))
          "the pure check reached a verdict; the external validation reached none"))))

;;; ------------------------------------ how often ------------------------------------

(deftest an-io-dep-fetches-once-however-many-checks-name-it
  (is (rec/sealed? (->Shared 3)))
  (is (= 1 (ran ::d)) "two checks name it; naming them is what that is for")
  (is (= 1 (ran ::d-pos)))
  (is (= 1 (ran ::d-fits))))

(deftest a-fetch-is-shared-for-one-call-and-no-longer
  (let [s (->Shared 3)]
    (is (= 1 (ran ::d)))
    (reset-log!)
    (is (rec/sealed? (assoc s ::a 4)))
    (is (= 1 (ran ::d)) "a later call fetches again -- the value was not retained")))

(deftest an-io-dep-nothing-names-never-fetches
  (is (rec/sealed? (->Shared 3)))
  (is (= 0 (ran ::unused))))

(deftest a-draft-carries-no-io-state
  (let [d (whole)]
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (ran ::roster)))
    (testing "what is kept is which checks passed, not what was fetched"
      (is (contains? (rec/passed d) ::only-roster-gets-a-cut))
      (is (nil? (get d ::roster)) "the fetched value is not a key of the draft")
      (is (nil? (::roster d)))
      (is (= #{::name ::crew ::cuts} (set (keys d)))))
    (testing "and nothing about the draft holds it for the next call"
      (reset-log!)
      (let [d' (assoc d ::cuts {:lyle 1})]
        (is (true? (rec/check d' ::only-roster-gets-a-cut)))
        (is (= 1 (ran ::roster)))))))

;;; ------------------------------- io-deps naming io-deps -------------------------------

(deftest an-io-dep-may-name-another-and-they-fetch-in-graph-order
  (is (rec/sealed? (->Chain 2)))
  (is (= [::one ::two ::sum] (order ::one ::two ::sum))
      "declared two-then-one, fetched one-then-two: the order is the graph's")
  (is (= 1 (ran ::one)))
  (is (= 1 (ran ::two))))

(deftest a-transitively-named-io-dep-fetches-once-per-call
  (let [d (assoc (rec/draft Chain) ::a 2)]
    (is (true? (rec/check d ::sum)))
    (is (= 1 (ran ::one)))
    (is (= 1 (ran ::two)))))

(deftest a-check-waits-on-the-fields-of-the-fetches-behind-the-fetch-it-names
  ;; `::sum` names `::two`, which names `::one`, which is the only thing that reads `a`.
  ;; What `::sum` waits on is `a`'s shape validation, two links away from anything it
  ;; mentions.
  (let [d (assoc (rec/draft Chain) ::a "two")]
    (is (= [::a] (vec (keys (rec/check d ::sum))))
        "the shape validation of the field, and nothing else, is what came back")
    (is (= 0 (ran ::sum)) "so the check itself never ran")
    (is (= 0 (ran ::one)))
    (is (= 0 (ran ::two)) "and neither fetch happened"))
  (testing "and once the field is well-formed the whole chain runs"
    (reset-log!)
    (let [d (assoc (rec/draft Chain) ::a 2)]
      (is (true? (rec/check d ::sum)))
      (is (= [::a ::one ::two ::sum] (order ::a ::one ::two ::sum))))))

;;; ------------------------------ what a fetch waits on ------------------------------

(deftest a-fetch-waits-on-the-checks-over-the-fields-it-names
  ;; `roster/fetch-roster` throws on anything that is not a crew, so a fetch that ran here
  ;; would be an exception rather than a report.
  (let [d (-> (whole) (assoc ::crew "not a crew"))
        e (err #(rec/seal d))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::crew] (vec (keys (:failures e)))))
    (is (= 0 (ran ::roster)) "nothing reached the fetch")
    (is (= 0 (roster/call-count)))
    (is (= 0 (ran ::only-roster-gets-a-cut)) "and the check behind it did not run either")))

(deftest a-fetch-does-not-wait-on-checks-over-fields-it-does-not-name
  ;; `::roster` names `crew`, so a malformed `name` is no business of it.
  (let [d (-> (whole) (assoc ::name 42))]
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (ran ::roster)))
    (is (= 1 (ran ::crew)) "the check over crew ran")
    (is (= 0 (ran ::name)) "and nothing else did")))

(deftest a-failing-shape-validation-is-reported-and-the-fetch-does-not-happen
  (let [d (-> (whole) (assoc ::crew "not a crew"))
        r (rec/check d ::only-roster-gets-a-cut)]
    (is (= [::crew] (vec (keys r)))
        "the check could not run, and the failing shape validation is why")
    (is (not (contains? r ::only-roster-gets-a-cut)) "it never ran, so it has no entry")
    (is (= 0 (ran ::roster)))
    (is (= 0 (ran ::only-roster-gets-a-cut)))
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::only-roster-gets-a-cut))))
        "and check! throws the same failures")
    (testing "and whatever did run and pass is recorded"
      (is (= #{::cuts} (rec/passed d))
          "::cuts is the shape validation ::only-roster-gets-a-cut waits on; it passed"))))

(deftest a-certificate-reports-a-failing-shape-validation-it-does-not-name
  (let [d (-> (whole) (assoc ::crew "not a crew"))]
    (is (= [::crew] (vec (keys (rec/check d ::payout))))
        "::payout does not name ::crew, but ::crew is why nothing in it could be run")
    (is (= 0 (ran ::roster)))
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::payout)))))))

(deftest seal-collects-every-pure-failure-before-it-fetches-anything
  ;; Every pure failure is collected and thrown before anything is fetched, so a pure
  ;; failure no fetch depends on still stops the fetch.
  (let [d (-> (whole) (assoc ::name 42) (assoc ::cuts "not a map"))
        e (err #(rec/seal d))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::name ::cuts] (vec (keys (:failures e))))
        "every pure failure, keyed in declaration order")
    (is (= 0 (ran ::roster)))))

(deftest an-external-validation-runs-once-its-io-deps-have-fetched
  (let [d (whole)]
    (is (rec/sealed? (rec/seal d)))
    (is (= [::roster ::only-roster-gets-a-cut] (order ::roster ::only-roster-gets-a-cut)))))

(deftest an-unwritten-field-an-io-dep-names-is-judged-as-nil-before-the-fetch
  (let [d (assoc (rec/draft Job) ::cuts {:charlie 1})
        r (rec/check d ::only-roster-gets-a-cut)
        e (err #(rec/check! d ::only-roster-gets-a-cut))]
    (is (= [::crew] (vec (keys r))) "the shape validation of the field the fetch names")
    (is (= [::crew] (vec (keys (:failures e)))))
    (is (= {::crew nil} (:values (get (:failures e) ::crew))))
    (is (= 0 (ran ::roster)))
    (is (= 0 (roster/call-count)))))

;;; ---------------------------------- what a write clears ----------------------------------

(deftest a-write-to-a-field-an-io-dep-names-clears-the-checks-behind-it
  ;; `::only-roster-gets-a-cut` does not name `crew`; it names `::roster`, which names
  ;; `crew`, so a write to `crew` clears it.
  (let [d (rec/draft job)]
    (is (= #{::name ::crew ::cuts ::only-roster-gets-a-cut} (rec/passed d)))
    (let [d' (assoc d ::crew #{:charlie :lyle :mole})]
      (is (= #{::name ::cuts} (rec/passed d'))
          "::crew because it names the field, ::only-roster-gets-a-cut because its fetch does")
      (testing "and the index says the same thing"
        (is (= [::crew ::only-roster-gets-a-cut]
               (mapv (comp :name (:checks (internal/model-of Job)))
                     (get (:index (internal/model-of Job)) ::crew))))))))

(deftest a-check-already-passing-costs-no-fetch-at-the-seal
  (let [d (whole)]
    (is (true? (rec/check d rec/every-check)))
    (is (= 1 (ran ::roster)))
    (reset-log!)
    (is (rec/sealed? (rec/seal d)))
    (is (= 0 (ran ::roster)) "the seal found every check already passing")
    (is (= 0 (ran ::only-roster-gets-a-cut)))))

(deftest a-draft-of-a-sealed-value-seals-without-touching-io
  (let [d (rec/draft job)]
    (is (rec/sealed? (rec/seal d)))
    (is (= 0 (ran ::roster)))
    (is (= 0 (roster/call-count)))))

(deftest a-write-to-a-field-no-fetch-touches-costs-no-fetch
  (let [s (rec/drafting [d job] (assoc d ::name "The Sequel"))]
    (is (rec/sealed? s))
    (is (= 1 (ran ::name)))
    (is (= 0 (ran ::roster)) "::roster names crew, and crew did not move")))

(deftest a-check-already-recorded-as-passing-fetches-nothing
  (let [d (rec/draft job)]
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 0 (ran ::roster)))
    (is (= 0 (ran ::only-roster-gets-a-cut)))))

(deftest a-failed-io-check-is-run-again-next-time
  (let [d (-> (whole) (assoc ::cuts {:stranger 1}))]
    (is (= {::only-roster-gets-a-cut false} (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (ran ::roster)))
    (is (= {::only-roster-gets-a-cut false} (rec/check d ::only-roster-gets-a-cut)))
    (is (= 2 (ran ::roster)) "a failure is not recorded, so it runs again")))

;;; ------------------------- writes to a sealed value reach IO too -------------------------

(deftest assoc-on-a-sealed-value-runs-the-io-checks-depending-on-the-field
  (let [s (assoc job ::cuts {:charlie 2})]
    (is (rec/sealed? s))
    (is (= 1 (ran ::roster)))
    (is (= 1 (ran ::only-roster-gets-a-cut))))
  (testing "and a write that breaks one is refused"
    (reset-log!)
    (let [e (err #(assoc job ::cuts {:stranger 1}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::only-roster-gets-a-cut] (vec (keys (:failures e)))))
      (is (= 1 (ran ::roster)))))
  (testing "while a write that fails a pure check never fetches"
    (reset-log!)
    (is (= ::rec/check-failed (:error (err #(assoc job ::cuts "not a map")))))
    (is (= 0 (ran ::roster)))))

(deftest merge-is-one-write-and-one-fetch
  (let [s (merge job {::crew #{:charlie :lyle :mole} ::cuts {:mole 1}})]
    (is (rec/sealed? s))
    (is (= 1 (ran ::roster)) "one round of checks, so one fetch")
    (is (= 1 (ran ::only-roster-gets-a-cut)))))

(deftest a-constructor-fetches-after-the-pure-checks-have-passed
  (testing "map->Type waits the same way"
    (let [e (err #(map->Job {::name "Job" ::crew "not a crew" ::cuts {:charlie 1}}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::crew] (vec (keys (:failures e)))))
      (is (= 0 (ran ::roster)))))
  (testing "and fetches once when they pass"
    (reset-log!)
    (is (rec/sealed? (map->Job {::name "Job" ::crew #{:charlie} ::cuts {:charlie 1}})))
    (is (= 1 (ran ::roster)))))

;;; ------------------------------- what does not compile -------------------------------

(deftest an-io-dep-arglist-is-resolved-like-any-other
  (let [e (compile-data '(recognitum.core/defrecord Stray
                           [a]
                           {:io-deps {:scratch/roster ([nope] [nope])}}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= 'Stray (:record e)))
    (is (= :scratch/roster (:io-dep e)) "reported under its own kind, not as a check")
    (is (nil? (:check e)))
    (is (= 'nope (:dependency e))))
  (testing "and a keyword in one must name another io-dep of this type"
    (let [e (compile-data '(recognitum.core/defrecord Stray2
                             [a]
                             {:io-deps {:scratch/roster ([:scratch/nonesuch] nonesuch)}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= :scratch/nonesuch (:dependency e)))
      (is (= #{:scratch/roster} (:io-deps e)) "and what it could have named"))))

(deftest a-check-may-name-an-io-dep-that-exists-and-only-one-that-does
  (is (nil? (compiles '(recognitum.core/defrecord Fetching
                         [a]
                         {:checks  {:scratch/c ([:scratch/roster] (some? roster))}
                          :io-deps {:scratch/roster ([a] a)}}))))
  (testing "while one that does not is the offender named"
    (let [e (compile-data '(recognitum.core/defrecord Fetching2
                             [a]
                             {:checks  {:scratch/c ([:scratch/vetted] (some? vetted))}
                              :io-deps {:scratch/roster ([a] a)}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= :scratch/c (:check e)))
      (is (= :scratch/vetted (:dependency e)))
      (is (= #{:scratch/roster} (:io-deps e))))))

(deftest a-name-cannot-be-both-a-check-and-an-io-dep
  (let [e (compile-data '(recognitum.core/defrecord Both
                           [a]
                           {:checks  {:scratch/roster ([a] (int? a))}
                            :io-deps {:scratch/roster ([a] a)}}))]
    (is (= ::rec/duplicate-name (:error e)))
    (is (= :scratch/roster (:name e)))))

(deftest an-io-dep-cannot-be-spelled-like-a-field
  ;; A field key and an io-dep name are both keywords and both appear in the same resolved
  ;; dependency list. One name for both would make an arglist entry mean two things.
  (let [e (compile-data '(recognitum.core/defrecord Clashing
                           [a]
                           {:io-deps {:recognitum.io-deps-scratch/a ([a] a)}}))]
    (is (= ::rec/duplicate-name (:error e)))
    (is (= :recognitum.io-deps-scratch/a (:name e)))))

(deftest a-cycle-among-io-deps-does-not-compile
  (testing "an io-dep naming itself"
    (let [e (compile-data '(recognitum.core/defrecord Selfish
                             [a]
                             {:io-deps {:scratch/x ([:scratch/x] x)}}))]
      (is (= ::rec/io-dep-cycle (:error e)))
      (is (= 'Selfish (:record e)))
      (is (= [:scratch/x :scratch/x] (:cycle e)))))
  (testing "two naming each other"
    (let [e (compile-data '(recognitum.core/defrecord Pair
                             [a]
                             {:io-deps {:scratch/x ([:scratch/y] y)
                                        :scratch/y ([:scratch/x] x)}}))]
      (is (= ::rec/io-dep-cycle (:error e)))
      (is (= [:scratch/x :scratch/y :scratch/x] (:cycle e)))))
  (testing "and a longer ring"
    (let [e (compile-data '(recognitum.core/defrecord Ring
                             [a]
                             {:io-deps {:scratch/x ([:scratch/y] y)
                                        :scratch/y ([:scratch/z] z)
                                        :scratch/z ([:scratch/x] x)}}))]
      (is (= ::rec/io-dep-cycle (:error e)))
      (is (= [:scratch/x :scratch/y :scratch/z :scratch/x] (:cycle e)))
      (is (re-find #":scratch/y" (compile-message
                                  '(recognitum.core/defrecord Ring2
                                     [a]
                                     {:io-deps {:scratch/x ([:scratch/y] y)
                                                :scratch/y ([:scratch/x] x)}})))))))

(deftest an-acyclic-graph-of-io-deps-compiles
  (is (nil? (compiles '(recognitum.core/defrecord Layered
                         [a]
                         {:checks  {:scratch/c ([:scratch/z] (some? z))}
                          :io-deps {:scratch/z ([:scratch/y :scratch/x] [x y])
                                    :scratch/y ([:scratch/x] x)
                                    :scratch/x ([a] a)}})))
      "a diamond is not a cycle"))

(deftest an-io-dep-gives-exactly-one-arity
  (let [e (compile-data '(recognitum.core/defrecord Arities
                           [a]
                           {:io-deps {:scratch/x (([a] a) ([a b] a))}}))]
    (is (= ::rec/malformed-io-dep (:error e)))
    (is (= :scratch/x (:io-dep e))))
  (testing "and the message says io-dep, since that is what it is"
    (is (re-find #"io-dep" (compile-message
                            '(recognitum.core/defrecord Arities2
                               [a]
                               {:io-deps {:scratch/x ("doc" [a])}}))))))

(deftest the-batched-form-is-an-io-dep-arglist-too
  ;; `[[crew] :as rows]` is not a legal function arglist, which is how the reader tells it
  ;; from the singular form. Here it is enough that an io-dep may be written that way and
  ;; resolves to the same dependency either way; what it then means is
  ;; `recognitum.batched-test`.
  (is (nil? (compiles '(recognitum.core/defrecord Batched
                         [a]
                         {:checks  {:scratch/c ([:scratch/x] (some? x))}
                          :io-deps {:scratch/x ([[a] :as rows] (zipmap rows (repeat :v)))}}))))
  (let [model (binding [*ns* @scratch]
                (eval '(recognitum.internal/model-of (->Batched 1))))
        d     (first (:io-deps model))]
    (is (= [:recognitum.io-deps-scratch/a] (:deps d))
        "the inner vector is the dependency declaration, resolved as any arglist is")
    (is (true? (:batched d)))))

(deftest an-arglist-may-not-bind-one-name-twice
  ;; What the body sees of `::roster` is `roster`, so an io-dep whose name spells a field's
  ;; would shadow it -- and silently, since a repeated parameter is legal Clojure and the
  ;; second one wins. A dependency declared and then shadowed is a dependency the body
  ;; cannot read, so the form does not compile.
  (let [e (compile-data '(recognitum.core/defrecord Shadowed
                           [crew]
                           {:checks  {:scratch/c ([crew :other/crew] (= crew crew))}
                            :io-deps {:other/crew ([crew] crew)}}))]
    (is (= ::rec/duplicate-dependency (:error e)))
    (is (= :scratch/c (:check e)))
    (is (= 'crew (:dependency e))))
  (testing "which is the same rule a field named twice runs into"
    (let [e (compile-data '(recognitum.core/defrecord Twice
                             [a]
                             {:checks {:scratch/c ([a a] (int? a))}}))]
      (is (= ::rec/duplicate-dependency (:error e)))
      (is (= 'a (:dependency e)))))
  (testing "while an io-dep named for a field it does not itself name is fine"
    (is (nil? (compiles '(recognitum.core/defrecord Fine
                           [crew]
                           {:checks  {:scratch/c ([:other/crew] (some? crew))}
                            :io-deps {:other/crew ([crew] crew)}}))))))

(deftest a-rejected-io-dep-defines-nothing
  (is (some? (compiles '(recognitum.core/defrecord Refused
                          [a]
                          {:io-deps {:scratch/x ([:scratch/x] x)}}))))
  (is (nil? (ns-resolve @scratch '->Refused)))
  (is (nil? (ns-resolve @scratch 'Refused-model))))
