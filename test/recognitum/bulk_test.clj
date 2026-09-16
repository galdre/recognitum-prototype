(ns recognitum.bulk-test
  "Checking many values at once: `check-all` and `check-all!`.

  One call spanning many values, which is what makes a batched fetch worth writing: every
  argument tuple in the group arrives in one call, and distinct tuples are computed once
  however many values share them. The singular form deduplicates the same way -- it simply
  pays one call per distinct tuple instead of one call for all of them.

  A group may hold values of more than one type. Batched fetches are declared per type, so
  a mixed group is one fetch pass per type and still batched within each; a check or
  certificate name resolves per type. A name one of the types does not know fails the whole
  call, which is the rule a single `check` already applies -- a typo is a programmer error,
  and burying it among per-value failures is how one hides.

  Results are index-aligned with the input. Two drafts holding identical data are `=`, so
  keying results by the draft would silently collapse them into one; a vector cannot.

  Partial success is a workspace notion, so only the non-throwing forms offer it. They
  report per value and leave each draft's mask updated to match what was established about
  it. The bang forms run the same work, collect every failure across every value, and throw
  once with all of them.

  Nothing bulk produces a sealed value. A guarantee does not come in partial -- and it does
  not need to, because the masks bulk checking leaves behind are what make sealing each
  value afterwards free."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec]
   [recognitum.roster :as roster]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that is
  not an ExceptionInfo propagates -- which is where a fetch body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- msg [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

;;; ------------------------------------- the log -------------------------------------

(def ^:private log (atom []))
(defn- note! [k] (swap! log conj k) nil)
(defn- ran [k] (count (filter #(= k %) @log)))

;; What a batched fetch was handed, call by call.
(def ^:private handed (atom []))
(defn- saw! [rows] (swap! handed conj (vec rows)) rows)

(defn- reset-log! [] (reset! log []) (reset! handed []))

(use-fixtures :each (fn [f] (reset-log!) (roster/reset-calls!) (f)))

;;; ------------------------------------- the types -------------------------------------

;; Pure, for everything that is about alignment, masks and reporting rather than about IO.
(rec/defrecord Ledger
  [a b]
  {:checks
   {::a    ([a] (if (int? a) true {:not-an-int a}))
    ::b    ([b] (if (string? b) true :nope))
    ::fits ("a fits inside b" [a b] (and (int? a) (string? b) (< a (count b))))}

   :certificates
   {::shape #{::a ::b}}})

;; A batched fetch over `crew`, so a group of drafts is one call.
(rec/defrecord Crew
  [name crew cuts]
  {:checks
   {::name ([name] (do (note! ::name) (string? name)))
    ::crew ([crew] (do (note! ::crew) (and (set? crew) (every? keyword? crew))))
    ::cuts ([cuts] (do (note! ::cuts) (map? cuts)))

    ::known-get-cuts
    ("only people the house knows about get a cut"
     [cuts ::roster] (do (note! ::known-get-cuts) (every? roster (keys cuts))))}

   :certificates
   {::shape  #{::name ::crew ::cuts}
    ::payout #{::known-get-cuts}}

   :io-deps
   {::roster ("who the house knows about"
              [[crew] :as rows] (do (note! ::roster) (roster/fetch-rosters (saw! rows))))}})

;; The same shape with the fetch written singular, so the deduplication can be counted in
;; calls rather than in tuples.
(rec/defrecord Sing
  [crew]
  {:checks
   {::c    ([crew] (and (set? crew) (every? keyword? crew)))
    ::mine ([crew ::roster] (= roster (into #{} (filter roster/table) crew)))}

   :io-deps
   {::roster ([crew] (do (note! ::roster) (roster/fetch-roster crew)))}})

;; An io-dep naming another, one singular and one batched, so the per-value caches have to
;; be wired through the graph and not merely filled in.
(rec/defrecord Chain
  [a]
  {:checks
   {::a  ([a] (int? a))
    ::ok ([a ::two] (= two (+ 2 a)))}

   :io-deps
   {::two ([[::one] :as rows] (do (note! ::two)
                                  (into {} (map (fn [[o :as t]] [t (inc o)])) (saw! rows))))
    ::one ([a] (do (note! ::one) (inc a)))}})

;;; ------------------------------------ helpers ------------------------------------

(defn- ledger [a b] (-> (rec/draft Ledger) (assoc ::a a) (assoc ::b b)))

(defn- crew-draft [nm crew cuts]
  (-> (rec/draft Crew) (assoc ::name nm) (assoc ::crew crew) (assoc ::cuts cuts)))

(defn- sing [crew] (assoc (rec/draft Sing) ::crew crew))

;;; --------------------------------- index alignment ---------------------------------

(deftest check-all-answers-once-per-value-in-the-order-it-was-given-them
  (let [ds [(ledger 1 "xxx") (ledger :nope "xxx") (ledger 2 :nope)]
        rs (rec/check-all ds ::a)]
    (is (vector? rs))
    (is (= 3 (count rs)) "one answer per value")
    (is (= [true {::a {:not-an-int :nope}} true] rs)
        "true where it passed, and the check's own return value under its name where not")))

(deftest the-answer-is-the-checks-own-data-as-it-is-for-check
  (let [d  (ledger :nope "xxx")
        d2 (ledger :nope "xxx")]
    (is (= (rec/check d ::a) (first (rec/check-all [d2] ::a)))
        "`check-all` is the plural of `check`, and answers what it answers")))

(deftest two-drafts-holding-identical-data-stay-two-answers
  (let [d1 (ledger :nope "xxx")
        d2 (ledger :nope "xxx")]
    (is (= d1 d2) "equal, so keying the results by draft would collapse them")
    (is (= 2 (count (rec/check-all [d1 d2] ::a))))
    (is (= 2 (count (rec/check-all [d1 d2] ::shape))))
    (testing "and the same draft twice is still two answers"
      (is (= 2 (count (rec/check-all [d1 d1] ::a)))))))

(deftest any-collection-of-drafts-will-do-and-a-vector-comes-back
  (let [ds (list (ledger 1 "xxx") (ledger :nope "xxx"))]
    (is (= [true {::a {:not-an-int :nope}}] (rec/check-all ds ::a)))
    (is (vector? (rec/check-all ds ::a)))
    (is (vector? (rec/check-all (seq ds) ::shape)))))

(deftest nothing-to-check-is-answered-and-not-refused
  (is (= [] (rec/check-all [] ::a)))
  (is (= [] (rec/check-all [] ::shape)))
  (is (true? (rec/check-all! [] ::a)))
  (is (true? (rec/check-all! [] ::shape))))

(deftest a-certificate-answers-per-value-the-way-one-check-does
  (let [ds [(ledger 1 "xxx") (ledger :nope 7)]
        rs (rec/check-all ds ::shape)]
    (is (= 2 (count rs)))
    (is (true? (first rs)))
    (is (= {::a {:not-an-int :nope} ::b :nope} (second rs))
        "check name -> what that check returned, for those that did not pass")
    (is (= [::a ::b] (vec (keys (second rs)))) "in declaration order")))

;;; ------------------------------ nothing bulk seals ------------------------------

(deftest nothing-bulk-produces-a-sealed-value
  (let [d  (ledger 1 "xxx")
        rs (rec/check-all [d] ::a)]
    (is (not (rec/sealed? (first rs))))
    (is (not (rec/draft? (first rs))))
    (is (true? (first rs)) "the answer to the question asked, not a value"))
  (let [d (ledger 1 "xxx")]
    (is (true? (rec/check-all! [d] ::a)) "nor does the bang form")
    (is (rec/draft? d) "and the draft it was given is still a draft")))

(deftest bulk-checking-leaves-the-masks-populated-so-sealing-is-free
  (let [ds [(crew-draft "one" #{:charlie} {:charlie 1})
            (crew-draft "two" #{:lyle} {:lyle 1})]]
    (is (= [true true] (rec/check-all ds rec/every-check)))
    (is (= 1 (ran ::roster)) "one fetch for the group")
    (reset-log!)
    (roster/reset-calls!)
    (let [sealed (mapv rec/seal ds)]
      (is (every? rec/sealed? sealed))
      (is (= 0 (ran ::roster)) "sealing afterwards performs no IO")
      (is (= 0 (roster/call-count)))
      (is (= 0 (ran ::crew)) "and re-runs nothing"))))

;;; ------------------------------------ the masks ------------------------------------

(deftest each-draft-is-left-with-its-own-verdicts
  (let [good (ledger 1 "xxx")
        bad  (ledger :nope "xxx")]
    (rec/check-all [good bad] ::a)
    (is (= #{::a} (rec/passed good)))
    (is (= #{} (rec/passed bad)) "a failure clears the bit rather than setting it")
    (testing "and a second call re-runs only what is not passing"
      (is (= [true {::a {:not-an-int :nope}}] (rec/check-all [good bad] ::a))))))

(deftest a-certificate-a-group-established-is-held-per-value
  (let [good (ledger 1 "xxx")
        bad  (ledger 1 :nope)]
    (is (= [true {::b :nope}] (rec/check-all [good bad] ::shape)))
    (is (= #{::shape} (rec/certificates good)))
    (is (= #{} (rec/certificates bad)))
    (is (= #{::a} (rec/passed bad)) "what passed for it is still recorded")))

(deftest the-bang-forms-record-what-they-established-too
  (let [good (ledger 1 "xxx")
        bad  (ledger :nope "xxx")]
    (is (= ::rec/check-failed (:error (err #(rec/check-all! [good bad] ::a)))))
    (is (= #{::a} (rec/passed good)) "work is not lost because the call threw")
    (is (= #{} (rec/passed bad)))))

(deftest a-verdict-already-established-is-not-established-again
  (let [ds [(crew-draft "one" #{:charlie} {:charlie 1})
            (crew-draft "two" #{:lyle} {:lyle 1})]]
    (is (= [true true] (rec/check-all ds ::known-get-cuts)))
    (is (= 1 (ran ::roster)) "one fetch for the group")
    (is (= 2 (ran ::known-get-cuts)) "and a check per value -- a check judges one value")
    (reset-log!)
    (is (= [true true] (rec/check-all ds ::known-get-cuts)))
    (is (= 0 (ran ::roster)) "nothing was pending, so nothing was fetched")
    (is (= 0 (ran ::known-get-cuts)))))

(deftest only-what-a-value-still-owes-is-run-for-it
  (let [established (crew-draft "one" #{:charlie} {:charlie 1})
        fresh       (crew-draft "two" #{:charlie} {:charlie 1})]
    (is (true? (rec/check established ::known-get-cuts)))
    (reset-log!)
    (roster/reset-calls!)
    (is (= [true true] (rec/check-all [established fresh] ::known-get-cuts)))
    (is (= 1 (ran ::known-get-cuts)) "only the one that owed it ran")
    (is (= 1 (ran ::roster)) "and only it fetched")))

;;; --------------------------- one call, every argument tuple ---------------------------

(deftest a-batched-fetch-sees-the-whole-group-in-one-call
  (let [ds [(crew-draft "one"   #{:charlie} {:charlie 1})
            (crew-draft "two"   #{:lyle}    {:lyle 1})
            (crew-draft "three" #{:charlie} {:charlie 2})]]
    (is (= [true true true] (rec/check-all ds ::known-get-cuts)))
    (is (= 1 (count @handed)) "one call for three values")
    (is (= 1 (roster/call-count :fetch-rosters)))
    (is (= #{[#{:charlie}] [#{:lyle}]} (set (first @handed)))
        "handed the tuples of the group")
    (is (= 2 (count (first @handed)))
        "distinct tuples: two values sharing a crew are one fetch's worth of work")))

(deftest the-result-is-distributed-to-the-value-it-was-fetched-for
  ;; `::known-get-cuts` passes only if the roster this value sees is its own crew's. A
  ;; result handed to the wrong value shows up as a failure rather than as nothing.
  (let [ds [(crew-draft "one"   #{:charlie} {:charlie 1})
            (crew-draft "two"   #{:lyle}    {:lyle 1})
            (crew-draft "three" #{:mole}    {:mole 1})]]
    (is (= [true true true] (rec/check-all ds ::known-get-cuts)))
    (is (= 1 (count @handed)))
    (testing "and a value the fetch does not vindicate fails on its own"
      (reset-log!)
      (let [ds [(crew-draft "one" #{:charlie} {:charlie 1})
                (crew-draft "two" #{:lyle}    {:stranger 1})]]
        (is (= [true {::known-get-cuts false}] (rec/check-all ds ::known-get-cuts)))))))

(deftest a-singular-fetch-is-deduplicated-across-the-group-too
  (let [ds [(sing #{:charlie}) (sing #{:lyle}) (sing #{:charlie})]]
    (is (= [true true true] (rec/check-all ds ::mine)))
    (is (= 2 (ran ::roster)) "three values, two distinct tuples, two fetches")
    (is (= 2 (roster/call-count :fetch-roster)))))

(deftest a-fetch-is-shared-for-one-call-and-no-longer
  (let [ds [(sing #{:charlie}) (sing #{:charlie})]]
    (is (= [true true] (rec/check-all ds ::mine)))
    (is (= 1 (ran ::roster)))
    (reset-log!)
    (let [ds' (mapv #(assoc % ::crew #{:charlie}) ds)]
      (is (= [true true] (rec/check-all ds' ::mine)))
      (is (= 1 (ran ::roster)) "a later call fetches again"))))

(deftest a-graph-of-fetches-runs-in-order-across-the-group
  (let [ds [(assoc (rec/draft Chain) ::a 1)
            (assoc (rec/draft Chain) ::a 2)
            (assoc (rec/draft Chain) ::a 1)]]
    (is (= [true true true] (rec/check-all ds ::ok)))
    (is (= 2 (ran ::one)) "the singular one, once per distinct tuple")
    (is (= 1 (ran ::two)) "the batched one that names it, once for the group")
    (is (= #{[2] [3]} (set (first @handed)))
        "and it was handed the tuples built out of what the first answered")))

;;; ------------------------------------- the gate -------------------------------------

(deftest a-value-whose-shape-failed-carries-nothing-to-the-fetch
  ;; `roster/fetch-rosters` throws on anything that is not a crew, so a leaking gate is an
  ;; exception rather than a quietly wrong report.
  (let [ds [(crew-draft "one" #{:charlie}  {:charlie 1})
            (crew-draft "two" "not a crew" {:lyle 1})]
        rs (rec/check-all ds ::known-get-cuts)]
    (is (true? (first rs)))
    (is (= {::crew false} (second rs))
        "the gate's failure is the only honest answer about a check that never ran")
    (is (= 1 (count @handed)))
    (is (= [[#{:charlie}]] (first @handed))
        "and only the well-formed value's tuple was carried to the fetch")))

(deftest a-group-that-is-entirely-malformed-fetches-nothing
  (let [ds [(crew-draft "one" "nope" {:charlie 1})
            (crew-draft "two" 42     {:lyle 1})]]
    (is (= [{::crew false} {::crew false}] (rec/check-all ds ::known-get-cuts)))
    (is (= 0 (ran ::roster)))
    (is (= 0 (roster/call-count)))
    (is (= 0 (ran ::known-get-cuts)))))

(deftest the-gate-is-established-as-a-verdict-like-anything-else
  (let [d (crew-draft "one" #{:charlie} {:charlie 1})]
    (is (= [true] (rec/check-all [d] ::known-get-cuts)))
    (is (= #{::crew ::cuts ::known-get-cuts} (rec/passed d))
        "the check asked about, the shape validation of the field it names, and the gate its
         fetch waited on")
    (is (= 0 (ran ::name)) "and nothing that gates nothing")))

;;; --------------------------------- the bang forms ---------------------------------

(deftest check-all-bang-answers-true-when-every-value-passes
  (is (true? (rec/check-all! [(ledger 1 "xxx") (ledger 2 "xxx")] ::a)))
  (is (true? (rec/check-all! [(ledger 1 "xxx") (ledger 2 "xxx")] ::shape))))

(deftest check-all-bang-throws-once-with-every-failure-across-every-value
  (let [ds [(ledger 1 "xxx") (ledger :nope "xxx") (ledger :also-nope "xxx")]
        e  (err #(rec/check-all! ds ::a))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 'Ledger (:record e)))
    (is (true? (:bulk e)) "keyed by index rather than by check name")
    (is (= [1 2] (vec (keys (:failures e)))) "the values that failed, in order")
    (is (= [1 2] (:indexes e)))
    (let [f (get-in (:failures e) [1 ::a])]
      (is (= {:not-an-int :nope} (:returned f)))
      (is (= [::a] (vec (keys (:values f)))) "the fields it declared")
      (is (= :nope (get (:values f) ::a))))))

(deftest one-throw-carries-every-check-of-every-value
  (let [ds [(ledger :nope 7) (ledger 1 "xxx") (ledger 2 :nope)]
        e  (err #(rec/check-all! ds ::shape))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [0 2] (vec (keys (:failures e)))) "index order")
    (is (= [::a ::b] (vec (keys (get (:failures e) 0))))
        "and within a value, declaration order")
    (is (= [::b] (vec (keys (get (:failures e) 2)))))))

(deftest a-failure-carries-the-same-report-it-carries-anywhere-else
  ;; The shapes pass here, which is what lets ::fits run at all and therefore be reported.
  (let [e (err #(rec/check-all! [(ledger 9 "xx")] rec/every-check))
        f (get-in (:failures e) [0 ::fits])]
    (is (= "a fits inside b" (:doc f)) "the docstring where there is one")
    (is (= [::a ::b] (vec (keys (:values f)))) "the fields it declared, in arglist order")
    (is (= {::a 9 ::b "xx"} (into {} (:values f))))
    (is (false? (:returned f))))
  (testing "and a check written without one says nothing where the docstring would be"
    (let [f (get-in (:failures (err #(rec/check-all! [(ledger :nope 7)] rec/every-check)))
                    [0 ::a])]
      (is (nil? (:doc f)))
      (is (= {:not-an-int :nope} (:returned f))))))

(deftest the-message-names-the-value-a-failure-belongs-to
  (let [ds [(ledger 1 "xxx") (ledger :nope "xxx")]
        m  (msg #(rec/check-all! ds ::a))]
    (is (re-find #"Ledger" m))
    (is (re-find #"1:" m) "the index of the value that failed")
    (is (re-find #":recognitum.bulk-test/a" m))))

(deftest a-single-value-still-reports-the-way-check-does
  (let [e (err #(rec/check! (ledger :nope "xxx") ::a))]
    (is (= ::rec/check-failed (:error e)))
    (is (nil? (:bulk e)) "the singular form is keyed by check name and says nothing of indices")
    (is (= [::a] (vec (keys (:failures e)))))))

(deftest the-bang-form-does-the-same-work-as-the-answering-one
  (let [ds [(crew-draft "one" #{:charlie} {:charlie 1})
            (crew-draft "two" #{:lyle}    {:lyle 1})]]
    (is (true? (rec/check-all! ds ::payout)))
    (is (= 1 (ran ::roster)) "one fetch for the group here too")
    (is (every? #(= #{::payout} (rec/certificates %)) ds))))

;;; ------------------------------- a value nobody wrote -------------------------------

(deftest a-field-nobody-wrote-is-judged-as-nil-for-that-value-alone
  ;; No pre-flight: a draft nobody finished is an ordinary draft holding nils, and the
  ;; shape validation over each of them says what it thinks. One incomplete value discards
  ;; nothing about the others.
  (let [ds [(ledger 1 "xxx") (assoc (rec/draft Ledger) ::a 1)]]
    (is (= [true {::b :nope}] (rec/check-all ds ::fits))
        "::fits waits on ::b, which judged the nil nobody wrote")
    (is (= #{::a ::b ::fits} (rec/passed (first ds)))
        "and the whole value was established, not held up by the other one")))

(deftest every-value-answers-for-itself
  (let [ds [(assoc (rec/draft Ledger) ::a 1)
            (ledger 1 "xxx")
            (rec/draft Ledger)]]
    (is (= [{::b :nope} true {::a {:not-an-int nil} ::b :nope}]
           (rec/check-all ds ::shape))
        "index-aligned, and each of them the whole of what that value owes")))

(deftest an-unwritten-field-behind-a-fetch-stops-that-values-fetch
  (let [ds [(crew-draft "one" #{:charlie} {:charlie 1})
            (assoc (rec/draft Crew) ::cuts {:lyle 1})]]
    (is (= [true {::crew false}] (rec/check-all ds ::known-get-cuts))
        "the gate over the field the fetch names is what the second value failed")
    (is (= 1 (ran ::roster)) "one fetch, for the value that earned it")
    (is (= [[#{:charlie}]] (first @handed))
        "and the incomplete value's tuple was not carried to the outside")))

;;; --------------------------------- what is refused ---------------------------------

(deftest every-value-must-be-a-value-of-this-library
  (let [e (err #(rec/check-all [(ledger 1 "xxx") 42] ::a))]
    (is (= ::rec/not-a-value (:error e)))
    (is (= 1 (:index e)) "and the one that is not is named by position")
    (is (= 42 (:value e))))
  (testing "and the bang forms refuse it the same way"
    (is (= ::rec/not-a-value (:error (err #(rec/check-all! [nil] ::a)))))
    (is (= ::rec/not-a-value (:error (err #(rec/check-all! [nil] ::shape)))))))

(deftest a-sealed-value-in-a-group-answers-true-and-runs-nothing
  ;; A sealed value is one every check passed on, so there is nothing to establish.
  (let [ds [(->Ledger 1 "xxx") (ledger 2 "xxx")]]
    (is (= [true true] (rec/check-all ds ::shape)))
    (is (true? (rec/check-all! ds rec/every-check)))))

(deftest a-check-or-certificate-the-type-does-not-declare-is-refused
  (doseq [f [#(rec/check-all [(ledger 1 "xxx")] ::nonesuch)
             #(rec/check-all! [(ledger 1 "xxx")] ::nonesuch)]]
    (let [e (err f)]
      (is (= ::rec/unknown-check (:error e)))
      (is (= ::nonesuch (:check e))))))

;;; ------------------------------- groups of more than one type -------------------------------

(deftest a-group-may-hold-drafts-of-more-than-one-type
  (let [ds [(ledger 1 "xxx") (sing #{:charlie}) (ledger 2 "xxx")]]
    (is (= [true true true] (rec/check-all ds rec/every-check))
        "the built-in resolves per type, and the answers stay index-aligned")
    (is (= 1 (ran ::roster)) "the type that fetches did; the one that does not did not")))

(deftest a-name-resolves-per-type
  ;; `::c` is `Sing`'s check and `::shape` is `Ledger`'s certificate; neither type knows
  ;; the other's name, which is what makes a shared name a per-type question.
  (let [ds [(ledger 1 "xxx") (sing #{:charlie})]]
    (is (= ::rec/unknown-check (:error (err #(rec/check-all ds ::shape))))
        "Sing has no ::shape, so the whole call is refused")
    (is (= ::rec/unknown-check (:error (err #(rec/check-all ds ::a))))
        "and Sing has no ::a either"))
  (testing "a name every type in the group knows resolves against each of them"
    (is (= [true true] (rec/check-all [(ledger 1 "xxx") (sing #{:charlie})]
                                      rec/every-check)))))

(deftest an-unknown-name-fails-the-whole-call-and-establishes-nothing
  (let [d  (ledger 1 "xxx")
        s  (sing #{:charlie})]
    (is (= ::rec/unknown-check (:error (err #(rec/check-all [d s] ::shape)))))
    (is (= #{} (rec/passed d)) "nothing ran for the value whose type did know the name")
    (is (= 0 (ran ::roster)))))

(deftest a-mixed-group-is-one-fetch-pass-per-type-and-batched-within-each
  (let [ds [(crew-draft "one"   #{:charlie} {:charlie 1})
            (sing #{:lyle})
            (crew-draft "two"   #{:charlie} {:charlie 2})
            (sing #{:lyle})
            (crew-draft "three" #{:lyle}    {:lyle 1})]]
    (is (= [true true true true true] (rec/check-all ds rec/every-check)))
    (is (= 1 (roster/call-count :fetch-rosters))
        "Crew's fetch is batched: one call for its three values")
    (is (= 1 (count @handed)))
    (is (= 2 (count (first @handed))) "two distinct tuples among them")
    (is (= 1 (roster/call-count :fetch-roster))
        "Sing's is singular, and its two values share one tuple")))

(deftest results-stay-index-aligned-across-a-mixed-group
  (let [ds [(ledger :nope "xxx") (sing "not a crew") (ledger 1 "xxx")]
        rs (rec/check-all ds rec/every-check)]
    (is (= 3 (count rs)))
    (is (= {::a {:not-an-int :nope}} (first rs)) "Ledger's check name")
    (is (= {::c false} (second rs)) "Sing's")
    (is (true? (nth rs 2)))))

(deftest a-mixed-group-throws-once-naming-every-type-that-failed
  (let [ds [(ledger :nope "xxx") (sing "not a crew")]
        e  (err #(rec/check-all! ds rec/every-check))]
    (is (= ::rec/check-failed (:error e)))
    (is (true? (:bulk e)))
    (is (= [0 1] (vec (keys (:failures e)))))
    (is (= ['Ledger 'Sing] (:record e))
        "one type is named as itself; more than one is named as all of them")
    (is (= [::a] (vec (keys (get (:failures e) 0)))))
    (is (= [::c] (vec (keys (get (:failures e) 1)))))))

(deftest a-mixed-group-leaves-every-value-its-own-verdicts
  (let [l (ledger 1 "xxx")
        g (sing #{:charlie})]
    (is (= [true true] (rec/check-all [l g] rec/every-check)))
    (is (= #{::a ::b ::fits} (rec/passed l)))
    (is (= #{::c ::mine} (rec/passed g)))))

(deftest the-built-in-certificate-covers-the-group-as-it-covers-one
  (let [ds [(ledger 1 "xxx") (ledger :nope 7) (ledger 9 "xx")]
        rs (rec/check-all ds rec/every-check)]
    (is (true? (first rs)))
    (is (= [::a ::b] (vec (keys (second rs))))
        "both shape validations, in declaration order; ::fits waits on the pair")
    (is (= [::fits] (vec (keys (nth rs 2))))
        "and where the shapes pass, the check that waits on them is what is left")))

;;; ------------------------------- a fetch that throws -------------------------------

(rec/defrecord Flaky
  [a]
  {:checks
   {::a       ([a] (int? a))
    ::has-row ([::row] (some? row))}

   :io-deps
   {::row ([a] (if (= a 13)
                 (throw (ex-info "the database is unreachable" {:down true}))
                 :ok))}})

(deftest a-fetch-that-throws-propagates-rather-than-becoming-a-failure
  (let [ds [(assoc (rec/draft Flaky) ::a 1) (assoc (rec/draft Flaky) ::a 13)]
        e  (try (rec/check-all ds ::has-row) nil
                (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e))
    (is (= {:down true} (ex-data e)))
    (is (nil? (:error (ex-data e))) "not a :check-failed -- it is not a claim about the data")))
