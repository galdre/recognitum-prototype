(ns recognitum.check-test
  "Running one named check against a draft.

  `check` answers the question and `check!` insists on the answer: both return `true` when
  the check passes, and neither returns a draft. There is nowhere for a verdict to go in a
  return value, because the verdict goes in the draft's mask -- which is what frees the
  return value to be the answer to the question that was asked.

  What `check` means is *establish this*, not *run exactly this*. A check whose bit is
  already set has been established against exactly this data, so there is nothing to do and
  it is not run again; a write cleared the bit of everything naming the field written, so
  after a write it runs. That is the whole value of the mask: the evidence behind a verdict
  is not kept, but the verdict is.

  A check with nothing to run against cannot be run at all. A field the draft has never
  been written to is not `nil` -- the two are different things everywhere else in this
  library, and a check body must never be handed the sentinel that tells them apart -- so a
  check naming an unwritten field reports `:missing-field` rather than judging a value
  nobody wrote.

  Checking is not sealing. An undeclared key, an unwritten field some other check names, a
  check that has not been run: none of them stop the check in hand. That is what running
  checks early on an incomplete value means."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates, which is the point where a check body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ---------------------------------------- types ----------------------------------------

;; `a` and `b` are each named by a check of their own and together by a third; `c` is named
;; by no check at all; `::always` names no field, so it can run against an empty draft.
(rec/defrecord Ledger
  [a b c]
  {:checks
   {::a      int?
    ::b      ([b] (string? b))
    ::a-fits ("a fits inside b" [a b] (and (int? a) (string? b) (< a (count b))))
    ::always ([] true)}})

(def led (->Ledger 1 "xxx" :anything))

(def all-checks #{::a ::b ::a-fits ::always})

;; What each check returns when it disapproves, so a failure is visibly the check's own
;; return value rather than something the library supplied.
(rec/defrecord Reporting
  [n]
  {:checks
   {::data    ("n is positive" [n] (if (pos? n) true {:too-small n}))
    ::truthy  ([n] (if (pos? n) true :nope))
    ::nil     ([n] (if (pos? n) true nil))
    ::boolean ([n] (pos? n))}})

;; Counts its own runs, which is the only way to see that an established verdict is not
;; re-established.
(def ^:private runs (atom {}))

(defn- tally! [k] (swap! runs update k (fnil inc 0)) nil)

(defn- ran [k] (get @runs k 0))

(rec/defrecord Counted
  [n s]
  {:checks
   {::n    ([n]   (do (tally! ::n) (int? n)))
    ::s    ([s]   (do (tally! ::s) (string? s)))
    ::both ([n s] (do (tally! ::both) (and (int? n) (string? s))))}})

;; `nil` is a legal value here, which is what makes "never written" a different thing.
(rec/defrecord Maybe
  [n]
  {:checks {::m ([n] (or (nil? n) (int? n)))}})

(rec/defrecord Fragile
  [n]
  {:checks
   {::boom ([n] (if (number? n) true (throw (IllegalArgumentException. "boom"))))}})

(defn- fresh
  "A draft of `Ledger` with every field written, so nothing is waiting on anything, and
  nothing established."
  []
  (-> (rec/draft Ledger) (assoc ::a 1) (assoc ::b "xxx") (assoc ::c :anything)))

;;; -------------------------------- what they hand back --------------------------------

(deftest a-passing-check-answers-true
  (let [d (fresh)]
    (is (true? (rec/check d ::a)))
    (is (true? (rec/check d ::b)))
    (is (true? (rec/check d ::a-fits)))
    (is (true? (rec/check d ::always))))
  (testing "and check! answers exactly the same way"
    (let [d (fresh)]
      (doseq [c [::a ::b ::a-fits ::always]]
        (is (true? (rec/check! d c)) (pr-str c))))))

(deftest neither-form-returns-a-draft
  ;; The verdict goes in the mask, so the return value is free to be the answer.
  (let [d (fresh)]
    (doseq [r [(rec/check d ::a) (rec/check! d ::b)]]
      (is (not (rec/draft? r)))
      (is (not (rec/sealed? r))))
    (is (= #{::a ::b} (rec/passing d))
        "what was learned is on the draft that was asked, not in what came back")))

(deftest check-answers-with-whatever-the-failing-check-returned
  (let [d (assoc (rec/draft Reporting) ::n -1)]
    (is (= {:too-small -1} (rec/check d ::data)))
    (is (= :nope (rec/check d ::truthy))
        "truthy is not `true`, and the contract is `true` exactly")
    (is (nil? (rec/check d ::nil)))
    (is (false? (rec/check d ::boolean)))
    (is (= #{} (rec/passing d)) "none of them passed, so none of them are recorded")))

(deftest check-bang-throws-one-check-failed-carrying-the-whole-report
  (let [d (assoc (rec/draft Reporting) ::n -1)
        e (err #(rec/check! d ::data))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 'Reporting (:record e)))
    (is (= [::data] (keys (:failures e))) "one check was asked, so one failure is reported")
    (let [f (get (:failures e) ::data)]
      (is (= "n is positive" (:doc f)))
      (is (= {::n -1} (:values f)) "the fields it declared, and what they held")
      (is (= {:too-small -1} (:returned f))))))

(deftest check-bang-reports-the-declared-fields-in-arglist-order
  (let [d (-> (rec/draft Ledger) (assoc ::a 99) (assoc ::b "xx"))
        e (err #(rec/check! d ::a-fits))]
    (is (= ::rec/check-failed (:error e)))
    (let [f (get (:failures e) ::a-fits)]
      (is (= "a fits inside b" (:doc f)))
      (is (= [::a ::b] (keys (:values f))))
      (is (= {::a 99 ::b "xx"} (:values f)))
      (is (= false (:returned f))))))

;;; ------------------------------ the verdict is recorded ------------------------------

(deftest a-check-that-passes-sets-its-bit
  (let [d (fresh)]
    (is (= #{} (rec/passing d)) "built from the type, so nothing was established")
    (rec/check d ::a)
    (is (= #{::a} (rec/passing d)))
    (rec/check! d ::b)
    (is (= #{::a ::b} (rec/passing d)))
    (rec/check d ::a-fits)
    (rec/check d ::always)
    (is (= all-checks (rec/passing d)))))

(deftest a-check-that-fails-leaves-its-bit-clear
  (let [d (-> (rec/draft Ledger) (assoc ::a "not an int") (assoc ::b "xx"))]
    (is (false? (rec/check d ::a)))
    (is (= #{} (rec/passing d)))
    (is (true? (rec/check d ::b)))
    (is (= #{::b} (rec/passing d)) "and the one that passed alongside it is recorded")
    (testing "which check! records the same way before it throws"
      (is (= ::rec/check-failed (:error (err #(rec/check! d ::a-fits)))))
      (is (= #{::b} (rec/passing d))))))

(deftest checking-touches-no-verdict-but-its-own
  (let [d (fresh)]
    (rec/check d ::a-fits)
    (is (= #{::a-fits} (rec/passing d))
        "a check spanning two fields establishes itself and nothing else")))

(deftest a-verdict-established-on-one-version-says-nothing-about-another
  ;; The seed is a snapshot, so versions diverge rather than share.
  (let [d  (fresh)
        d' (assoc d ::a 2)]
    (is (true? (rec/check d ::a)))
    (is (= #{::a} (rec/passing d)))
    (is (= #{} (rec/passing d')) "d' was seeded from d before the check ran")
    (testing "and establishing it on d' leaves d where it was"
      (is (true? (rec/check d' ::a)))
      (is (= #{::a} (rec/passing d)))
      (is (= #{::a} (rec/passing d'))))))

;;; -------------------------- establish, rather than re-run --------------------------

(deftest a-check-already-passing-is-not-run-again
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::n)))
    (is (= 1 (ran ::n)))
    (is (true? (rec/check d ::n)))
    (is (= 1 (ran ::n)) "the verdict stands, so there is nothing to establish")
    (is (true? (rec/check! d ::n)))
    (is (= 1 (ran ::n)) "and check! is no different -- it insists on the answer, not the work")))

(deftest a-draft-of-a-sealed-value-has-nothing-left-to-establish
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (let [d (rec/draft c)]
      (is (= #{::n ::s ::both} (rec/passing d)))
      (doseq [k [::n ::s ::both]] (is (true? (rec/check d k)) (pr-str k)))
      (is (= {} @runs)
          "sealed means every check passed on this data; running them again would learn nothing"))))

(deftest a-write-makes-the-checks-naming-that-field-run-again
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (let [d (assoc (rec/draft c) ::n 2)]
      (is (= #{::s} (rec/passing d)) "the write cleared ::n and ::both")
      (is (true? (rec/check d ::n)))
      (is (true? (rec/check d ::both)))
      (is (true? (rec/check d ::s)))
      (is (= {::n 1 ::both 1} @runs)
          "::s does not name n, so its verdict stood and it was not run"))))

(deftest a-failed-check-is-run-again-next-time
  ;; Failures are not retained: the call that finds one returns it, and a later call
  ;; re-runs anything not passing and produces it again.
  (reset! runs {})
  (let [d (assoc (rec/draft Counted) ::n "not an int")]
    (is (false? (rec/check d ::n)))
    (is (false? (rec/check d ::n)))
    (is (= 2 (ran ::n)))
    (is (= #{} (rec/passing d)))))

(deftest a-pure-check-has-no-prerequisites
  ;; Checks have no ordering among themselves, so establishing one establishes nothing
  ;; else -- a check spanning two fields restates the shapes it needs rather than
  ;; assuming the checks over those fields ran first.
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::both)))
    (is (= {::both 1} @runs))
    (is (= #{::both} (rec/passing d)))))

;;; ---------------------------- a check needs something to read ----------------------------

(deftest a-check-naming-a-field-never-written-cannot-run
  (let [d (rec/draft Ledger)]
    (let [e (err #(rec/check d ::a))]
      (is (= ::rec/missing-field (:error e)))
      (is (= 'Ledger (:record e)))
      (is (= ::a (:check e)))
      (is (= ::a (:key e))))
    (testing "check! says the same thing -- an unwritten field is not a verdict either way"
      (is (= ::rec/missing-field (:error (err #(rec/check! d ::a))))))
    (testing "and nothing is recorded, because nothing was determined"
      (is (= #{} (rec/passing d))))))

(deftest it-names-every-field-it-is-waiting-on-in-arglist-order
  (let [d (rec/draft Ledger)]
    (is (= [::a ::b] (:keys (err #(rec/check d ::a-fits)))))
    (is (= [::b] (:keys (err #(rec/check (assoc d ::a 1) ::a-fits)))))
    (is (= [::a] (:keys (err #(rec/check (assoc d ::b "xx") ::a-fits)))))))

(deftest the-sentinel-for-never-written-never-reaches-a-check-body
  (reset! runs {})
  (is (= ::rec/missing-field (:error (err #(rec/check (rec/draft Counted) ::n)))))
  (is (= {} @runs) "the body was never called, so it cannot have seen what it must not see"))

(deftest written-to-nil-is-not-never-written
  (is (true? (rec/check (assoc (rec/draft Maybe) ::n nil) ::m))
      "nil is a legal value where a check admits it")
  (is (= ::rec/missing-field (:error (err #(rec/check (rec/draft Maybe) ::m))))
      "and absent is a different thing entirely"))

(deftest a-check-naming-no-field-runs-against-an-empty-draft
  (let [d (rec/draft Ledger)]
    (is (true? (rec/check d ::always)))
    (is (= #{::always} (rec/passing d)))))

;;; ------------------------------ checking is not sealing ------------------------------

(deftest an-incomplete-draft-can-still-establish-what-it-has
  (let [d (assoc (rec/draft Ledger) ::b "xxx")]
    (is (true? (rec/check d ::b)))
    (is (= #{::b} (rec/passing d)))
    (is (= ::rec/missing-field (:error (err #(rec/seal d))))
        "sealing still wants every field -- running a check early is not a claim to be whole")))

(deftest an-undeclared-key-does-not-stop-a-check
  (let [d (assoc (fresh) :getaway-car "mini")]
    (is (true? (rec/check d ::a)))
    (is (true? (rec/check! d ::a-fits)))
    (is (= ::rec/extraneous-keys (:error (err #(rec/seal d))))
        "which sealing refuses, as it always did")))

(deftest a-check-that-throws-propagates-and-records-nothing
  ;; A throw is a bug in the check rather than a verdict about the data, so there is
  ;; nothing to write down and nothing to report as invalid.
  (let [d (assoc (rec/draft Fragile) ::n "not a number")]
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check d ::boom)))
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check! d ::boom)))
    (is (= #{} (rec/passing d)))))

;;; ----------------------------- what it refuses to answer -----------------------------

(deftest a-check-name-the-type-does-not-declare-is-refused
  (let [d (fresh)]
    (doseq [[label f] [["check" #(rec/check d ::nonesuch)]
                       ["check!" #(rec/check! d ::nonesuch)]]]
      (let [e (err f)]
        (is (= ::rec/unknown-check (:error e)) label)
        (is (= 'Ledger (:record e)) label)
        (is (= ::nonesuch (:check e)) label)
        (is (= [::a ::b ::a-fits ::always] (:checks e)) label)))
    (testing "including a check declared on some other type"
      (is (= ::rec/unknown-check (:error (err #(rec/check d ::data))))))
    (testing "and the bare spelling of one this type does declare"
      (is (= ::rec/unknown-check (:error (err #(rec/check d :a))))))
    (testing "and nothing is established by asking"
      (is (= #{} (rec/passing d))))))

(deftest checking-is-a-question-about-a-draft
  (doseq [x [led Ledger {::a 1 ::b "xxx" ::c :x} nil 42]]
    (is (= ::rec/not-a-draft (:error (err #(rec/check x ::a)))) (pr-str x))
    (is (= ::rec/not-a-draft (:error (err #(rec/check! x ::a)))) (pr-str x))))

;;; -------------------------------- and the guarantee --------------------------------

(deftest establishing-every-check-by-hand-still-seals
  (let [d (fresh)]
    (doseq [c [::a ::b ::a-fits ::always]] (is (true? (rec/check d c))))
    (let [s (rec/seal d)]
      (is (rec/sealed? s))
      (is (= 1 (::a s))))))

(deftest a-verdict-established-before-a-write-does-not-cover-the-write
  (let [d (fresh)]
    (doseq [c [::a ::b ::a-fits ::always]] (rec/check d c))
    (let [d' (assoc d ::a "not an int")]
      (is (= #{::b ::always} (rec/passing d')) "the write cleared ::a and ::a-fits")
      (let [e (err #(rec/seal d'))]
        (is (= ::rec/check-failed (:error e)))
        (is (= [::a ::a-fits] (keys (:failures e))))))))

(deftest a-check-inside-a-drafting-form-answers-about-the-draft-in-hand
  (let [a (atom nil)]
    (is (rec/sealed?
         (rec/drafting [x Ledger]
           (let [x (-> x (assoc ::a 1) (assoc ::b "xxx") (assoc ::c :anything))]
             (reset! a [(rec/check x ::a) (rec/check x ::a-fits) (rec/passing x)])
             x))))
    (is (= [true true #{::a ::a-fits}] @a))))
