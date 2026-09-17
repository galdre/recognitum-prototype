(ns recognitum.check-test
  "Running one named check against a draft.

  `check` returns `true` or a map of what each failing check returned, keyed by check name;
  `check!` throws on failure. Neither returns a draft: each result is written into the
  draft's record of which checks pass. A check already recorded as passing does not run
  again, and a write clears the checks reading the field written. A check not named after
  a field waits on the shape validations of the fields it names, and where one of those
  fails the check asked about does not run and has no entry. A field never written reads as
  `nil`. Checking is not sealing: an undeclared key, or an unwritten field elsewhere, does
  not stop the check in hand."
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
    ::always ([] true)}
   :certificates {::shape #{::a ::b}}})

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
    ::boolean ([n] (pos? n))}
   :certificates {::most #{::data ::truthy ::nil}}})

;; Counts its own runs, so a check recorded as passing can be seen not to run again.
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
  nothing recorded as passing."
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
  (let [d (fresh)]
    (doseq [r [(rec/check d ::a) (rec/check! d ::b)]]
      (is (not (rec/draft? r)))
      (is (not (rec/sealed? r))))
    (is (= #{::a ::b} (rec/passed d))
        "the results are recorded on the draft, not returned")))

(deftest check-answers-with-whatever-the-failing-check-returned-keyed-by-its-name
  (let [d (assoc (rec/draft Reporting) ::n -1)]
    (is (= {::data {:too-small -1}} (rec/check d ::data)))
    (is (= {::truthy :nope} (rec/check d ::truthy))
        "truthy is not `true`, and the contract is `true` exactly")
    (is (= {::nil nil} (rec/check d ::nil))
        "a check that returned nil still has an entry -- the key is what says it failed")
    (is (= {::boolean false} (rec/check d ::boolean)))
    (is (= #{} (rec/passed d)) "none of them passed, so none of them are recorded")))

(deftest a-lone-check-is-a-certificate-containing-itself
  (let [d (assoc (rec/draft Reporting) ::n -1)]
    (is (= {::data {:too-small -1}} (rec/check d ::data)))
    (is (= {::data {:too-small -1} ::truthy :nope ::nil nil} (rec/check d ::most))
        "the certificate's answer has the same shape, with an entry per failing check"))
  (let [d (assoc (rec/draft Reporting) ::n 1)]
    (is (true? (rec/check d ::data)))
    (is (true? (rec/check d ::most)))))

(deftest a-check-name-and-a-certificate-name-go-through-the-same-verb
  (let [d (fresh)]
    (is (true? (rec/check d ::a)))
    (is (true? (rec/check d ::shape)))
    (is (true? (rec/check! d ::a)))
    (is (true? (rec/check! d ::shape)))
    (is (true? (rec/check d rec/every-check)) "including the built-in")))

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

;;; ------------------------------ the result is recorded ------------------------------

(deftest a-check-that-passes-is-recorded-as-passing
  (let [d (fresh)]
    (is (= #{} (rec/passed d)) "built from the type, so nothing has run")
    (rec/check d ::a)
    (is (= #{::a} (rec/passed d)))
    (rec/check! d ::b)
    (is (= #{::a ::b} (rec/passed d)))
    (rec/check d ::a-fits)
    (rec/check d ::always)
    (is (= all-checks (rec/passed d)))))

(deftest a-check-that-fails-is-not-recorded-as-passing
  (let [d (-> (rec/draft Ledger) (assoc ::a "not an int") (assoc ::b "xx"))]
    (is (= {::a false} (rec/check d ::a)))
    (is (= #{} (rec/passed d)))
    (is (true? (rec/check d ::b)))
    (is (= #{::b} (rec/passed d)) "and the one that passed alongside it is recorded")
    (testing "which check! records the same way before it throws"
      (is (= ::rec/check-failed (:error (err #(rec/check! d ::a-fits)))))
      (is (= #{::b} (rec/passed d))))))

(deftest checking-runs-what-it-waits-on-and-nothing-else
  (let [d (fresh)]
    (rec/check d ::a-fits)
    (is (= #{::a ::b ::a-fits} (rec/passed d))
        "a check spanning two fields runs their shape validations on the way")
    (is (not (contains? (rec/passed d) ::always))
        "and nothing it does not wait on")))

(deftest a-result-recorded-on-one-version-says-nothing-about-another
  (let [d  (fresh)
        d' (assoc d ::a 2)]
    (is (true? (rec/check d ::a)))
    (is (= #{::a} (rec/passed d)))
    (is (= #{} (rec/passed d')) "d' was seeded from d before the check ran")
    (testing "and running it on d' leaves d where it was"
      (is (true? (rec/check d' ::a)))
      (is (= #{::a} (rec/passed d)))
      (is (= #{::a} (rec/passed d'))))))

;;; ---------------------- a check already passing is not run again ----------------------

(deftest a-check-already-passing-is-not-run-again
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::n)))
    (is (= 1 (ran ::n)))
    (is (true? (rec/check d ::n)))
    (is (= 1 (ran ::n)) "it is recorded as passing, so there is nothing to run")
    (is (true? (rec/check! d ::n)))
    (is (= 1 (ran ::n)) "and check! runs nothing either")))

(deftest a-draft-of-a-sealed-value-runs-nothing
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (let [d (rec/draft c)]
      (is (= #{::n ::s ::both} (rec/passed d)))
      (doseq [k [::n ::s ::both]] (is (true? (rec/check d k)) (pr-str k)))
      (is (= {} @runs)
          "sealed means every check passed on this data; running them again would learn nothing"))))

(deftest a-write-makes-the-checks-naming-that-field-run-again
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (let [d (assoc (rec/draft c) ::n 2)]
      (is (= #{::s} (rec/passed d)) "the write cleared ::n and ::both")
      (is (true? (rec/check d ::n)))
      (is (true? (rec/check d ::both)))
      (is (true? (rec/check d ::s)))
      (is (= {::n 1 ::both 1} @runs)
          "::s does not name n, so it stayed recorded as passing and was not run"))))

(deftest a-failed-check-is-run-again-next-time
  (reset! runs {})
  (let [d (assoc (rec/draft Counted) ::n "not an int")]
    (is (= {::n false} (rec/check d ::n)))
    (is (= {::n false} (rec/check d ::n)))
    (is (= 2 (ran ::n)))
    (is (= #{} (rec/passed d)))))

(deftest a-pure-check-waits-on-the-shape-validations-of-the-fields-it-names
  ;; `::both` is passed an n and an s that `::n` and `::s` have already passed, so it
  ;; restates neither.
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::both)))
    (is (= {::n 1 ::s 1 ::both 1} @runs))
    (is (= #{::n ::s ::both} (rec/passed d))))
  (testing "and a shape validation already passing is not run again"
    (reset! runs {})
    (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
      (is (true? (rec/check d ::n)))
      (is (true? (rec/check d ::s)))
      (reset! runs {})
      (is (true? (rec/check d ::both)))
      (is (= {::both 1} @runs)))))

;;; ---------------------------- a check needs something to read ----------------------------

(deftest a-check-naming-a-field-never-written-judges-nil
  (let [d (rec/draft Ledger)]
    (is (= {::a false} (rec/check d ::a)) "`(int? nil)`, which is the check's own answer")
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::a)))))
    (testing "and nothing is recorded, because nothing passed"
      (is (= #{} (rec/passed d))))))

(deftest a-check-whose-shape-validation-fails-answers-with-that-failure-and-no-entry-of-its-own
  (let [d (rec/draft Ledger)
        r (rec/check d ::a-fits)]
    (is (= [::a ::b] (vec (keys r))) "both shape validations it was waiting on")
    (is (not (contains? r ::a-fits)) "the check asked about never ran, so it has no entry"))
  (is (= [::b] (vec (keys (rec/check (assoc (rec/draft Ledger) ::a 1) ::a-fits))))
      "and only the one still in the way")
  (testing "which is what keeps the body from being handed a shape it cannot read"
    (let [d (rec/draft Ledger)]
      (is (map? (rec/check d ::a-fits))
          "rather than a NullPointerException out of (count nil)")))
  (testing "and check! throws the same failures it would for any other"
    (let [e (err #(rec/check! (rec/draft Ledger) ::a-fits))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::a ::b] (vec (keys (:failures e))))))))

(deftest an-unwritten-field-reaches-a-check-body-as-nil
  (reset! runs {})
  (is (= {::n false} (rec/check (rec/draft Counted) ::n)))
  (is (= {::n 1} @runs) "the body ran, and what it was given was nil"))

(deftest writing-nil-is-the-same-as-never-writing
  (is (true? (rec/check (assoc (rec/draft Maybe) ::n nil) ::m))
      "nil is a legal value where a check admits it")
  (is (true? (rec/check (rec/draft Maybe) ::m))
      "and never writing it is the same thing, judged the same way"))

(deftest a-check-naming-no-field-runs-against-an-empty-draft
  (let [d (rec/draft Ledger)]
    (is (true? (rec/check d ::always)))
    (is (= #{::always} (rec/passed d)))))

;;; ------------------------------ checking is not sealing ------------------------------

(deftest an-incomplete-draft-can-still-run-the-checks-it-has-fields-for
  (let [d (assoc (rec/draft Ledger) ::b "xxx")]
    (is (true? (rec/check d ::b)))
    (is (= #{::b} (rec/passed d)))
    (let [e (err #(rec/seal d))]
      (is (= ::rec/check-failed (:error e))
          "sealing still runs everything -- running a check early is not a claim to be whole")
      (is (= [::a] (keys (:failures e)))
          "::a judges the nil nobody wrote; ::a-fits waits on ::a and does not run"))))

(deftest an-undeclared-key-does-not-stop-a-check
  (let [d (assoc (fresh) :getaway-car "mini")]
    (is (true? (rec/check d ::a)))
    (is (true? (rec/check! d ::a-fits)))
    (is (= ::rec/extraneous-keys (:error (err #(rec/seal d))))
        "which sealing refuses")))

(deftest a-check-that-throws-propagates-and-records-nothing
  (let [d (assoc (rec/draft Fragile) ::n "not a number")]
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check d ::boom)))
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check! d ::boom)))
    (is (= #{} (rec/passed d)))))

;;; ----------------------------- what it refuses to answer -----------------------------

(deftest a-check-name-the-type-does-not-declare-is-refused
  (let [d (fresh)]
    (doseq [[label f] [["check" #(rec/check d ::nonesuch)]
                       ["check!" #(rec/check! d ::nonesuch)]]]
      (let [e (err f)]
        (is (= ::rec/unknown-check (:error e)) label)
        (is (= 'Ledger (:record e)) label)
        (is (= ::nonesuch (:check e)) label)
        (is (= [::a ::b ::a-fits ::always] (:checks e)) label)
        (is (= [::shape ::rec/every-check] (:certificates e))
            (str label ": the refusal names both the checks and the certificates it could have been"))))
    (testing "including a check declared on some other type"
      (is (= ::rec/unknown-check (:error (err #(rec/check d ::data))))))
    (testing "and the bare spelling of one this type does declare"
      (is (= ::rec/unknown-check (:error (err #(rec/check d :a))))))
    (testing "and a certificate declared on some other type"
      (is (= ::rec/unknown-check (:error (err #(rec/check d ::most))))))
    (testing "and nothing is recorded by asking"
      (is (= #{} (rec/passed d))))))

(deftest a-sealed-value-answers-true-and-runs-nothing
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (is (true? (rec/check c ::n)))
    (is (true? (rec/check c ::both)))
    (is (true? (rec/check c rec/every-check)))
    (is (true? (rec/check! c rec/every-check)))
    (is (= {} @runs) "nothing ran"))
  (testing "and an unknown name is refused there too"
    (is (= ::rec/unknown-check (:error (err #(rec/check led ::nonesuch)))))))

(deftest checking-is-a-question-about-a-recognitum-value
  (doseq [x [Ledger {::a 1 ::b "xxx" ::c :x} nil 42]]
    (is (= ::rec/not-a-value (:error (err #(rec/check x ::a)))) (pr-str x))
    (is (= ::rec/not-a-value (:error (err #(rec/check! x ::a)))) (pr-str x))
    (is (= ::rec/not-a-value (:error (err #(rec/passed x)))) (pr-str x))
    (is (= ::rec/not-a-value (:error (err #(rec/certificates x)))) (pr-str x))))

;;; -------------------------------- and the guarantee --------------------------------

(deftest running-every-check-by-hand-still-seals
  (let [d (fresh)]
    (doseq [c [::a ::b ::a-fits ::always]] (is (true? (rec/check d c))))
    (let [s (rec/seal d)]
      (is (rec/sealed? s))
      (is (= 1 (::a s))))))

(deftest a-pass-recorded-before-a-write-does-not-cover-the-write
  (let [d (fresh)]
    (doseq [c [::a ::b ::a-fits ::always]] (rec/check d c))
    (let [d' (assoc d ::a "not an int")]
      (is (= #{::b ::always} (rec/passed d')) "the write cleared ::a and ::a-fits")
      (let [e (err #(rec/seal d'))]
        (is (= ::rec/check-failed (:error e)))
        (is (= [::a] (keys (:failures e)))
            "::a-fits waits on ::a, so the shape failure is the whole of the report")))))

(deftest a-check-inside-a-drafting-form-answers-about-the-draft-in-hand
  (let [a (atom nil)]
    (is (rec/sealed?
         (rec/drafting [x Ledger]
           (let [x (-> x (assoc ::a 1) (assoc ::b "xxx") (assoc ::c :anything))]
             (reset! a [(rec/check x ::a) (rec/check x ::a-fits) (rec/passed x)])
             x))))
    (is (= [true true #{::a ::b ::a-fits}] @a)
        "::b came with ::a-fits, which waits on it")))
