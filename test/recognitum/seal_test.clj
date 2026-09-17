(ns recognitum.seal-test
  "What `seal` runs. `seal` returns a value validated as if every check had run; which
  checks are called is the implementation underneath that. It runs exactly the checks the
  draft does not record as passing, which is sound because a draft of a sealed value starts
  with every check passing, a draft of the type starts with none, and a write clears
  exactly the checks reading the field written. So work done on a draft with `check` is not
  repeated at the seal, and a failure, which is not recorded, runs again."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))
(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates, which is where a check body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ---------------------------------------- types ----------------------------------------

;; Every check counts its own calls, since a check seal ran and one it took as passing
;; produce the same value.
(def ^:private runs (atom {}))

(defn- tally! [k] (swap! runs update k (fnil inc 0)))

(defn- ran [k] (get @runs k 0))

(defn- reset-runs! [] (reset! runs {}))

;; `a` and `b` are each named by a check of their own and together by a third; `c` is a
;; declared field no check names; `::always` names no field at all, so no write can clear it
;; and a draft of a sealed value has nothing left to learn about it.
(rec/defrecord Ledger
  [a b c]
  {:checks
   {::a      ([a] (do (tally! ::a) (int? a)))
    ::b      ([b] (do (tally! ::b) (string? b)))
    ::a-fits ("a fits inside b"
              [a b] (do (tally! ::a-fits)
                        (and (int? a) (string? b) (< a (count b)))))
    ::always ([] (do (tally! ::always) true))}
   :certificates
   {::shape #{::a ::b}}})

(def led (->Ledger 1 "xxx" :anything))

(def all-checks [::a ::b ::a-fits ::always])

(defn- counts [] (into {} (map (juxt identity ran)) all-checks))

(defn- fresh
  "A draft built from the type with every field written: complete, and with nothing
  recorded as passing."
  []
  (-> (rec/draft Ledger) (assoc ::a 1) (assoc ::b "xxx") (assoc ::c :anything)))

;; Two fields, so a write can clear one check without clearing the other.
(rec/defrecord Fragile
  [n m]
  {:checks
   {::n    ([n] (do (tally! ::n) (number? n)))
    ::boom ([m] (do (tally! ::boom)
                    (if (number? m) true (throw (IllegalArgumentException. "boom")))))}})

;;; ------------------------------- a draft of the type -------------------------------

(deftest sealing-a-draft-of-the-type-runs-every-check
  (reset-runs!)
  (let [d (fresh)]
    (is (= #{} (rec/passed d)) "nothing recorded as passing")
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "nothing was recorded as passing, so every check ran -- once each")))

(deftest the-constructors-run-every-check
  (reset-runs!)
  (is (rec/sealed? (map->Ledger {::a 1 ::b "xxx" ::c :anything})))
  (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
      "map->Ledger builds an empty draft and seals it, so nothing is skipped"))

;;; ---------------------------- a draft of a sealed value ----------------------------

(deftest sealing-an-untouched-draft-of-a-sealed-value-runs-nothing
  (reset-runs!)
  (let [d (rec/draft led)
        s (rec/seal d)]
    (is (rec/sealed? s))
    (is (= led s))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts)))))

(deftest the-check-naming-no-field-is-not-re-run-either
  ;; A check with an empty arglist reads nothing, so no write clears it and a draft of a
  ;; sealed value has it passing.
  (reset-runs!)
  (rec/seal (assoc (rec/draft led) ::a 2))
  (is (= 0 (ran ::always)) "recorded as passing from the sealed value, and nothing cleared it"))

(deftest a-drafting-form-over-a-sealed-value-that-writes-nothing-runs-nothing
  (reset-runs!)
  (is (= led (rec/drafting [x led] x)))
  (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))))

;;; ------------------------------ a write selects the work ------------------------------

(deftest sealing-runs-exactly-the-checks-the-write-cleared
  (testing "a names ::a and ::a-fits"
    (reset-runs!)
    (rec/seal (assoc (rec/draft led) ::a 2))
    (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))))
  (testing "b names ::b and ::a-fits"
    (reset-runs!)
    (rec/seal (assoc (rec/draft led) ::b "yyyy"))
    (is (= {::a 0 ::b 1 ::a-fits 1 ::always 0} (counts))))
  (testing "and the two together run each of the three once, not ::a-fits twice"
    (reset-runs!)
    (rec/seal (-> (rec/draft led) (assoc ::a 2) (assoc ::b "yyyy")))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 0} (counts)))))

(deftest a-write-to-a-field-no-check-names-leaves-nothing-to-run
  (reset-runs!)
  (let [s (rec/seal (assoc (rec/draft led) ::c :something-else))]
    (is (= :something-else (::c s)))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))
        "c is declared and no check reads it -- no verdict can have moved")))

(deftest writing-the-same-value-back-still-costs-the-checks-naming-it
  ;; What a write clears is computed from the arglists, not by comparing values.
  (reset-runs!)
  (rec/seal (assoc (rec/draft led) ::a (::a led)))
  (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))))

(deftest repeated-writes-to-one-field-cost-one-round-of-checks
  (reset-runs!)
  (rec/seal (reduce (fn [d _] (assoc d ::a 2)) (rec/draft led) (range 20)))
  (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))
      "N writes under one drafting cost one round at the seal, not N"))

;;; ---------------------------- work done on the draft is kept ----------------------------

(deftest a-check-run-by-hand-is-not-run-again-at-the-seal
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d ::a)))
    (is (= 1 (ran ::a)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "::a was already passing; the other three were not")))

(deftest running-everything-by-hand-leaves-the-seal-no-work
  (reset-runs!)
  (let [d (fresh)]
    (doseq [c all-checks] (is (true? (rec/check d c))))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "the seal ran nothing: every check was already recorded as passing")))

(deftest check-bang-saves-the-same-work
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check! d ::a-fits)))
    (rec/seal d)
    (is (= 1 (ran ::a-fits)))))

(deftest a-certificate-run-by-hand-is-not-run-again-at-the-seal
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d ::shape)))
    (is (= {::a 1 ::b 1 ::a-fits 0 ::always 0} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "only what ::shape does not cover was left for the seal")))

(deftest a-draft-with-every-check-passing-seals-without-running-anything
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d rec/every-check)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))))

(deftest a-pass-recorded-before-a-write-does-not-cover-the-write
  (reset-runs!)
  (let [d (fresh)]
    (rec/check d rec/every-check)
    (reset-runs!)
    (rec/seal (assoc d ::b "yyyy"))
    (is (= {::a 0 ::b 1 ::a-fits 1 ::always 0} (counts))
        "the write cleared exactly ::b and ::a-fits, and those are what ran")))

;;; ------------------------- the record travels with the draft -------------------------

(deftest operations-that-are-not-writes-do-not-cost-the-seal-anything
  (testing "with-meta"
    (reset-runs!)
    (rec/seal (with-meta (rec/draft led) {:user-key 1}))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))))
  (testing "clean, having stripped an undeclared key"
    (reset-runs!)
    (rec/seal (rec/clean (assoc (rec/draft led) :getaway-car "mini")))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))))
  (testing "dissoc of an undeclared key"
    (reset-runs!)
    (rec/seal (-> (rec/draft led) (assoc :getaway-car "mini") (dissoc :getaway-car)))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))))
  (testing "adoption by a drafting form"
    (reset-runs!)
    (is (rec/sealed? (rec/drafting [x (rec/draft led)] x)))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts)))))

(deftest a-draft-passed-through-a-nested-drafting-keeps-what-it-recorded
  (reset-runs!)
  (let [inner (fn [x] (rec/drafting [y x] (assoc y ::a 2)))]
    (is (rec/sealed? (rec/drafting [x led] (inner x))))
    (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))
        "only the outer form sealed, and it ran only what the inner write cleared")))

;;; -------------------------------- failures are not saved --------------------------------

(deftest a-failed-check-runs-again-at-the-seal
  (reset-runs!)
  (let [d (-> (rec/draft Ledger) (assoc ::a "not an int") (assoc ::b "xxx") (assoc ::c :x))]
    (is (= {::a false} (rec/check d ::a)))
    (is (= 1 (ran ::a)))
    (let [e (err #(rec/seal d))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::a] (vec (keys (:failures e))))
          "::a-fits waits on ::a, so it is not run against what ::a rejected"))
    (is (= 2 (ran ::a)) "the failing check ran again")))

(deftest a-second-seal-runs-only-what-is-still-not-passing
  (reset-runs!)
  (let [d (-> (rec/draft Ledger) (assoc ::a 99) (assoc ::b "xxx") (assoc ::c :x))]
    (is (= ::rec/check-failed (:error (err #(rec/seal d)))) "99 does not fit inside \"xxx\"")
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))
    (is (= #{::a ::b ::always} (rec/passed d)) "the passes were written down")
    (is (= ::rec/check-failed (:error (err #(rec/seal d)))))
    (is (= {::a 1 ::b 1 ::a-fits 2 ::always 1} (counts))
        "only the check that failed was run again")))

(deftest seal-reports-every-failure-shape-validations-first
  (reset-runs!)
  (let [e (err #(rec/seal (-> (rec/draft Ledger)
                              (assoc ::a "not an int") (assoc ::b :not-a-string)
                              (assoc ::c :x))))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::a ::b] (vec (keys (:failures e))))
        "two failures, one throw, in the order the type declares its checks")
    (is (= {::a 1 ::b 1 ::a-fits 0 ::always 1} (counts))
        "neither shape failure stopped the pass, and ::a-fits waits on both of them"))
  (testing "and where the shapes hold, what waits on them is what is reported"
    (reset-runs!)
    (let [e (err #(rec/seal (-> (rec/draft Ledger)
                                (assoc ::a 99) (assoc ::b "xx") (assoc ::c :x))))]
      (is (= [::a-fits] (vec (keys (:failures e)))))
      (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))))))

(deftest fixing-the-field-and-sealing-again-works
  (reset-runs!)
  (let [d (-> (rec/draft Ledger) (assoc ::a 99) (assoc ::b "xxx") (assoc ::c :x))]
    (is (= ::rec/check-failed (:error (err #(rec/seal d)))))
    (let [s (rec/seal (assoc d ::a 1))]
      (is (rec/sealed? s))
      (is (= 1 (::a s)))
      (is (= {::a 2 ::b 1 ::a-fits 2 ::always 1} (counts))
          "the write cleared ::a and ::a-fits; ::b and ::always were still passing"))))

;;; ------------------------------ a check that throws ------------------------------

(deftest a-check-that-throws-records-nothing-and-loses-nothing-else
  (reset-runs!)
  (let [d (-> (rec/draft Fragile) (assoc ::n 1) (assoc ::m "not a number"))]
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/seal d)))
    (is (= #{::n} (rec/passed d))
        "::n reached a verdict and keeps it; ::boom reached none")
    (is (= 1 (ran ::n)))
    (is (= 1 (ran ::boom)))
    (testing "so a second seal runs only the check that has no verdict"
      (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/seal d)))
      (is (= 1 (ran ::n)))
      (is (= 2 (ran ::boom))))))

;;; --------------------- what seal refuses, it refuses before running ---------------------

(deftest a-field-never-written-is-judged-as-nil
  ;; The field holds nil, and its shape validation is what says whether that will do; `c`
  ;; is named by no check, so it seals holding nil.
  (reset-runs!)
  (let [e (err #(rec/seal (assoc (rec/draft Ledger) ::a 1)))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::b] (vec (keys (:failures e)))))
    (is (= {::b nil} (:values (get (:failures e) ::b)))))
  (is (= {::a 1 ::b 1 ::a-fits 0 ::always 1} (counts))
      "every check that could run did; ::a-fits waits on ::b"))

(deftest undeclared-keys-are-refused-before-any-check-runs
  (reset-runs!)
  (let [e (err #(rec/seal (assoc (fresh) :getaway-car "mini")))]
    (is (= ::rec/extraneous-keys (:error e)))
    (is (= [:getaway-car] (:keys e))))
  (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts))))

(deftest an-already-sealed-value-runs-nothing
  (reset-runs!)
  (is (identical? led (rec/seal led)))
  (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts)))
  (testing "and something that is neither is still refused"
    (is (= ::rec/cannot-seal (:error (err #(rec/seal {::a 1})))))))

;;; ---------------------------------- the seal leaves ----------------------------------

(deftest a-successful-seal-leaves-every-check-passing-on-the-draft
  (let [d (fresh)]
    (is (rec/sealed? (rec/seal d)))
    (is (= (set all-checks) (rec/passed d)))
    (is (= #{::shape ::rec/every-check} (rec/certificates d))))
  (testing "including one that had nothing to run"
    (let [d (rec/draft led)]
      (is (rec/sealed? (rec/seal d)))
      (is (= (set all-checks) (rec/passed d))))))

;;; ------------------------------------ the guarantee ------------------------------------

(defn- run-every-check
  "Every check of `x`'s type, called with what its arglist names read off `x`: the oracle
  for what `seal` selected, since the claim is that the result is valid as if all of them
  had run."
  [x]
  (let [model (or (internal/model-of x) (internal/draft-model x))]
    (into {} (map (fn [{:keys [name deps pred]}]
                    [name (apply pred (map #(get x %) deps))]))
          (:checks model))))

(deftest whatever-seal-returns-passes-every-check-however-little-it-ran
  ;; Every path into a seal that can run less than everything: untouched, written,
  ;; partially checked, fully checked, checked and then written over.
  (let [ds [(rec/draft led)
            (assoc (rec/draft led) ::a 2)
            (assoc (rec/draft led) ::c :other)
            (-> (rec/draft led) (assoc ::a 2) (assoc ::b "yyyy"))
            (doto (fresh) (rec/check ::a))
            (doto (fresh) (rec/check ::shape))
            (doto (fresh) (rec/check rec/every-check))
            (assoc (doto (fresh) (rec/check rec/every-check)) ::b "yy")
            (rec/clean (assoc (rec/draft led) :getaway-car "mini"))
            (with-meta (rec/draft led) {:user-key 1})]]
    (doseq [[i d] (map-indexed vector ds)]
      (let [s (rec/seal d)]
        (is (rec/sealed? s) (str "draft " i))
        (doseq [[cname ret] (run-every-check s)]
          (is (true? ret) (str "draft " i ", check " cname)))))))

(deftest a-check-that-did-not-pass-is-never-recorded-as-passing-by-a-write
  ;; A check is recorded as passing only by passing against exactly the data in hand, and a
  ;; draft inherits the record only from a sealed value whose data it holds.
  (let [bad (fn [d] (some false? (vals (run-every-check d))))]
    (doseq [d [(assoc (rec/draft led) ::a "not an int")
               (assoc (rec/draft led) ::b :not-a-string)
               (assoc (doto (fresh) (rec/check rec/every-check)) ::a "not an int")
               (-> (rec/draft led) (assoc ::a 99))]]
      (is (bad d) "the draft really is invalid")
      (is (= ::rec/check-failed (:error (err #(rec/seal d))))
          "and the seal did not let it through"))))
