(ns recognitum.seal-test
  "What `seal` runs, and why it is allowed to run less than everything.

  `seal` returns a value validated exactly as if every check had been rerun. That is the
  guarantee, and it is extensional: which checks were actually called is an implementation
  strategy underneath it. The strategy is the draft's verdict mask -- `seal` runs exactly
  the checks whose bit is not set, because a set bit says that check passed against exactly
  this data.

  Three things make that sound, and all three are the mask's own rules rather than
  anything `seal` arranges: a draft of a sealed value starts with every bit set, because
  that is what being sealed means; a draft of the type starts with none; and a write clears
  exactly the checks naming the field written. So the only way a bit is set is that some
  check ran and passed against data that has not moved since.

  The consequence worth having is that work done on a draft is not repeated. A check
  established with `check`, or a certificate established with `certify`, costs nothing at
  the seal -- which is what makes incremental checking worth doing at all, and what will
  let bulk checking populate masks and then seal each value without touching IO.

  A failure is not work saved. Nothing about a cleared bit distinguishes \"failed\" from
  \"never run\", so a check that failed runs again at the next seal and fails again."
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

;; Every check counts its own calls. Counting is the only way to see the difference between
;; "seal ran this" and "seal took the mask's word for it", since both produce the same
;; value -- which is the point: the guarantee is extensional and the saving is not.
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
  established about it."
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
    (is (= #{} (rec/passed d)) "nothing established: the mask is empty")
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "every bit was clear, so every check ran -- once each")))

(deftest the-constructors-run-every-check
  (reset-runs!)
  (is (rec/sealed? (map->Ledger {::a 1 ::b "xxx" ::c :anything})))
  (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
      "map->Ledger builds an empty draft and seals it, so nothing is skipped"))

;;; ---------------------------- a draft of a sealed value ----------------------------

(deftest sealing-an-untouched-draft-of-a-sealed-value-runs-nothing
  ;; A sealed value is one every check passed on, and the draft inherits exactly that
  ;; claim. Nothing has moved, so there is nothing to establish.
  (reset-runs!)
  (let [d (rec/draft led)
        s (rec/seal d)]
    (is (rec/sealed? s))
    (is (= led s))
    (is (= {::a 0 ::b 0 ::a-fits 0 ::always 0} (counts)))))

(deftest the-check-naming-no-field-is-not-re-run-either
  ;; A check with an empty arglist reads nothing, so no write can invalidate it and a draft
  ;; of a sealed value has it passing. Selecting by the mask is what makes that count: the
  ;; dirty-key rule it replaces had no key to select such a check by and ran it every time.
  (reset-runs!)
  (rec/seal (assoc (rec/draft led) ::a 2))
  (is (= 0 (ran ::always)) "its verdict was inherited and nothing cleared it"))

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
  ;; The mask is computed from the arglists, not by comparing values: the write cleared the
  ;; bits, so the checks run. A cheaper sound strategy is permitted, not required.
  (reset-runs!)
  (rec/seal (assoc (rec/draft led) ::a (::a led)))
  (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))))

(deftest repeated-writes-to-one-field-cost-one-round-of-checks
  (reset-runs!)
  (rec/seal (reduce (fn [d _] (assoc d ::a 2)) (rec/draft led) (range 20)))
  (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))
      "N writes under one drafting cost one round at the seal, not N"))

;;; ---------------------------- work done on the draft is kept ----------------------------

(deftest a-check-established-by-hand-is-not-re-run-at-the-seal
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d ::a)))
    (is (= 1 (ran ::a)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "::a was already passing; the other three were not")))

(deftest establishing-everything-by-hand-leaves-the-seal-no-work
  (reset-runs!)
  (let [d (fresh)]
    (doseq [c all-checks] (is (true? (rec/check d c))))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "the seal ran nothing: every bit was already set")))

(deftest check-bang-saves-the-same-work
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check! d ::a-fits)))
    (rec/seal d)
    (is (= 1 (ran ::a-fits)))))

(deftest a-certified-certificate-is-not-re-run-at-the-seal
  ;; The half of `certify` that D3 could not deliver: certifying left the mask populated,
  ;; but the seal selected by the dirty-key set and ran everything again.
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d ::shape)))
    (is (= {::a 1 ::b 1 ::a-fits 0 ::always 0} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts))
        "only what ::shape does not cover was left for the seal")))

(deftest a-fully-certified-draft-seals-without-running-anything
  (reset-runs!)
  (let [d (fresh)]
    (is (true? (rec/check d rec/every-check)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))
    (is (rec/sealed? (rec/seal d)))
    (is (= {::a 1 ::b 1 ::a-fits 1 ::always 1} (counts)))))

(deftest a-verdict-established-before-a-write-does-not-cover-the-write
  (reset-runs!)
  (let [d (fresh)]
    (rec/check d rec/every-check)
    (reset-runs!)
    (rec/seal (assoc d ::b "yyyy"))
    (is (= {::a 0 ::b 1 ::a-fits 1 ::always 0} (counts))
        "the write cleared exactly ::b and ::a-fits, and those are what ran")))

;;; -------------------------- the mask travels with the draft --------------------------

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

(deftest a-draft-passed-through-a-nested-drafting-keeps-what-it-established
  (reset-runs!)
  (let [inner (fn [x] (rec/drafting [y x] (assoc y ::a 2)))]
    (is (rec/sealed? (rec/drafting [x led] (inner x))))
    (is (= {::a 1 ::b 0 ::a-fits 1 ::always 0} (counts))
        "only the outer form sealed, and it ran only what the inner write cleared")))

;;; -------------------------------- failures are not saved --------------------------------

(deftest a-failed-check-runs-again-at-the-seal
  ;; Nothing about a cleared bit distinguishes "failed" from "never run", which is why a
  ;; failure is produced again rather than remembered.
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

(deftest seal-still-reports-every-failure-in-phase-order
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
  ;; There is nothing to refuse: the field holds nil, as it would on a `defrecord` whose
  ;; field was never set, and its shape check is what says whether that will do. `c` is
  ;; named by no check, so nothing has anything to say about it and it seals holding nil.
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
  "Every check of `x`'s type, called the way it declared it wanted to be -- its arglist,
  read off the model, is the whole of what it depends on. The oracle for what `seal`
  selected: the claim is not that seal ran a particular set, but that the result is valid
  as if all of them had."
  [x]
  (let [model (or (internal/model-of x) (internal/draft-model x))]
    (into {} (map (fn [{:keys [name deps pred]}]
                    [name (apply pred (map #(get x %) deps))]))
          (:checks model))))

(deftest whatever-seal-returns-passes-every-check-however-little-it-ran
  ;; Every path into a seal that the mask can shorten: untouched, written, partially
  ;; checked, fully certified, checked and then written over.
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

(deftest a-mask-full-of-lies-is-not-reachable-through-any-write
  ;; The mask is only ever set by a check that passed, and only ever inherited by a draft
  ;; whose data is the data that check saw. Nothing a caller can do to a draft sets a bit
  ;; for a check that did not pass against exactly what is in hand.
  (let [bad (fn [d] (some false? (vals (run-every-check d))))]
    (doseq [d [(assoc (rec/draft led) ::a "not an int")
               (assoc (rec/draft led) ::b :not-a-string)
               (assoc (doto (fresh) (rec/check rec/every-check)) ::a "not an int")
               (-> (rec/draft led) (assoc ::a 99))]]
      (is (bad d) "the draft really is invalid")
      (is (= ::rec/check-failed (:error (err #(rec/seal d))))
          "and the mask did not let it through"))))
