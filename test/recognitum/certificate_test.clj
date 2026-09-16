(ns recognitum.certificate-test
  "Certificates: a named subset of a type's checks, run early against a draft.

  A certificate is a draft-level facility. It names checks you can establish before the
  value is complete, and it changes nothing about what a sealed value guarantees -- `seal`
  runs every check on the type whether or not any certificate mentions it.

  Holding is *derived*, never asserted. A draft holds a certificate exactly when every
  check in it currently passes, which is a question about the verdict mask and nothing
  else. So overlapping certificates need no coordination, running individual checks can
  complete a certificate incidentally, and there is no way to mark a draft as holding one
  without the checks having actually passed.

`check` takes a certificate name exactly as it takes a check name: a lone check is a
  certificate containing itself, so there is one verb and one answer shape. `true` when
  every check named passes, and otherwise the checks' own return values -- the failure
  data, as the contract defines it -- keyed by check name in declaration order. Neither
  form returns a draft. The richer report, with the docstring and the fields the check
  declared, is what `check!` throws.

  A certificate is a declaration, so it is resolved while the macro runs. A name it gives
  that is not a check of this type, a certificate naming nothing at all, or one spelled the
  same as a check or as the built-in -- each is a compile error naming the offender, for the
  same reason a mistyped section is: a declaration that means nothing must not quietly
  become a value asserting a validation that never ran."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))
(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates, which is the point where a check body throws."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which
  ;; interns vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.certificate-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))

;;; ---------------------------------------- types ----------------------------------------

;; `::shape` covers the three single-field checks; `::payroll` covers a single-field check
;; and the cross-field one, so the two certificates overlap on `::pay` and neither knows it.
;; `::notes` belongs to no certificate at all, which is what makes "sealing enforces every
;; check" visible.
(rec/defrecord Job
  [title crew pay notes]
  {:checks
   {::title       ([title] (string? title))
    ::crew        ([crew]  (set? crew))
    ::pay         ([pay]   (int? pay))
    ::paid-enough ("the pay covers the crew"
                   [crew pay] (and (set? crew) (int? pay) (>= pay (count crew))))
    ::notes       ([notes] (or (nil? notes) (string? notes)))}
   :certificates
   {::shape   #{::title ::crew ::pay}
    ::payroll #{::pay ::paid-enough}}})

(def ^:private job (->Job "heist" #{:a :b} 10 nil))

(defn- whole
  "A draft of `Job` with every field written and nothing established."
  ([] (whole "heist" #{:a :b} 10 nil))
  ([title crew pay notes]
   (-> (rec/draft Job)
       (assoc ::title title) (assoc ::crew crew) (assoc ::pay pay) (assoc ::notes notes))))

;; What each check returns when it disapproves, so a failure is visibly the check's own
;; return value rather than something the library supplied.
(rec/defrecord Reporting
  [n]
  {:checks
   {::data    ("n is positive" [n] (if (pos? n) true {:too-small n}))
    ::truthy  ([n] (if (pos? n) true :nope))
    ::nil     ([n] (if (pos? n) true nil))
    ::boolean ([n] (pos? n))}
   :certificates
   {::most #{::data ::truthy ::nil}}})

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
    ::both ([n s] (do (tally! ::both) (and (int? n) (string? s))))}
   :certificates
   {::ns    #{::n ::s}
    ::joint #{::both}}})

;; A certificate written as a vector, in an order that is not declaration order.
(rec/defrecord Ten
  [v]
  {:checks
   {::c0 ([v] (= v :ok))
    ::c1 ([v] (= v :ok))
    ::c2 ([v] (= v :ok))
    ::c3 ([v] (= v :ok))
    ::c4 ([v] (= v :ok))
    ::c5 ([v] (= v :ok))
    ::c6 ([v] (= v :ok))
    ::c7 ([v] (= v :ok))
    ::c8 ([v] (= v :ok))
    ::c9 ([v] (= v :ok))}
   :certificates
   {::nine [::c9 ::c8 ::c7 ::c6 ::c5 ::c4 ::c3 ::c2 ::c1]}})

(rec/defrecord Fragile
  [n]
  {:checks
   {::ok   ([n] (do (tally! ::ok) true))
    ::boom ([n] (if (number? n) true (throw (IllegalArgumentException. "boom"))))}
   :certificates {::pair #{::ok ::boom}}})

(rec/defrecord Blank [a] {})

;;; ------------------------------ the section is declared ------------------------------

(deftest a-populated-certificates-section-compiles
  ;; C1 accepted the section only empty, because accepting a declaration nothing reads is
  ;; the silent mis-declaration this design exists to prevent. It now means something.
  (is (rec/sealed? job))
  (is (nil? (compiles '(recognitum.core/defrecord Fine
                         [a]
                         {:checks       {:scratch/a int?}
                          :certificates {:scratch/shape #{:scratch/a}}})))))

(deftest the-model-records-what-each-certificate-covers
  (let [certs (:certificates (internal/model-of Job))]
    (is (= [::shape ::payroll ::rec/every-check] (vec (keys certs)))
        "the declared ones as written, and the built-in behind them")
    (is (= [::title ::crew ::pay] (:checks (get certs ::shape))))
    (is (= [::pay ::paid-enough] (:checks (get certs ::payroll)))
        "in the type's own check order, whatever order the certificate was written in")
    (is (= [::title ::crew ::pay ::paid-enough ::notes]
           (:checks (get certs ::rec/every-check))))))

(def ^:private ten-order
  ;; `Ten`'s checks in the order the model records them. The macro is handed a
  ;; `PersistentHashMap` once a map literal passes eight pairs, so past that the order it
  ;; sees is hash order rather than written order -- stable, and what every failure report
  ;; on this type follows.
  (mapv :name (:checks (internal/model-of Ten))))

(deftest a-certificate-is-read-in-the-type-s-own-check-order
  ;; Written backwards as a vector; what comes out follows the order the type's checks are
  ;; recorded in, which is the order every failure report in this library uses.
  (is (= (filterv (complement #{::c0}) ten-order)
         (:checks (get (:certificates (internal/model-of Ten)) ::nine))))
  (is (not= [::c9 ::c8 ::c7 ::c6 ::c5 ::c4 ::c3 ::c2 ::c1]
            (:checks (get (:certificates (internal/model-of Ten)) ::nine)))
      "which is not the order the certificate was written in"))

;;; ------------------------------ what a declaration must be ------------------------------

(deftest a-certificate-naming-a-check-the-type-does-not-declare-is-a-compile-error
  (let [e (compile-data '(recognitum.core/defrecord Stray
                           [a]
                           {:checks       {:scratch/a int?}
                            :certificates {:scratch/shape #{:scratch/nonesuch}}}))]
    (is (= ::rec/unknown-check (:error e)))
    (is (= 'Stray (:record e)))
    (is (= :scratch/shape (:certificate e)))
    (is (= :scratch/nonesuch (:check e)))
    (is (= [:scratch/a] (:checks e)) "and it says what it could have named"))
  (testing "including an entry that is not a check name at all"
    (doseq [entry ['a "a" 1]]
      (let [e (compile-data (list 'recognitum.core/defrecord 'Stray2
                                  '[a]
                                  {:checks       '{:scratch/a int?}
                                   :certificates {:scratch/shape #{entry}}}))]
        (is (= ::rec/unknown-check (:error e)) (pr-str entry))
        (is (= entry (:check e)) (pr-str entry))))))

(deftest a-certificate-must-be-a-collection-of-check-names
  (doseq [v [:scratch/a 'a 1 "a" {:scratch/a true}]]
    (let [e (compile-data (list 'recognitum.core/defrecord 'Shapeless
                                '[a]
                                {:checks       '{:scratch/a int?}
                                 :certificates {:scratch/shape v}}))]
      (is (= ::rec/malformed-certificate (:error e)) (pr-str v))
      (is (= 'Shapeless (:record e)) (pr-str v))
      (is (= :scratch/shape (:certificate e)) (pr-str v)))))

(deftest a-certificate-naming-nothing-is-refused
  ;; It would be held by every draft of the type, including one nothing had been checked
  ;; on, so `check` would answer `true` while asserting nothing whatsoever.
  (doseq [v [#{} [] nil]]
    (let [e (compile-data (list 'recognitum.core/defrecord 'Empty
                                '[a]
                                {:checks       '{:scratch/a int?}
                                 :certificates {:scratch/nothing v}}))]
      (is (= ::rec/malformed-certificate (:error e)) (pr-str v))
      (is (= :scratch/nothing (:certificate e)) (pr-str v)))))

(deftest a-certificate-may-not-be-spelled-like-a-check-of-the-same-type
  (let [e (compile-data '(recognitum.core/defrecord Collide
                           [a]
                           {:checks       {:scratch/a int?}
                            :certificates {:scratch/a #{:scratch/a}}}))]
    (is (= ::rec/duplicate-name (:error e)))
    (is (= 'Collide (:record e)))
    (is (= :scratch/a (:name e)))))

(deftest a-certificate-may-not-take-the-built-in-s-name
  (let [e (compile-data '(recognitum.core/defrecord Reserved
                           [a]
                           {:checks       {:scratch/a int?}
                            :certificates {:recognitum.core/every-check #{:scratch/a}}}))]
    (is (= ::rec/reserved-certificate (:error e)))
    (is (= 'Reserved (:record e)))
    (is (= ::rec/every-check (:certificate e)))))

;;; -------------------------------- the built-in certificate --------------------------------

(deftest every-check-exists-without-being-declared
  (is (= ::rec/every-check rec/every-check))
  (testing "on a type that declares no certificate at all"
    (is (= [::rec/every-check] (vec (keys (:certificates (internal/model-of Blank))))))
    (is (= [] (:checks (get (:certificates (internal/model-of Blank)) ::rec/every-check)))))
  (testing "and it covers every check the type has"
    (reset! runs {})
    (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
      (is (true? (rec/check d rec/every-check)))
      (is (= {::n 1 ::s 1 ::both 1} @runs))
      (is (= #{::n ::s ::both} (rec/passed d))))))

(deftest a-type-with-no-checks-holds-the-built-in-vacuously
  ;; Every check passes when there are none, which is honest rather than a special case.
  (is (= #{::rec/every-check} (rec/certificates (rec/draft Blank))))
  (is (true? (rec/check (rec/draft Blank) rec/every-check))))

;;; ------------------------------------- holding -------------------------------------

(deftest certificates-are-derived-from-the-mask
  (let [d (whole)]
    (is (= #{} (rec/certificates d)) "built from the type, so nothing has been established"))
  (testing "a draft of a sealed value holds everything, since every check passed on it"
    (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates (rec/draft job))))))

(deftest checking-a-certificate-is-what-makes-it-held
  (let [d (whole)]
    (is (true? (rec/check d ::shape)))
    (is (= #{::shape} (rec/certificates d)))
    (is (= #{::title ::crew ::pay} (rec/passed d)))))

(deftest running-individual-checks-can-complete-a-certificate-incidentally
  ;; Holding is derived, so nothing has to know a certificate exists in order to complete
  ;; one.
  (let [d (whole)]
    (rec/check d ::title)
    (is (= #{} (rec/certificates d)))
    (rec/check d ::crew)
    (is (= #{} (rec/certificates d)))
    (rec/check! d ::pay)
    (is (= #{::shape} (rec/certificates d)))
    (testing "and the overlapping one needs only what it has left"
      (rec/check d ::paid-enough)
      (is (= #{::shape ::payroll} (rec/certificates d)))
      (testing "while the built-in still wants the check no certificate names"
        (is (not (contains? (rec/certificates d) ::rec/every-check)))
        (rec/check d ::notes)
        (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates d)))))))

(deftest overlapping-certificates-need-no-coordination
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::ns)))
    (is (true? (rec/check d ::joint)))
    (is (= #{::ns ::joint ::rec/every-check} (rec/certificates d)))
    (is (= {::n 1 ::s 1 ::both 1} @runs) "each check ran once, for whoever wanted it")))

(deftest a-failing-check-leaves-its-certificate-unheld
  (let [d (whole "heist" #{:a :b} :free nil)]
    (is (= {::pay false} (rec/check d ::shape)))
    (is (= #{::title ::crew} (rec/passed d)) "and what did pass is still recorded")
    (is (= #{} (rec/certificates d)))))

(deftest a-write-can-take-a-certificate-away
  (let [d (rec/draft job)]
    (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates d)))
    (let [d' (assoc d ::pay 99)]
      (is (= #{} (rec/certificates d')) "::pay and ::paid-enough were cleared, so both go")
      (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates d))
          "the seed is a snapshot, so the version it came from keeps its own")
      (testing "and re-establishing them gives it back"
        (is (true? (rec/check d' ::shape)))
        (is (= #{::shape} (rec/certificates d')))))))

(deftest certificates-answers-for-a-sealed-value-too
  ;; A sealed value passed every check, so it holds every certificate the type declares.
  (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates job)))
  (is (= #{::title ::crew ::pay ::paid-enough ::notes} (rec/passed job)))
  (testing "and the answer agrees with what meta reports under the same names"
    (is (= (rec/passed job) (::rec/passed (meta job))))
    (is (= (rec/certificates job) (::rec/certificates (meta job)))))
  (testing "but something that is not one of this library's values is refused"
    (doseq [x [Job {::title "x"} nil 42]]
      (is (= ::rec/not-a-value (:error (err #(rec/certificates x)))) (pr-str x))
      (is (= ::rec/not-a-value (:error (err #(rec/passed x)))) (pr-str x)))))

;;; --------------------------------- what check returns ---------------------------------

(deftest check-answers-true-when-every-check-in-the-certificate-passes
  (let [d (whole)]
    (is (true? (rec/check d ::shape)))
    (is (true? (rec/check d ::payroll)))
    (is (true? (rec/check! d ::shape)))
    (is (true? (rec/check! d ::payroll)))))

(deftest neither-form-returns-a-draft
  (let [d (whole)]
    (doseq [r [(rec/check d ::shape) (rec/check! d ::payroll)]]
      (is (not (rec/draft? r)))
      (is (not (rec/sealed? r))))))

(deftest check-answers-with-whatever-each-failing-check-returned
  ;; The same reading `check` takes: the failure data is what the check returned, and the
  ;; richer report is what the throwing form throws.
  (let [d (assoc (rec/draft Reporting) ::n -1)
        r (rec/check d ::most)]
    (is (= {::data {:too-small -1} ::truthy :nope ::nil nil} r))
    (is (= [::data ::truthy ::nil] (vec (keys r))) "declaration order")
    (is (not (contains? r ::boolean)) "which the certificate does not name")
    (is (= #{} (rec/passed d)))))

(deftest check-reports-only-the-checks-the-certificate-names
  (let [d (whole :untitled #{:a :b} :free nil)]
    (is (= {::pay false} (rec/check d ::payroll))
        "::title fails too, and is no business of this certificate -- and ::paid-enough
         waits on ::pay, so it did not run")
    (is (= {::title false ::pay false} (rec/check d ::shape))
        "::paid-enough shares a field with ::pay and is still no part of ::shape")))

(deftest check-runs-everything-it-was-asked-to-and-stops-at-no-failure
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n :no) (assoc ::s :no))]
    (is (= {::n false ::s false} (rec/check d rec/every-check))
        "both shape checks ran and both are reported -- the first failure stopped neither")
    (is (= {::n 1 ::s 1} @runs)
        "::both waits on the pair, so it was not run and is not reported")))

(deftest the-failure-map-is-ordered-at-any-size
  ;; A map literal would have stopped holding its insertion order at eight entries.
  (let [d (assoc (rec/draft Ten) ::v :no)
        r (rec/check d ::nine)]
    (is (= 9 (count r)))
    (is (instance? clojure.lang.PersistentArrayMap r))
    (is (= (filterv (complement #{::c0}) ten-order) (vec (keys r)))
        "the type's check order, not the order the certificate was written in")
    (is (not (contains? r ::c0)))))

;;; ---------------------------------- what check! throws ----------------------------------

(deftest check-bang-throws-one-check-failed-carrying-every-failure
  (let [d (whole :untitled #{:a :b} :free nil)
        e (err #(rec/check! d ::shape))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 'Job (:record e)))
    (is (= [::title ::pay] (vec (keys (:failures e))))
        "every failure in the certificate, keyed by check name in declaration order")
    (is (= {:doc nil :values {::title :untitled} :returned false}
           (get (:failures e) ::title)))
    (is (= {:doc nil :values {::pay :free} :returned false} (get (:failures e) ::pay)))))

(deftest check-bang-carries-the-docstring-and-the-declared-fields
  (let [d (whole "heist" #{:a :b :c} 1 nil)
        e (err #(rec/check! d ::payroll))]
    (is (= [::paid-enough] (vec (keys (:failures e)))))
    (let [f (get (:failures e) ::paid-enough)]
      (is (= "the pay covers the crew" (:doc f)))
      (is (= [::crew ::pay] (vec (keys (:values f)))) "in arglist order")
      (is (= {::crew #{:a :b :c} ::pay 1} (:values f)))
      (is (= false (:returned f))))))

(deftest check-bang-records-what-passed-before-it-threw
  (let [d (whole "heist" #{:a :b} :free nil)]
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::shape)))))
    (is (= #{::title ::crew} (rec/passed d)))))

(deftest the-order-past-eight-holds-for-the-throwing-form-too
  (let [d (assoc (rec/draft Ten) ::v :no)
        e (err #(rec/check! d ::nine))
        fs (:failures e)]
    (is (= 9 (count fs)))
    (is (instance? clojure.lang.PersistentArrayMap fs))
    (is (= (filterv (complement #{::c0}) ten-order) (vec (keys fs))))))

;;; ------------------------- establish, rather than re-run -------------------------

(deftest a-certificate-already-held-runs-nothing
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::ns)))
    (is (= {::n 1 ::s 1} @runs))
    (is (true? (rec/check d ::ns)))
    (is (= {::n 1 ::s 1} @runs) "the verdicts stand, so there is nothing to establish")
    (is (true? (rec/check! d ::ns)))
    (is (= {::n 1 ::s 1} @runs) "and check! insists on the answer, not on the work")))

(deftest a-draft-of-a-sealed-value-has-nothing-left-to-establish
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (let [d (rec/draft c)]
      (is (true? (rec/check d rec/every-check)))
      (is (= {} @runs)))))

(deftest a-certificate-runs-only-what-is-not-already-passing
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n 1) (assoc ::s "x"))]
    (is (true? (rec/check d ::n)))
    (is (= {::n 1} @runs))
    (is (true? (rec/check d ::ns)))
    (is (= {::n 1 ::s 1} @runs) "::n was already established")))

(deftest a-failed-check-in-a-certificate-is-run-again-next-time
  (reset! runs {})
  (let [d (-> (rec/draft Counted) (assoc ::n :no) (assoc ::s "x"))]
    (is (= {::n false} (rec/check d ::ns)))
    (is (= {::n false} (rec/check d ::ns)))
    (is (= {::n 2 ::s 1} @runs)
        "the failure is not retained, and the one that passed is")))

;;; -------------------------- a check needs something to read --------------------------

(deftest a-certificate-over-fields-nobody-wrote-judges-nil
  ;; There is nothing to refuse here: a field nobody wrote holds nil, and what each shape
  ;; check thinks of nil is a better answer than the library reporting on its own state.
  (let [d (rec/draft Job)]
    (is (= {::title false ::crew false ::pay false} (rec/check d ::shape))
        "every check in it ran, and every one of them is reported")
    (testing "check! throws the ordinary report"
      (is (= ::rec/check-failed (:error (err #(rec/check! d ::shape))))))
    (testing "and nothing is recorded, because nothing passed"
      (is (= #{} (rec/passed d))))))

(deftest a-cross-field-check-does-not-run-on-a-shape-that-failed
  (let [d (-> (rec/draft Job) (assoc ::title "heist") (assoc ::crew #{:a}))]
    (is (= {::pay false} (rec/check d ::shape))
        "::title and ::crew were written and pass; ::pay judges the nil nobody wrote")
    (is (= {::pay false} (rec/check d ::payroll))
        "::paid-enough waits on ::pay, so it neither runs nor appears")
    (is (not (contains? (rec/passed d) ::paid-enough))
        "and it is not recorded as passing either")))

(deftest a-certificate-already-established-answers-without-running-anything
  (let [d  (rec/draft job)
        d' (assoc (rec/draft Job) ::title "heist")]
    (is (true? (rec/check d ::shape)))
    (is (= {::crew false ::pay false} (rec/check d' ::shape))
        "and one that is not established runs what it has to")))

(deftest a-certificate-does-not-demand-the-fields-of-checks-it-does-not-name
  (let [d (-> (rec/draft Job) (assoc ::pay 10) (assoc ::crew #{:a :b}))]
    (is (true? (rec/check d ::payroll))
        "::title and ::notes are unwritten, and are no business of this certificate")
    (is (= #{::payroll} (rec/certificates d)))))

;;; ---------------------------------- and what it refuses ----------------------------------

(deftest a-certificate-name-the-type-does-not-declare-is-refused
  (let [d (whole)]
    (doseq [[label f] [["check" #(rec/check d ::nonesuch)]
                       ["check!" #(rec/check! d ::nonesuch)]]]
      (let [e (err f)]
        (is (= ::rec/unknown-check (:error e)) label)
        (is (= 'Job (:record e)) label)
        (is (= ::nonesuch (:check e)) label)
        (is (= [::shape ::payroll ::rec/every-check] (:certificates e)) label)
        (is (= [::title ::crew ::pay ::paid-enough ::notes] (:checks e)) label)))
    (testing "and a certificate declared on some other type is still refused"
      (is (= ::rec/unknown-check (:error (err #(rec/check d ::ns))))))
    (testing "and nothing is established by asking"
      (is (= #{} (rec/passed d)))))
  (testing "the name of a check is not refused any more -- it is the other half of the verb"
    (is (true? (rec/check (whole) ::title)))))

(deftest checking-a-certificate-on-a-sealed-value-answers-true-and-runs-nothing
  (let [c (->Counted 1 "x")]
    (reset! runs {})
    (is (true? (rec/check c ::ns)))
    (is (true? (rec/check c ::joint)))
    (is (true? (rec/check! c rec/every-check)))
    (is (= {} @runs) "everything is established on a sealed value, so nothing runs"))
  (testing "and anything that is not a value of this library is refused"
    (doseq [x [Job {::title "x"} nil 42]]
      (is (= ::rec/not-a-value (:error (err #(rec/check x ::shape)))) (pr-str x))
      (is (= ::rec/not-a-value (:error (err #(rec/check! x ::shape)))) (pr-str x)))))

(deftest a-check-that-throws-inside-a-certificate-propagates
  ;; A throw is a bug in the check rather than a verdict about the data, so there is
  ;; nothing to write down and nothing to report as invalid.
  (reset! runs {})
  (let [d (assoc (rec/draft Fragile) ::n "not a number")]
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check d ::pair)))
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/check! d ::pair)))
    (is (= #{::ok} (rec/passed d))
        "the check that reached a verdict before it keeps that verdict")
    (is (= 1 (ran ::ok)) "and is not re-run by the second attempt")))

;;; ------------------------- certificates change nothing about sealing -------------------------

(deftest sealing-enforces-every-check-whether-a-certificate-names-it-or-not
  (let [d (whole "heist" #{:a :b} 10 :not-a-string)]
    (is (true? (rec/check d ::shape)))
    (is (true? (rec/check d ::payroll)))
    (is (= #{::shape ::payroll} (rec/certificates d)))
    (let [e (err #(rec/seal d))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::notes] (vec (keys (:failures e))))
          "the check no certificate names is exactly what sealing still wants"))))

(deftest holding-every-certificate-a-draft-can-is-not-a-licence-to-seal
  (let [d (-> (rec/draft Job) (assoc ::pay 10) (assoc ::crew #{:a :b}))]
    (is (true? (rec/check d ::payroll)))
    (let [e (err #(rec/seal d))]
      (is (= ::rec/check-failed (:error e))
          "a certificate is a claim about the checks it names and nothing else")
      (is (= [::title] (vec (keys (:failures e))))
          "::title judges the nil nobody wrote; ::notes admits nil and passes"))))

(deftest a-certified-draft-still-seals
  (let [d (whole)]
    (is (true? (rec/check d rec/every-check)))
    (let [s (rec/seal d)]
      (is (rec/sealed? s))
      (is (= "heist" (::title s))))))

(deftest a-sealed-value-holds-every-certificate-and-carries-none-as-data
  ;; A sealed value passed every check, which is strictly more than any certificate claims,
  ;; so it holds all of them -- and none of that is part of the value.
  (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates job)))
  (is (not (contains? (into {} job) ::shape)))
  (is (not (re-find #"shape" (pr-str job)))))

(deftest checking-a-certificate-inside-a-drafting-form-answers-about-the-draft-in-hand
  (let [a (atom nil)]
    (is (rec/sealed?
         (rec/drafting [x Job]
           (let [x (-> x (assoc ::title "heist") (assoc ::crew #{:a :b})
                       (assoc ::pay 10) (assoc ::notes nil))]
             (reset! a [(rec/check x ::shape) (rec/certificates x)])
             x))))
    (is (= [true #{::shape}] @a))))

;;; --------------------- certificates are no part of the value itself ---------------------

(deftest certificates-take-no-part-in-equality-or-hashing
  ;; Two drafts holding identical data are the same value however much has been
  ;; established about each.
  (let [a (whole)
        b (whole)]
    (rec/check a rec/every-check)
    (is (= #{::shape ::payroll ::rec/every-check} (rec/certificates a)))
    (is (= #{} (rec/certificates b)))
    (is (= a b))
    (is (= b a))
    (is (= (hash a) (hash b)))
    (is (= (into {} a) (into {} b)))))

(deftest certificates-do-not-survive-printing
  (let [a (whole)
        b (whole)]
    (rec/check a ::shape)
    (is (= (pr-str a) (pr-str b)))
    (is (not (re-find #"shape|payroll|every-check" (pr-str a))))))
