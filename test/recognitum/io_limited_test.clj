(ns recognitum.io-limited-test
  "`:options {:io-limited true}`: draft-only writes.

  A check that queries a database, run on every bare `assoc`, means business logic makes a
  query per write. The option says so where the type is written: direct `assoc` and `merge`
  on a *sealed* value are refused for any field an IO-bound check reads, and those writes
  go through `drafting` instead, where N writes cost one round of checks at the seal rather
  than N rounds on the way there.

  It is a refusal, not a deferral. The write does not happen, nothing is fetched, and no
  check runs -- there is nothing to defer to, because the caller asked for a sealed value
  and a sealed value is what it would have had to produce.

  Which fields it reaches is computed, not declared. A field an IO-bound check reads --
  its own fields, and the fields of every fetch it names -- is draft-only; a field only
  pure checks read, or that no check reads at all, is written exactly as before. Declaring
  io-deps is what makes that distinction computable, so the option narrows itself and the
  author never restates it.

  Nothing else is affected: drafts take any write, both constructors still validate and
  still fetch, a protocol method on a sealed value writes freely because its receiver is a
  draft, and `seal` does the work the writes deferred."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]
   [recognitum.roster :as roster]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- msg [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which
  ;; interns vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.io-limited-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

;;; ------------------------------------- the log -------------------------------------

;; Every check and every fetch says so. A refused write is supposed to run nothing at all,
;; and counting is the only way to tell that from "it ran and then threw".
(def ^:private log (atom []))

(defn- note! [k] (swap! log conj k) nil)

(defn- reset-log! [] (reset! log []))

(defn- ran [k] (count (filter #(= k %) @log)))

(use-fixtures :each (fn [f] (reset-log!) (roster/reset-calls!) (f)))

;;; ------------------------------------- the types -------------------------------------

;; `crew` is read by an IO-bound check only through the fetch that names it; `cuts` is read
;; by one directly; `name` is read by a pure check and nothing else; `extra` is read by no
;; check at all. So two of the four are draft-only and two are not, and the difference is
;; computed from the arglists rather than written anywhere.
(rec/defrecord Job
  [name crew cuts extra]
  {:options {:io-limited true}

   :checks
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
   {::shape #{::name ::crew ::cuts}}})

;; The same shape without the option. Everything about it is the same except that a write
;; to an IO-reached field is taken rather than refused.
(rec/defrecord Open
  [name crew cuts extra]
  {:checks
   {::oname ([name] (do (note! ::oname) (string? name)))
    ::ocrew ([crew] (do (note! ::ocrew) (and (set? crew) (every? keyword? crew))))
    ::ocuts ([cuts] (do (note! ::ocuts) (map? cuts)))
    ::oroster-gets-a-cut ([cuts ::oroster]
                          (do (note! ::oroster-gets-a-cut) (every? oroster (keys cuts))))}
   :io-deps
   {::oroster ([crew] (do (note! ::oroster) (roster/fetch-roster crew)))}})

;; The option written out as false, which is the same as not writing it.
(rec/defrecord Unlimited
  [a b]
  {:options {:io-limited false}
   :checks  {::ua ([a] (int? a))
             ::ub-fetched ([::ud] (pos? ud))}
   :io-deps {::ud ([b] (do (note! ::ud) (* 2 b)))}})

;; The option on a type with no io-deps at all: nothing is IO-bound, so nothing is
;; draft-only, and the declaration narrows itself to nothing rather than to everything.
(rec/defrecord Pure
  [a b]
  {:options {:io-limited true}
   :checks  {::pa ([a] (int? a))
             ::pab ([a b] (and (int? a) (int? b) (< a b)))}})

(defprotocol Jobbish
  (rename [j s] "a write to a field no IO touches")
  (recrew [j c] "a write to a draft-only field, from inside a method"))

;; The same option, plus a protocol: a method's receiver is a draft, so the body writes
;; whatever it likes and the wrapper seals once on the way out.
(rec/defrecord Crewed
  [name crew]
  {:options {:io-limited true}
   :checks
   {::cname ([name] (string? name))
    ::ccrew ([crew] (and (set? crew) (every? keyword? crew)))
    ::crew-is-known ("everyone on the crew is someone the house knows"
                     [::croster crew] (= (set croster) (set crew)))}
   :io-deps
   {::croster ([crew] (do (note! ::croster) (roster/fetch-roster crew)))}}

  Jobbish
  (rename [j s] (assoc j ::name s))
  (recrew [j c] (assoc j ::crew c)))

(def ^:private job (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1} :anything))

(defn- fresh-job []
  (->Job "The Italian Job" #{:charlie :lyle} {:charlie 1 :lyle 1} :anything))

;;; --------------------------- which fields the option reaches ---------------------------

(deftest the-option-is-carried-on-the-model-and-read-from-it
  (is (= {:io-limited true} (:options (internal/model-of Job))))
  (is (true? (:io-limited (internal/model-of Job))))
  (testing "and a type that did not declare it, or declared it false, is not limited"
    (is (false? (:io-limited (internal/model-of Open))))
    (is (false? (:io-limited (internal/model-of Unlimited))))
    (is (= {:io-limited false} (:options (internal/model-of Unlimited))))))

(deftest the-draft-only-fields-are-exactly-those-an-io-bound-check-reads
  (testing "cuts is named by an IO-bound check; crew is read by the fetch it names"
    (is (= #{::cuts ::crew} (:io-limited-keys (internal/model-of Job)))))
  (testing "name is read by a pure check and extra by none, so neither is limited"
    (is (not (contains? (:io-limited-keys (internal/model-of Job)) ::name)))
    (is (not (contains? (:io-limited-keys (internal/model-of Job)) ::extra))))
  (testing "a type that did not declare the option limits nothing, whatever it fetches"
    (is (= #{} (:io-limited-keys (internal/model-of Open))))
    (is (= #{} (:io-limited-keys (internal/model-of Unlimited)))))
  (testing "and a limited type with no io-deps limits nothing either"
    (is (= #{} (:io-limited-keys (internal/model-of Pure))))))

;;; ------------------------------------ assoc ------------------------------------

(deftest assoc-of-a-draft-only-field-on-a-sealed-value-is-refused
  (let [e (err #(assoc job ::crew #{:charlie}))]
    (is (= ::rec/io-limited (:error e)))
    (is (= 'Job (:record e)))
    (is (= ::crew (:key e)))
    (is (= [::crew] (:keys e))))
  (testing "the same for a field an IO-bound check names directly"
    (is (= ::rec/io-limited (:error (err #(assoc job ::cuts {:charlie 2})))))))

(deftest a-refused-write-runs-nothing-and-fetches-nothing
  (let [_ (err #(assoc job ::crew #{:charlie}))]
    (is (= 0 (roster/call-count)))
    (is (= 0 (count @log))))
  (testing "not even the pure check over the field written"
    (err #(assoc job ::cuts {:charlie 2}))
    (is (= 0 (ran ::cuts)))
    (is (= 0 (ran ::only-roster-gets-a-cut)))))

(deftest a-refused-write-is-refused-whatever-it-would-have-produced
  (testing "a value that would have passed every check is refused just the same"
    (is (= ::rec/io-limited (:error (err #(assoc job ::crew #{:charlie :lyle}))))))
  (testing "and so is one that would have failed"
    (is (= ::rec/io-limited (:error (err #(assoc job ::crew "not a crew")))))
    (is (= 0 (roster/call-count)))))

(deftest a-field-no-io-check-reaches-is-written-exactly-as-before
  (let [j (assoc job ::name "The Lavender Hill Mob")]
    (is (rec/sealed? j))
    (is (= "The Lavender Hill Mob" (::name j)))
    (is (= 1 (ran ::name)) "its own check ran")
    (is (= 0 (roster/call-count)) "and nothing else did"))
  (testing "including one no check reaches at all"
    (is (= :other (::extra (assoc job ::extra :other)))))
  (testing "and the checks over it still hold"
    (is (= ::rec/check-failed (:error (err #(assoc job ::name :not-a-string)))))))

(deftest an-undeclared-key-is-still-an-undeclared-key
  (is (= ::rec/unknown-field (:error (err #(assoc job :crew #{:charlie})))))
  (is (= ::rec/unknown-field (:error (err #(assoc job ::nonesuch 1))))))

(deftest dissoc-of-a-draft-only-field-is-still-cannot-dissoc
  (is (= ::rec/cannot-dissoc (:error (err #(dissoc job ::crew)))))
  (testing "the option is about writes; a declared field is permanent either way"
    (is (= ::rec/cannot-dissoc (:error (err #(dissoc job ::name)))))))

(deftest update-and-assoc-in-go-through-assoc-and-are-refused-with-it
  (is (= ::rec/io-limited (:error (err #(update job ::crew conj :mole)))))
  (is (= ::rec/io-limited (:error (err #(assoc-in job [::cuts] {})))))
  (testing "and an unlimited field updates normally"
    (is (= "The Italian Job!" (::name (update job ::name str "!"))))))

(deftest the-error-names-the-checks-behind-the-refusal
  (let [e (err #(assoc job ::crew #{:charlie}))]
    (is (= {::crew [::only-roster-gets-a-cut]} (:checks e)))
    (is (= '(recognitum.core/drafting [x m] (assoc x :recognitum.io-limited-test/crew v))
           (:instead e))))
  (testing "and the message says where the write belongs"
    (let [m (msg #(assoc job ::crew #{:charlie}))]
      (is (re-find #"io-limited" m))
      (is (re-find #"drafting" m))
      (is (re-find #"crew" m)))))

;;; ------------------------------------ merge ------------------------------------

(deftest merge-is-refused-for-a-draft-only-field
  (let [e (err #(merge job {::crew #{:charlie}}))]
    (is (= ::rec/io-limited (:error e)))
    (is (= [::crew] (:keys e))))
  (testing "including where the rest of the merge is fine"
    (is (= ::rec/io-limited (:error (err #(merge job {::name "x" ::cuts {}}))))))
  (testing "and nothing runs and nothing is fetched"
    (err #(merge job {::name "x" ::cuts {}}))
    (is (= 0 (roster/call-count)))
    (is (= 0 (ran ::name)))))

(deftest a-merge-reports-every-draft-only-field-it-was-given
  (let [e (err #(merge job {::cuts {} ::crew #{} ::name "x"}))]
    (is (= [::crew ::cuts] (:keys e)) "in field declaration order, not in the merge's")
    (is (= ::crew (:key e)))
    (is (= {::crew [::only-roster-gets-a-cut]
            ::cuts [::only-roster-gets-a-cut]}
           (:checks e)))))

(deftest a-merge-of-fields-no-io-check-reaches-is-applied-as-before
  (let [j (merge job {::name "The Lavender Hill Mob" ::extra :bullion})]
    (is (rec/sealed? j))
    (is (= "The Lavender Hill Mob" (::name j)))
    (is (= :bullion (::extra j)))
    (is (= 1 (ran ::name)))
    (is (= 0 (roster/call-count))))
  (testing "and the checks over them still run"
    (is (= ::rec/check-failed (:error (err #(merge job {::name 7})))))))

(deftest an-undeclared-key-in-a-merge-is-reported-before-the-option-is
  (testing "a key the type does not declare is not a key drafting could write either"
    (is (= ::rec/unknown-field (:error (err #(merge job {:crew #{} ::crew #{}})))))
    (is (= ::rec/unknown-field (:error (err #(merge job {::nonesuch 1 ::cuts {}})))))))

(deftest conj-and-into-reach-the-same-refusal
  (is (= ::rec/io-limited (:error (err #(conj job [::crew #{:charlie}])))))
  (is (= ::rec/io-limited (:error (err #(into job {::cuts {}})))))
  (testing "and pass the same unlimited writes through"
    (is (= "x" (::name (conj job [::name "x"]))))
    (is (= "y" (::name (into job {::name "y"}))))))

;;; --------------------------- what the option does not touch ---------------------------

(deftest a-draft-takes-every-write
  (let [d (-> (rec/draft job)
              (assoc ::crew #{:charlie :lyle :left-ear})
              (assoc ::cuts {:charlie 1 :lyle 1 :left-ear 1})
              (assoc ::name "The Italian Job"))]
    (is (rec/draft? d))
    (is (= #{:charlie :lyle :left-ear} (::crew d)))
    (is (= 0 (roster/call-count)) "a draft write checks nothing and fetches nothing"))
  (testing "and a draft of the type builds from nothing the same way"
    (is (rec/draft? (-> (rec/draft Job) (assoc ::crew #{:charlie}))))))

(deftest drafting-is-the-way-the-write-is-spelled-instead
  (let [j (rec/drafting [d job]
            (-> d
                (assoc ::crew #{:charlie :lyle :left-ear})
                (assoc ::cuts {:charlie 1 :lyle 1 :left-ear 1})))]
    (is (rec/sealed? j))
    (is (= #{:charlie :lyle :left-ear} (::crew j)))
    (is (= 1 (roster/call-count)) "two writes, one fetch, at the seal")
    (is (= 1 (ran ::only-roster-gets-a-cut))))
  (testing "and the checks are enforced there, so nothing was skipped by going around"
    (reset-log!)
    (roster/reset-calls!)
    (is (= ::rec/check-failed
           (:error (err #(rec/drafting [d job] (assoc d ::cuts {:napster 1}))))))))

(deftest seal-of-a-draft-holding-a-refused-write-still-validates-it
  (let [d (-> (rec/draft job) (assoc ::crew "not a crew"))]
    (is (= ::rec/check-failed (:error (err #(rec/seal d)))))
    (is (= 0 (roster/call-count)) "the gate still keeps a malformed crew away from the fetch"))
  (testing "and a good one seals"
    (is (rec/sealed? (rec/seal (-> (rec/draft job) (assoc ::crew #{:charlie :lyle})))))))

(deftest both-constructors-still-validate-and-still-fetch
  (let [j (->Job "A Job" #{:charlie} {:charlie 1} nil)]
    (is (rec/sealed? j))
    (is (= 1 (roster/call-count)) "construction is not a write to a sealed value"))
  (testing "map->Type the same"
    (roster/reset-calls!)
    (is (rec/sealed? (map->Job {::name "A Job" ::crew #{:charlie} ::cuts {:charlie 1}
                                ::extra nil})))
    (is (= 1 (roster/call-count))))
  (testing "and both still refuse what the checks refuse"
    (is (= ::rec/check-failed (:error (err #(->Job "A Job" #{:charlie} {:napster 1} nil)))))
    (is (= ::rec/check-failed (:error (err #(map->Job {::name "A Job"})))))))

(deftest check-and-certify-on-a-draft-are-untouched
  (let [d (-> (rec/draft Job)
              (assoc ::name "A Job")
              (assoc ::crew #{:charlie})
              (assoc ::cuts {:charlie 1})
              (assoc ::extra nil))]
    (is (true? (rec/check d ::crew)))
    (is (true? (rec/certify d ::shape)))
    (is (true? (rec/check d ::only-roster-gets-a-cut)))
    (is (= 1 (roster/call-count)))))

(deftest a-protocol-method-on-a-sealed-value-writes-a-draft-only-field-freely
  (let [c (->Crewed "A Job" #{:charlie :lyle})]
    (roster/reset-calls!)
    (let [c2 (recrew c #{:charlie :left-ear})]
      (is (rec/sealed? c2))
      (is (= #{:charlie :left-ear} (::crew c2)))
      (is (= 1 (roster/call-count)) "the receiver is a draft, so the seal is where IO happens")))
  (testing "and what it produces is still validated"
    (is (= ::rec/check-failed (:error (err #(recrew (->Crewed "A Job" #{:charlie})
                                                    #{:nobody}))))))
  (testing "while a direct assoc of the same field from outside is refused"
    (is (= ::rec/io-limited (:error (err #(assoc (->Crewed "A Job" #{:charlie})
                                                 ::crew #{:lyle})))))
    (is (rec/sealed? (rename (->Crewed "A Job" #{:charlie}) "Another")))))

(deftest with-meta-and-the-reads-are-untouched
  (let [j (with-meta job {:tagged true})]
    (is (rec/sealed? j))
    (is (true? (:tagged (meta j))))
    (is (= #{::name ::crew ::cuts ::only-roster-gets-a-cut} (::rec/passed (meta j)))))
  (is (= #{:charlie :lyle} (::crew job)))
  (is (= [::name ::crew ::cuts ::extra] (vec (keys job))))
  (is (= 4 (count job))))

;;; --------------------------- the option is what makes the difference ---------------------------

(deftest without-the-option-the-same-write-is-taken
  (let [o (->Open "A Job" #{:charlie} {:charlie 1} nil)]
    (roster/reset-calls!)
    (reset-log!)
    (let [o2 (assoc o ::crew #{:charlie :lyle})]
      (is (rec/sealed? o2))
      (is (= #{:charlie :lyle} (::crew o2)))
      (is (= 1 (roster/call-count)) "which is the query per write the option exists to refuse")))
  (testing "and a merge likewise"
    (roster/reset-calls!)
    (is (rec/sealed? (merge (->Open "A Job" #{:charlie} {:charlie 1} nil)
                            {::cuts {:charlie 2}})))
    (is (pos? (roster/call-count)))))

(deftest the-option-declared-false-limits-nothing
  (let [u (->Unlimited 1 2)]
    (is (rec/sealed? (assoc u ::b 3)))
    (is (rec/sealed? (merge u {::a 2 ::b 3})))
    (is (= ::rec/check-failed (:error (err #(assoc u ::b 0)))) "the check still decides")))

(deftest a-limited-type-with-no-io-deps-writes-as-it-always-did
  (let [p (->Pure 1 2)]
    (is (= 0 (::a (assoc p ::a 0))))
    (is (rec/sealed? (merge p {::a 5 ::b 6})))
    (is (= ::rec/check-failed (:error (err #(assoc p ::a 9)))))))

;;; ------------------------------------ declaring it ------------------------------------

(deftest the-option-takes-a-boolean-and-nothing-else
  (testing "it is read where the form is written, so a value that is not a literal boolean
            would be truthy whatever it later evaluated to"
    (let [e (compile-data '(recognitum.core/defrecord Maybe
                             [a] {:options {:io-limited "yes"} :checks {:scratch/a int?}}))]
      (is (= ::rec/malformed-option (:error e)))
      (is (= 'Maybe (:record e)))
      (is (= :io-limited (:option e)))
      (is (= "yes" (:value e))))
    (let [m (compile-message '(recognitum.core/defrecord Maybe2
                                [a] {:options {:io-limited (= 1 1)}}))]
      (is (re-find #":io-limited" m))))
  (testing "both booleans compile"
    (is (nil? (compiles '(recognitum.core/defrecord Yes [a] {:options {:io-limited true}}))))
    (is (nil? (compiles '(recognitum.core/defrecord No [a] {:options {:io-limited false}}))))))
