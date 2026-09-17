(ns recognitum.io-limited-test
  "`:options {:io-limited true}`: draft-only writes.

  Direct `assoc` and `merge` on a sealed value are refused for any field an external
  validation reads -- its own fields and those of every fetch it names -- so those writes
  go through `drafting`, where N writes cost one round of checks at the seal. The write
  does not happen, nothing is fetched, and no check runs. Which fields are limited is
  computed from the arglists. Drafts take any write, both constructors validate and fetch,
  and a protocol method on a sealed value writes freely because its receiver is a draft."
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

;; Every check and every fetch logs itself: a refused write runs nothing at all, and
;; counting is the only way to tell that from "it ran and then threw".
(def ^:private log (atom []))

(defn- note! [k] (swap! log conj k) nil)

(defn- reset-log! [] (reset! log []))

(defn- ran [k] (count (filter #(= k %) @log)))

(use-fixtures :each (fn [f] (reset-log!) (roster/reset-calls!) (f)))

;;; ------------------------------------- the types -------------------------------------

;; `crew` is read by an external validation only through the fetch that names it; `cuts` is
;; read by one directly; `name` is read by a pure check and nothing else; `extra` is read by
;; no check at all. So two of the four are draft-only and two are not, and the difference is
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
  ^:drafting
  (rename [j s] (assoc j ::name s))
  ^:drafting
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

(deftest the-draft-only-fields-are-exactly-those-an-external-validation-reads
  (testing "cuts is named by an external validation; crew is read by the fetch it names"
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
  (testing "the same for a field an external validation names directly"
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
      (is (re-find #"an external validation" m))
      (is (re-find #"inside drafting" m))
      (is (re-find #"crew" m))))
  (testing "and several keys at once are named in the plural"
    (let [m (msg #(merge job {::crew #{:charlie} ::cuts {}}))]
      (is (re-find #"external validations" m)))))

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
    (is (= 0 (roster/call-count)) "a malformed crew still does not reach the fetch"))
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

(deftest checking-a-draft-is-untouched
  (let [d (-> (rec/draft Job)
              (assoc ::name "A Job")
              (assoc ::crew #{:charlie})
              (assoc ::cuts {:charlie 1})
              (assoc ::extra nil))]
    (is (true? (rec/check d ::crew)))
    (is (true? (rec/check d ::shape)))
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

;; The option is read where the form is written, and it takes a literal `true` or `false`
;; or a symbol naming a var holding one. These are the vars the types and the refused forms
;; below are written against. They are public because a form compiled in another namespace
;; names them qualified.

(def limit-io
  "A var holding the option's value, read when the types below are compiled."
  true)

(def leave-it-open
  "The same, holding the other boolean."
  false)

(def unset-io "A var holding nil, which is not a boolean." nil)

(def io-as-a-string "A var holding a string, which is not a boolean either." "true")

;; The option written as an unqualified symbol naming a var in this namespace. Its crew is
;; read by an external validation, so a direct write to it on a sealed value is refused -- the
;; declaration has to do the work, not merely be recorded.
(rec/defrecord ViaVar
  [name crew]
  {:options {:io-limited limit-io}
   :checks
   {::vname ([name] (string? name))
    ::vcrew-is-known ([::vroster crew] (= (set vroster) (set crew)))}
   :io-deps
   {::vroster ([crew] (do (note! ::vroster) (roster/fetch-roster crew)))}})

;; The same, named by its qualified symbol.
(rec/defrecord ViaQualifiedVar
  [name crew]
  {:options {:io-limited recognitum.io-limited-test/limit-io}
   :checks
   {::qname ([name] (string? name))
    ::qcrew-is-known ([::qroster crew] (= (set qroster) (set crew)))}
   :io-deps
   {::qroster ([crew] (do (note! ::qroster) (roster/fetch-roster crew)))}})

;; And a var holding false, which limits nothing, exactly as the literal does not.
(rec/defrecord ViaFalseVar
  [name crew]
  {:options {:io-limited leave-it-open}
   :checks
   {::fname ([name] (string? name))
    ::fcrew-is-known ([::froster crew] (= (set froster) (set crew)))}
   :io-deps
   {::froster ([crew] (do (note! ::froster) (roster/fetch-roster crew)))}})

(defn- compiled-model
  "Compile `form` in the scratch namespace and hand back the model of the type `sym` names
  there. Two evaluations rather than one, so the type is interned before it is read back."
  [form sym]
  (binding [*ns* @scratch]
    (eval form)
    (internal/model-of (eval sym))))

(deftest the-option-takes-a-literal-boolean
  (is (nil? (compiles '(recognitum.core/defrecord Yes [a] {:options {:io-limited true}}))))
  (is (nil? (compiles '(recognitum.core/defrecord No [a] {:options {:io-limited false}}))))
  (is (true? (:io-limited (internal/model-of Job))))
  (is (false? (:io-limited (internal/model-of Unlimited)))))

(deftest the-option-takes-a-var-holding-a-boolean
  (testing "the model carries what the var held, not the symbol that named it"
    (is (= {:io-limited true} (:options (internal/model-of ViaVar))))
    (is (true? (:io-limited (internal/model-of ViaVar)))))
  (testing "and the type is limited for real, not merely recorded as limited"
    (is (= #{::crew} (:io-limited-keys (internal/model-of ViaVar))))
    (let [v (->ViaVar "A Job" #{:charlie :lyle})]
      (is (rec/sealed? v))
      (let [e (err #(assoc v ::crew #{:charlie}))]
        (is (= ::rec/io-limited (:error e)))
        (is (= [::crew] (:keys e))))
      (testing "while a field no external validation reaches is written as before"
        (is (= "Another" (::name (assoc v ::name "Another")))))
      (testing "and drafting is where that write belongs"
        (is (rec/sealed? (rec/drafting [d v] (assoc d ::crew #{:charlie}))))))))

(deftest a-var-holding-false-limits-nothing
  (is (= {:io-limited false} (:options (internal/model-of ViaFalseVar))))
  (is (false? (:io-limited (internal/model-of ViaFalseVar))))
  (is (= #{} (:io-limited-keys (internal/model-of ViaFalseVar))))
  (let [f (->ViaFalseVar "A Job" #{:charlie :lyle})]
    (is (rec/sealed? (assoc f ::crew #{:charlie})))
    (testing "the checks still decide what such a write produces"
      (is (= ::rec/check-failed (:error (err #(assoc f ::crew #{:nobody}))))))))

(deftest a-qualified-symbol-resolves-to-the-var-its-namespace-names
  (is (= {:io-limited true} (:options (internal/model-of ViaQualifiedVar))))
  (is (= #{::crew} (:io-limited-keys (internal/model-of ViaQualifiedVar))))
  (is (= ::rec/io-limited
         (:error (err #(assoc (->ViaQualifiedVar "A Job" #{:charlie}) ::crew #{:lyle})))))
  (testing "including from a namespace that has not referred it"
    (is (true? (:io-limited
                (compiled-model
                 '(recognitum.core/defrecord Elsewhere
                    [a] {:options {:io-limited recognitum.io-limited-test/limit-io}})
                 'Elsewhere))))
    (is (false? (:io-limited
                 (compiled-model
                  '(recognitum.core/defrecord ElsewhereOpen
                     [a] {:options {:io-limited recognitum.io-limited-test/leave-it-open}})
                  'ElsewhereOpen))))))

(deftest a-symbol-that-does-not-resolve-does-not-compile
  (let [form '(recognitum.core/defrecord Nowhere
                [a] {:options {:io-limited no-such-flag} :checks {:scratch/a int?}})
        e    (compile-data form)]
    (is (= ::rec/malformed-option (:error e)))
    (is (= 'Nowhere (:record e)))
    (is (= :io-limited (:option e)))
    (is (= 'no-such-flag (:value e)))
    (testing "and the message says which case it was"
      (let [m (compile-message form)]
        (is (re-find #":io-limited" m))
        (is (re-find #"does not resolve" m))
        (is (re-find #"no-such-flag" m))))))

(deftest a-symbol-that-resolves-to-something-other-than-a-var-does-not-compile
  (let [form '(recognitum.core/defrecord Classy
                [a] {:options {:io-limited String} :checks {:scratch/a int?}})
        e    (compile-data form)]
    (is (= ::rec/malformed-option (:error e)))
    (is (= 'String (:value e)))
    (let [m (compile-message form)]
      (is (re-find #":io-limited" m))
      (is (re-find #"not a var" m)))))

(deftest a-var-whose-value-is-not-a-boolean-does-not-compile
  (testing "nil is not a boolean, whatever it is truthy about"
    (let [form '(recognitum.core/defrecord Unset
                  [a] {:options {:io-limited recognitum.io-limited-test/unset-io}
                       :checks  {:scratch/a int?}})
          e    (compile-data form)]
      (is (= ::rec/malformed-option (:error e)))
      (is (= 'recognitum.io-limited-test/unset-io (:value e)))
      (let [m (compile-message form)]
        (is (re-find #":io-limited" m))
        (is (re-find #"holds" m))
        (is (re-find #"nil" m))
        (is (re-find #"not a boolean" m)))))
  (testing "and neither is a string"
    (let [form '(recognitum.core/defrecord Stringly
                  [a] {:options {:io-limited recognitum.io-limited-test/io-as-a-string}
                       :checks  {:scratch/a int?}})
          e    (compile-data form)]
      (is (= ::rec/malformed-option (:error e)))
      (let [m (compile-message form)]
        (is (re-find #"holds" m))
        (is (re-find #"not a boolean" m))))))

(deftest a-form-that-is-neither-a-boolean-nor-a-symbol-does-not-compile
  (testing "a call is evaluated nowhere: the option is read where it is written"
    (let [form '(recognitum.core/defrecord Called
                  [a] {:options {:io-limited (io-enabled?)} :checks {:scratch/a int?}})
          e    (compile-data form)]
      (is (= ::rec/malformed-option (:error e)))
      (is (= '(io-enabled?) (:value e)))
      (let [m (compile-message form)]
        (is (re-find #":io-limited" m))
        (is (re-find #"is neither" m)))))
  (testing "nor is a string, a keyword, a map or a number one of the two"
    (let [e (compile-data '(recognitum.core/defrecord Maybe
                             [a] {:options {:io-limited "yes"} :checks {:scratch/a int?}}))]
      (is (= ::rec/malformed-option (:error e)))
      (is (= 'Maybe (:record e)))
      (is (= :io-limited (:option e)))
      (is (= "yes" (:value e))))
    (doseq [v [:yes {} 1]]
      (let [e (compile-data (list 'recognitum.core/defrecord 'Odd
                                  '[a] {:options {:io-limited v}}))]
        (is (= ::rec/malformed-option (:error e)) (pr-str v))
        (is (= v (:value e)) (pr-str v))))))
