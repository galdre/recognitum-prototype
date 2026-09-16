(ns recognitum.recheck-test
  "`recheck`, and why one verb was not enough.

  `check` *establishes*: it skips whatever the verdict mask already says passes, which is
  what makes a second call free and a seal after a bulk check perform no IO. That is the
  right default and the wrong answer to one question -- \"is this still true of the world?\"
  A check over a fetch is a verdict at a point in time, and the mask cannot tell a verdict
  that still holds from one that has quietly stopped holding.

  So `recheck` runs what it names whether or not its bit is set. On a draft the result is
  written back into the mask, which means a check that passed an hour ago and fails against
  today's database is *un-passed* -- the draft stops claiming it, and a later `seal` runs it
  again. On a sealed value there is no mask to write, and the value cannot change: `recheck`
  runs and answers, and the answer is a report about the world rather than a verdict about
  the value.

  Everything else is `check`. Same four names, same arguments, same keyed answer, the same
  one fetch per call shared across everything that names it."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec]))

(defn- err
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ------------------------------------- the world -------------------------------------

(def ^:private vetted
  "Who the house has vouched for, as of right now. Mutable, because that is the whole
  point: a verdict about it can stop being true without anything about the value moving."
  (atom #{:charlie :lyle}))

(def ^:private log (atom []))
(defn- note! [k] (swap! log conj k) nil)
(defn- ran [k] (count (filter #(= k %) @log)))
(defn- handed [] (filterv vector? @log))

(use-fixtures :each (fn [f]
                      (reset! vetted #{:charlie :lyle})
                      (reset! log [])
                      (f)))

;;; ------------------------------------- the types -------------------------------------

(rec/defrecord Crew
  [name crew]
  {:checks
   {::name ([name] (do (note! ::name) (string? name)))
    ::crew ([crew] (do (note! ::crew) (and (set? crew) (every? keyword? crew))))
    ::all-vetted
    ("everyone on the crew is vouched for"
     [crew ::roster] (do (note! ::all-vetted) (= crew roster)))}

   :certificates
   {::shape #{::name ::crew}
    ::vouched #{::all-vetted}}

   :io-deps
   {::roster ("who of them the house vouches for"
              [[crew] :as rows] (do (note! ::roster)
                                    (note! (vec rows))
                                    (into {} (map (fn [[c :as t]]
                                                    [t (into #{} (filter @vetted) c)]))
                                          rows)))}})

;; A second type, so the group forms have more than one model to partition by.
(rec/defrecord Tally
  [n]
  {:checks
   {::n     ([n] (do (note! ::n) (int? n)))
    ::big   ([n ::quota] (do (note! ::big) (>= n quota)))}
   :certificates {::sized #{::big}}
   :io-deps
   {::quota ([[n] :as rows] (do (note! ::quota)
                                (note! (vec rows))
                                (into {} (map (fn [t] [t 10])) rows)))}})

(defn- crew [nm c] (-> (rec/draft Crew) (assoc ::name nm) (assoc ::crew c)))

;;; --------------------------- it runs what check would skip ---------------------------

(deftest recheck-runs-a-check-whose-bit-is-already-set
  (let [d (crew "one" #{:charlie})]
    (is (true? (rec/check d ::name)))
    (is (= 1 (ran ::name)))
    (is (true? (rec/check d ::name)))
    (is (= 1 (ran ::name)) "check establishes, so the second call is free")
    (is (true? (rec/recheck d ::name)))
    (is (= 2 (ran ::name)) "recheck runs it regardless")
    (is (true? (rec/recheck! d ::name)))
    (is (= 3 (ran ::name)))))

(deftest recheck-takes-a-certificate-name-as-check-does
  (let [d (crew "one" #{:charlie})]
    (is (true? (rec/check d ::shape)))
    (is (= 1 (ran ::name)))
    (is (= 1 (ran ::crew)))
    (is (true? (rec/recheck d ::shape)))
    (is (= 2 (ran ::name)) "every check the certificate names, run again")
    (is (= 2 (ran ::crew)))
    (testing "and the built-in covers the type"
      (is (true? (rec/recheck d rec/every-check)))
      (is (= 3 (ran ::name)))
      (is (= 1 (ran ::all-vetted))))))

(deftest the-answer-is-keyed-exactly-as-check-s-is
  (let [d (crew 42 #{:charlie})]
    (is (= {::name false} (rec/recheck d ::name)))
    (is (= {::name false} (rec/recheck d ::shape)) "::crew passes and is not reported")
    (let [e (err #(rec/recheck! d ::shape))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::name] (vec (keys (:failures e))))))))

;;; ------------------------- a verdict the world took back -------------------------

(deftest a-check-that-passed-and-now-fails-is-un-passed
  ;; The case the verb exists for. Nothing about the draft moved; the database did.
  (let [d (crew "one" #{:charlie :lyle})]
    (is (true? (rec/check d ::all-vetted)))
    (is (contains? (rec/passed d) ::all-vetted))
    (swap! vetted disj :lyle)
    (testing "check believes the mask, so it says nothing has changed"
      (is (true? (rec/check d ::all-vetted)))
      (is (= 1 (ran ::roster)) "and fetches nothing, which is what the mask buys"))
    (testing "recheck asks the world"
      (is (= {::all-vetted false} (rec/recheck d ::all-vetted)))
      (is (= 2 (ran ::roster)))
      (is (not (contains? (rec/passed d) ::all-vetted))
          "the bit is cleared: the draft stops claiming what is no longer true")
      (is (not (contains? (rec/certificates d) ::vouched))))
    (testing "so the seal it would have sailed through now refuses"
      (let [e (err #(rec/seal d))]
        (is (= ::rec/check-failed (:error e)))
        (is (= [::all-vetted] (vec (keys (:failures e)))))))))

(deftest a-check-that-passes-again-keeps-its-bit
  (let [d (crew "one" #{:charlie})]
    (is (true? (rec/check d ::all-vetted)))
    (is (true? (rec/recheck d ::all-vetted)))
    (is (contains? (rec/passed d) ::all-vetted))
    (is (= #{::crew ::all-vetted} (rec/passed d))
        "::crew is the gate it waited on; ::name gates nothing and was never asked for")))

(deftest a-check-that-was-failing-and-now-passes-is-set-by-recheck
  (let [d (crew "one" #{:charlie :mole})]
    (is (= {::all-vetted false} (rec/check d ::all-vetted)))
    (is (not (contains? (rec/passed d) ::all-vetted)))
    (swap! vetted conj :mole)
    (is (true? (rec/recheck d ::all-vetted)))
    (is (contains? (rec/passed d) ::all-vetted))))

(deftest recheck-still-shares-one-fetch-across-the-call
  (let [d (crew "one" #{:charlie})]
    (is (true? (rec/recheck d rec/every-check)))
    (is (= 1 (ran ::roster)) "one fetch for the call, however many checks name it")
    (is (true? (rec/recheck d rec/every-check)))
    (is (= 2 (ran ::roster)) "and no longer than the call")))

(deftest a-gate-that-fails-stops-a-recheck-as-it-stops-a-check
  (let [d (crew "one" "not a crew")]
    (is (= {::crew false} (rec/recheck d ::all-vetted))
        "the blocking check's entry, and no entry for the one that never ran")
    (is (= 0 (ran ::roster)) "and nothing reached the outside")))

;;; -------------------------------- a sealed value --------------------------------

(deftest recheck-runs-against-a-sealed-value-and-answers
  (let [c (->Crew "one" #{:charlie :lyle})]
    (reset! log [])
    (is (true? (rec/check c rec/every-check)))
    (is (= [] @log) "check has nothing to establish on a sealed value")
    (is (true? (rec/recheck c rec/every-check)))
    (is (= 1 (ran ::all-vetted)) "recheck runs it")
    (is (= 1 (ran ::roster)))
    (testing "and answers about the world rather than about the value"
      (swap! vetted disj :lyle)
      (is (= {::all-vetted false} (rec/recheck c ::all-vetted)))
      (is (= ::rec/check-failed (:error (err #(rec/recheck! c ::all-vetted)))))
      (is (rec/sealed? c) "the value is what it always was")
      (is (= #{::name ::crew ::all-vetted} (rec/passed c))
          "and still reports every check passing, because a sealed value is one that did"))))

(deftest recheck-on-a-sealed-value-of-a-type-with-no-io-is-just-work
  (let [c (->Tally 12)]
    (reset! log [])
    (is (true? (rec/recheck c ::n)))
    (is (= 1 (ran ::n)))))

;;; ------------------------------------ groups ------------------------------------

(deftest recheck-all-runs-every-value-regardless-of-its-mask
  (let [ds [(crew "one" #{:charlie}) (crew "two" #{:lyle})]]
    (is (= [true true] (rec/check-all ds ::all-vetted)))
    (is (= 1 (ran ::roster)))
    (is (= [true true] (rec/check-all ds ::all-vetted)))
    (is (= 1 (ran ::roster)) "nothing pending, so nothing fetched")
    (is (= [true true] (rec/recheck-all ds ::all-vetted)))
    (is (= 2 (ran ::roster)) "one more fetch, for the whole group")
    (is (= 4 (ran ::all-vetted)))))

(deftest recheck-all-un-passes-per-value
  (let [ds [(crew "one" #{:charlie}) (crew "two" #{:lyle})]]
    (is (= [true true] (rec/check-all ds ::all-vetted)))
    (swap! vetted disj :lyle)
    (is (= [true {::all-vetted false}] (rec/recheck-all ds ::all-vetted)))
    (is (contains? (rec/passed (first ds)) ::all-vetted))
    (is (not (contains? (rec/passed (second ds)) ::all-vetted)))))

(deftest recheck-all-bang-insists-on-the-answer
  (let [ds [(crew "one" #{:charlie}) (crew "two" #{:lyle})]]
    (is (true? (rec/recheck-all! ds ::all-vetted)))
    (swap! vetted disj :lyle)
    (let [e (err #(rec/recheck-all! ds ::all-vetted))]
      (is (= ::rec/check-failed (:error e)))
      (is (true? (:bulk e)))
      (is (= [1] (vec (keys (:failures e))))))))

(deftest recheck-all-batches-per-type-the-way-check-all-does
  (let [ds [(crew "one" #{:charlie}) (crew "two" #{:charlie}) (crew "three" #{:lyle})]]
    (is (= [true true true] (rec/recheck-all ds ::all-vetted)))
    (is (= 1 (ran ::roster)) "one call for three values")
    (is (= 1 (count (handed))))
    (is (= 2 (count (first (handed)))) "two distinct tuples")))

(deftest nothing-to-recheck-is-answered-and-not-refused
  (is (= [] (rec/recheck-all [] ::name)))
  (is (true? (rec/recheck-all! [] ::name))))

(deftest a-sealed-value-in-a-group-is-rechecked-alongside-the-drafts
  (let [ds [(->Crew "one" #{:charlie}) (crew "two" #{:lyle})]]
    (reset! log [])
    (is (= [true true] (rec/recheck-all ds ::all-vetted)))
    (is (= 1 (ran ::roster)) "one fetch spanning both forms")
    (swap! vetted disj :lyle)
    (is (= [true {::all-vetted false}] (rec/recheck-all ds ::all-vetted)))))

;;; ----------------------------------- what it refuses -----------------------------------

(deftest an-unknown-name-is-refused-by-every-re-form
  (let [d (crew "one" #{:charlie})]
    (doseq [f [#(rec/recheck d ::nonesuch)
               #(rec/recheck! d ::nonesuch)
               #(rec/recheck-all [d] ::nonesuch)
               #(rec/recheck-all! [d] ::nonesuch)]]
      (is (= ::rec/unknown-check (:error (err f)))))))

(deftest rechecking-is-a-question-about-a-recognitum-value
  (doseq [x [Crew {::name "x"} nil 42]]
    (is (= ::rec/not-a-value (:error (err #(rec/recheck x ::name)))) (pr-str x))
    (is (= ::rec/not-a-value (:error (err #(rec/recheck! x ::name)))) (pr-str x))))
