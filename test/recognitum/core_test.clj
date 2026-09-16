(ns recognitum.core-test
  "Reads and writes on a validated value: which checks run, which do not, and what they throw."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.audit-fixture :as fixture]
   [recognitum.heist :as heist]
   [recognitum.core :as rec])
  (:import
   (recognitum.heist Heist)))

(def job heist/italian-job)

(use-fixtures :once fixture/audit)

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ----------------------------------------- reading -----------------------------------------

(deftest reads-like-a-map
  (is (= 3000 (::heist/take job)))
  (is (= 3000 (get job ::heist/take)))
  (is (nil? (get job :nope)))
  (is (= ::missing (get job :nope ::missing)))
  (is (= ["The Italian Job" 3000] (let [{::heist/keys [name take]} job] [name take])))
  (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts] (keys job)))
  (is (= 4 (count job)))
  (is (contains? job ::heist/cuts))
  (is (= {::heist/name "The Italian Job"
          ::heist/crew #{:charlie :lyle :left-ear}
          ::heist/take 3000
          ::heist/cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}
         (into {} job))))

(deftest carries-meta
  (let [m (with-meta job {:src :test})]
    (is (= {:src :test} (meta m)))
    (is (instance? Heist m))
    (is (= 3000 (::heist/take m)))
    (is (nil? (meta job)) "with-meta does not mutate the original")))

;;; ----------------------------------------- writing -----------------------------------------

(deftest assoc-checks-the-field
  (let [h (assoc job ::heist/name "The Sequel")]
    (is (instance? Heist h))
    (is (= "The Sequel" (::heist/name h)))
    (is (= "The Italian Job" (::heist/name job)) "the original is untouched"))
  (let [e (err #(assoc job ::heist/take "lots"))]
    (is (= ::rec/invalid-field (:error e)))
    (is (= ::heist/take (:key e)))
    (is (= "lots" (:value e)))))

(deftest assoc-rejects-a-key-the-type-does-not-declare
  (let [e (err #(assoc job :getaway-car "mini"))]
    (is (= ::rec/unknown-field (:error e)))
    (is (= :getaway-car (:key e)))))

(deftest assoc-runs-the-invariants-that-depend-on-the-key
  (let [e (err #(assoc job ::heist/take 4000))]
    (is (= ::rec/invariant-violated (:error e)))
    (is (= "the cuts add up to the take" (:invariant e)))
    (is (= #{::heist/take ::heist/cuts} (:depends-on e)))
    (is (= #{::heist/take ::heist/cuts} (set (keys (:values e))))
        "the failure reports the values the invariant read")))

(deftest dissoc-refuses-a-declared-field
  (let [e (err #(dissoc job ::heist/cuts))]
    (is (= ::rec/cannot-dissoc (:error e)))
    (is (= ::heist/cuts (:key e)))
    (is (= '(assoc m ::heist/cuts nil) (:instead e)) "and proposes the rewrite")))

(deftest dissoc-of-an-undeclared-key-is-the-ordinary-no-op
  (is (identical? job (dissoc job :getaway-car))
      "a validated value cannot be holding one, so there is nothing to remove"))

;;; --------------------------------- generated constructors ---------------------------------

(deftest positional-constructor-validates
  (let [h (heist/->Heist "Job" #{:a} 10 {:a 10})]
    (is (instance? Heist h))
    (is (= {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10 ::heist/cuts {:a 10}} (into {} h))))
  (is (= ::rec/invalid-field      (:error (err #(heist/->Heist "" #{:a} 10 {:a 10})))))
  (is (= ::rec/invariant-violated (:error (err #(heist/->Heist "Job" #{:a} 99 {:a 10}))))
      "every invariant runs, since construction touches every field")
  (testing "it takes the declared fields and nothing else"
    (is (= '([name crew take cuts])
           (:arglists (meta (resolve 'recognitum.heist/->Heist)))))))

(deftest map-constructor-validates
  (is (instance? Heist (heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10 ::heist/cuts {:a 10}})))
  (is (= ::rec/missing-field
         (:error (err #(heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10})))))
  (is (= ::rec/extraneous-keys
         (:error (err #(heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10 ::heist/cuts {:a 10} :x 1})))))
  (is (= ::rec/invalid-field
         (:error (err #(heist/map->Heist {::heist/name "" ::heist/crew #{:a} ::heist/take 10 ::heist/cuts {:a 10}}))))))

(deftest there-is-no-unvalidated-back-door
  ;; deftype interns a positional factory that checks nothing. If it survives, every guarantee
  ;; in this library is one function call away from being bypassed.
  (is (= ::rec/invalid-field (:error (err #(heist/->Heist "" :not-a-set -999 "garbage"))))
      "the forging attempt that used to succeed")
  (is (nil? (resolve 'recognitum.heist/->HeistDraft))
      "and the draft type's raw factory is unmapped"))

;;; ------------------------------------- merge is atomic -------------------------------------

(deftest merge-applies-every-entry-before-checking-invariants
  (let [h (merge job {::heist/take 4000 ::heist/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})]
    (is (instance? Heist h))
    (is (= 4000 (::heist/take h))))
  (testing "either key on its own breaks the invariant the pair satisfies"
    (is (= ::rec/invariant-violated (:error (err #(assoc job ::heist/take 4000)))))))

(deftest merge-still-checks-what-it-writes
  (is (= ::rec/invariant-violated
         (:error (err #(merge job {::heist/take 4000 ::heist/cuts {:charlie 1 :lyle 1 :left-ear 1}})))))
  (is (= ::rec/unknown-field (:error (err #(merge job {:getaway-car "mini"})))))
  (is (= ::rec/invalid-field (:error (err #(merge job {::heist/name ""}))))))

;;; ------------------------- only the applicable invariants run -------------------------

(def ^:private runs (atom 0))

(defn- counted [pred] (fn [m] (swap! runs inc) (pred m)))

(rec/defrecord Counted
  [^{:valid int?} a ^{:valid int?} b ^{:valid int?} c]
  [{:doc "a is below b" :keys #{::a ::b} :pred (counted (fn [m] (< (::a m) (::b m))))}])

(def ^:private counted-base (rec/drafting [x Counted] (merge x {::a 1 ::b 2 ::c 3})))

(deftest a-write-runs-only-the-invariants-declaring-that-key
  (reset! runs 0)
  (assoc counted-base ::c 99)
  (is (zero? @runs) "no invariant declares ::c")
  (assoc counted-base ::a 0)
  (is (= 1 @runs) "one does declare ::a"))

(deftest merge-checks-the-invariants-once-not-once-per-key
  (reset! runs 0)
  (merge counted-base {::a 1 ::b 5})
  (is (= 1 @runs) "both keys feed the same invariant"))
