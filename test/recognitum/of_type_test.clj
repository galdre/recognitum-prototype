(ns recognitum.of-type-test
  "`of-type?`: is this a value of that type, in either of its forms?

  `defrecord` generates two classes, so `instance?` stopped answering the question people
  usually mean by it. The four predicates divide up as:

      (rec/of-type? Heist x)   either form of Heist
      (instance? Heist x)      a sealed Heist, specifically
      (rec/draft? x)           any draft, of any type
      (rec/sealed? x)          any sealed value, of any type

  It is false for anything unregistered, including the type object itself: a class is not a
  value of its type."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as heist])
  (:import
   (recognitum.heist Heist HeistDraft)))

(def job heist/italian-job)

;;; A second type, so "of this type" can be told from "of some type". Its fields deliberately
;;; spell the same names as Heist's, since the question is about identity of type and not
;;; about shape.

(rec/defrecord Caper
  [name crew take cuts]
  {::name string?})

(def caper (->Caper "The Lavender Hill Mob" #{:holland} 1 {:holland 1}))

;;; A plain `clojure.core/defrecord` over the same field names, to show that structural
;;; agreement buys nothing.

(clojure.core/defrecord PlainHeist [name crew take cuts])

;;; ------------------------------------ both forms are it ------------------------------------

(deftest a-sealed-value-is-of-its-type
  (is (true? (rec/of-type? Heist job)))
  (is (true? (rec/of-type? Heist (heist/->Heist "The Job" #{:a} 1 {:a 1}))))
  (is (true? (rec/of-type? Heist (heist/map->Heist {::heist/name "The Job"
                                                    ::heist/crew #{:a}
                                                    ::heist/take 1
                                                    ::heist/cuts {:a 1}}))))
  (testing "metadata does not change the answer"
    (is (true? (rec/of-type? Heist (with-meta job {:tagged true}))))))

(deftest a-draft-is-of-its-type
  (testing "drafted from a sealed value"
    (is (true? (rec/of-type? Heist (rec/draft job)))))
  (testing "drafted from the type, with no field written yet"
    (is (true? (rec/of-type? Heist (rec/draft Heist)))))
  (testing "mid-update, in a state no sealed value could hold"
    (is (true? (rec/of-type? Heist (assoc (rec/draft job) ::heist/take "nonsense")))))
  (testing "holding a key the type does not declare"
    (is (true? (rec/of-type? Heist (assoc (rec/draft job) :getaway-car "mini")))))
  (testing "inside a drafting body, and again on what it returns"
    (is (rec/drafting [h job]
          (is (true? (rec/of-type? Heist h)))
          h))
    (is (true? (rec/of-type? Heist (rec/drafting [h job] (heist/rename h "Another Job")))))))

(deftest the-two-forms-answer-alike
  (let [d (rec/draft job)]
    (is (= (rec/of-type? Heist job) (rec/of-type? Heist d)))
    (testing "which is exactly what instance? does not do"
      (is (instance? Heist job))
      (is (not (instance? Heist d))))))

;;; --------------------------------- a class is not a value ---------------------------------

(deftest the-type-object-itself-is-not-of-the-type
  (is (false? (rec/of-type? Heist Heist))
      "a class is not a value of its type, however registered it is")
  (is (false? (rec/of-type? Heist HeistDraft)))
  (testing "even though the type is what draft accepts to build from nothing"
    (is (true? (rec/of-type? Heist (rec/draft Heist))))))

;;; ------------------------------------ everything else -------------------------------------

(deftest another-type-is-not-this-one
  (is (false? (rec/of-type? Heist caper)))
  (is (false? (rec/of-type? Heist (rec/draft caper))))
  (is (false? (rec/of-type? Caper job)))
  (is (false? (rec/of-type? Caper (rec/draft job))))
  (testing "each is of its own"
    (is (true? (rec/of-type? Caper caper)))
    (is (true? (rec/of-type? Caper (rec/draft caper))))))

(deftest structural-agreement-is-not-identity-of-type
  (let [m {::heist/name "The Italian Job"
           ::heist/crew #{:charlie :lyle :left-ear}
           ::heist/take 3000
           ::heist/cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}]
    (is (= m (into {} job)) "the same entries, exactly")
    (is (false? (rec/of-type? Heist m))
        "a bare map with the right keys is not a Heist"))
  (is (false? (rec/of-type? Heist (->PlainHeist "The Italian Job" #{:charlie} 3000 {:charlie 3000})))
      "nor is a clojure.core record over the same field names"))

(deftest nothing-unregistered-is-of-any-type
  (is (false? (rec/of-type? Heist nil)))
  (is (false? (rec/of-type? Heist "The Italian Job")))
  (is (false? (rec/of-type? Heist ::heist/name)))
  (is (false? (rec/of-type? Heist 3000)))
  (is (false? (rec/of-type? Heist [1 2 3])))
  (is (false? (rec/of-type? Heist (Object.))))
  (is (false? (rec/of-type? Heist java.lang.String))))

;;; --------------------------- an unregistered type in first position ---------------------------

(deftest an-unregistered-first-argument-answers-false-rather-than-throwing
  (is (false? (rec/of-type? nil job)))
  (is (false? (rec/of-type? java.lang.String "a string")))
  (is (false? (rec/of-type? PlainHeist (->PlainHeist 1 2 3 4)))
      "a clojure.core record type is not a type this library knows about")
  (is (false? (rec/of-type? Object (Object.))))
  (testing "a value in the type position is not a type either"
    (is (false? (rec/of-type? job job)))
    (is (false? (rec/of-type? (rec/draft job) job)))))

;;; ------------------------- the rest of the table, for the contrast -------------------------

(deftest draft-and-sealed-ask-about-any-type
  (let [d (rec/draft job)]
    (is (true? (rec/draft? d)))
    (is (true? (rec/draft? (rec/draft caper))))
    (is (false? (rec/draft? job)))
    (is (true? (rec/sealed? job)))
    (is (true? (rec/sealed? caper)))
    (is (false? (rec/sealed? d)))
    (testing "of-type? is the conjunction of one of those with the type"
      (is (= (rec/of-type? Heist d) (and (rec/draft? d) (instance? HeistDraft d))))
      (is (= (rec/of-type? Heist job) (and (rec/sealed? job) (instance? Heist job)))))))

;;; ----------------------------------- nothing is interned -----------------------------------

(deftest of-type-is-a-function-and-not-a-generated-predicate
  (is (fn? @(resolve 'recognitum.core/of-type?)))
  (testing "defining a type interns nothing that could collide with a hand-written predicate"
    (let [interned (set (keys (ns-interns 'recognitum.heist)))]
      (doseq [s '[Heist? heist? of-type? Heist-of-type?]]
        (is (not (contains? interned s))
            (str s " should not be interned by defrecord"))))))
