(ns recognitum.audit-test
  "The opt-in check that an invariant declares every key it reads."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.audit-fixture :as fixture]
   [recognitum.core :as rec]))

(use-fixtures :once fixture/audit)

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(rec/defrecord Honest
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "a matches b" :keys #{:a :b} :pred (fn [m] (= (:a m) (:b m)))}])

(rec/defrecord Liar
  ;; declares :a, reads :b
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "a matches b" :keys #{:a} :pred (fn [m] (= (:a m) (:b m)))}])

(rec/defrecord Sweeper
  ;; asks for the whole map, so its reads cannot be attributed to particular keys
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "every value is positive" :keys #{:a} :pred (fn [m] (every? pos? (vals m)))}])

(defn- build [t m] (rec/drafting [x t] (merge x m)))

(deftest off-by-default
  ;; the root value, not the current binding -- the suite may be running under the fixture
  (is (false? (.getRawRoot #'rec/*audit-invariant-keys*))))

(deftest the-hole-the-audit-exists-to-find
  ;; No invariant declares :b, so writing :b runs nothing at all -- and the result seals.
  (binding [rec/*audit-invariant-keys* false]
    (let [ok  (build Liar {:a 1 :b 1})
          bad (rec/drafting [x ok] (assoc x :b 999))]
      (is (= 1 (:a bad)))
      (is (= 999 (:b bad)))
      (is (instance? Liar bad) "sealed, reporting itself validated, and wrong"))))

(deftest the-audit-catches-it-at-construction
  ;; Construction touches every field, so every invariant runs and every lie is exposed.
  (binding [rec/*audit-invariant-keys* true]
    (let [e (err #(build Liar {:a 1 :b 1}))]
      (is (= ::rec/undeclared-invariant-key (:error e)))
      (is (= "a matches b" (:invariant e)))
      (is (= #{:a} (:declared e)))
      (is (= #{:b} (:also-read e))))))

(deftest an-honest-type-passes
  (binding [rec/*audit-invariant-keys* true]
    (is (instance? Honest (build Honest {:a 1 :b 1})))
    (is (instance? Honest (rec/drafting [x (build Honest {:a 1 :b 1})] (merge x {:a 2 :b 2}))))))

(deftest the-audit-does-not-change-what-is-produced
  (let [plain   (binding [rec/*audit-invariant-keys* false] (build Honest {:a 1 :b 1}))
        audited (binding [rec/*audit-invariant-keys* true]  (build Honest {:a 1 :b 1}))]
    (is (= (into {} plain) (into {} audited)))
    (is (= (class plain) (class audited)))))

(deftest real-violations-still-throw-under-the-audit
  (binding [rec/*audit-invariant-keys* true]
    (is (= ::rec/invariant-violated (:error (err #(build Honest {:a 1 :b 2})))))
    (is (= ::rec/invalid-field (:error (err #(build Honest {:a "x" :b 1})))))))

(deftest a-mis-declaration-outranks-a-data-failure
  ;; Liar with a /= b both lies about its keys and fails. The defect in the type is the
  ;; more fundamental of the two, so that is what gets reported.
  (binding [rec/*audit-invariant-keys* true]
    (is (= ::rec/undeclared-invariant-key (:error (err #(build Liar {:a 1 :b 2})))))))

(deftest a-predicate-reading-the-whole-map-is-not-accused
  (testing "asking for every value cannot be attributed to a key, so the audit abstains"
    (binding [rec/*audit-invariant-keys* true]
      (is (instance? Sweeper (build Sweeper {:a 1 :b 2})))))
  (testing "it still validates"
    (binding [rec/*audit-invariant-keys* true]
      (is (= ::rec/invariant-violated (:error (err #(build Sweeper {:a 1 :b 0}))))))))

(deftest the-binding-does-not-leak
  (binding [rec/*audit-invariant-keys* false]
    (is (instance? Liar (build Liar {:a 1 :b 1})) "off: the lie is invisible")
    (binding [rec/*audit-invariant-keys* true]
      (is (= ::rec/undeclared-invariant-key (:error (err #(build Liar {:a 1 :b 1}))))))
    (is (instance? Liar (build Liar {:a 1 :b 1})) "and off again once the binding unwinds")))
