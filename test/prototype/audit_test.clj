(ns prototype.audit-test
  "The opt-in check that an invariant declares every key it reads."
  (:require
   [clojure.test :refer [deftest is testing]]
   [prototype.validated-record :as v]))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(v/defvalidated Honest
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "a matches b" :keys #{:a :b} :pred (fn [m] (= (:a m) (:b m)))}])

(v/defvalidated Liar
  ;; declares :a, reads :b
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "a matches b" :keys #{:a} :pred (fn [m] (= (:a m) (:b m)))}])

(v/defvalidated Sweeper
  ;; asks for the whole map, so its reads cannot be attributed to particular keys
  [^{:valid int?} a ^{:valid int?} b]
  [{:doc "every value is positive" :keys #{:a} :pred (fn [m] (every? pos? (vals m)))}])

(defn- build [t m] (v/revising [x t] (merge x m)))

(deftest off-by-default
  (is (false? v/*audit-invariant-keys*)))

(deftest the-hole-the-audit-exists-to-find
  ;; No invariant declares :b, so writing :b runs nothing at all -- and the result seals.
  (let [ok  (build Liar {:a 1 :b 1})
        bad (v/revising [x ok] (assoc x :b 999))]
    (is (= 1 (:a bad)))
    (is (= 999 (:b bad)))
    (is (instance? Liar bad) "sealed, reporting itself validated, and wrong")))

(deftest the-audit-catches-it-at-construction
  ;; Construction touches every field, so every invariant runs and every lie is exposed.
  (binding [v/*audit-invariant-keys* true]
    (let [e (err #(build Liar {:a 1 :b 1}))]
      (is (= ::v/undeclared-invariant-key (:error e)))
      (is (= "a matches b" (:invariant e)))
      (is (= #{:a} (:declared e)))
      (is (= #{:b} (:also-read e))))))

(deftest an-honest-type-passes
  (binding [v/*audit-invariant-keys* true]
    (is (instance? Honest (build Honest {:a 1 :b 1})))
    (is (instance? Honest (v/revising [x (build Honest {:a 1 :b 1})] (merge x {:a 2 :b 2}))))))

(deftest the-audit-does-not-change-what-is-produced
  (let [plain   (build Honest {:a 1 :b 1})
        audited (binding [v/*audit-invariant-keys* true] (build Honest {:a 1 :b 1}))]
    (is (= (into {} plain) (into {} audited)))
    (is (= (class plain) (class audited)))))

(deftest real-violations-still-throw-under-the-audit
  (binding [v/*audit-invariant-keys* true]
    (is (= ::v/invariant-violated (:error (err #(build Honest {:a 1 :b 2})))))
    (is (= ::v/invalid-field (:error (err #(build Honest {:a "x" :b 1})))))))

(deftest a-mis-declaration-outranks-a-data-failure
  ;; Liar with a /= b both lies about its keys and fails. The defect in the type is the
  ;; more fundamental of the two, so that is what gets reported.
  (binding [v/*audit-invariant-keys* true]
    (is (= ::v/undeclared-invariant-key (:error (err #(build Liar {:a 1 :b 2})))))))

(deftest a-predicate-reading-the-whole-map-is-not-accused
  (testing "asking for every value cannot be attributed to a key, so the audit abstains"
    (binding [v/*audit-invariant-keys* true]
      (is (instance? Sweeper (build Sweeper {:a 1 :b 2})))))
  (testing "it still validates"
    (binding [v/*audit-invariant-keys* true]
      (is (= ::v/invariant-violated (:error (err #(build Sweeper {:a 1 :b 0}))))))))

(deftest the-binding-does-not-leak
  (binding [v/*audit-invariant-keys* true]
    (is (= ::v/undeclared-invariant-key (:error (err #(build Liar {:a 1 :b 1}))))))
  (is (instance? Liar (build Liar {:a 1 :b 1}))
      "outside the binding the lie is invisible again"))
