(ns prototype.draft-test
  "Drafts, `revising`, and what `seal` guarantees."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [prototype.audit-fixture :as fixture]
   [prototype.heist :as heist]
   [prototype.validated-record :as v])
  (:import
   (prototype.heist Heist)
   (prototype.validated_record Invariant)))

(def job heist/italian-job)

(use-fixtures :once fixture/audit)

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ------------------------------- a draft validates nothing -------------------------------

(deftest writes-into-a-draft-are-unchecked
  (let [d (assoc (v/draft job) :take "nope")]
    (is (v/draft? d))
    (is (= "nope" (:take d)) "the bad value is simply there"))
  (testing "seal is where it fails"
    (is (= ::v/invalid-field (:error (err #(v/seal (assoc (v/draft job) :take "nope"))))))))

(deftest a-draft-may-pass-through-states-no-single-write-could
  (testing "either write alone throws on a validated value"
    (is (= ::v/invariant-violated (:error (err #(assoc job :take 3500))))))
  (testing "but the pair is fine inside one revising"
    (is (instance? Heist (v/revising [x job] (heist/add-member x :napster 500))))))

;;; ---------------------------------------- sealing ----------------------------------------

(deftest seal-reports-keys-the-type-does-not-declare
  (let [e (err #(v/revising [x job] (assoc x :getaway-car "mini")))]
    (is (= ::v/extraneous-keys (:error e)))
    (is (= [:getaway-car] (:keys e)))))

(deftest clean-strips-them-instead
  (is (instance? Heist (v/revising [x job] (-> x (assoc :getaway-car "mini") v/clean)))))

(deftest dissoc-removes-one-undeclared-key-from-a-draft
  (is (instance? Heist (v/revising [x job] (-> x (assoc :getaway-car "mini") (dissoc :getaway-car)))))
  (testing "a declared field is still permanent"
    (is (= ::v/cannot-dissoc (:error (err #(dissoc (v/draft job) :cuts))))))
  (testing "and removing an absent undeclared key allocates nothing"
    (let [d (v/draft job)]
      (is (identical? d (dissoc d :never-there))))))

(deftest seal-reports-a-field-never-written
  (let [e (err #(v/revising [x Heist] (assoc x :name "X")))]
    (is (= ::v/missing-field (:error e)))
    (is (contains? #{:crew :take :cuts} (:key e)))))

;;; --------------------------- "never written" is not "written to nil" ---------------------------

(v/defvalidated Note
  [^{:valid string?}                            title
   ^{:valid (fn [x] (or (nil? x) (string? x)))} body]
  [])

(deftest nil-is-a-legal-value-when-the-predicate-admits-it
  (let [n (v/revising [x Note] (merge x {:title "t" :body nil}))]
    (is (nil? (:body n)))
    (is (= [:title :body] (keys n)) "the field is present, holding nil"))
  (testing "leaving it unwritten is a different thing entirely"
    (is (= ::v/missing-field (:error (err #(v/revising [x Note] (assoc x :title "t"))))))))

;;; --------------------------------------- revising ---------------------------------------

(deftest revising-builds-from-the-type-itself
  (let [h (v/revising [x Heist]
            (merge x {:name "A" :crew #{:x} :take 1 :cuts {:x 1}}))]
    (is (instance? Heist h))
    (is (= "A" (:name h))))
  (testing "and from an explicit draft of the type, which once did not work"
    (let [h (v/revising [x (v/draft Heist)]
              (merge x {:name "B" :crew #{:x} :take 1 :cuts {:x 1}}))]
      (is (instance? Heist h))
      (is (= "B" (:name h))))))

(deftest revising-is-re-entrant
  (testing "standalone, the inner form seals"
    (is (instance? Heist (heist/promote job :lyle 250))))
  (testing "nested, it passes the draft through and only the outer form seals"
    (let [seen (atom nil)
          h    (v/revising [x job]
                 (let [inner (heist/promote x :lyle 250)]
                   (reset! seen (v/draft? inner))
                   (heist/rename inner "The Sequel")))]
      (is (true? @seen))
      (is (instance? Heist h))
      (is (= "The Sequel" (:name h)))
      (is (= 3250 (:take h))))))

(deftest a-draft-made-outside-any-revising-is-adopted
  (is (instance? Heist (v/revising [x (v/draft job)] (heist/rename x "S"))))
  (testing "or can be sealed by hand"
    (is (instance? Heist (v/seal (heist/rename (v/draft job) "S"))))))

(deftest a-failed-revising-returns-nothing-and-changes-nothing
  (is (thrown? clojure.lang.ExceptionInfo
               (v/revising [x job] (assoc x :take 9999))))
  (is (= 3000 (:take job)))
  (is (= {:charlie 1000 :lyle 1000 :left-ear 1000} (:cuts job))))

;;; ------------------------------------ the seal contract ------------------------------------

(deftest whatever-seal-returns-passes-every-check
  ;; The guarantee is not "seal ran the checks it selected" but "the result is valid as if
  ;; every check had run". Assert the second directly: seal's key selection is the thing
  ;; under test, so re-running everything is the only honest oracle.
  (let [model     (v/model-of job)
        changeset (fn [i] {:op :add-member :who (keyword (str "m" i)) :cut (* 10 (inc i))})
        history   (reductions (fn [h i] (v/revising [x h] (heist/apply-changeset x (changeset i))))
                              job (range 25))]
    (doseq [[i h] (map-indexed vector history)]
      (is (instance? Heist h))
      (doseq [[k pred] (:preds model)]
        (is (pred (get h k)) (str "step " i ", field " k)))
      (doseq [^Invariant inv (:invariants model)]
        (is ((.-pred inv) h) (str "step " i ", invariant " (.-doc inv)))))))
