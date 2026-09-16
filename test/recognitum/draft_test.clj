(ns recognitum.draft-test
  "Drafts, `drafting`, and what `seal` guarantees."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.heist :as heist]
   [recognitum.core :as rec])
  (:import
   (recognitum.heist Heist)))

(def job heist/italian-job)

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ------------------------------- a draft validates nothing -------------------------------

(deftest writes-into-a-draft-are-unchecked
  (let [d (assoc (rec/draft job) ::heist/take "nope")]
    (is (rec/draft? d))
    (is (= "nope" (::heist/take d)) "the bad value is simply there"))
  (testing "seal is where it fails"
    (is (contains? (:failures (err #(rec/seal (assoc (rec/draft job) ::heist/take "nope"))))
                   ::heist/take))))

(deftest a-draft-may-pass-through-states-no-single-write-could
  (testing "either write alone throws on a validated value"
    (is (= ::rec/check-failed (:error (err #(assoc job ::heist/take 3500))))))
  (testing "but the pair is fine inside one drafting"
    (is (instance? Heist (rec/drafting [x job] (heist/add-member x :napster 500))))))

;;; ---------------------------------------- sealing ----------------------------------------

(deftest seal-reports-keys-the-type-does-not-declare
  (let [e (err #(rec/drafting [x job] (assoc x :getaway-car "mini")))]
    (is (= ::rec/extraneous-keys (:error e)))
    (is (= [:getaway-car] (:keys e)))))

(deftest clean-strips-them-instead
  (is (instance? Heist (rec/drafting [x job] (-> x (assoc :getaway-car "mini") rec/clean)))))

(deftest dissoc-removes-one-undeclared-key-from-a-draft
  (is (instance? Heist (rec/drafting [x job] (-> x (assoc :getaway-car "mini") (dissoc :getaway-car)))))
  (testing "a declared field is still permanent"
    (is (= ::rec/cannot-dissoc (:error (err #(dissoc (rec/draft job) ::heist/cuts))))))
  (testing "and removing an absent undeclared key allocates nothing"
    (let [d (rec/draft job)]
      (is (identical? d (dissoc d :never-there))))))

(deftest seal-reports-a-field-never-written
  (let [e (err #(rec/drafting [x Heist] (assoc x ::heist/name "X")))]
    (is (= ::rec/missing-field (:error e)))
    (is (contains? #{::heist/crew ::heist/take ::heist/cuts} (:key e)))))

;;; --------------------------- "never written" is not "written to nil" ---------------------------

(rec/defrecord Note
  [title body]
  {:checks
   {::title string?
    ::body  ([body] (or (nil? body) (string? body)))}})

(deftest nil-is-a-legal-value-when-the-check-admits-it
  (let [n (rec/drafting [x Note] (merge x {::title "t" ::body nil}))]
    (is (nil? (::body n)))
    (is (= [::title ::body] (keys n)) "the field is present, holding nil"))
  (testing "leaving it unwritten is a different thing entirely"
    (is (= ::rec/missing-field (:error (err #(rec/drafting [x Note] (assoc x ::title "t"))))))))

;;; --------------------------------------- drafting ---------------------------------------

(deftest drafting-builds-from-the-type-itself
  (let [h (rec/drafting [x Heist]
            (merge x {::heist/name "A" ::heist/crew #{:x} ::heist/take 1 ::heist/cuts {:x 1}}))]
    (is (instance? Heist h))
    (is (= "A" (::heist/name h))))
  (testing "and from an explicit draft of the type, which once did not work"
    (let [h (rec/drafting [x (rec/draft Heist)]
              (merge x {::heist/name "B" ::heist/crew #{:x} ::heist/take 1 ::heist/cuts {:x 1}}))]
      (is (instance? Heist h))
      (is (= "B" (::heist/name h))))))

(deftest drafting-is-re-entrant
  (testing "standalone, the inner form seals"
    (is (instance? Heist (heist/promote job :lyle 250))))
  (testing "nested, it passes the draft through and only the outer form seals"
    (let [seen (atom nil)
          h    (rec/drafting [x job]
                 (let [inner (heist/promote x :lyle 250)]
                   (reset! seen (rec/draft? inner))
                   (heist/rename inner "The Sequel")))]
      (is (true? @seen))
      (is (instance? Heist h))
      (is (= "The Sequel" (::heist/name h)))
      (is (= 3250 (::heist/take h))))))

(deftest seal-is-idempotent
  ;; A sealed value is a fixed point of seal, so seal is a projection: seal . seal = seal.
  (is (identical? job (rec/seal job)))
  (let [once (rec/seal (rec/draft job))]
    (is (identical? once (rec/seal once))))
  (testing "which lets a drafting body end in a call that may already have sealed"
    (is (= 3250 (::heist/take (rec/drafting [x job] (heist/promote job :lyle 250)))))
    (is (= "S"  (::heist/name (rec/drafting [x job] (rec/seal (heist/rename x "S")))))))
  (testing "but a value that is neither a draft nor sealed is still refused"
    (is (= ::rec/cannot-seal (:error (err #(rec/seal {:a 1})))))
    (is (= ::rec/cannot-seal (:error (err #(rec/drafting [x job] {:a 1})))))))

(deftest sealed?-distinguishes-the-two
  (is (true?  (rec/sealed? job)))
  (is (false? (rec/sealed? (rec/draft job))))
  (is (false? (rec/sealed? {})))
  (is (false? (rec/draft? job)))
  (is (true?  (rec/draft? (rec/draft job)))))

(deftest a-draft-made-outside-any-drafting-is-adopted
  (is (instance? Heist (rec/drafting [x (rec/draft job)] (heist/rename x "S"))))
  (testing "or can be sealed by hand"
    (is (instance? Heist (rec/seal (heist/rename (rec/draft job) "S"))))))

(deftest a-failed-drafting-returns-nothing-and-changes-nothing
  (is (thrown? clojure.lang.ExceptionInfo
               (rec/drafting [x job] (assoc x ::heist/take 9999))))
  (is (= 3000 (::heist/take job)))
  (is (= {:charlie 1000 :lyle 1000 :left-ear 1000} (::heist/cuts job))))

;;; ------------------------------------ the seal contract ------------------------------------

(deftest whatever-seal-returns-passes-every-check
  ;; The guarantee is not "seal ran the checks it selected" but "the result is valid as if
  ;; every check had run". Assert the second directly: seal's key selection is the thing
  ;; under test, so re-running everything is the only honest oracle. Each check is called
  ;; here the way it declared it wanted to be -- its arglist, read off the model, is the
  ;; whole of what it depends on.
  (let [model     (rec/model-of job)
        run       (fn [{:keys [deps pred]} h] (apply pred (map #(get h %) deps)))
        changeset (fn [i] {:op :add-member :who (keyword (str "m" i)) :cut (* 10 (inc i))})
        history   (reductions (fn [h i] (rec/drafting [x h] (heist/apply-changeset x (changeset i))))
                              job (range 25))]
    (is (= 6 (count (:checks model))) "and every one of them is exercised")
    (doseq [[i h] (map-indexed vector history)]
      (is (instance? Heist h))
      (doseq [c (:checks model)]
        (is (true? (run c h)) (str "step " i ", check " (:name c)))))))
