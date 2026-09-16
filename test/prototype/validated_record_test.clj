(ns prototype.validated-record-test
  "Reads and writes on a validated value: which checks run, which do not, and what they throw."
  (:require
   [clojure.test :refer [deftest is testing]]
   [prototype.heist :as heist]
   [prototype.validated-record :as v])
  (:import
   (prototype.heist Heist)))

(def job heist/italian-job)

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ----------------------------------------- reading -----------------------------------------

(deftest reads-like-a-map
  (is (= 3000 (:take job)))
  (is (= 3000 (get job :take)))
  (is (nil? (get job :nope)))
  (is (= ::missing (get job :nope ::missing)))
  (is (= ["The Italian Job" 3000] (let [{:keys [name take]} job] [name take])))
  (is (= [:name :crew :take :cuts] (keys job)))
  (is (= 4 (count job)))
  (is (contains? job :cuts))
  (is (= {:name "The Italian Job"
          :crew #{:charlie :lyle :left-ear}
          :take 3000
          :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}
         (into {} job))))

(deftest carries-meta
  (let [m (with-meta job {:src :test})]
    (is (= {:src :test} (meta m)))
    (is (instance? Heist m))
    (is (= 3000 (:take m)))
    (is (nil? (meta job)) "with-meta does not mutate the original")))

;;; ----------------------------------------- writing -----------------------------------------

(deftest assoc-checks-the-field
  (let [h (assoc job :name "The Sequel")]
    (is (instance? Heist h))
    (is (= "The Sequel" (:name h)))
    (is (= "The Italian Job" (:name job)) "the original is untouched"))
  (let [e (err #(assoc job :take "lots"))]
    (is (= ::v/invalid-field (:error e)))
    (is (= :take (:key e)))
    (is (= "lots" (:value e)))))

(deftest assoc-rejects-a-key-the-type-does-not-declare
  (let [e (err #(assoc job :getaway-car "mini"))]
    (is (= ::v/unknown-field (:error e)))
    (is (= :getaway-car (:key e)))))

(deftest assoc-runs-the-invariants-that-depend-on-the-key
  (let [e (err #(assoc job :take 4000))]
    (is (= ::v/invariant-violated (:error e)))
    (is (= "the cuts add up to the take" (:invariant e)))
    (is (= #{:take :cuts} (:depends-on e)))
    (is (= #{:take :cuts} (set (keys (:values e))))
        "the failure reports the values the invariant read")))

(deftest dissoc-refuses-a-declared-field
  (let [e (err #(dissoc job :cuts))]
    (is (= ::v/cannot-dissoc (:error e)))
    (is (= :cuts (:key e)))
    (is (= '(assoc m :cuts nil) (:instead e)) "and proposes the rewrite")))

(deftest dissoc-of-an-undeclared-key-is-the-ordinary-no-op
  (is (identical? job (dissoc job :getaway-car))
      "a validated value cannot be holding one, so there is nothing to remove"))

;;; ------------------------------------- merge is atomic -------------------------------------

(deftest merge-applies-every-entry-before-checking-invariants
  (let [h (merge job {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})]
    (is (instance? Heist h))
    (is (= 4000 (:take h))))
  (testing "either key on its own breaks the invariant the pair satisfies"
    (is (= ::v/invariant-violated (:error (err #(assoc job :take 4000)))))))

(deftest merge-still-checks-what-it-writes
  (is (= ::v/invariant-violated
         (:error (err #(merge job {:take 4000 :cuts {:charlie 1 :lyle 1 :left-ear 1}})))))
  (is (= ::v/unknown-field (:error (err #(merge job {:getaway-car "mini"})))))
  (is (= ::v/invalid-field (:error (err #(merge job {:name ""}))))))

;;; ------------------------- only the applicable invariants run -------------------------

(def ^:private runs (atom 0))

(defn- counted [pred] (fn [m] (swap! runs inc) (pred m)))

(v/defvalidated Counted
  [^{:valid int?} a ^{:valid int?} b ^{:valid int?} c]
  [{:doc "a is below b" :keys #{:a :b} :pred (counted (fn [m] (< (:a m) (:b m))))}])

(def ^:private counted-base (v/revising [x Counted] (merge x {:a 1 :b 2 :c 3})))

(deftest a-write-runs-only-the-invariants-declaring-that-key
  (reset! runs 0)
  (assoc counted-base :c 99)
  (is (zero? @runs) "no invariant declares :c")
  (assoc counted-base :a 0)
  (is (= 1 @runs) "one does declare :a"))

(deftest merge-checks-the-invariants-once-not-once-per-key
  (reset! runs 0)
  (merge counted-base {:a 1 :b 5})
  (is (= 1 @runs) "both keys feed the same invariant"))
