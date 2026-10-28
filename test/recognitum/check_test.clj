(ns recognitum.check-test
  "Selections, check-all across types, and what proves? asks."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.engine :as engine]
   [recognitum.example.card :as card]
   [recognitum.example.db :as db]
   [recognitum.example.perms :as perms]
   [recognitum.heist :as h])
  (:import
   (recognitum.example.card Card Dashboard)
   (recognitum.heist Heist)))

(def row {::card/id 1 ::card/name "Plan" ::card/collection-id 1 ::card/archived false
          ::card/acting-user "ann"})

(def board {::card/id 7 ::card/title "Board" ::card/collection-id 2 ::card/acting-user "cy"})

;;; ------------------------------------------------------------------ selections

(deftest selections-name-checks-in-a-type
  (is (= #{::card/name ::card/collection-id ::card/archived} (rec/checks-of rec/shape Card)))
  (is (= #{::perms/may-write ::perms/current-may-write ::card/moves-only-where-allowed}
         (rec/checks-of perms/permission Card)
         (rec/checks-of ::perms/permission Card))
      "a concern by its var or its keyword")
  (is (= #{::card/owner-active} (rec/checks-of (rec/fetching db/users) Card)))
  (is (= #{::card/name ::card/collection-id ::card/archived ::card/not-archived-on-edit
           ::card/moves-only-where-allowed}
         (rec/checks-of rec/pure Card)))
  (is (= #{::perms/current-collection-exists ::perms/current-may-write ::card/not-archived-on-edit}
         (rec/checks-of (rec/union (rec/reading ::card/stored-collection-id)
                                   (rec/reading ::card/stored-archived))
                        Card)))
  (is (= #{::card/name} (rec/checks-of (rec/except rec/shape ::card/collection-id ::card/archived) Card)))
  (testing "a selection that names nothing in a type selects nothing there"
    (is (= #{} (rec/checks-of perms/permission Heist)))
    (is (rec/proves? h/italian-job perms/permission))))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))

(deftest a-keyword-naming-nothing-is-refused-when-the-selection-is-built
  (is (= ["no concern, bundle check or check of a type is named :recognitum.example.perms/permision"
          {:error :recognitum/unknown-name :name ::perms/permision}]
         (refusal #(rec/except rec/every ::perms/permision))))
  (is (= :recognitum/unknown-name (:error (second (refusal #(rec/union rec/shape ::card/nmae))))))
  (is (= :recognitum/unknown-name (:error (second (refusal #(rec/union :no.such.ns/check))))))
  (testing "a concern, a bundle check, or a check of a type of the keyword's namespace is accepted"
    (is (map? (rec/union ::perms/permission ::perms/may-write ::card/name ::card/owner-active)))))

(deftest an-own-check-id-a-type-lacks-is-refused-when-the-selection-meets-the-type
  (is (= ["recognitum.example.card/Card has no check :recognitum.example.card/nmae, and no concern or bundle check is named :recognitum.example.card/nmae"
          {:error :recognitum/unknown-name :name ::card/nmae :type 'recognitum.example.card/Card}]
         (refusal #(rec/checks-of ::card/nmae Card))))
  (is (= 'recognitum.heist/Heist (:type (second (refusal #(rec/checks-of ::card/name Heist)))))
      "another type's own check")
  (is (= 'recognitum.example.card/Dashboard
         (:type (second (refusal #(rec/checks-of (rec/union ::card/owner-active) Dashboard))))))
  (is (= :recognitum/unknown-name (:error (second (refusal #(rec/check (rec/draft Card) ::perms/permision)))))))

(deftest a-concern-or-bundle-check-a-type-lacks-selects-nothing-in-it
  (is (= #{} (rec/checks-of ::perms/permission Heist)))
  (is (= #{} (rec/checks-of ::perms/may-write Heist)))
  (is (= #{} (rec/checks-of perms/may-write Heist))))

;;; ------------------------------------------------------------------ check-all

(deftest check-all-runs-a-group-of-several-types-in-one-run
  (let [runs (atom [])
        run-all engine/run-all
        drafts [(into (rec/draft Card) row)
                (into (rec/draft Dashboard) board)
                (into (rec/draft Card) (assoc row ::card/acting-user "bob"))]]
    (with-redefs [engine/run-all (fn [items] (swap! runs conj (count items)) (run-all items))]
      (let [[a b c] (rec/check-all drafts perms/may-write)]
        (is (true? a))
        (is (true? b))
        (is (= #{::perms/may-write} (set (keys (:failed c)))))))
    (is (= [3] @runs))))

(def ^:private runs (atom []))

(rec/defproven Counted [a b]
  rec/checks
  (a [] (swap! runs conj ::a) (int? a))
  (b [] (swap! runs conj ::b) (int? b))
  (a-below-b [] (swap! runs conj ::a-below-b) (< a b)))

(deftest check-all-remembers-on-each-draft-as-check-does
  (let [d1 (into (rec/draft Counted) {::a 1 ::b 2})
        d2 (into (rec/draft Counted) {::a 3 ::b 4})
        v (map->Counted {::a 5 ::b 6})]
    (reset! runs [])
    (is (= [true true true] (rec/check-all [d1 d2 v] rec/shape)))
    (is (= {::a 2 ::b 2} (frequencies @runs)) "the sealed value runs nothing")
    (is (= #{::a ::b} (rec/proven-checks d1) (rec/proven-checks d2)))
    (is (rec/sealed? v))
    (is (= (rec/checks-of rec/every Counted) (rec/proven-checks v)))
    (testing "sealing each draft runs only the rest"
      (reset! runs [])
      (is (instance? Counted (rec/seal d1)))
      (is (instance? Counted (rec/seal d2)))
      (is (= [::a-below-b ::a-below-b] @runs))))
  (testing "a failed check is not remembered, and a check that did not run is left as it was"
    (let [d (into (rec/draft Counted) {::a 1 ::b "two"})]
      (rec/check d ::a)
      (is (= [{:failed {::b {:data false :blocked #{::a-below-b}}} :blocked #{::a-below-b}}]
             (rec/check-all [d] rec/every)))
      (is (= #{::a} (rec/proven-checks d))))))

(deftest check-all-runs-nothing-on-sealed-values
  (let [runs (atom 0)
        run-all engine/run-all
        values [(card/map->Card row) h/italian-job]]
    (with-redefs [engine/run-all (fn [items] (swap! runs inc) (run-all items))]
      (is (= [true true] (rec/check-all values rec/every))))
    (is (zero? @runs))))

(deftest a-wait-pulls-back-a-check-that-except-removed
  (let [d (into (rec/draft Card) (dissoc row ::card/acting-user))
        sel (rec/except rec/every perms/permission)]
    (testing "owner-active waits on collection-exists only, so leaving out permissions leaves out may-write"
      (is (true? (rec/check d sel))))
    (testing "a check that waits on may-write pulls it back, and the report says why it ran"
      (let [T (binding [*ns* (the-ns 'recognitum.example.card)]
                (eval '(rec/defproven Puller [collection-id ^:optional acting-user]
                         perms/collection-write
                         (user [] acting-user)
                         (collection-id [] collection-id)
                         rec/checks
                         (audited [perms/collection-write] true))))
            r (rec/check (into (rec/draft T) {::card/collection-id 1}) sel)]
        (is (= #{::perms/may-write} (set (keys (:failed r)))))
        (is (= #{::card/audited} (get-in r [:pulled-back ::perms/may-write :waited-on-by])))
        (is (re-find #"ran although an except removed it" (get-in r [:pulled-back ::perms/may-write :hint])))))))

;;; ------------------------------------------------------------------ proves?

(deftest proves?-holds-for-every-sealed-value-and-asks-a-drafts-memory
  (is (rec/proves? (card/map->Card row) rec/every))
  (let [d (into (rec/draft Card) row)]
    (is (not (rec/proves? d rec/shape)))
    (rec/check d rec/shape)
    (is (rec/proves? d rec/shape))
    (testing "a check runs with what it waits on"
      (rec/check d ::card/moves-only-where-allowed)
      (is (every? (rec/proven-checks d) [::perms/may-write ::perms/collection-exists])))
    (testing "every shape check of a present or required field is a floor under each proof"
      (let [d (into (rec/draft Card) row)]
        (rec/check d ::perms/collection-exists)
        (is (not (rec/proves? d ::perms/collection-exists)))
        (rec/check d rec/shape)
        (is (rec/proves? d ::perms/collection-exists))))
    (is (contains? (rec/proven-concerns (rec/draft (card/map->Card row))) ::perms/permission)))
  (is (not (rec/proves? row rec/every)) "a plain map proves nothing"))

(deftest the-floor-under-a-drafts-proof-is-its-shape-checks-only
  (let [d (into (rec/draft Counted) {::a 1 ::b 2})]
    (rec/check d ::a)
    (is (not (rec/proves? d ::a)) "the shape check of the required field b is part of every proof")
    (rec/check d ::b)
    (is (rec/proves? d ::a))
    (is (not (contains? (rec/proven-checks d) ::a-below-b)) "an invariant that was not asked for is not")))
