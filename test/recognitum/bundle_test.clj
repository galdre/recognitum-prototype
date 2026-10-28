(ns recognitum.bundle-test
  "Bundles bound into types: input clauses, `:when-extant`, copies, overrides, waits on bundle
  checks, and concerns."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.example.card :as card]
   [recognitum.example.db :as db]
   [recognitum.example.perms :as perms])
  (:import
   (recognitum.example.card Card Dashboard)))

(defn- failed [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:failed (ex-data e)))))

(def row {::card/id 1 ::card/name "Plan" ::card/collection-id 1 ::card/archived false
          ::card/acting-user "ann"})

(deftest a-bundle-is-a-var-and-each-check-a-var-with-its-id
  (is (= [:recognitum.example.perms/collection-exists :recognitum.example.perms/may-write]
         (mapv :id [perms/collection-exists perms/may-write])))
  (is (= "the acting user may write to the collection" (:doc (meta #'perms/may-write))))
  (is (= #{::perms/permission} (:concerns perms/may-write))))

(deftest a-bound-bundle-reads-its-inputs-from-the-fields
  (is (instance? Card (card/map->Card row)))
  (is (= {:data false :doc "the acting user may write to the collection"
          :blocked #{::card/moves-only-where-allowed}}
         (::perms/may-write (failed #(card/map->Card (assoc row ::card/acting-user "bob")))))))

(deftest an-input-whose-field-is-absent-is-absent
  (is (= {:absent :user} (:data (::perms/may-write (failed #(card/map->Card (dissoc row ::card/acting-user))))))))

(deftest when-extant-skips-a-binding-as-passing
  (testing "no stored collection: the copy's checks pass without running"
    (is (contains? (rec/proven-checks (card/map->Card row)) ::perms/current-may-write)))
  (testing "a stored collection runs them"
    (is (= #{::perms/current-may-write}
           (set (keys (failed #(card/map->Card (assoc row ::card/stored-collection-id 2))))))))
  (testing "writing the field later runs the binding it gated"
    (let [c (card/map->Card row)]
      (is (instance? Card (assoc c ::card/stored-collection-id 3)))
      (is (contains? (failed #(assoc c ::card/stored-collection-id 2)) ::perms/current-may-write)))))

(deftest a-copy-has-its-own-ids-in-its-namespace-and-keeps-concerns
  (is (= ::perms/current-may-write (:id perms/current-may-write)))
  (is (= #{::perms/permission} (:concerns perms/current-may-write)))
  (is (= #{::perms/current-collection-exists} (:waits perms/current-may-write))))

(deftest an-override-keeps-the-id-and-concerns
  (let [d (card/map->Dashboard {::card/id 1 ::card/title "Board" ::card/collection-id 2 ::card/acting-user "cy"})]
    (is (instance? Dashboard d))
    (is (= #{::perms/may-write} (rec/checks-of perms/permission d)))
    (is (= "dashboards also allow the collection's curators"
           (:doc (::perms/may-write (failed #(assoc d ::card/acting-user "dee"))))))))

(deftest an-override-inherits-its-bindings-when-extant
  (let [T (binding [*ns* (the-ns 'recognitum.example.card)]
            (eval '(rec/defproven Gated [id ^:optional acting-user ^:optional stored-collection-id]
                     ^{:when-extant stored-collection-id}
                     perms/current-collection-write
                     (user [] acting-user)
                     (collection-id [] stored-collection-id)
                     rec/checks
                     ^:override (perms/current-may-write [] false))))
        make @(ns-resolve 'recognitum.example.card 'map->Gated)]
    (is (instance? T (make {::card/id 1})) "skipped where the gate field is absent")
    (is (contains? (failed #(make {::card/id 1 ::card/stored-collection-id 1})) ::perms/current-may-write))))

(deftest waits-name-bundle-checks
  (testing "a check that waits on a failed bundle check is blocked by it"
    (is (= #{::card/moves-only-where-allowed}
           (:blocked (::perms/may-write (failed #(card/map->Card (assoc row ::card/acting-user "bob"))))))))
  (testing "selections name bundle checks and bundles by their vars"
    (is (= #{::perms/may-write ::perms/current-may-write}
           (rec/checks-of (rec/union perms/may-write perms/current-may-write) Card)))
    (is (= #{::perms/collection-exists ::perms/may-write} (rec/checks-of perms/collection-write Card)))))

(deftest a-bundle-check-waits-on-the-shape-checks-of-the-fields-its-inputs-read
  (reset! db/calls [])
  (let [e (try (card/map->Card (assoc row ::card/collection-id "one"))
               (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= #{::card/collection-id} (set (keys (:failed e)))))
    (is (every? (:blocked e) [::perms/collection-exists ::perms/may-write ::card/owner-active
                              ::card/moves-only-where-allowed]))
    (is (empty? @db/calls) "nothing read the collection")))

(deftest a-check-reads-a-source-keyed-by-another-read
  (is (= {:data false :doc "the collection's owner is active" :blocked #{}}
         (::card/owner-active (failed #(card/map->Card (assoc row ::card/collection-id 3 ::card/acting-user "ann"))))))
  (reset! db/calls [])
  (card/map->Card row)
  (is (some #(= :users (first %)) @db/calls)))

(def ^:private clause-runs (atom []))

(rec/defchecks positives [x y]
  rec/checks
  (x-positive [] (pos? x))
  (y-positive [] (pos? y)))

(rec/defproven Both [a b]
  positives
  (x [] (swap! clause-runs conj :x) (if (= -13 a) (throw (IllegalStateException. "unlucky")) a))
  (y [] (swap! clause-runs conj :y) b)
  rec/checks
  (a [] (int? a))
  (b [] (int? b)))

(deftest an-input-clause-runs-only-for-the-checks-that-read-its-input
  (testing "each clause runs once per value, for its one reader"
    (reset! clause-runs [])
    (is (instance? Both (map->Both {::a 1 ::b 2})))
    (is (= {:x 1 :y 1} (frequencies @clause-runs))))
  (testing "a reader that a shape check blocks runs no clause"
    (reset! clause-runs [])
    (is (= #{::a} (set (keys (failed #(map->Both {::a "one" ::b 2}))))))
    (is (= [:y] @clause-runs)))
  (testing "a throw from a clause propagates only when its reader runs"
    (let [d (into (rec/draft Both) {::a -13 ::b 2})]
      (reset! clause-runs [])
      (is (true? (rec/check d y-positive)))
      (is (= [:y] @clause-runs))
      (is (thrown-with-msg? IllegalStateException #"unlucky" (rec/check d x-positive))))))

(defn- refusal [form]
  (try (binding [*ns* (the-ns 'recognitum.bundle-test)] (eval form)) nil
       (catch clojure.lang.Compiler$CompilerException e (:message (ex-data (ex-cause e))))))

(deftest a-bare-wait-on-a-bundle-of-this-namespace-says-to-qualify-it
  (is (= (str "wait positives of c is not a check of Bare. Its own checks: c."
              " A wait on a bundle is qualified: recognitum.bundle-test/positives")
         (refusal '(rec/defproven Bare [a b] positives (x [] a) (y [] b)
                     rec/checks (c [positives] true)))))
  (is (= (str "wait x-positive of c is not a check of Bare. Its own checks: c."
              " A wait on a bundle's check is qualified: recognitum.bundle-test/x-positive")
         (refusal '(rec/defproven Bare [a b] positives (x [] a) (y [] b)
                     rec/checks (c [x-positive] true)))))
  (is (nil? (refusal '(rec/defproven Qualified [a b] positives (x [] a) (y [] b)
                       rec/checks (c [recognitum.bundle-test/positives] true))))
      "qualified, the wait is accepted"))

(rec/defcopy other-collection-write perms/collection-write
  {collection-exists other-collection-exists
   may-write         other-may-write})

(deftest a-copys-ids-are-in-the-namespace-of-the-copy
  (is (= [::other-collection-exists ::other-may-write]
         (mapv :id [other-collection-exists other-may-write])))
  (is (= #{::other-collection-exists} (:waits other-may-write)))
  (is (= #{::perms/permission} (:concerns other-may-write))))

(deftest a-qualified-clause-replaces-a-bundle-check-only-when-marked
  (is (= (str "recognitum.bundle-test/x-positive is qualified:"
              " mark it ^:override to replace that bundle check")
         (refusal '(rec/defproven Unmarked [a b] positives (x [] a) (y [] b)
                     rec/checks (recognitum.bundle-test/x-positive [] true)))))
  (is (= "^:override needs a qualified bundle check, got x-positive"
         (refusal '(rec/defproven Plain [a b] positives (x [] a) (y [] b)
                     rec/checks ^:override (x-positive [] true)))))
  (is (nil? (refusal '(rec/defproven Marked [a b] positives (x [] a) (y [] b)
                       rec/checks ^:override (recognitum.bundle-test/x-positive [] true))))
      "marked, the override is accepted"))

(deftest two-input-clauses-of-one-name-are-refused
  (is (= "two input clauses of positives are named x"
         (refusal '(rec/defproven Twice [a b] positives (x [] a) (x [] 1) (y [] b)
                     rec/checks (a [] true))))))
