(ns recognitum.contract-test
  "The check contract, and what every entry point owes when a check says no.

  A check returns `true` -- exactly `true` -- to pass. Anything else is a failure, and what
  it returned is the failure data. An exception is neither: a throw is a bug in the check
  rather than a verdict about the data, so it propagates untouched rather than being
  reported as invalid.

  And nothing stops at the first failure. Every entry point runs everything it was asked to
  run, collects every failure, and throws once carrying all of them, keyed by check name in
  declaration order."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))
(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates, which is the point in half this namespace."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ============================== exactly `true` passes ==============================

(rec/defrecord Strict
  [v]
  ;; Returns what it was given when it disapproves, so the failure data is visibly the
  ;; check's return value rather than a constant the library supplied.
  {:checks {::strict ("v is :ok" [v] (if (= v :ok) true v))}})

(def ^:private ok (->Strict :ok))

(deftest true-passes
  (is (rec/sealed? ok))
  (is (= :ok (::v ok))))

(deftest a-truthy-value-that-is-not-true-is-a-failure
  (doseq [ret [:yes 1 "ok" [1 2] {:a 1} (Object.)]]
    (let [e (err #(->Strict ret))]
      (is (= ::rec/check-failed (:error e)) (str "returned " (pr-str ret)))
      (is (= ret (:returned (get (:failures e) ::strict)))
          "and what it returned is the failure data"))))

(deftest false-and-nil-are-failures-and-are-reported-as-themselves
  (is (= false (:returned (get (:failures (err #(->Strict false))) ::strict))))
  (is (= nil (:returned (get (:failures (err #(->Strict nil))) ::strict)))))

(deftest the-contract-holds-at-every-entry-point
  (doseq [[label f] [["->Strict"    #(->Strict :nope)]
                     ["map->Strict" #(map->Strict {::v :nope})]
                     ["assoc"       #(assoc ok ::v :nope)]
                     ["merge"       #(merge ok {::v :nope})]
                     ["seal"        #(rec/seal (assoc (rec/draft ok) ::v :nope))]
                     ["drafting"    #(rec/drafting [x ok] (assoc x ::v :nope))]]]
    (let [e (err f)]
      (is (= ::rec/check-failed (:error e)) label)
      (is (= 'Strict (:record e)) label)
      (is (= :nope (:returned (get (:failures e) ::strict))) label))))

(rec/defrecord Counted
  [n]
  ;; A predicate that forgot to return a boolean. It is truthy for every input it will ever
  ;; see, and it passes nothing -- which is the failure mode this rule exists to make loud.
  {:checks {::counted ([n] (count (str n)))}})

(deftest a-check-that-never-returns-true-never-passes
  (let [e (err #(->Counted 5))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 1 (:returned (get (:failures e) ::counted))))))

;;; ============================== exceptions propagate ==============================

(rec/defrecord Fragile
  [v]
  {:checks
   {::fragile ([v] (if (= v :boom)
                     (throw (IllegalStateException. "a bug in the check"))
                     (= v :ok)))}})

(def ^:private sturdy (->Fragile :ok))

(deftest a-throw-from-a-check-is-not-a-failure
  (doseq [[label f] [["->Fragile"    #(->Fragile :boom)]
                     ["map->Fragile" #(map->Fragile {::v :boom})]
                     ["assoc"        #(assoc sturdy ::v :boom)]
                     ["merge"        #(merge sturdy {::v :boom})]
                     ["seal"         #(rec/seal (assoc (rec/draft sturdy) ::v :boom))]
                     ["drafting"     #(rec/drafting [x sturdy] (assoc x ::v :boom))]]]
    (is (thrown-with-msg? IllegalStateException #"a bug in the check" (f)) label)))

(rec/defrecord Thrower
  [v]
  {:checks
   {::thrower ([v] (if (= v :ok)
                     true
                     (throw (ex-info "from inside the check" {:error ::mine}))))}})

(deftest an-ex-info-thrown-by-a-check-is-not-mistaken-for-a-check-failure
  ;; A check is free to throw an ExceptionInfo of its own, and it must come out the way it
  ;; went in -- not rewritten into the library's own :check-failed.
  (let [e (err #(->Thrower :no))]
    (is (= ::mine (:error e)))
    (is (nil? (:failures e))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"from inside the check" (->Thrower :no))))

(rec/defrecord Mixed
  [a b]
  {:checks
   {::a-int  ([a] (int? a))
    ::b-boom ([b] (if (= b :boom) (throw (IllegalStateException. "still a bug")) true))}})

(deftest a-throw-is-not-softened-by-a-failure-beside-it
  ;; "We could not determine whether this is valid" is a different claim from "this is
  ;; invalid", and the first one must not be dressed up as the second because some other
  ;; check happened to fail first.
  (is (thrown-with-msg? IllegalStateException #"still a bug" (->Mixed "not an int" :boom))))

;;; ======================= every failure, not the first one =======================

(rec/defrecord Multi
  [a b]
  ;; Written deliberately out of sorted order, so "declaration order" is distinguishable
  ;; from any order a map or a set would have imposed on its own.
  {:checks
   {::a-int     ("a is an int" [a] (int? a))
    ::a-small   ("a is under ten" [a] (and (int? a) (< a 10)))
    ::b-int     ([b] (int? b))
    ::a-below-b ("a is below b" [a b] (and (int? a) (int? b) (< a b)))}})

(def ^:private pair (->Multi 1 2))
(def ^:private multi-order [::a-int ::a-small ::b-int ::a-below-b])

(deftest construction-reports-every-failing-check
  (let [e (err #(->Multi "x" "y"))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 'Multi (:record e)))
    (is (= multi-order (vec (keys (:failures e))))
        "all four, in declaration order")))

(deftest the-map-constructor-reports-every-failing-check
  (is (= multi-order (vec (keys (:failures (err #(map->Multi {::a "x" ::b "y"}))))))))

(deftest assoc-reports-every-check-naming-the-field-that-fails
  (let [e (err #(assoc pair ::a 50))]
    (is (= [::a-small ::a-below-b] (vec (keys (:failures e))))
        "both checks over ::a that 50 breaks")
    (is (not (contains? (:failures e) ::a-int)) "and not the one it satisfies")
    (is (not (contains? (:failures e) ::b-int)) "nor one that does not name ::a")))

(deftest merge-reports-every-failure-across-every-key-it-wrote
  (is (= multi-order (vec (keys (:failures (err #(merge pair {::a "x" ::b "y"}))))))))

(deftest seal-reports-every-failure-in-the-draft
  (is (= multi-order
         (vec (keys (:failures (err #(rec/drafting [x pair]
                                       (-> x (assoc ::a "x") (assoc ::b "y")))))))))
  (testing "and so does sealing a free draft by hand"
    (is (= multi-order
           (vec (keys (:failures (err #(rec/seal (-> (rec/draft pair)
                                                     (assoc ::a "x")
                                                     (assoc ::b "y")))))))))))

(deftest every-failure-carries-the-same-three-things
  (let [fs (:failures (err #(->Multi "x" "y")))]
    (doseq [[cname f] fs]
      (is (= #{:doc :values :returned} (set (keys f))) (str cname)))
    (is (= "a is under ten" (:doc (get fs ::a-small))))
    (is (nil? (:doc (get fs ::b-int))) "a check written without a docstring has none")
    (is (= false (:returned (get fs ::a-below-b))))
    (testing "and the declared fields, with their values, in arglist order"
      (is (= {::a "x"} (:values (get fs ::a-int))))
      (is (= [::a ::b] (vec (keys (:values (get fs ::a-below-b))))))
      (is (= ["x" "y"] (vec (vals (:values (get fs ::a-below-b)))))))))

(deftest a-failure-reports-the-value-being-written-not-the-one-being-replaced
  (let [fs (:failures (err #(assoc pair ::a 50)))]
    (is (= {::a 50 ::b 2} (:values (get fs ::a-below-b))))
    (is (= {::a 50} (:values (get fs ::a-small))))))

(deftest the-message-names-every-check-that-failed
  (let [msg (try (->Multi "x" "y") nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (doseq [cname multi-order]
      (is (re-find (re-pattern (java.util.regex.Pattern/quote (str cname))) msg) (str cname)))
    (is (re-find #"a is under ten" msg) "and repeats the docstrings it was given")))

;;; ------------------------ a failure does not stop the pass ------------------------

(def ^:private ran (atom []))

(rec/defrecord Tallied
  [a b]
  {:checks
   {::first  ([a] (do (swap! ran conj ::first) false))
    ::second ([b] (do (swap! ran conj ::second) false))}})

(deftest a-failing-check-does-not-stop-the-ones-after-it-from-running
  (reset! ran [])
  (is (= ::rec/check-failed (:error (err #(->Tallied 1 2)))))
  (is (= [::first ::second] @ran)))

;;; ------------------ the failure map is ordered at any size ------------------

(rec/defrecord Ten
  [v]
  {:checks
   {::c0 ([v] (= v :ok))
    ::c1 ([v] (= v :ok))
    ::c2 ([v] (= v :ok))
    ::c3 ([v] (= v :ok))
    ::c4 ([v] (= v :ok))
    ::c5 ([v] (= v :ok))
    ::c6 ([v] (= v :ok))
    ::c7 ([v] (= v :ok))
    ::c8 ([v] (= v :ok))
    ::c9 ([v] (= v :ok))}})

(deftest ten-failures-come-back-in-declaration-order-in-an-array-map
  ;; A map literal would have stopped holding its insertion order at eight entries. The
  ;; failure map is built with `array-map` applied to the pairs instead, which does not
  ;; promote, so the order the checks ran in is the order they are reported in.
  (let [e     (err #(->Ten :no))
        model (internal/model-of (->Ten :ok))
        fs    (:failures e)]
    (is (= 10 (count fs)))
    (is (instance? clojure.lang.PersistentArrayMap fs))
    (is (= (mapv :name (:checks model)) (vec (keys fs)))
        "declaration order as the model records it, past the eight-entry cliff")
    (testing "and it is still a map"
      (doseq [cname (keys fs)]
        (is (= {:doc nil :values {::v :no} :returned false} (get fs cname)) (str cname))))
    (testing "and the order is the same every time it is produced"
      (is (= (vec (keys fs)) (vec (keys (:failures (err #(->Ten :nope))))))))))

(deftest the-order-of-the-failures-is-the-order-of-the-checks
  ;; The invariant that holds at every size: failures come out as a subsequence of the
  ;; model's own check order, which is the order the macro saw them declared in.
  (let [model (internal/model-of pair)
        order (mapv :name (:checks model))]
    (is (= multi-order order) "and at four checks that is the order they are written in")
    (doseq [f [#(->Multi "x" "y") #(merge pair {::a "x" ::b "y"}) #(assoc pair ::a 50)]]
      (let [ks (vec (keys (:failures (err f))))]
        (is (= ks (filterv (set ks) order)))))))
