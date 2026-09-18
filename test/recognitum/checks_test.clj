(ns recognitum.checks-test
  "The `:checks` map: the three forms a check is written in, and the arglist as the
  dependency declaration. The field -> checks index is derived from the arglists, and a
  body can read nothing it did not name."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))
(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which interns
  ;; vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.checks-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))

(defn- checks-on
  "The names of the checks `model` runs when `k` is written, in declaration order."
  [model k]
  (mapv (comp :name (:checks model)) (get (:index model) k)))

;;; --------------------------------- the three check forms ---------------------------------

(rec/defrecord Forms
  [a b]
  {:checks
   {::a int?                               ; shorthand: a bare fn over the field the key names
    ::b-positive ([b] (pos? b))            ; arglist and body
    ::ordered ("a is below b"              ; docstring, arglist and body
               [a b] (and (int? a) (int? b) (< a b)))}})

(deftest all-three-forms-define-a-check
  (is (rec/sealed? (->Forms 1 2)))
  (testing "the shorthand checks the single field its key names"
    (let [e (err #(->Forms "x" 2))]
      (is (= ::rec/check-failed (:error e)))
      (is (= 'Forms (:record e)))
      (is (contains? (:failures e) ::a))
      (is (= {:doc nil :values {::a "x"} :returned false :stopped [::ordered]} (get (:failures e) ::a)))))
  (testing "the arglist form"
    (let [e (err #(->Forms 1 -1))]
      (is (= ::rec/check-failed (:error e)))
      (is (contains? (:failures e) ::b-positive))
      (is (= {:doc nil :values {::b -1} :returned false} (get (:failures e) ::b-positive)))))
  (testing "the docstring form, and only one check can fail here"
    (let [e (err #(->Forms 5 2))]
      (is (= [::ordered] (keys (:failures e))))
      (is (= {:doc "a is below b" :values {::a 5 ::b 2} :returned false}
             (get (:failures e) ::ordered))))))

(deftest a-failure-reports-the-declared-fields-in-arglist-order
  (let [given (:values (get (:failures (err #(->Forms 5 2))) ::ordered))]
    (is (= [::a ::b] (keys given)) "the arglist is the order, and it survives at any size")
    (is (= [5 2] (vals given)))))

(deftest what-the-check-returned-is-carried-as-failure-data
  (is (= false (:returned (get (:failures (err #(->Forms 5 2))) ::ordered)))))

(rec/defrecord Nilly
  [a]
  {:checks {::a ([a] (when (int? a) true))}})

(deftest a-check-returning-nil-fails-and-nil-is-what-is-reported
  (is (rec/sealed? (->Nilly 1)))
  (let [e (err #(->Nilly "x"))]
    (is (= ::rec/check-failed (:error e)))
    (is (= {:doc nil :values {::a "x"} :returned nil} (get (:failures e) ::a)))))

;;; ------------------------------- the shorthand takes a value -------------------------------

(def ^:private saw (atom ::nothing))

(rec/defrecord Shorthand
  [v]
  ;; A `(fn ...)` literal is a list, and so is the arglist form -- they are told apart by what
  ;; the list starts with, so this is the shorthand and receives the field's value.
  {:checks {::v (fn [x] (reset! saw x) (string? x))}})

(deftest the-shorthand-receives-the-field-value-not-the-record
  (reset! saw ::nothing)
  (->Shorthand "hi")
  (is (= "hi" @saw))
  (is (= ::rec/check-failed (:error (err #(->Shorthand 1))))))

;;; --------------------------- the arglist is the dependency list ---------------------------

(def ^:private got (atom nil))

(rec/defrecord Ordered
  [a b]
  {:checks {::reversed ([b a] (reset! got [b a]) true)}})

(deftest the-body-receives-its-arglist-positionally-in-the-order-written
  (reset! got nil)
  (->Ordered 1 2)
  (is (= [2 1] @got) "b then a -- the arglist's order, not the field order")
  (testing "and the model records that order"
    (is (= [[::b ::a]] (mapv :deps (:checks (internal/model-of (->Ordered 1 2))))))))

(deftest a-check-body-cannot-read-a-field-it-did-not-name
  ;; Nothing but the arglist is in scope, so an undeclared dependency does not compile.
  (let [t (compiles '(recognitum.core/defrecord Peeker
                       [a b]
                       {:checks {:scratch/peek ([a] (+ a b))}}))]
    (is (some? t))
    (is (re-find #"Unable to resolve symbol: b" (ex-message (root-cause t)))))
  (testing "while naming it compiles"
    (is (nil? (compiles '(recognitum.core/defrecord Namer
                           [a b]
                           {:checks {:scratch/named ([a b] (< a b))}}))))))

(deftest a-check-takes-exactly-one-arity
  (let [e (compile-data '(recognitum.core/defrecord Multi
                           [a b]
                           {:checks {:scratch/two (([a] (int? a)) ([a b] (< a b)))}}))]
    (is (= ::rec/malformed-check (:error e)))
    (is (= :scratch/two (:check e)))))

(deftest a-check-form-that-does-not-parse-names-itself
  (testing "a docstring and nothing else"
    (let [e (compile-data '(recognitum.core/defrecord NoArgs [a]
                             {:checks {:scratch/c ("just a doc")}}))]
      (is (= ::rec/malformed-check (:error e)))
      (is (= :scratch/c (:check e)))))
  (testing "an arglist and no body"
    (let [e (compile-data '(recognitum.core/defrecord NoBody [a] {:checks {:scratch/c ([a])}}))]
      (is (= ::rec/malformed-check (:error e)))
      (is (= :scratch/c (:check e)))))
  (testing "a docstring and an arglist and no body"
    (let [e (compile-data '(recognitum.core/defrecord NoBody2 [a]
                             {:checks {:scratch/c ("d" [a])}}))]
      (is (= ::rec/malformed-check (:error e)))
      (is (= :scratch/c (:check e))))))

;;; ------------------------- the index comes from the arglists -------------------------

(def ^:private hits (atom {}))

(defn- tally [k v] (swap! hits update k (fnil inc 0)) v)

(rec/defrecord Indexed
  [a b c]
  {:checks
   {::a ([a] (tally ::a (int? a)))
    ::b ([b] (tally ::b (int? b)))
    ::c ([c] (tally ::c (int? c)))
    ::a-below-b ([a b] (tally ::a-below-b (and (int? a) (int? b) (< a b))))}})

(defn- indexed [] (rec/drafting [x Indexed] (merge x {::a 1 ::b 2 ::c 3})))

(deftest the-model-carries-the-checks-in-declaration-order
  (let [model (internal/model-of (indexed))]
    (is (= [::a ::b ::c ::a-below-b] (mapv :name (:checks model))))
    (is (= [[::a] [::b] [::c] [::a ::b]] (mapv :deps (:checks model))))))

(deftest the-index-maps-a-field-to-the-checks-that-name-it
  (let [model (internal/model-of (indexed))]
    (is (= [::a ::a-below-b] (checks-on model ::a)))
    (is (= [::b ::a-below-b] (checks-on model ::b)))
    (is (= [::c] (checks-on model ::c)))
    (is (empty? (checks-on model ::nothing-of-the-sort)))))

(deftest a-write-runs-exactly-the-checks-naming-that-field
  (let [v (indexed)]
    (reset! hits {})
    (assoc v ::c 9)
    (is (= {::c 1} @hits) "nothing else names c")
    (reset! hits {})
    (assoc v ::a 0)
    (is (= {::a 1 ::a-below-b 1} @hits) "both checks naming a, once each")))

(deftest merge-runs-each-applicable-check-once-not-once-per-key
  (let [v (indexed)]
    (reset! hits {})
    (merge v {::a 0 ::b 5})
    (is (= {::a 1 ::b 1 ::a-below-b 1} @hits))))

(deftest construction-runs-every-check
  (reset! hits {})
  (->Indexed 1 2 3)
  (is (= {::a 1 ::b 1 ::c 1 ::a-below-b 1} @hits))
  (reset! hits {})
  (map->Indexed {::a 1 ::b 2 ::c 3})
  (is (= {::a 1 ::b 1 ::c 1 ::a-below-b 1} @hits)))

(deftest seal-runs-the-checks-naming-what-the-draft-wrote
  (let [v (indexed)]
    (reset! hits {})
    (rec/drafting [x v] (assoc x ::c 4))
    (is (= {::c 1} @hits))
    (reset! hits {})
    (rec/drafting [x v] (-> x (assoc ::a 0) (assoc ::b 9)))
    (is (= {::a 1 ::b 1 ::a-below-b 1} @hits)
        "the invariant validation runs once, not twice")))

(deftest a-draft-write-runs-no-check-at-all
  (let [v (indexed)]
    (reset! hits {})
    (let [d (assoc (rec/draft v) ::a "not an int")]
      (is (= {} @hits))
      (is (= "not an int" (::a d))))
    (testing "and seal is where it fails"
      (is (= ::rec/check-failed
             (:error (err #(rec/seal (assoc (rec/draft v) ::a "not an int")))))))))

;;; ------------------------ a check that names nothing is not skipped ------------------------

(def ^:private grounded (atom 0))

(rec/defrecord Grounded
  [a]
  {:checks
   {::a int?
    ::always ("depends on nothing at all" [] (do (swap! grounded inc) true))}})

(deftest a-check-with-an-empty-arglist-runs-where-there-is-no-earlier-verdict-to-trust
  (reset! grounded 0)
  (let [g (->Grounded 1)]
    (is (= 1 @grounded) "construction runs it")
    (reset! grounded 0)
    (rec/drafting [x Grounded] (assoc x ::a 2))
    (is (= 1 @grounded) "so does building one from the type")
    (reset! grounded 0)
    (assoc g ::a 3)
    (is (= 0 @grounded) "but a write cannot disturb a check that names no field")))

;;; ---------------------------- unknown keys are still unknown ----------------------------

(deftest a-key-the-type-does-not-declare-is-still-an-unknown-field
  (let [v (indexed)]
    (is (= ::rec/unknown-field (:error (err #(assoc v :a 1))))
        "a bare key naming a field is not a field")
    (is (= ::rec/unknown-field (:error (err #(assoc v ::nope 1)))))
    (is (= ::rec/unknown-field (:error (err #(merge v {::nope 1})))))
    (testing "and a check name is not a key either"
      (is (= ::rec/unknown-field (:error (err #(assoc v ::a-below-b 1))))))))

;;; ------------------------------ two checks over one field ------------------------------

(rec/defrecord Doubled
  [n]
  {:checks
   {::an-int ([n] (int? n))
    ::in-range ("n is between 1 and 10" [n] (and (int? n) (<= 1 n 10)))}})

(deftest several-checks-may-name-the-same-field-and-all-of-them-run
  (is (rec/sealed? (->Doubled 5)))
  (let [model (internal/model-of (->Doubled 5))]
    (is (= [::an-int ::in-range] (checks-on model ::n))))
  (let [e (err #(->Doubled 99))]
    (is (= [::in-range] (keys (:failures e))) "only the one it breaks")
    (is (= "n is between 1 and 10" (:doc (get (:failures e) ::in-range)))))
  (let [e (err #(assoc (->Doubled 5) ::n 99))]
    (is (= ::rec/check-failed (:error e)))
    (is (contains? (:failures e) ::in-range))))
