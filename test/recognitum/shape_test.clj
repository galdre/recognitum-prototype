(ns recognitum.shape-test
  "Shape checks: the check named after a field, and what it gates.

  A check whose name is a field's key is that field's shape check -- the check that says
  what a well-formed value of that field is. The naming convention is the whole of the
  declaration; nothing else marks it, and nothing else has to.

  Two things follow, and both are here. A shape check names exactly its own field, or the
  form does not compile: that is what keeps the dependency structure two levels deep --
  shape checks depend on nothing, every other check depends only on shape checks, and no
  ordering among checks ever has to be computed. And every other check waits on the shape
  checks of the fields it names, so it receives well-formed input by construction and
  restates nothing. A check whose gate has not passed does not run, is not reported, and
  records no verdict: \"cuts is not a map\" is the actionable failure, and \"did not run
  because cuts is not a map\" adds nothing to it."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing. Anything that
  is not an ExceptionInfo propagates -- which is where an ungated check body throws on a
  shape the gate was supposed to have stopped."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

;;; ---------------------------------------- the type ----------------------------------------

;; `item`, `price` and `cuts` each have a shape check -- a check named after the field.
;; `tags` has none, so it has no declared shape and `::tagged` has to be total over anything.
;; `::cheap` and `::split` are the ones the gate is for: both would throw outright on a
;; price or a cuts map their shape checks reject.
(rec/defrecord Sale
  [item price cuts tags]
  {:checks
   {::item   string?
    ::price  ([price] (and (int? price) (not (neg? price))))
    ::cuts   ([cuts] (and (map? cuts) (every? int? (vals cuts))))
    ::cheap  ("the price is under a hundred" [price] (< price 100))
    ::split  ("the cuts add up to the price" [price cuts] (= price (reduce + 0 (vals cuts))))
    ::tagged ("every tag is a keyword" [tags] (and (coll? tags) (every? keyword? tags)))}
   :certificates
   {::pricing #{::cheap ::split}}})

(defn- sale [] (->Sale "hat" 50 {:a 50} [:x]))

(defn- by-name [x] (into {} (map (juxt :name identity)) (:checks (internal/model-of x))))

(defn- gate-names
  "The checks a check waits on, by name."
  [x cname]
  (let [model (internal/model-of x)
        idx   (into {} (map-indexed (fn [i c] [(:name c) i])) (:checks model))]
    (mapv (comp :name (:checks model)) (:shape-gate (nth (:checks model) (idx cname))))))

;;; ------------------------------ what the model says about them ------------------------------

(deftest a-check-named-after-a-field-is-that-fields-shape-check
  (let [by (by-name (sale))]
    (testing "the model marks it structurally, so nothing downstream re-derives it"
      (is (= ::item  (:shape (by ::item))))
      (is (= ::price (:shape (by ::price))))
      (is (= ::cuts  (:shape (by ::cuts)))))
    (testing "and a check named anything else is not one, however few fields it names"
      (is (nil? (:shape (by ::cheap))))
      (is (nil? (:shape (by ::split))))
      (is (nil? (:shape (by ::tagged)))))))

(deftest the-shorthand-and-the-arglist-form-declare-the-same-shape-check
  (let [by (by-name (sale))]
    (is (= ::item (:shape (by ::item))) "written as a bare fn")
    (is (= ::price (:shape (by ::price))) "written with an arglist")))

(deftest a-checks-gate-is-the-shape-checks-of-the-fields-it-names
  (let [by (by-name (sale))]
    (testing "a shape check waits on nothing -- that is what makes the graph two levels"
      (is (= [] (gate-names (sale) ::item)))
      (is (= [] (gate-names (sale) ::price)))
      (is (= [] (gate-names (sale) ::cuts))))
    (testing "every other check waits on the shape checks of the fields it names"
      (is (= [::price] (gate-names (sale) ::cheap)))
      (is (= [::price ::cuts] (gate-names (sale) ::split))))
    (testing "a field with no shape check gates nothing"
      (is (= [] (gate-names (sale) ::tagged))))
    (testing "and the mask is the same fact, for a draft to test in one and"
      (is (zero? (:shape-mask (by ::item))))
      (is (pos? (:shape-mask (by ::split)))))))

;;; ------------------- a shape check names exactly its own field, or else -------------------

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which
  ;; interns vars and defines classes. Do it somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.shape-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))

(defn- sk
  "A keyword in the scratch namespace -- which is where a field key of a type defined there
  lands, and therefore how a shape check of that type has to be spelled."
  [n]
  (keyword "recognitum.shape-scratch" n))

(defn- defform
  [tname fields definition]
  (list 'recognitum.core/defrecord tname fields definition))

(deftest a-shape-check-naming-another-field-does-not-compile
  (let [e (compile-data (defform 'ShapeElsewhere '[a b]
                          {:checks {(sk "a") '([b] (int? b))}}))]
    (is (= ::rec/malformed-shape-check (:error e)))
    (is (= 'ShapeElsewhere (:record e)))
    (is (= (sk "a") (:check e)))
    (is (= (sk "a") (:field e)))
    (is (= [(sk "b")] (:names e)))))

(deftest a-shape-check-naming-its-own-field-and-another-does-not-compile
  (let [e (compile-data (defform 'ShapeTooWide '[a b]
                          {:checks {(sk "a") '([a b] (and (int? a) (int? b)))}}))]
    (is (= ::rec/malformed-shape-check (:error e)))
    (is (= [(sk "a") (sk "b")] (:names e)))))

(deftest a-shape-check-naming-nothing-does-not-compile
  (let [e (compile-data (defform 'ShapeGrounded '[a]
                          {:checks {(sk "a") '([] true)}}))]
    (is (= ::rec/malformed-shape-check (:error e)))
    (is (= [] (:names e)))))

(deftest a-shape-check-cannot-be-io-bound
  ;; It would make the two-level graph three, and put a database between a field and the
  ;; statement of what that field is.
  (let [e (compile-data (defform 'ShapeFetching '[a]
                          {:checks  {(sk "a") (list [(sk "row")] true)}
                           :io-deps {(sk "row") '([a] a)}}))]
    (is (= ::rec/malformed-shape-check (:error e)))
    (is (= [(sk "row")] (:names e)))))

(deftest the-two-legal-spellings-compile
  (is (nil? (compiles (defform 'ShapeShort '[a] {:checks {(sk "a") 'int?}}))))
  (is (nil? (compiles (defform 'ShapeLong '[a] {:checks {(sk "a") '([a] (int? a))}}))))
  (testing "and a check that is not named after a field may name whatever it likes"
    (is (nil? (compiles (defform 'ShapeFree '[a b]
                          {:checks {:scratch/both '([a b] (= a b))}}))))))

;;; --------------------------------- the gate at construction ---------------------------------

(deftest a-gated-check-does-not-run-when-its-shape-check-fails
  (let [e (err #(->Sale "hat" "free" {:a 1} [:x]))]
    (is (= ::rec/check-failed (:error e))
        "and not a ClassCastException out of (< \"free\" 100)")
    (is (= [::price] (keys (:failures e)))
        "the shape failure is the actionable one; the two checks it gates are not reported")))

(deftest one-shape-failure-does-not-silence-the-checks-it-does-not-gate
  (let [e (err #(->Sale "hat" "free" {:a 1} ["x"]))]
    (is (= [::price ::tagged] (keys (:failures e)))
        "::tagged names a field with no shape check, so nothing gates it")))

(deftest a-failing-check-that-is-not-a-shape-check-gates-nothing
  (let [e (err #(->Sale "hat" 500 {:a 1} [:x]))]
    (is (= [::cheap ::split] (keys (:failures e)))
        "::cheap failing does not stop ::split, which names the same field")))

(rec/defrecord Phased
  [tag n extra]
  {:checks
   {::extra-ok ("extra is a vector" [extra] (vector? extra))
    ::n        int?
    ::n-small  ("n is under ten" [n] (< n 10))
    ::tag      keyword?}})

(deftest failures-come-back-in-phase-order-then-declaration-order
  ;; Shape first is the order you fix them in anyway, and it is not the order they are
  ;; written in: `::extra-ok` is declared before either shape check here.
  (let [e (err #(->Phased "not a keyword" 5 "not a vector"))]
    (is (= [::tag ::extra-ok] (keys (:failures e)))
        "the shape failure leads, though ::extra-ok is declared first"))
  (let [e (err #(->Phased "not a keyword" "not an int" "not a vector"))]
    (is (= [::n ::tag ::extra-ok] (keys (:failures e)))
        "both shape failures in declaration order, then the pure one; ::n-small is gated")))

;;; ------------------------------ the gate at every other write ------------------------------

(deftest the-gate-holds-on-a-write-to-a-sealed-value
  (let [s (sale)]
    (testing "assoc"
      (let [e (err #(assoc s ::price "free"))]
        (is (= ::rec/check-failed (:error e)))
        (is (= [::price] (keys (:failures e))))))
    (testing "merge, which is one write of several keys"
      (let [e (err #(merge s {::price "free" ::cuts "not a map"}))]
        (is (= ::rec/check-failed (:error e)))
        (is (= [::price ::cuts] (keys (:failures e))))))))

(deftest the-gate-holds-at-the-map-constructor
  (let [e (err #(map->Sale {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]}))]
    (is (= [::price] (keys (:failures e))))))

(deftest the-gate-holds-at-the-seal
  (let [e (err #(rec/drafting [x (sale)] (assoc x ::cuts "not a map")))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::cuts] (keys (:failures e)))
        "::split names cuts, so the seal does not put it in front of a string")))

;;; ----------------------------- the gate when a check is asked for -----------------------------

(defn- fresh
  "A draft of the type with every field written to `vs`."
  [vs]
  (reduce-kv assoc (rec/draft Sale) vs))

(deftest a-check-whose-gate-passes-answers-for-itself
  (let [d (fresh {::item "hat" ::price 500 ::cuts {:a 1} ::tags [:x]})]
    (is (= {::cheap false} (rec/check d ::cheap))
        "run, and its own return value is the answer, under its own name")
    (is (true? (rec/check d ::price)) "which is what the gate established on the way")))

(deftest a-check-whose-gate-fails-has-no-verdict-to-answer-with
  ;; The question was whether this check passes. "The data never got far enough to ask" is
  ;; not `false`, and the keyed answer says so without inventing one: the blocking check
  ;; gets the entry and the check asked about gets none -- the same ruling the IO gate makes.
  (let [d (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})
        r (rec/check d ::cheap)]
    (is (= [::price] (vec (keys r))))
    (is (not (contains? r ::cheap))))
  (let [d (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})]
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::cheap)))))))

(deftest a-certificate-reports-the-gate-that-stopped-it
  (let [d (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})]
    (is (= {::price false} (rec/check d ::pricing))
        "a check the certificate does not name, because it is the only honest thing to say")
    (is (= #{} (rec/certificates d)) "nothing in ::pricing was established"))
  (let [d (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})]
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::pricing)))))))

(deftest a-group-reports-the-gate-per-value
  (let [bad  (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})
        good (fresh {::item "hat" ::price 50 ::cuts {:a 50} ::tags [:x]})]
    (is (= [{::price false} true] (rec/check-all [bad good] ::cheap))
        "one value's gate failing discards nothing about the other")
    (is (= [{::price false} true] (rec/check-all [bad good] ::pricing)))))

;;; ------------------------- a skipped check records no verdict -------------------------

(deftest a-check-that-did-not-run-is-not-recorded-as-passing
  ;; The sharp end of the whole arrangement: a skipped check must not leave a bit behind
  ;; that `seal` would later believe.
  (let [d (fresh {::item "hat" ::price "free" ::cuts {:a 1} ::tags [:x]})]
    (is (= {::price false} (rec/check d rec/every-check)))
    (is (= #{::item ::cuts ::tagged} (rec/passed d))
        "everything that ran and passed, and nothing that was skipped")
    (is (= #{} (rec/certificates d)))
    (testing "so fixing the shape runs them at last"
      (let [e (err #(rec/seal (assoc d ::price 500)))]
        (is (= [::cheap ::split] (keys (:failures e))))))))

;;; ----------------------------- a field with no declared shape -----------------------------

(rec/defrecord Loose
  [n]
  {:checks {::positive ("n is positive" [n] (pos? n))}})

(deftest a-check-over-a-shapeless-field-runs-against-whatever-it-holds
  ;; The author's choice, made loudly: no shape check means no gate, so the check has to be
  ;; total over anything -- and a throw out of it stays a bug in the check.
  (is (rec/sealed? (->Loose 1)))
  (is (thrown? ClassCastException (->Loose "x"))))

;;; --------------------------------- shape gates IO, still ---------------------------------

(def ^:private fetched (atom []))

(defn- lookup [id] (swap! fetched conj id) {:id id :ok (= 1 id)})

(rec/defrecord Account
  [id note]
  {:checks
   {::id      int?
    ::note    string?
    ::short   ("the note is under ten characters" [note] (< (count note) 10))
    ::vouched ("the house has vouched for it" [::row] (:ok row))}
   :io-deps
   {::row ([id] (lookup id))}})

(deftest a-shape-failure-keeps-the-payload-away-from-the-fetch
  (reset! fetched [])
  (let [e (err #(->Account "not an int" "n"))]
    (is (= [::id] (keys (:failures e)))))
  (is (= [] @fetched) "nothing reached the outside"))

(deftest an-io-bound-check-still-runs-once-its-gate-passes
  (reset! fetched [])
  (let [e (err #(->Account 2 "n"))]
    (is (= [::vouched] (keys (:failures e)))))
  (is (= [2] @fetched))
  (reset! fetched [])
  (is (rec/sealed? (->Account 1 "n")))
  (is (= [1] @fetched)))

(deftest a-pure-failure-elsewhere-still-keeps-the-fetch-from-happening
  (reset! fetched [])
  (let [e (err #(->Account 1 "a note that is far too long"))]
    (is (= [::short] (keys (:failures e)))))
  (is (= [] @fetched)))
