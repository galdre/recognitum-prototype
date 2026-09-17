(ns recognitum.verdict-test
  "The record a draft keeps of which of its checks currently pass, reached here through
  `verdict-mask` and `record-verdict!`. It is a mutable field: the draft's data is
  immutable, and which checks have been run against that data is not part of the data. A
  draft opened from a sealed value starts with every check passing and one opened from the
  type with none; a check is recorded as passing when it passes and cleared when it fails;
  a write copies the record with exactly the checks reading the written field cleared, and
  the version written from keeps its own. Which checks `seal` runs is pinned in
  `recognitum.seal-test`."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]))
(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ---------------------------------------- types ----------------------------------------

;; `a` and `b` are each named by a check of their own and together by a third; `c` is named
;; by no check at all; `::always` names no field, so no write can invalidate it.
(rec/defrecord Ledger
  [a b c]
  {:checks
   {::a      int?
    ::b      ([b] (string? b))
    ::a-fits ("a fits inside b" [a b] (and (int? a) (string? b) (< a (count b))))
    ::always ([] true)}})

(rec/defrecord Nothing [a] {})

;; Two fields, so a verdict about one can stand while the check naming the other throws.
(rec/defrecord Fragile
  [n m]
  {:checks
   {::n    number?
    ::boom ([m] (if (number? m) true (throw (IllegalArgumentException. "boom"))))}})

(def led (->Ledger 1 "xxx" :anything))

(def all-checks #{::a ::b ::a-fits ::always})

(defn- idx
  "The index of check `cname` on type `t` -- the bit it owns in the mask."
  [t cname]
  (first (keep-indexed (fn [i c] (when (= cname (:name c)) i))
                       (:checks (internal/model-of t)))))

(defn- seen
  "What `f` reports about the draft a `drafting` over `init` binds. The draft itself never
  escapes the form, so the question is asked where it is answerable."
  [init f]
  (let [a (atom ::unset)]
    (try (rec/drafting [x init] (reset! a (f x)) x)
         (catch Exception _ nil))
    @a))

;;; -------------------------------------- the seed --------------------------------------

(deftest a-draft-of-the-type-starts-with-nothing-passing
  (let [d (rec/draft Ledger)]
    (is (rec/draft? d))
    (is (= #{} (rec/passed d)))
    (is (= 0 (internal/verdict-mask d)) "nothing has run against a draft with no field written"))
  (testing "however it was opened"
    (is (= #{} (rec/passed ((:draft-empty (internal/model-of Ledger)) nil))))
    (is (= #{} (seen Ledger rec/passed)))
    (is (= #{} (seen (rec/draft Ledger) rec/passed)))))

(deftest a-draft-of-a-sealed-value-starts-with-every-check-passing
  (is (= all-checks (rec/passed (rec/draft led)))
      "a sealed value is one every check passed on -- the draft inherits exactly that")
  (testing "which is a full mask, one bit per check in declaration order"
    (is (= 2r1111 (internal/verdict-mask (rec/draft led)))))
  (testing "and it is the same draft a drafting form opens"
    (is (= all-checks (seen led rec/passed))))
  (testing "while drafting an existing draft changes nothing"
    (let [d (rec/draft led)]
      (is (identical? d (rec/draft d)))
      (is (= all-checks (rec/passed d))))))

(deftest a-type-with-no-checks-has-an-empty-mask-either-way
  (is (= #{} (rec/passed (rec/draft Nothing))))
  (is (= #{} (rec/passed (rec/draft (->Nothing 1)))))
  (is (= 0 (internal/verdict-mask (rec/draft (->Nothing 1))))))

;;; ------------------------------- set on pass, clear on fail -------------------------------

(deftest a-verdict-is-set-when-a-check-passes
  (let [d (rec/draft Ledger)]
    (internal/record-verdict! d (idx Ledger ::b) true)
    (is (= #{::b} (rec/passed d)))
    (internal/record-verdict! d (idx Ledger ::always) true)
    (is (= #{::b ::always} (rec/passed d)))
    (testing "and recording the same pass twice says the same thing"
      (internal/record-verdict! d (idx Ledger ::b) true)
      (is (= #{::b ::always} (rec/passed d))))))

(deftest a-verdict-is-cleared-when-a-check-fails
  ;; Without a clear, a check that passed earlier and fails now would still count as passed.
  (let [d (rec/draft led)]
    (is (= all-checks (rec/passed d)))
    (internal/record-verdict! d (idx Ledger ::a-fits) false)
    (is (= #{::a ::b ::always} (rec/passed d)))
    (testing "and passing it again restores it"
      (internal/record-verdict! d (idx Ledger ::a-fits) true)
      (is (= all-checks (rec/passed d))))))

(deftest recording-a-verdict-answers-with-the-verdict
  (let [d (rec/draft Ledger)]
    (is (true? (internal/record-verdict! d (idx Ledger ::a) true)))
    (is (false? (internal/record-verdict! d (idx Ledger ::a) false)))))

;;; ------------------------------- what a write invalidates -------------------------------

(deftest a-write-clears-exactly-the-checks-naming-the-field
  (let [d (rec/draft led)]
    (testing "a names ::a and ::a-fits"
      (is (= #{::b ::always} (rec/passed (assoc d ::a 2)))))
    (testing "b names ::b and ::a-fits"
      (is (= #{::a ::always} (rec/passed (assoc d ::b "yyyy")))))
    (testing "and the two together leave only what names neither"
      (is (= #{::always} (rec/passed (-> d (assoc ::a 2) (assoc ::b "yyyy"))))))))

(deftest a-write-to-a-field-no-check-names-clears-nothing
  (is (= all-checks (rec/passed (assoc (rec/draft led) ::c :something-else)))
      "c is a declared field no check reads -- nothing about it can have moved a verdict"))

(deftest a-write-of-an-undeclared-key-clears-nothing
  (let [d (rec/draft led)]
    (is (= all-checks (rec/passed (assoc d :getaway-car "mini"))))
    (testing "and removing it again leaves the mask alone"
      (is (= all-checks (rec/passed (-> d (assoc :getaway-car "mini") (dissoc :getaway-car))))))
    (testing "as does cleaning it off"
      (is (= all-checks (rec/passed (rec/clean (assoc d :getaway-car "mini"))))))))

(deftest a-check-naming-no-field-survives-every-write
  (let [d (rec/draft led)]
    (doseq [k [::a ::b ::c]]
      (is (contains? (rec/passed (assoc d k (get led k))) ::always)
          (str "no write can invalidate a check that reads nothing -- " k)))))

(deftest writing-the-same-value-back-still-invalidates
  ;; What a write clears is computed from the arglists; nothing compares values.
  (is (= #{::b ::always} (rec/passed (assoc (rec/draft led) ::a (::a led))))))

(deftest merging-invalidates-every-field-it-writes
  (is (= #{::b ::always} (rec/passed (merge (rec/draft led) {::a 2 ::c :x})))))

;;; ------------------------------- the seed is a snapshot -------------------------------

(deftest versions-diverge-rather-than-share
  (let [d  (rec/draft led)
        d' (assoc d ::a 2)]
    (is (= all-checks (rec/passed d)) "the parent keeps every verdict it had")
    (is (= #{::b ::always} (rec/passed d')))
    (testing "and a verdict recorded on the child says nothing about the parent"
      (internal/record-verdict! d' (idx Ledger ::b) false)
      (is (= #{::always} (rec/passed d')))
      (is (= all-checks (rec/passed d))))
    (testing "nor the other way round"
      (internal/record-verdict! d (idx Ledger ::a) false)
      (is (= #{::b ::a-fits ::always} (rec/passed d)))
      (is (= #{::always} (rec/passed d'))))))

(deftest a-verdict-recorded-before-a-write-carries-into-it
  (let [d (rec/draft Ledger)]
    (internal/record-verdict! d (idx Ledger ::b) true)
    (internal/record-verdict! d (idx Ledger ::always) true)
    (is (= #{::b ::always} (rec/passed (assoc d ::c :x))) "c names neither")
    (is (= #{::always} (rec/passed (assoc d ::b "z"))) "b names ::b")))

;;; ------------------------- the record travels with the draft -------------------------

(deftest the-record-survives-everything-that-is-not-a-write
  (let [d (rec/draft led)]
    (testing "with-meta"
      (let [m (with-meta d {:user-key 1})]
        (is (= all-checks (rec/passed m)))
        (is (= 1 (:user-key (meta m))))
        (is (= all-checks (:recognitum.core/passed (meta m)))
            "and `meta` reports the same record alongside the caller's keys")))
    (testing "adoption, by hand and by a drafting form"
      (is (= all-checks (rec/passed (internal/adopt d (Object.)))))
      (is (= all-checks (seen d rec/passed))))))

(deftest a-draft-passed-through-a-nested-drafting-keeps-its-verdicts
  (let [a     (atom nil)
        inner (fn [x] (rec/drafting [y x] (assoc y ::a 2)))]
    (is (rec/sealed? (rec/drafting [x led]
                       (let [x' (inner x)]
                         (reset! a (rec/passed x'))
                         (assoc x' ::a 1)))))
    (is (= #{::b ::always} @a)
        "the inner form passed the draft through, write and verdicts intact")))

;;; ------------------------- the record is not part of the value -------------------------

(deftest two-drafts-holding-the-same-data-are-equal-whatever-they-record
  (let [fresh (rec/draft led)
        used  (assoc (rec/draft led) ::a (::a led))]
    (is (not= (rec/passed fresh) (rec/passed used)))
    (is (= fresh used) "which checks have been run is not part of the data")
    (is (= (hash fresh) (hash used)))
    (is (= (into {} fresh) (into {} used)))))

;;; --------------------------- running a check writes it down ---------------------------

(deftest sealing-records-every-verdict-it-reached
  (let [d (-> (rec/draft Ledger) (assoc ::a 1) (assoc ::b "xxx") (assoc ::c :x))]
    (is (= #{} (rec/passed d)) "built from the type, so nothing has run")
    (is (rec/sealed? (rec/seal d)))
    (is (= all-checks (rec/passed d)) "and the seal wrote down what it found")))

(deftest a-failed-seal-records-the-passes-and-clears-the-failure
  (let [d (assoc (rec/draft led) ::b "")]          ; "" is a string, but 1 does not fit in it
    (is (= #{::a ::always} (rec/passed d)) "the write cleared ::b and ::a-fits")
    (let [e (err #(rec/seal d))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::a-fits] (keys (:failures e)))))
    (is (= #{::a ::b ::always} (rec/passed d))
        "::b ran and passed, ::a-fits stays cleared, ::a was never touched")))

(deftest a-check-that-throws-leaves-its-verdict-alone
  ;; A throw records nothing, so the bit says what it said before; the results reached
  ;; before it stand.
  (let [d (-> (rec/draft (->Fragile 1 2)) (assoc ::n 3) (assoc ::m "not a number"))]
    (is (= #{} (rec/passed d)) "each write cleared the check naming the field it wrote")
    (is (thrown-with-msg? IllegalArgumentException #"boom" (rec/seal d)))
    (is (= #{::n} (rec/passed d))
        "::n ran and passed; ::boom threw before it had a verdict to record")))

;;; -------------------------------- what passed answers --------------------------------

(deftest passed-answers-for-a-sealed-value-too
  (is (= all-checks (rec/passed led)))
  (is (= #{::rec/every-check} (rec/certificates led)))
  (is (= #{} (rec/passed (->Nothing 1))) "a type with no checks has none passing")
  (is (= #{::rec/every-check} (rec/certificates (->Nothing 1))))
  (testing "and it agrees with what meta reports under the same names"
    (is (= (rec/passed led) (::rec/passed (meta led))))
    (is (= (rec/certificates led) (::rec/certificates (meta led))))))

(deftest passed-is-a-question-about-a-value-of-this-library
  (doseq [x [{::a 1} nil Ledger 42]]
    (is (= ::rec/not-a-value (:error (err #(rec/passed x)))) (pr-str x))
    (is (= ::rec/not-a-value (:error (err #(rec/certificates x)))) (pr-str x))))

;;; ------------------------------- the mask has a width -------------------------------

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating whole `defrecord` forms, which intern
  ;; vars and define classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.verdict-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(defn- compiles [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))

(defn- n-fields [n] (mapv #(symbol (str "f" %)) (range n)))

(defn- n-checks [n]
  (into {}
        (map (fn [i] [(keyword "recognitum.verdict-scratch" (str "c" i)) (list '[f0] true)]))
        (range n)))

(deftest a-type-wider-than-the-mask-does-not-compile
  ;; The record is one long, so a 65th check would alias the first and a value could seal
  ;; on a check that never ran.
  (let [e (compile-data (list 'recognitum.core/defrecord 'TooManyChecks
                              (n-fields 1) {:checks (n-checks 65)}))]
    (is (= ::rec/too-many-checks (:error e)))
    (is (= 'TooManyChecks (:record e)))
    (is (= 65 (:count e)))
    (is (= 64 (:limit e))))
  (testing "the full width itself is fine"
    (is (nil? (compiles (list 'recognitum.core/defrecord 'WideChecks
                              (n-fields 1) {:checks (n-checks 64)})))))
  (testing "while the field count is under no such limit"
    (is (nil? (compiles (list 'recognitum.core/defrecord 'WideFields (n-fields 21) {}))))))

(deftest the-highest-bit-still-answers
  ;; 64 checks over one field: the last of them owns the sign bit, which is where an
  ;; arithmetic shift would quietly lose it.
  (binding [*ns* @scratch]
    (eval (list 'recognitum.core/defrecord 'Edge (n-fields 1) {:checks (n-checks 64)})))
  (let [t          (ns-resolve @scratch 'Edge)
        d          (rec/draft ((ns-resolve @scratch '->Edge) 1))
        last-check :recognitum.verdict-scratch/c63]
    (is (= 64 (count (rec/passed d))))
    (is (contains? (rec/passed d) last-check))
    (internal/record-verdict! d (idx t last-check) false)
    (is (= 63 (count (rec/passed d))))
    (is (not (contains? (rec/passed d) last-check)))
    (testing "and a write to the one field they all name clears all 64"
      (is (= #{} (rec/passed (assoc d :recognitum.verdict-scratch/f0 2)))))))
