(ns recognitum.record-test
  "The generated types against `clojure.core/defrecord`, operation by operation.

  `SPEC.md`'s baseline section says the first question about any behaviour is what
  `defrecord` does for the same operation, and that an unrecorded difference is a defect.
  So this file holds a twin: a real `defrecord` over the same two fields, and every
  map-surface operation asked of both. Where the answers agree the assertion is written
  once against both; where `SPEC.md` departs on purpose the test says so in its name and
  pins each side separately.

  The six rows of the defects table live here -- `=` against a plain map and against an
  unrelated type, `hash`, `record?`, `empty`, `Serializable` -- and so does the consequence
  that made them worth fixing: `clojure.walk/postwalk` reaches for `empty` when it does not
  find `IRecord`, so before this the most ordinary tool in the standard library turned a
  validated value into a `PersistentArrayMap` without a word."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [recognitum.core :as rec]
   [recognitum.heist :as heist])
  (:import
   (java.io ByteArrayInputStream ByteArrayOutputStream ObjectInputStream ObjectOutputStream)
   (recognitum.heist Heist)))

;;; ----------------------------------- the two types -----------------------------------

;; No checks at all, so nothing about the map surface is constrained by validation and
;; every difference from the twin below is a difference of the template rather than of the
;; library's business.
(rec/defrecord Twin [alpha beta] {})

;; The same two fields again, under another name. `defrecord` holds that these are not
;; equal and that their hashes differ; `potemkin/def-map-type` held the opposite.
(rec/defrecord Doppel [alpha beta] {})

;; A type with checks, for the paths where a rebuild has to survive them.
(rec/defrecord Ledger
  [owner balance]
  {:checks {::owner   string?
            ::balance int?}})

(clojure.core/defrecord PlainTwin [alpha beta])
(clojure.core/defrecord PlainDoppel [alpha beta])

(def ^:private ours   (->Twin 1 2))
(def ^:private theirs (->PlainTwin 1 2))

(defn- bare
  "`m` with the defining namespace dropped from its keys -- the twin's spelling of ours.

  Namespace-qualified field keys are a recorded deviation of their own (`SPEC.md`,
  \"Fields\"), pinned in `recognitum.qualified-keys-test`. Here it is noise, so it is
  divided out rather than re-asserted at every line."
  [m]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) m))

(defn- round-trip
  "`x` through Java serialization and back."
  [x]
  (let [bos (ByteArrayOutputStream.)]
    (with-open [o (ObjectOutputStream. bos)] (.writeObject o x))
    (with-open [i (ObjectInputStream. (ByteArrayInputStream. (.toByteArray bos)))]
      (.readObject i))))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- threw [f]
  (try (f) nil (catch Throwable t (class t))))

;;; ============================== the surface both share ==============================

(deftest reading-answers-what-a-defrecord-answers
  (testing "get, by key and by the keyword itself"
    (is (= (get theirs :alpha) (get ours ::alpha) 1))
    (is (= (:alpha theirs) (::alpha ours) 1))
    (is (= (get theirs :nope) (get ours ::nope) nil))
    (is (= (get theirs :nope :fallback) (get ours ::nope :fallback) :fallback))
    (testing "and a declared field never reaches the default"
      (is (= (get theirs :alpha :fallback) (get ours ::alpha :fallback) 1))
      (is (= (get (->PlainTwin nil 2) :alpha :fallback)
             (get (->Twin nil 2) ::alpha :fallback)
             nil))))
  (testing "destructuring"
    (is (= (let [{:keys [alpha beta]} theirs] [alpha beta])
           (let [{::keys [alpha beta]} ours] [alpha beta])
           [1 2]))
    (is (= (let [{:keys [gamma] :or {gamma :d}} theirs] gamma)
           (let [{::keys [gamma] :or {gamma :d}} ours] gamma)
           :d)))
  (testing "keys, vals, count, seq"
    (is (= (keys theirs) (map (comp keyword name) (keys ours)) '(:alpha :beta)))
    (is (= (vals theirs) (vals ours) '(1 2)))
    (is (= (count theirs) (count ours) 2))
    (is (= (mapv (fn [e] [(key e) (val e)]) (seq theirs))
           (mapv (fn [e] [(keyword (name (key e))) (val e)]) (seq ours))
           [[:alpha 1] [:beta 2]])))
  (testing "contains? and find"
    (is (= (contains? theirs :alpha) (contains? ours ::alpha) true))
    (is (= (contains? theirs :nope) (contains? ours ::nope) false))
    (is (= (contains? (->PlainTwin nil 2) :alpha) (contains? (->Twin nil 2) ::alpha) true)
        "a declared field is present whatever it holds")
    (is (= (val (find theirs :alpha)) (val (find ours ::alpha)) 1))
    (is (= (find theirs :nope) (find ours ::nope) nil))
    (testing "and an entry is a real IMapEntry, which is what clojure.walk descends into"
      (is (instance? clojure.lang.IMapEntry (find theirs :alpha)))
      (is (instance? clojure.lang.IMapEntry (find ours ::alpha)))
      (is (instance? clojure.lang.IMapEntry (first (seq ours))))))
  (testing "into {}"
    (is (= (into {} theirs) (bare (into {} ours)) {:alpha 1 :beta 2}))))

(deftest writing-a-declared-field-answers-what-a-defrecord-answers
  (is (= (class theirs) (class (assoc theirs :alpha 9))))
  (is (= (class ours) (class (assoc ours ::alpha 9))))
  (is (= (into {} (assoc theirs :alpha 9)) (bare (into {} (assoc ours ::alpha 9)))))
  (testing "and conj of an entry is the same write"
    (is (= (assoc theirs :alpha 9) (conj theirs [:alpha 9])))
    (is (= (assoc ours ::alpha 9) (conj ours [::alpha 9])))))

(deftest a-value-is-a-map-to-java-as-a-defrecord-is
  (doseq [[what x k] [["defrecord" theirs :alpha] ["ours" ours ::alpha]]]
    (testing what
      (is (instance? java.util.Map x))
      (is (map? x))
      (is (= 2 (.size x)))
      (is (false? (.isEmpty x)))
      (is (= 1 (.get x k)))
      (is (true? (.containsKey x k)))
      (is (= [1 2] (vec (.values x))))
      (is (= #{:alpha :beta} (set (map (comp keyword name) (.keySet x)))))
      (is (= #{[:alpha 1] [:beta 2]}
             (set (map (fn [e] [(keyword (name (key e))) (val e)]) (.entrySet x)))))
      (is (= [[:alpha 1] [:beta 2]]
             (mapv (fn [e] [(keyword (name (key e))) (val e)]) (iterator-seq (.iterator x)))))
      (testing "and the mutators refuse, as an immutable map's do"
        (is (= UnsupportedOperationException (threw #(.put x :z 1))))
        (is (= UnsupportedOperationException (threw #(.remove x k))))
        (is (= UnsupportedOperationException (threw #(.clear x))))))))

;;; ============================== the six defect rows ==============================

(deftest a-bare-map-with-the-right-keys-is-not-a-value-of-the-type
  ;; Row one, and the sentence SPEC.md opens with. Both directions, because the answer
  ;; from the map's side is the map's to give: `APersistentMap.equiv` refuses any
  ;; `IPersistentMap` that does not carry `clojure.lang.MapEquivalence`, so the fix is the
  ;; absence of that marker rather than anything our own `equiv` could say.
  (let [m {::alpha 1 ::beta 2}]
    (is (= m (into {} ours)) "the same entries, exactly")
    (is (false? (= ours m)))
    (is (false? (= m ours)))
    (testing "which is what a defrecord answers for the same question"
      (is (false? (= theirs {:alpha 1 :beta 2})))
      (is (false? (= {:alpha 1 :beta 2} theirs)))))
  (testing "and a draft is no more a map than a sealed value is"
    (is (false? (= (rec/draft ours) {::alpha 1 ::beta 2})))
    (is (false? (= {::alpha 1 ::beta 2} (rec/draft ours))))))

(deftest two-unrelated-types-with-the-same-fields-are-not-equal
  ;; Row two. Twin and Doppel are declared in one namespace, so their field keys are the
  ;; same keywords and their entries are indistinguishable; the class is the whole of the
  ;; difference, which is exactly the case `defrecord` decides on the class.
  (let [d (->Doppel 1 2)]
    (is (= (into {} ours) (into {} d)) "the same entries, exactly")
    (is (false? (= ours d)))
    (is (false? (= d ours)))
    (is (false? (= (rec/draft ours) (rec/draft d))))
    (testing "which is what a defrecord answers for the same question"
      (is (false? (= theirs (->PlainDoppel 1 2))))
      (is (false? (= (->PlainDoppel 1 2) theirs)))))
  (testing "and a draft is not the value it was opened from"
    (is (false? (= ours (rec/draft ours))))
    (is (false? (= (rec/draft ours) ours))))
  (testing "while equal data of the same class is still equal"
    (is (= ours (->Twin 1 2)))
    (is (= (rec/draft ours) (rec/draft ours)))
    (is (= theirs (->PlainTwin 1 2)))))

(deftest hash-is-type-aware
  ;; Row three, which follows from the two above: `=` and `hash` have to agree or the
  ;; value is broken as a key.
  (is (= (hash ours) (hash (->Twin 1 2))))
  (is (not= (hash ours) (hash (into {} ours))))
  (is (not= (hash ours) (hash (->Doppel 1 2))))
  (is (not= (hash ours) (hash (rec/draft ours))))
  (testing "which is what a defrecord answers for the same question"
    (is (= (hash theirs) (hash (->PlainTwin 1 2))))
    (is (not= (hash theirs) (hash (into {} theirs))))
    (is (not= (hash theirs) (hash (->PlainDoppel 1 2)))))
  (testing "so a value works as a key, and a look-alike map does not find it"
    (is (= 2 (count #{ours {::alpha 1 ::beta 2}})))
    (is (= 3 (count #{ours (->Doppel 1 2) (rec/draft ours)})))
    (is (contains? #{ours} (->Twin 1 2)))
    (is (false? (contains? #{ours} {::alpha 1 ::beta 2})))
    (is (= :v (get {ours :v} (->Twin 1 2))))
    (is (nil? (get {ours :v} {::alpha 1 ::beta 2}))))
  (testing "and the hash is the same number in the next JVM as in this one"
    (is (= (hash ours) (hash (round-trip ours)))
        "mixed in by class name rather than by Class identity")))

(deftest a-value-is-a-record
  ;; Row four. The marker is what `clojure.walk` asks for, and what the row below is about.
  (is (true? (record? ours)))
  (is (true? (record? (rec/draft ours))))
  (is (true? (record? theirs)))
  (is (instance? clojure.lang.IRecord ours))
  (testing "and is not a MapEquivalence, which is the same claim from the other side"
    (is (false? (instance? clojure.lang.MapEquivalence ours)))
    (is (false? (instance? clojure.lang.MapEquivalence (rec/draft ours))))
    (is (false? (instance? clojure.lang.MapEquivalence theirs)))))

(deftest there-is-no-empty-value-of-the-type
  ;; Row five. The fields are the type, so a value without them is not one -- and a caller
  ;; asking for an empty one has to be told rather than handed a plain map.
  (is (= UnsupportedOperationException (threw #(empty ours))))
  (is (= UnsupportedOperationException (threw #(empty (rec/draft ours)))))
  (is (= UnsupportedOperationException (threw #(empty theirs)))))

(deftest a-value-serializes
  ;; Row six.
  (is (instance? java.io.Serializable ours))
  (is (instance? java.io.Serializable (rec/draft ours)))
  (is (instance? java.io.Serializable theirs))
  (testing "and comes back as itself"
    (let [back (round-trip ours)]
      (is (= (class ours) (class back)))
      (is (= ours back))
      (is (rec/sealed? back))
      (is (rec/of-type? Twin back)))
    (let [back (round-trip theirs)]
      (is (= (class theirs) (class back)))
      (is (= theirs back))))
  (testing "a draft too, still a draft of its type"
    (let [d    (rec/draft (->Ledger "zoe" 1))
          back (round-trip d)]
      (is (rec/draft? back))
      (is (rec/of-type? Ledger back))
      (is (= d back))
      (is (= (rec/passing d) (rec/passing back))
          "and the mask is a field like any other, so the verdicts come back with it"))))

;;; ============================== what the rows were for ==============================

(deftest postwalk-hands-back-the-value-rather-than-a-plain-map
  ;; The consequence SPEC.md calls the sharpest: without `IRecord`, `clojure.walk/walk`
  ;; falls through to `(into (empty form) ...)` and the empty of a map type is a map. It
  ;; never asks for `empty` now, which is why the refusal above costs nothing here.
  (doseq [[what x] [["a sealed value" ours]
                    ["a draft"        (rec/draft ours)]
                    ["a checked type" (->Ledger "zoe" 10)]]]
    (testing what
      (let [back (walk/postwalk identity x)]
        (is (= (class x) (class back)))
        (is (= x back)))))
  (testing "which is what a defrecord does"
    (is (= (class theirs) (class (walk/postwalk identity theirs)))))
  (testing "nested inside something else"
    (is (= (class ours) (class (::v (walk/postwalk identity {::v ours}))))))
  (testing "and prewalk the same way"
    (is (= (class ours) (class (walk/prewalk identity ours)))))
  (testing "editing a field rebuilds the value, not a map of it"
    (let [back (walk/postwalk #(if (= % 10) 25 %) (->Ledger "zoe" 10))]
      (is (rec/of-type? Ledger back))
      (is (= 25 (::balance back))))
    (is (= (->PlainTwin 9 2) (walk/postwalk #(if (= % 1) 9 %) theirs))))
  (testing "and an edit the checks refuse is refused, because a rebuild is a write"
    (is (= ::rec/check-failed
           (:error (err #(walk/postwalk (fn [x] (if (= x 10) :free x)) (->Ledger "zoe" 10))))))))

(deftest postwalk-on-an-io-limited-value-refuses-rather-than-degrades
  ;; `SPEC.md`'s example of the walk defect is the Heist, which is `:io-limited`, and
  ;; `clojure.walk` rebuilds a record by `conj`-ing its entries back one at a time -- so
  ;; every io-limited field is written directly on a sealed value, which is the one thing
  ;; the option exists to refuse. It refuses, loudly and by name, which is a different
  ;; thing from the silent degradation the marker was added to stop.
  (is (= ::rec/io-limited (:error (err #(walk/postwalk identity heist/italian-job)))))
  (testing "and the draft, which the option says nothing about, walks"
    (let [d (rec/draft heist/italian-job)]
      (is (rec/of-type? Heist (walk/postwalk identity d)))
      (is (= d (walk/postwalk identity d))))))

;;; ======================= where SPEC.md departs from defrecord =======================

(deftest ours-departs-dissoc-of-a-declared-field-is-refused-where-a-defrecord-drops-to-a-map
  ;; SPEC.md, "Reading and writing a sealed value": declared fields are permanent.
  (is (= {:beta 2} (dissoc theirs :alpha)))
  (is (= clojure.lang.PersistentArrayMap (class (dissoc theirs :alpha))))
  (is (= ::rec/cannot-dissoc (:error (err #(dissoc ours ::alpha)))))
  (is (= ::rec/cannot-dissoc (:error (err #(dissoc (rec/draft ours) ::alpha)))))
  (testing "an undeclared key dissocs the same way on both"
    (is (= theirs (dissoc theirs :nope)))
    (is (= ours (dissoc ours ::nope)))))

(deftest ours-departs-an-undeclared-key-is-refused-on-a-sealed-value
  ;; SPEC.md, "Undeclared keys": a draft accepts any key and sealing rejects what is left.
  (is (= {:alpha 1 :beta 2 :gamma 3} (into {} (assoc theirs :gamma 3))))
  (is (= (class theirs) (class (assoc theirs :gamma 3))))
  (is (= ::rec/unknown-field (:error (err #(assoc ours ::gamma 3)))))
  (testing "while a draft takes it, and sealing is where it is refused"
    (let [d (assoc (rec/draft ours) ::gamma 3)]
      (is (= 3 (::gamma d)))
      (is (= ::rec/extraneous-keys (:error (err #(rec/seal d))))))))

(deftest ours-departs-meta-carries-the-librarys-keys-over-the-callers
  ;; SPEC.md, "Metadata". A fresh defrecord has no metadata; ours always answers with what
  ;; the value can show for itself, in both forms.
  (is (nil? (meta theirs)))
  (is (= #{::rec/passed ::rec/certificates} (set (keys (meta ours)))))
  (testing "and otherwise with-meta behaves the same on both"
    (is (= {:src :test} (meta (with-meta theirs {:src :test}))))
    (is (= :test (:src (meta (with-meta ours {:src :test})))))
    (is (= (class ours) (class (with-meta ours {:src :test}))))
    (testing "taking no part in equality or hash"
      (is (= theirs (with-meta theirs {:src :test})))
      (is (= ours (with-meta ours {:src :test})))
      (is (= (hash ours) (hash (with-meta ours {:src :test})))))))

(deftest ours-departs-a-value-is-callable-where-a-defrecord-is-not
  ;; potemkin's `AbstractMap` makes the type an `IFn` over its keys and `AbstractRecord`
  ;; keeps it. `defrecord` does not, so this is the one place the twin throws and we
  ;; answer.
  (is (= 1 (ours ::alpha)))
  (is (= :d (ours ::nope :d)))
  (is (= ClassCastException (threw #(theirs :alpha)))))

(deftest ours-departs-equals-and-hashcode-require-the-class-where-a-defrecords-do-not
  ;; `defrecord` leaves `.equals` and `.hashCode` to `APersistentMap`'s map contract, so at
  ;; the Java level a bare map with the right keys *is* a record -- the hole `=` was closed
  ;; against. We close it on our side too, at the price of an asymmetry against a plain
  ;; map, which the class-mixed `.hashCode` keeps out of every hash-based collection: the
  ;; two never land in one bucket, so nothing ever asks.
  (is (false? (.equals ours {::alpha 1 ::beta 2})))
  (is (false? (.equals ours (->Doppel 1 2))))
  (is (false? (.equals ours (rec/draft ours))))
  (is (true? (.equals ours (->Twin 1 2))))
  (is (not= (.hashCode ours) (.hashCode (into {} ours))))
  (is (= (.hashCode ours) (.hashCode (->Twin 1 2))))
  (testing "the asymmetry, written down: the map's side is the map's to answer"
    (is (true? (.equals {::alpha 1 ::beta 2} ours))))
  (testing "where a defrecord answers true to all of it"
    (is (true? (.equals theirs {:alpha 1 :beta 2})))
    (is (true? (.equals theirs (->PlainDoppel 1 2))))
    (is (= (.hashCode theirs) (.hashCode (into {} theirs))))))

(deftest ours-departs-tostring-is-the-entries-where-a-defrecords-is-the-object-identity
  ;; `defrecord` defines no `toString`, so `str` of one is `Object`'s; potemkin's template
  ;; prints the entries and `AbstractRecord` keeps that. Nothing in `SPEC.md` rules on it,
  ;; and the readable answer is the one worth keeping.
  (is (= (str (into {} ours)) (str ours)))
  (is (re-find #"^recognitum\.record_test\.PlainTwin@" (str theirs))))

;;; ======================= the same treatment for both types =======================

(deftest the-draft-type-is-the-sealed-type-in-everything-but-what-spec-names
  (let [d (rec/draft ours)]
    (doseq [[what x] [["sealed" ours] ["draft" d]]]
      (testing what
        (is (record? x))
        (is (map? x))
        (is (instance? java.io.Serializable x))
        (is (false? (instance? clojure.lang.MapEquivalence x)))
        (is (= UnsupportedOperationException (threw #(empty x))))
        (is (false? (= x {::alpha 1 ::beta 2})))
        (is (false? (= {::alpha 1 ::beta 2} x)))
        (is (= 2 (count x)))
        (is (= 1 (::alpha x)))
        (is (= (class x) (class (walk/postwalk identity x))))))
    (testing "and the two are not each other"
      (is (not= (class ours) (class d)))
      (is (false? (= ours d)))
      (is (not= (hash ours) (hash d))))))
