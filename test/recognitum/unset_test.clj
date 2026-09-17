(ns recognitum.unset-test
  "Unset is `nil`. A field that has not been written reads as `nil`, as it would on a
  `defrecord` whose field was never set: a draft opened from the type starts with every
  field `nil`, `keys` answers with every declared key, and a check receives `nil` for a
  field nobody wrote, and `dissoc` of a declared field is the write that puts it back
  there. Whether `nil` is acceptable is the shape validation's business; a field with no
  shape validation seals holding it."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; `title` and `body` admit nil; `tags` is named by no check at all, so it has no declared
;; shape and seals holding whatever it holds.
(rec/defrecord Note
  [title body tags]
  {:checks
   {::title ([title] (or (nil? title) (string? title)))
    ::body  ([body] (or (nil? body) (string? body)))}})

;; The other half of the choice: a shape validation that does not admit nil.
(rec/defrecord Strict
  [title]
  {:checks {::title string?}})

;;; ---------------------------- a draft of the type is all nil ----------------------------

(deftest a-draft-from-the-type-starts-with-every-field-nil
  (let [d (rec/draft Note)]
    (is (rec/draft? d))
    (is (= [::title ::body ::tags] (keys d))
        "every declared key, always -- not the ones that happen to have been written")
    (is (= 3 (count d)))
    (is (nil? (::title d)))
    (is (= {::title nil ::body nil ::tags nil} (into {} d)))
    (is (contains? d ::title) "the field is there, holding nil")
    (is (= [nil nil nil] (vals d)))))

(deftest an-unwritten-field-answers-nil-rather-than-a-lookup-default
  ;; What `defrecord` does: a declared field is present, so `get` never reaches its default.
  (let [d (rec/draft Note)]
    (is (nil? (get d ::title ::fallback)))
    (is (= ::fallback (get d ::undeclared ::fallback))))
  (testing "and a write leaves the others exactly as they were"
    (let [d (assoc (rec/draft Note) ::title "t")]
      (is (= [::title ::body ::tags] (keys d)))
      (is (= {::title "t" ::body nil ::tags nil} (into {} d))))))

(deftest writing-nil-is-not-a-different-state-from-never-writing
  (let [never (rec/draft Note)
        wrote (-> (rec/draft Note) (assoc ::title nil) (assoc ::body nil))]
    (is (= (into {} never) (into {} wrote)))
    (is (= (keys never) (keys wrote)))
    (is (= (rec/seal never) (rec/seal wrote))
        "and they seal to the same value, because they are the same value")))

;;; ------------------------------------ sealing one ------------------------------------

(deftest an-unwritten-field-seals-as-nil-where-its-shape-validation-admits-nil
  (let [n (rec/drafting [x Note] (assoc x ::title "t"))]
    (is (rec/sealed? n))
    (is (= {::title "t" ::body nil ::tags nil} (into {} n)))))

(deftest a-field-no-check-names-seals-holding-nil
  (let [n (rec/seal (rec/draft Note))]
    (is (rec/sealed? n))
    (is (nil? (::tags n)) "no shape validation, no declared shape, nothing to fail")))

(deftest a-shape-validation-that-refuses-nil-is-what-refuses-an-unwritten-field
  (let [e (err #(rec/seal (rec/draft Strict)))]
    (is (= ::rec/check-failed (:error e))
        "the check's verdict about nil, not a report that nobody wrote anything")
    (is (= [::title] (keys (:failures e))))
    (is (= {::title nil} (:values (get (:failures e) ::title)))
        "and what it was given is written down as nil, which is what it was given")))

;;; ---------------------------- dissoc of a declared field ----------------------------

(deftest dissoc-of-a-declared-field-is-the-write-that-sets-it-to-nil
  (let [n (map->Note {::title "t" ::body "b"})
        d (dissoc n ::title)]
    (is (rec/sealed? d))
    (is (nil? (::title d)))
    (is (contains? d ::title))
    (is (= (assoc n ::title nil) d)))
  (testing "and the shape validation decides, as it does for the field nobody wrote"
    (let [e (err #(dissoc (map->Strict {::title "t"}) ::title))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::title] (keys (:failures e))))
      (is (= {::title nil} (:values (get (:failures e) ::title)))))))

;;; -------------------------------- the map constructor --------------------------------

(deftest the-map-constructor-treats-an-omitted-key-as-nil
  (let [n (map->Note {::title "t"})]
    (is (rec/sealed? n))
    (is (= {::title "t" ::body nil ::tags nil} (into {} n))))
  (testing "and the shape validation is what decides whether that is acceptable"
    (let [e (err #(map->Strict {}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::title] (keys (:failures e))))))
  (testing "while a key the type does not declare is refused"
    (is (= ::rec/extraneous-keys (:error (err #(map->Note {::title "t" :nope 1})))))))

;;; ----------------------- running checks against an incomplete draft -----------------------

(deftest a-check-naming-an-unwritten-field-runs-and-answers
  (let [d (rec/draft Strict)]
    (is (= {::title false} (rec/check d ::title))
        "nil is what it judged, and false is what it said")
    (is (= ::rec/check-failed (:error (err #(rec/check! d ::title)))))
    (is (= #{} (rec/passed d))))
  (testing "and one that admits nil simply passes"
    (let [d (rec/draft Note)]
      (is (true? (rec/check d ::title)))
      (is (contains? (rec/passed d) ::title)))))

(deftest a-certificate-judges-an-incomplete-draft-rather-than-refusing-it
  (is (= {::title false} (rec/check (rec/draft Strict) rec/every-check)))
  (is (true? (rec/check (rec/draft Note) rec/every-check))))

(deftest a-group-of-incomplete-drafts-is-judged-value-by-value
  (let [empty-one (rec/draft Strict)
        full      (assoc (rec/draft Strict) ::title "t")]
    (is (= [{::title false} true] (rec/check-all [empty-one full] ::title)))
    (is (= [{::title false} true] (rec/check-all [empty-one full] rec/every-check)))))

;;; ------------------------------------ what it prints ------------------------------------

(deftest a-draft-prints-every-declared-field
  (let [s (pr-str (rec/draft Note))]
    (is (str/starts-with? s "#<Note draft "))
    (doseq [k ["title" "body" "tags"]]
      (is (str/includes? s (str ":" k)) (str "the printed draft omits " k)))))

;;; ---------------------------- no entry point reports a missing field ----------------------------

(deftest no-entry-point-reports-a-missing-field
  (doseq [f [#(rec/seal (rec/draft Strict))
             #(map->Strict {})
             #(rec/check! (rec/draft Strict) ::title)
             #(rec/check! (rec/draft Strict) rec/every-check)
             #(rec/drafting [x Strict] x)]]
    (is (= ::rec/check-failed (:error (err f))))))
