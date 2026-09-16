(ns recognitum.unset-test
  "Unset is `nil`.

  A field that has not been written reads as `nil`, everywhere, exactly as it would on a
  `defrecord` whose field was never set. There is no sentinel and no \"never written\"
  state distinct from `nil`: a draft opened from the type starts with every field `nil`,
  `keys` answers with every declared key whatever has been written, and every check
  receives `nil` for a field nobody wrote. Whether `nil` is acceptable is the shape
  validation's business and nothing else's; a field with no shape validation seals
  holding it.

  So `:missing-field` is gone. It was the library reporting on its own internals -- there
  was a sentinel a check must not be handed, and the error existed to keep it away from
  one. With the sentinel gone there is nothing to keep away, and a check saying what it
  thinks of `nil` is a better answer than the library saying nobody wrote anything."
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

;;; -------------------------------- the map constructor --------------------------------

(deftest the-map-constructor-treats-an-omitted-key-as-nil
  (let [n (map->Note {::title "t"})]
    (is (rec/sealed? n))
    (is (= {::title "t" ::body nil ::tags nil} (into {} n))))
  (testing "and the shape validation is what decides whether that is acceptable"
    (let [e (err #(map->Strict {}))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::title] (keys (:failures e))))))
  (testing "while a key the type does not declare is refused as it always was"
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
  ;; There is no pre-flight left to throw before the first check of the first value: an
  ;; incomplete draft is an ordinary draft holding nils.
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

;;; --------------------------------- the error itself is gone ---------------------------------

(deftest nothing-reports-a-missing-field-any-more
  ;; No entry point reports one: an incomplete draft is an ordinary draft holding nils, and
  ;; what comes back is whatever the checks think of `nil`.
  (doseq [f [#(rec/seal (rec/draft Strict))
             #(map->Strict {})
             #(rec/check! (rec/draft Strict) ::title)
             #(rec/check! (rec/draft Strict) rec/every-check)
             #(rec/drafting [x Strict] x)]]
    (is (= ::rec/check-failed (:error (err f))))))
