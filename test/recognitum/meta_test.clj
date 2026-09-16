(ns recognitum.meta-test
  "Uniform `meta`: the library's own keys merged over the user's, from both forms of a value.

  `meta` answers the same shape whether it is handed a draft or a sealed value, so nothing
  has to establish which it is holding before asking. Two keys are always present:

      :recognitum.core/passed       the checks currently passing
      :recognitum.core/certificates the certificates currently held

  On a draft both are read off the verdict mask, so they say what has actually been
  established about that draft as it stands. On a sealed value every check passed and every
  certificate is therefore held, so both are constants of the type -- they carry no
  information there, but a common interface is worth more than the absence of a constant.

  The library's keys are merged *over* the user's, not under. `with-meta` is public, so
  anything a caller could set a caller could forge; keys written last cannot be overridden
  by anything handed in. Nothing here is part of the value: the keys are metadata, take no
  part in `=` or `hash`, and do not survive printing."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as heist]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; ---------------------------------------- types ----------------------------------------

;; `::long` spans two fields, so a write to either clears it; `::tag` belongs to no
;; certificate, which is what makes "every certificate is held" distinct from "every check
;; passed" on a draft.
(rec/defrecord Note
  [title body tag]
  {:checks
   {::title ([title] (string? title))
    ::body  ([body]  (string? body))
    ::long  ("the body says more than the title"
             [title body] (and (string? title) (string? body)
                               (> (count body) (count title))))
    ::tag   ([tag]   (keyword? tag))}
   :certificates
   {::shape #{::title ::body ::tag}
    ::sized #{::long}}})

(def ^:private all-checks #{::title ::body ::long ::tag})
(def ^:private all-certs  #{::shape ::sized ::rec/every-check})

(def ^:private note (->Note "hi" "a longer body" :memo))

(defn- whole
  "A draft of `Note` with every field written and nothing established."
  ([] (whole "hi" "a longer body" :memo))
  ([title body tag]
   (-> (rec/draft Note)
       (assoc ::title title) (assoc ::body body) (assoc ::tag tag))))

;; No checks at all: the built-in certificate is held vacuously, by every draft of it.
(rec/defrecord Bare [a] {})

;; Checks, but no certificate written by hand -- so the built-in is the whole of it.
(rec/defrecord Plain [a] {:checks {::a int?}})

(defn- passed [x] (::rec/passed (meta x)))
(defn- certs  [x] (::rec/certificates (meta x)))

;;; ------------------------------- a sealed value answers -------------------------------

(deftest a-sealed-value-reports-every-check-and-every-certificate
  (is (= {::rec/passed all-checks ::rec/certificates all-certs} (meta note)))
  (is (= all-checks (passed note)))
  (is (= all-certs (certs note))))

(deftest the-built-in-certificate-is-held-by-a-sealed-value-with-no-declared-certificates
  (is (= {::rec/passed #{::a} ::rec/certificates #{::rec/every-check}} (meta (->Plain 1)))))

(deftest a-sealed-value-of-a-type-with-no-checks-holds-the-built-in-vacuously
  (is (= {::rec/passed #{} ::rec/certificates #{::rec/every-check}} (meta (->Bare :x)))))

(deftest the-sealed-answer-is-one-constant-per-type
  ;; Every check passes on every sealed value, so there is nothing per-value to compute and
  ;; nothing per-value to allocate.
  (is (identical? (meta note) (meta (->Note "a" "bb" :t))))
  (is (= (meta note) (meta (->Note "a" "bb" :t)))))

(deftest the-example-model-answers-too
  (is (= #{::heist/name ::heist/crew ::heist/take ::heist/cuts
           ::heist/cuts-sum-to-take ::heist/only-crew-get-cuts
           ::heist/crew-is-vouched-for}
         (passed heist/italian-job))
      "the external validation included: a sealed value is one every check passed on")
  (is (= #{::heist/shape ::heist/payout ::rec/every-check} (certs heist/italian-job))
      "and every certificate the type declares follows from that"))

;;; ---------------------------------- a draft answers ----------------------------------

(deftest a-draft-from-the-type-has-established-nothing
  (let [d (rec/draft Note)]
    (is (= {::rec/passed #{} ::rec/certificates #{}} (meta d)))))

(deftest a-draft-from-a-sealed-value-starts-with-everything
  (let [d (rec/draft note)]
    (is (= {::rec/passed all-checks ::rec/certificates all-certs} (meta d)))))

(deftest a-draft-of-a-type-with-no-checks-holds-the-built-in-vacuously
  (is (= {::rec/passed #{} ::rec/certificates #{::rec/every-check}} (meta (rec/draft Bare)))))

(deftest a-draft-reports-what-has-been-established-on-it
  (let [d (whole)]
    (is (= #{} (passed d)))
    (is (true? (rec/check d ::title)))
    (is (= #{::title} (passed d)) "one check establishes one verdict")
    (is (= #{} (certs d)) "and no certificate, since ::shape wants three")
    (is (true? (rec/check d ::body)))
    (is (true? (rec/check d ::tag)))
    (is (= #{::title ::body ::tag} (passed d)))
    (is (= #{::shape} (certs d)) "a certificate completed by running its checks one at a time")
    (is (true? (rec/check d ::long)))
    (is (= all-checks (passed d)))
    (is (= all-certs (certs d)) "including the built-in, now that everything passes")))

(deftest a-failing-check-shows-in-the-metadata-as-not-passing
  (let [d (whole "a title longer than the body" "short" :memo)]
    (is (true? (rec/check d ::title)))
    (is (= {::long false} (rec/check d ::long)) "the body says less than the title")
    (is (= #{::title ::body} (passed d))
        "::long waits on both shape validations, so establishing it established them")
    (is (= #{} (certs d)))))

(deftest certifying-shows-up-as-the-certificate-being-held
  (let [d (whole)]
    (is (true? (rec/check d ::shape)))
    (is (= #{::title ::body ::tag} (passed d)))
    (is (= #{::shape} (certs d)))
    (is (true? (rec/check d ::rec/every-check)))
    (is (= all-checks (passed d)))
    (is (= all-certs (certs d)))))

(deftest a-write-clears-what-it-invalidates
  (let [d (rec/draft note)]
    (is (= all-checks (passed d)))
    (let [d' (assoc d ::tag :other)]
      (is (= #{::title ::body ::long} (passed d')) "only the checks naming tag are cleared")
      (is (= #{::sized} (certs d')) "::shape and the built-in both want ::tag")
      (is (= all-checks (passed d)) "the parent keeps its own verdicts"))
    (let [d' (assoc d ::body "x")]
      (is (= #{::title ::tag} (passed d')) "::body and ::long both name body")
      (is (= #{} (certs d'))))))

(deftest a-write-to-a-field-no-check-names-clears-nothing
  (let [d (assoc (rec/draft (->Bare :x)) ::a :y)]
    (is (= #{} (passed d)))
    (is (= #{::rec/every-check} (certs d)))))

(deftest the-metadata-and-the-predicates-agree-at-every-step
  ;; `passed` and `certificates` answer the same question directly; `meta` is the uniform
  ;; way to ask it, and the two must never diverge.
  (let [d (whole)]
    (doseq [step [#(rec/check % ::title)
                  #(rec/check % ::long)
                  #(rec/check % ::shape)
                  #(rec/check % ::tag)]]
      (step d)
      (is (= (rec/passed d) (passed d)))
      (is (= (rec/certificates d) (certs d))))))

;;; ------------------------------- merged over, not under -------------------------------

(deftest user-metadata-comes-back-alongside-the-library-keys
  (let [m (with-meta note {:src :test})]
    (is (= {:src :test ::rec/passed all-checks ::rec/certificates all-certs} (meta m)))
    (is (= {::rec/passed all-checks ::rec/certificates all-certs} (meta note))
        "with-meta does not mutate the original")))

(deftest user-metadata-on-a-draft-comes-back-alongside-the-library-keys
  (let [d (with-meta (whole) {:src :test})]
    (is (= {:src :test ::rec/passed #{} ::rec/certificates #{}} (meta d)))
    (rec/check d ::title)
    (is (= {:src :test ::rec/passed #{::title} ::rec/certificates #{}} (meta d))
        "the user's keys stay put while the library's move")))

(deftest the-library-keys-cannot-be-forged-on-a-sealed-value
  (let [m (with-meta (->Plain 1) {::rec/passed #{:everything} ::rec/certificates #{:everything}})]
    (is (= #{::a} (passed m)))
    (is (= #{::rec/every-check} (certs m)))))

(deftest the-library-keys-cannot-be-forged-on-a-draft
  (let [d (with-meta (rec/draft Note) {::rec/passed all-checks
                                       ::rec/certificates all-certs
                                       :src :test})]
    (is (= #{} (passed d)) "nothing has been established, whatever the caller wrote")
    (is (= #{} (certs d)))
    (is (= :test (:src (meta d))) "and the caller's own keys are untouched")))

(deftest handing-a-value-its-own-metadata-back-changes-nothing
  ;; The round trip a caller makes without thinking: read the metadata, add a key, write it
  ;; back. The library's keys go in as data and come back out as the answer.
  (let [d  (whole)
        _  (rec/check d ::title)
        d' (with-meta d (assoc (meta d) :src :test))]
    (is (= {:src :test ::rec/passed #{::title} ::rec/certificates #{}} (meta d')))
    (rec/check d' ::body)
    (is (= #{::title ::body} (passed d')))))

(deftest metadata-is-never-nil
  (is (some? (meta note)))
  (is (some? (meta (rec/draft Note))))
  (is (some? (meta (->Bare :x))))
  (is (some? (meta (rec/draft Bare)))))

;;; --------------------------- the same question of either form ---------------------------

(deftest a-draft-and-a-sealed-value-answer-the-same-way
  ;; The point of the uniformity: a caller reads the key without first asking which form it
  ;; is holding.
  (let [xs [note (rec/draft note) (whole) (rec/draft Note)]]
    (doseq [x xs]
      (is (set? (passed x)))
      (is (set? (certs x)))
      (is (every? all-checks (passed x)))
      (is (every? all-certs (certs x)))))
  (is (= (meta note) (meta (rec/draft note)))
      "a draft of a sealed value has established exactly what the sealed value guarantees"))

(deftest sealing-a-draft-produces-the-sealed-constant
  (let [d (with-meta (whole) {:src :test})
        s (rec/seal d)]
    (is (= {:src :test ::rec/passed all-checks ::rec/certificates all-certs} (meta s))
        "user metadata survives the seal; the library's keys become the type's constant")))

(deftest drafting-carries-user-metadata-and-reports-the-constant
  (let [s (rec/drafting [h (with-meta note {:src :test})]
            (assoc h ::title "another"))]
    (is (= {:src :test ::rec/passed all-checks ::rec/certificates all-certs} (meta s)))))

(deftest passed-and-certificates-answer-for-both-forms-and-agree-with-meta
  ;; They carry the same names as the keys and must carry the same values. A draft reads
  ;; its mask; a sealed value reads the type's constant, since being sealed is exactly
  ;; every check having passed.
  (doseq [x [note (rec/draft note) (whole) (rec/draft Note) (->Bare :x) (rec/draft Bare)]]
    (is (= (::rec/passed (meta x)) (rec/passed x)) (pr-str x))
    (is (= (::rec/certificates (meta x)) (rec/certificates x)) (pr-str x)))
  (is (= all-checks (rec/passed note)))
  (is (= all-certs (rec/certificates note)))
  (testing "and a value that is not one of this library's is refused by both"
    (doseq [x [Note {::title "x"} nil 42]]
      (is (= ::rec/not-a-value (:error (err #(rec/passed x)))) (pr-str x))
      (is (= ::rec/not-a-value (:error (err #(rec/certificates x)))) (pr-str x)))))

;;; ------------------------- not part of the value it is about -------------------------

(deftest the-library-keys-are-metadata-and-not-entries
  (let [d (rec/draft note)]
    (doseq [x [note d]]
      (is (nil? (get x ::rec/passed)))
      (is (nil? (get x ::rec/certificates)))
      (is (not (contains? x ::rec/passed)))
      (is (= [::title ::body ::tag] (vec (keys x))))
      (is (= 3 (count x)))
      (is (= {::title "hi" ::body "a longer body" ::tag :memo} (into {} x))))))

(deftest metadata-takes-no-part-in-equality-or-hash
  (let [a (whole)
        b (whole)]
    (rec/check a ::title)
    (rec/check a ::shape)
    (is (not= (meta a) (meta b)) "different verdicts")
    (is (= a b) "same data")
    (is (= (hash a) (hash b))))
  (is (= note (with-meta note {:src :test})))
  (is (= (hash note) (hash (with-meta note {:src :test}))))
  (is (false? (= note (rec/draft note)))
      "but the two forms are two types, and a record decides equality on the class first"))

(deftest the-library-keys-do-not-survive-printing
  (let [s (pr-str note)]
    (is (= "#recognitum.meta-test/Note{:body \"a longer body\", :tag :memo, :title \"hi\"}" s))
    (is (not (re-find #"passed|certificates" s))))
  (let [s (pr-str (whole))]
    (is (re-find #"^#<Note draft " s))
    (is (not (re-find #"passed|certificates" s)))))

(deftest a-value-read-back-reports-the-constant-rather-than-what-was-written
  (let [v (edn/read-string {:readers (rec/data-readers)} (pr-str note))]
    (is (= note v))
    (is (= {::rec/passed all-checks ::rec/certificates all-certs} (meta v)))))
