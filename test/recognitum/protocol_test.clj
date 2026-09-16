(ns recognitum.protocol-test
  "Protocol implementations: one body, installed on both generated types.

  `defrecord` generates two types, so a protocol written once has to serve both. On the
  draft type the body runs exactly as written. On the sealed type it is wrapped: the method
  opens a draft of the receiver, runs the body against that, and seals what comes back --
  so a method called on a sealed value cannot produce an invalid one whatever it does
  inside, and the same body serves the impl layer unchanged.

  Only a draft of *this* type is sealed on the way out. Most protocol methods return
  something else entirely -- a number, a string, a map -- and those pass through untouched,
  as does a value that is already sealed.

  `unsealed` opts a return out of the seal, as `reduced` opts out of a reduction. Both
  implementations unwrap it, so the body means the same thing wherever it runs; on the draft
  type it is simply a no-op. `(unsealed x)` where `x` is not a draft of this type throws --
  the marker asserts what is being returned, and a wrong assertion should say so. `drafting`
  does not honour it: the wrapper is implicit, so opting out of it is fair, but a `drafting`
  form is the author asking to seal."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which
  ;; interns vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.protocol-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

;;; ---------------------------------------- the model ----------------------------------------

(defprotocol Ledgerish
  (balance        [l]          "a number, which is not a draft of anything")
  (spend          [l amount]   "raises debits before credits: the state in between is invalid")
  (spend-twice    [l a b]      "calls the method again, so the impl layer is exercised")
  (receiver-report [l]         "what the receiver looks like from inside the body")
  (me             [l]          "the receiver itself, handed back unsealed")
  (revise         [l who]      "a write, opted out of the seal")
  (hand-back      [l other]    "whatever it was handed")
  (smuggle        [l other]    "a draft of something else, marked unsealed")
  (nested         [l amount]   "uses `drafting` internally, as an impl fn would")
  (boom           [l]          "throws"))

(defprotocol Described
  (describe [l] "a second protocol on the same form"))

(rec/defrecord Ledger
  [owner debits credits]

  {:checks
   {::owner   string?
    ::debits  ([debits] (and (int? debits) (not (neg? debits))))
    ::credits ([credits] (and (int? credits) (not (neg? credits))))
    ::in-the-black ("debits never exceed credits"
                    [debits credits]
                    (and (int? debits) (int? credits) (<= debits credits)))}}

  Ledgerish
  (balance [l] (- (::credits l) (::debits l)))

  (spend [l amount]
    ;; Neither write is legal on its own: the first breaks ::in-the-black and the second
    ;; repairs it. On a sealed value the first `assoc` alone would throw.
    (-> l
        (assoc ::debits (+ (::debits l) amount))
        (assoc ::credits (+ (::credits l) amount))))

  (spend-twice [l a b] (spend (spend l a) b))

  (receiver-report [l]
    {:draft?    (rec/draft? l)
     :sealed?   (rec/sealed? l)
     :instance? (instance? Ledger l)
     :of-type?  (rec/of-type? Ledger l)
     :owner     (::owner l)})

  (me [l] (rec/unsealed l))

  (revise [l who] (rec/unsealed (assoc l ::owner who)))

  (hand-back [l other] other)

  (smuggle [l other] (rec/unsealed (rec/draft other)))

  (nested [l amount]
    (rec/drafting [x l]
      (-> x
          (assoc ::debits (+ (::debits x) amount))
          (assoc ::credits (+ (::credits x) amount)))))

  (boom [l] (throw (ex-info "the body threw" {:error ::boom})))

  Described
  (describe [l] (str (::owner l) ":" (- (::credits l) (::debits l)))))

(rec/defrecord Tag [label] {:checks {::label string?}})

(def led (->Ledger "zoe" 0 10))
(def tag (->Tag "t"))

;;; ------------------------------- installed on both types -------------------------------

(deftest one-body-is-installed-on-both-generated-types
  (is (satisfies? Ledgerish led))
  (is (satisfies? Ledgerish (rec/draft led)))
  (is (satisfies? Described led))
  (is (satisfies? Described (rec/draft led)))
  (testing "and it is the two generated classes that carry it, not a catch-all extension"
    (is (extends? Ledgerish Ledger))
    (is (extends? Ledgerish LedgerDraft))
    (is (not (satisfies? Ledgerish {::owner "zoe" ::debits 0 ::credits 10})))
    (is (not (satisfies? Ledgerish tag)))))

(deftest a-second-protocol-on-the-same-form-is-installed-too
  (is (= "zoe:10" (describe led)))
  (is (= "zoe:10" (describe (rec/draft led)))))

;;; ------------------------------- what the receiver is -------------------------------

(deftest the-receiver-is-a-draft-in-both-implementations
  (testing "called on a sealed value, the body still sees a draft"
    (is (= {:draft? true :sealed? false :instance? false :of-type? true :owner "zoe"}
           (receiver-report led)))
    (testing "so a body asking `instance?` would surprise itself, and wants `of-type?`"
      (is (instance? Ledger led))
      (is (false? (:instance? (receiver-report led))))))
  (testing "called on a draft, it is that draft"
    (let [d (rec/draft led)]
      (is (= {:draft? true :sealed? false :instance? false :of-type? true :owner "zoe"}
             (receiver-report d)))
      (is (identical? d (me d)) "the draft impl runs the body as written"))))

(deftest the-sealed-wrapper-drafts-a-copy-and-leaves-the-original-alone
  (let [d (me led)]
    (is (rec/draft? d))
    (is (not (identical? led d)))
    (is (= 0 (::debits led)) "the original is untouched")
    (is (= 10 (::credits led)))))

;;; --------------------- a draft of this type is sealed on the way out ---------------------

(deftest a-body-returning-a-draft-of-this-type-comes-back-sealed
  (let [l (spend led 20)]
    (is (instance? Ledger l))
    (is (rec/sealed? l))
    (is (not (rec/draft? l)))
    (is (= 20 (::debits l)))
    (is (= 30 (::credits l)))
    (is (= 10 (balance l)))))

(deftest the-intermediate-state-the-body-passes-through-is-one-no-write-could-produce
  (testing "the first of the two writes alone is refused on a sealed value"
    (let [e (err #(assoc led ::debits 20))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::in-the-black] (keys (:failures e))))))
  (testing "yet the method containing both of them succeeds"
    (is (rec/sealed? (spend led 20)))))

(deftest the-same-body-on-a-draft-returns-a-draft
  (let [d (spend (rec/draft led) 20)]
    (is (rec/draft? d))
    (is (not (rec/sealed? d)))
    (is (= 20 (::debits d)))
    (is (= 30 (::credits d)))
    (testing "unvalidated, because a draft validates nothing"
      (is (rec/draft? (spend (rec/draft led) -100))))))

(deftest a-method-calling-a-method-is-the-impl-layer-unchanged
  (let [l (spend-twice led 20 5)]
    (is (rec/sealed? l))
    (is (= 25 (::debits l)))
    (is (= 35 (::credits l)))
    (testing "one seal for the whole method, not one per inner call"
      (is (instance? Ledger l))))
  (testing "and the same call on a draft seals nothing"
    (is (rec/draft? (spend-twice (rec/draft led) 20 5)))))

(deftest drafting-inside-a-wrapped-body-passes-the-draft-through-unsealed
  ;; The wrapper owns the draft, so the inner `drafting` recognises it and does not seal --
  ;; the re-entrancy `drafting` already provides, applied without anyone asking for it.
  (let [l (nested led 20)]
    (is (rec/sealed? l))
    (is (= 20 (::debits l)))
    (is (= 30 (::credits l))))
  (testing "called on a free draft the inner `drafting` owns it and does seal"
    (is (rec/sealed? (nested (rec/draft led) 20)))))

(deftest a-body-that-produces-an-invalid-value-throws-at-the-wrapper
  (let [e (err #(spend led -100))]
    (is (= ::rec/check-failed (:error e)))
    (is (= 'Ledger (:record e)))
    (testing "every failure, keyed by check name, never just the first"
      (is (= [::debits ::credits] (keys (:failures e))))
      (is (= -100 (get-in (:failures e) [::debits :values ::debits])))
      (is (= -90 (get-in (:failures e) [::credits :values ::credits]))))))

;;; ------------------------- anything else passes through untouched -------------------------

(deftest a-return-that-is-not-a-draft-of-this-type-passes-through
  (is (= 10 (balance led)))
  (is (= "zoe:10" (describe led)))
  (is (map? (receiver-report led)))
  (testing "including nil, false, and values of other types"
    (is (nil? (hand-back led nil)))
    (is (false? (hand-back led false)))
    (is (= :kw (hand-back led :kw)))
    (is (= [1 2] (hand-back led [1 2]))))
  (testing "including an already-sealed value, which is handed straight back"
    (is (identical? led (hand-back led led)))
    (is (identical? tag (hand-back led tag))))
  (testing "including a draft of some other type, which this type does not seal"
    (let [d (rec/draft tag)]
      (is (rec/draft? (hand-back led d))))))

(deftest an-exception-from-the-body-propagates
  (is (= ::boom (:error (err #(boom led)))))
  (is (= ::boom (:error (err #(boom (rec/draft led))))))
  (testing "it is not caught and reported as a failure of the data"
    (is (thrown? clojure.lang.ExceptionInfo (boom led)))))

;;; ---------------------------------------- unsealed ----------------------------------------

(deftest unsealed-opts-the-return-out-of-the-seal
  (let [d (revise led "ivy")]
    (is (rec/draft? d))
    (is (not (rec/sealed? d)))
    (is (= "ivy" (::owner d)))
    (testing "what the caller receives is a free draft, owned by nothing"
      (is (nil? (rec/owner-of d))))
    (testing "adoptable by a later drafting"
      (let [l (rec/drafting [x d] (assoc x ::credits 20))]
        (is (rec/sealed? l))
        (is (= "ivy" (::owner l)))
        (is (= 20 (::credits l)))))
    (testing "or sealable by hand"
      (is (rec/sealed? (rec/seal d))))))

(deftest the-opt-out-does-not-let-an-invalid-value-become-a-sealed-one
  ;; The draft comes back unvalidated -- that is the point -- but sealing it still refuses.
  (let [d (revise led :not-a-string)]
    (is (rec/draft? d))
    (is (= :not-a-string (::owner d)))
    (is (= ::rec/check-failed (:error (err #(rec/seal d)))))))

(deftest on-the-draft-type-unsealed-is-simply-a-no-op
  (let [d (rec/draft led)]
    (is (identical? d (me d)))
    (is (rec/draft? (revise d "ivy")))
    (is (= "ivy" (::owner (revise d "ivy")))))
  (testing "and it does not free a draft an enclosing drafting owns"
    (is (rec/sealed?
         (rec/drafting [x led]
           (let [y (me x)]
             (is (identical? x y))
             (is (rec/owned-here? y) "still owned by the enclosing form")
             y))))))

(deftest unsealed-refuses-anything-that-is-not-a-draft
  (doseq [x [led 1 "x" nil {} [1] tag]]
    (let [e (err #(rec/unsealed x))]
      (is (= ::rec/not-a-draft (:error e)) (pr-str x))))
  (testing "the message says what it was handed"
    (is (re-find #"draft" (ex-message (try (rec/unsealed 1)
                                           (catch clojure.lang.ExceptionInfo e e)))))))

(deftest unsealed-refuses-a-draft-of-some-other-type
  (testing "from a wrapped method"
    (let [e (err #(smuggle led tag))]
      (is (= ::rec/not-a-draft (:error e)))
      (is (= 'Ledger (:record e)))))
  (testing "and from the draft implementation, so the body means the same thing either way"
    (let [e (err #(smuggle (rec/draft led) tag))]
      (is (= ::rec/not-a-draft (:error e)))
      (is (= 'Ledger (:record e)))))
  (testing "a draft of this type is of course fine"
    (is (rec/draft? (smuggle led led)))
    (is (rec/of-type? Ledger (smuggle led led)))))

(deftest drafting-does-not-honour-unsealed
  ;; The wrapper is implicit, so opting out of it is fair; a `drafting` form is the author
  ;; asking to seal, and a marker that quietly defeated that would be a hole.
  (let [l (rec/drafting [x led] (rec/unsealed (assoc x ::credits 50)))]
    (is (rec/sealed? l))
    (is (= 50 (::credits l))))
  (testing "and it still validates what it seals"
    (is (= ::rec/check-failed
           (:error (err #(rec/drafting [x led] (rec/unsealed (assoc x ::owner 1))))))))
  (testing "seal itself ignores the marker too"
    (is (rec/sealed? (rec/seal (rec/unsealed (rec/draft led)))))))

;;; ------------------------- everything else about the type still holds -------------------------

(deftest a-type-carrying-protocols-is-a-validated-type-in-every-other-respect
  (is (= "zoe" (::owner led)))
  (is (= [::owner ::debits ::credits] (keys led)))
  (is (= {::owner "zoe" ::debits 0 ::credits 10} (into {} led)))
  (is (rec/sealed? (map->Ledger {::owner "a" ::debits 1 ::credits 2})))
  (is (= ::rec/unknown-field (:error (err #(assoc led :owner "x")))))
  (is (= ::rec/check-failed (:error (err #(assoc led ::owner 1)))))
  (is (= ::rec/cannot-dissoc (:error (err #(dissoc led ::owner)))))
  (is (= "#recognitum.protocol-test/Ledger{:credits 10, :debits 0, :owner \"zoe\"}"
         (pr-str led)))
  (is (re-find #"^#<Ledger draft " (pr-str (rec/draft led))))
  (is (nil? (resolve 'recognitum.protocol-test/->LedgerDraft))))

(deftest metadata-survives-a-wrapped-method
  (let [m (with-meta led {:tagged 1})
        l (spend m 20)]
    (is (= {:tagged 1} (meta l)))
    (is (= {:tagged 1} (meta (me m))))))

;;; ------------------------------- what does not compile -------------------------------

(deftest a-method-before-any-protocol-name-does-not-compile
  (let [e (compile-data '(recognitum.core/defrecord Early
                           [a] {}
                           (nope [x] 1)))]
    (is (= ::rec/malformed-impl (:error e)))
    (is (= 'Early (:record e)))
    (is (= '(nope [x] 1) (:form e)))))

(deftest a-trailing-form-that-is-neither-a-name-nor-a-method-does-not-compile
  (doseq [bad ['(recognitum.core/defrecord Junk1
                  [a] {} recognitum.protocol-test/Described :nope)
               '(recognitum.core/defrecord Junk2
                  [a] {} recognitum.protocol-test/Described [describe [x] 1])
               '(recognitum.core/defrecord Junk3
                  [a] {} recognitum.protocol-test/Described "describe")]]
    (is (= ::rec/malformed-impl (:error (compile-data bad))) (str bad))))

(deftest a-method-without-a-proper-arglist-does-not-compile
  (testing "no arglist at all"
    (is (= ::rec/malformed-impl
           (:error (compile-data '(recognitum.core/defrecord NoArgs
                                    [a] {}
                                    recognitum.protocol-test/Described
                                    (describe x 1)))))))
  (testing "no receiver in it -- there would be nothing to draft"
    (let [e (compile-data '(recognitum.core/defrecord NoRecv
                             [a] {}
                             recognitum.protocol-test/Described
                             (describe [] 1)))]
      (is (= ::rec/malformed-impl (:error e)))
      (is (re-find #"receiver" (compile-message '(recognitum.core/defrecord NoRecv2
                                                   [a] {}
                                                   recognitum.protocol-test/Described
                                                   (describe [] 1))))))))

(deftest a-method-named-for-the-map-interface-does-not-compile
  ;; A method called `keys` or `assoc` would silently replace the one that makes the type
  ;; behave as a map -- the generated type would still compile and quietly stop being one.
  (doseq [m '[get assoc dissoc keys meta with-meta empty cons]]
    (let [e (compile-data (list 'recognitum.core/defrecord (gensym "Reserved")
                                '[a] {}
                                'recognitum.protocol-test/Described
                                (list m '[x] 1)))]
      (is (= ::rec/reserved-method (:error e)) (str m))
      (is (= m (:method e)) (str m)))))

(deftest the-definition-map-is-still-required-before-the-implementations
  ;; A protocol name where the definition map goes is not a definition map, and treating it
  ;; as one would define a type with no checks.
  (let [e (compile-data '(recognitum.core/defrecord NoDefinition
                           [a]
                           recognitum.protocol-test/Described
                           (describe [x] 1)))]
    (is (= ::rec/malformed-definition (:error e)))
    (is (= 'NoDefinition (:record e)))))

(deftest a-well-formed-form-with-implementations-compiles
  (is (nil? (compiles '(recognitum.core/defrecord Fine
                         [a]
                         {:checks {:recognitum.protocol-scratch/a int?}}
                         recognitum.protocol-test/Described
                         (describe [x] (str (:recognitum.protocol-scratch/a x))))))))
