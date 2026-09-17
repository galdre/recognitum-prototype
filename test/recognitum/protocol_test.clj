(ns recognitum.protocol-test
  "Protocol implementations: one body, installed on both generated types. On the draft type
  the body runs as written. On the sealed type a method marked `^:drafting` opens a draft of
  the receiver, runs the body against that, and seals a draft of this type that it returns;
  an unmarked method receives the sealed value and runs its body as written. Either way
  anything but a draft of this type passes through untouched. `unsealed` marks a draft a
  method returns as one not to seal, and both implementations unwrap it. `(unsealed x)`
  throws where `x` is not a draft, and the method throws where it is a draft of another
  type. `drafting` and `seal` unwrap the marker and seal the draft inside it."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.collections :as rc]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]
   [recognitum.roster :as roster]))
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

(defn- in-scratch
  "Evaluate `form` in the scratch namespace and return what it evaluated to."
  [form]
  (binding [*ns* @scratch] (eval form)))

(defn- template-method-arities
  "Every method in the body of the template both generated types are built on, as a
  `[name arity]` pair whose arity counts the receiver, read out of the var itself:
  `potemkin.types/def-abstract-type` leaves the quoted `deftype` form there, and the body is
  interface names interleaved with method forms."
  []
  (let [[_deftype _name _fields & body] rc/AbstractRecord]
    (into #{} (comp (filter seq?) (map (fn [[nm args]] [nm (count args)]))) body)))

(defn- arglist
  "A method arglist of `n` distinct parameters, the receiver first."
  [n]
  (vec (repeatedly n #(gensym "p"))))

(defn- refusal
  "The ex-data of the refusal a one-method definition of `mname` at `arity` throws, or nil."
  [mname arity type-name]
  (compile-data (list 'recognitum.core/defrecord (gensym "Reserved")
                      '[a] {}
                      type-name
                      (list mname (arglist arity) 1))))

;;; ---------------------------------------- the model ----------------------------------------

(defprotocol Ledgerish
  (balance        [l]          "a number, which is not a draft of anything")
  (spend          [l amount]   "raises debits before credits: the state in between is invalid")
  (spend-twice    [l a b]      "calls the method again")
  (receiver-report [l]         "what the receiver looks like from inside the body")
  (me             [l]          "the receiver itself, returned unsealed")
  (revise         [l who]      "a write, opted out of the seal")
  (hand-back      [l other]    "whatever it was handed")
  (smuggle        [l other]    "a draft of something else, marked unsealed")
  (nested         [l amount]   "uses `drafting` internally")
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

  ^:drafting
  (spend [l amount]
    ;; Neither write is legal on its own: the first breaks ::in-the-black and the second
    ;; repairs it. On a sealed value the first `assoc` alone would throw.
    (-> l
        (assoc ::debits (+ (::debits l) amount))
        (assoc ::credits (+ (::credits l) amount))))

  ^:drafting
  (spend-twice [l a b] (spend (spend l a) b))

  ^:drafting
  (receiver-report [l]
    {:draft?    (rec/draft? l)
     :sealed?   (rec/sealed? l)
     :instance? (instance? Ledger l)
     :of-type?  (rec/of-type? Ledger l)
     :owner     (::owner l)})

  ^:drafting
  (me [l] (rec/unsealed l))

  ^:drafting
  (revise [l who] (rec/unsealed (assoc l ::owner who)))

  (hand-back [l other] other)

  ^:drafting
  (smuggle [l other] (rec/unsealed (rec/draft other)))

  ^:drafting
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

;; `recur` in a method body. A `defrecord` method body is a function body, so `recur`
;; targets it with the receiver excluded; a body written here has to mean the same thing,
;; on both generated types -- including the sealed one, where the body sits inside the
;; `try` that pops the ownership frame.
(defprotocol Counting
  (countdown [c n]     "recurs, as a defrecord method body may")
  (sum-down  [c n acc] "two extra arguments, both of them rebound by the recur")
  (no-recur  [c n]     "an ordinary body, to show the loop changes nothing")
  (label     [c]       "nothing but the receiver -- there is no binding to rebind"))

(rec/defrecord Counter
  [tag]
  {:checks {::tag string?}}

  Counting
  ;; Marked, so that the two recurring bodies sit inside the ownership frame the wrapper
  ;; pushes, which is the placement `recur` has to survive.
  ^:drafting
  (countdown [c n]     (if (pos? n) (recur (dec n)) [:done (::tag c)]))
  ^:drafting
  (sum-down  [c n acc] (if (pos? n) (recur (dec n) (+ acc n)) [:sum acc (::tag c)]))
  (no-recur  [c n]     [:plain n (::tag c)])
  (label     [c]       (::tag c)))

(def counter (->Counter "c"))

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

;;; ------------------------------------ recur in a body ------------------------------------

(deftest recur-in-a-method-body-targets-the-body-itself
  ;; The arity is the one a `defrecord` method body has: the receiver is not rebound, so
  ;; `(recur (dec n))` on a two-argument method takes one argument.
  (testing "on the sealed type, where the body sits inside the ownership try"
    (is (= [:done "c"] (countdown counter 3)))
    (is (= [:done "c"] (countdown counter 0)))
    (is (= [:sum 6 "c"] (sum-down counter 3 0))))
  (testing "and on the draft type, where the body is an argument to the return wrapper"
    (let [d (rec/draft counter)]
      (is (= [:done "c"] (countdown d 3)))
      (is (= [:sum 6 "c"] (sum-down d 3 0)))))
  (testing "the receiver the loop carries is still a draft of this type"
    (is (= "c" (::tag (rec/seal (rec/draft counter)))))))

(deftest a-body-that-does-not-recur-is-unchanged
  (is (= [:plain 2 "c"] (no-recur counter 2)))
  (is (= [:plain 2 "c"] (no-recur (rec/draft counter) 2)))
  (testing "including one taking nothing but the receiver, whose loop binds nothing"
    (is (= "c" (label counter)))
    (is (= "c" (label (rec/draft counter))))))

;;; --------------------------- a primitive-hinted parameter ---------------------------

(deftest a-primitive-hinted-parameter-is-refused-by-clojure-and-not-by-this-library
  ;; A method body cannot take `^long n`, and the refusal is Clojure's rather than the
  ;; wrapper's: each form is written against `clojure.core/defrecord` and against this
  ;; library's, and the two fail the same way at the same point.
  (testing "hinted where the protocol is declared, the form compiles and the call site does not"
    ;; `defprotocol` generates a dispatch fn that implements IFn and nothing narrower, so a
    ;; call the compiler emits as a primitive invocation has nothing to invoke.
    (doseq [[what form]
            [["clojure.core/defrecord"
              '(do (clojure.core/defprotocol PrimDecl (prim-a [h ^long n]))
                   (clojure.core/defrecord PlainPrimA [label]
                     PrimDecl (prim-a [h n] (+ n 1)))
                   (prim-a (->PlainPrimA "p") 3))]
             ["recognitum.core/defrecord"
              '(do (recognitum.core/defrecord RecPrimA
                     [label] {:checks {:recognitum.protocol-scratch/label string?}}
                     PrimDecl (prim-a [h n] (+ n 1)))
                   (prim-a (->RecPrimA "p") 3))]]]
      (let [t (some-> (compiles form) root-cause)]
        (is (instance? ClassCastException t) (str what " compiled and ran it"))
        (is (re-find #"IFn\$OLO" (str (some-> t .getMessage)))
            (str what ": the dispatch fn is not a primitive-capable one")))))
  (testing "hinted on the implementation's arglist, deftype* refuses the method outright"
    ;; The interface a protocol generates takes Objects, so there is no method with a long
    ;; parameter for the hinted arglist to match.
    (doseq [[what form]
            [["clojure.core/defrecord"
              '(do (clojure.core/defprotocol PrimImpl (prim-b [h n]))
                   (clojure.core/defrecord PlainPrimB [label]
                     PrimImpl (prim-b [h ^long n] (+ n 1))))]
             ["recognitum.core/defrecord"
              '(recognitum.core/defrecord RecPrimB
                 [label] {:checks {:recognitum.protocol-scratch/label string?}}
                 PrimImpl (prim-b [h ^long n] (+ n 1)))]]]
      (is (re-find #"leave off hints" (str (compile-message form)))
          (str what " accepted a primitive hint on a protocol method's arglist")))))

;;; ------------------- a hinted parameter on an interface method -------------------

;; Declared as interfaces rather than protocols: an interface method can take a primitive,
;; so an implementation of one may hint its parameter -- and an implementation of a name
;; the interface overloads by type must.
(definterface Arithmetical
  (tally [^long n])
  (drain [^long n])
  (scale [^double factor]))

(definterface Rendering
  (render [^long n])
  (render [^String s]))

(definterface Prefixing
  (initial [^String s]))

(deftest a-parameter-hinted-in-the-implementation-installs-on-both-types
  (is (nil? (compiles
             '(recognitum.core/defrecord Tallied
                [base]
                {:checks {:recognitum.protocol-scratch/base int?}}
                recognitum.protocol_test.Arithmetical
                (tally [t ^long n] (+ n (:recognitum.protocol-scratch/base t)))
                (drain [t ^long n]
                  (if (pos? n) (recur (dec n)) [:done (:recognitum.protocol-scratch/base t)]))
                (scale [t ^double factor]
                  (* factor (:recognitum.protocol-scratch/base t)))))))
  (let [sealed (in-scratch '(->Tallied 2))
        both   [["on the sealed value" sealed]
                ["on its draft" (rec/draft sealed)]]]
    (testing "a long parameter"
      (doseq [[what ^Arithmetical v] both]
        (is (= 5 (.tally v 3)) what)))
    (testing "a long parameter the body recurs on, down to zero"
      (doseq [[what ^Arithmetical v] both]
        (is (= [:done 2] (.drain v 4)) what)))
    (testing "a double parameter"
      (doseq [[what ^Arithmetical v] both]
        (is (= 3.0 (.scale v 1.5)) what)))))

(deftest an-overloaded-name-reaches-the-implementation-its-argument-matches
  (is (nil? (compiles
             '(recognitum.core/defrecord Rendered
                [base]
                {:checks {:recognitum.protocol-scratch/base int?}}
                recognitum.protocol_test.Rendering
                (render [r ^long n] [:long (+ n (:recognitum.protocol-scratch/base r))])
                (render [r ^String s] [:string (str s (:recognitum.protocol-scratch/base r))])))))
  (let [sealed (in-scratch '(->Rendered 2))
        both   [["on the sealed value" sealed]
                ["on its draft" (rec/draft sealed)]]]
    (testing "a long argument"
      (doseq [[what ^Rendering v] both]
        (is (= [:long 5] (.render v 3)) what)))
    (testing "a String argument"
      (doseq [[what ^Rendering v] both]
        (is (= [:string "x2"] (.render v "x")) what)))))

(deftest an-object-hinted-parameter-keeps-its-class-inside-the-body
  (let [warnings (java.io.StringWriter.)
        refused  (binding [*warn-on-reflection* true
                           *err*                warnings]
                   (compiles
                    '(recognitum.core/defrecord Prefixed
                       [prefix]
                       {:checks {:recognitum.protocol-scratch/prefix string?}}
                       recognitum.protocol_test.Prefixing
                       (initial [p ^String s]
                         (str (:recognitum.protocol-scratch/prefix p) (.substring s 0 1))))))]
    (is (nil? refused))
    (is (= "" (str warnings)) "the call in the body compiled to reflection"))
  (let [sealed (in-scratch '(->Prefixed ">"))]
    (doseq [[what ^Prefixing v] [["on the sealed value" sealed]
                                 ["on its draft" (rec/draft sealed)]]]
      (is (= ">a" (.initial v "abc")) what))))

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
  ;; The wrapper owns the draft, so the inner `drafting` passes it through and does not seal.
  (let [l (nested led 20)]
    (is (rec/sealed? l))
    (is (= 20 (::debits l)))
    (is (= 30 (::credits l))))
  (testing "called on a draft owned by no form, the inner `drafting` adopts it and seals"
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
    (testing "what the caller receives is a draft owned by no drafting"
      (is (nil? (internal/owner-of d))))
    (testing "adoptable by a later drafting"
      (let [l (rec/drafting [x d] (assoc x ::credits 20))]
        (is (rec/sealed? l))
        (is (= "ivy" (::owner l)))
        (is (= 20 (::credits l)))))
    (testing "or sealable by hand"
      (is (rec/sealed? (rec/seal d))))))

(deftest the-opt-out-does-not-let-an-invalid-value-become-a-sealed-one
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
             (is (internal/owned-here? y) "still owned by the enclosing form")
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
  ;; A `drafting` form is the author asking to seal, and a marker that defeated that would
  ;; be a way past the checks.
  (let [l (rec/drafting [x led] (rec/unsealed (assoc x ::credits 50)))]
    (is (rec/sealed? l))
    (is (= 50 (::credits l))))
  (testing "and it still validates what it seals"
    (is (= ::rec/check-failed
           (:error (err #(rec/drafting [x led] (rec/unsealed (assoc x ::owner 1))))))))
  (testing "seal itself ignores the marker too"
    (is (rec/sealed? (rec/seal (rec/unsealed (rec/draft led)))))))

;;; ------------------------------- drafting methods -------------------------------

;; Every method below is written twice, with the mark and without, so that a pair differs in
;; nothing but the mark.
(defprotocol Noting
  (here             [n]   "the receiver, from an unmarked body")
  (here-drafting    [n]   "the receiver, from a marked one")
  (retitle          [n s] "one write, unmarked")
  (retitle-drafting [n s] "the same write, marked")
  (loosen           [n]   "a write returned `unsealed`, unmarked")
  (loosen-drafting  [n]   "the same body, marked")
  (grow-directly    [n k] "two writes, the first of which breaks an invariant, unmarked")
  (grow             [n k] "the same two, marked on the method form")
  (grow-by-the-name [n k] "the same two, marked on the method name")
  (grow-nested      [n k] "the first write inside a `drafting` of its own, marked")
  (step-down        [n k] "recurs, unmarked"))

(rec/defrecord Note
  [title pages read]

  {:checks
   {::title string?
    ::pages ([pages] (and (int? pages) (not (neg? pages))))
    ::read  ([read]  (and (int? read) (not (neg? read))))
    ::read-fits ("no more pages read than the note has"
                 [pages read] (and (int? pages) (int? read) (<= read pages)))}}

  Noting
  (here [n]
    {:draft?    (rec/draft? n)
     :sealed?   (rec/sealed? n)
     :instance? (instance? Note n)
     :of-type?  (rec/of-type? Note n)
     :title     (::title n)})

  ^:drafting
  (here-drafting [n]
    {:draft?    (rec/draft? n)
     :sealed?   (rec/sealed? n)
     :instance? (instance? Note n)
     :of-type?  (rec/of-type? Note n)
     :title     (::title n)})

  (retitle [n s] (assoc n ::title s))
  ^:drafting
  (retitle-drafting [n s] (assoc n ::title s))

  (loosen [n] (rec/unsealed (assoc n ::title "loose")))
  ^:drafting
  (loosen-drafting [n] (rec/unsealed (assoc n ::title "loose")))

  ;; The first write leaves more pages read than the note has, so it is a state no single
  ;; write to a sealed value reaches and the second write repairs it.
  (grow-directly [n k]
    (-> n
        (assoc ::read (+ (::read n) k))
        (assoc ::pages (+ (::pages n) k))))

  ^:drafting
  (grow [n k]
    (-> n
        (assoc ::read (+ (::read n) k))
        (assoc ::pages (+ (::pages n) k))))

  (^:drafting grow-by-the-name [n k]
    (-> n
        (assoc ::read (+ (::read n) k))
        (assoc ::pages (+ (::pages n) k))))

  ^:drafting
  (grow-nested [n k]
    (-> (rec/drafting [x n] (assoc x ::read (+ (::read x) k)))
        (assoc ::pages (+ (::pages n) k))))

  (step-down [n k] (if (pos? k) (recur (dec k)) [:done (::title n)])))

(def note (->Note "a note" 10 10))

(defprotocol Enlisting
  (enlist          [c who] "a write to a draft-only field, unmarked")
  (enlist-drafting [c who] "the same write, marked"))

(rec/defrecord Crew
  [lead members]

  {:options {:io-limited true}

   :checks
   {::lead    keyword?
    ::members ([members] (and (set? members) (every? keyword? members)))
    ::all-on-file ("everyone on the crew is someone the house knows"
                   [::known members] (= (set known) (set members)))}

   :io-deps
   {::known ([members] (roster/fetch-roster members))}}

  Enlisting
  (enlist [c who] (assoc c ::members (conj (::members c) who)))
  ^:drafting
  (enlist-drafting [c who] (assoc c ::members (conj (::members c) who))))

(def crew (->Crew :charlie #{:charlie :lyle}))

(deftest an-unmarked-method-on-the-sealed-type-receives-the-sealed-value
  (is (= {:draft? false :sealed? true :instance? true :of-type? true :title "a note"}
         (here note))
      "so a body asking `instance?` is answered the way it reads")
  (testing "and the method answers with what the body answered"
    (is (= [:done "a note"] (step-down note 0)))))

(deftest an-unmarked-body-that-writes-answers-with-a-sealed-value
  (let [n (retitle note "another note")]
    (is (rec/sealed? n))
    (is (instance? Note n))
    (is (= "another note" (::title n))))
  (testing "and a write the checks refuse throws, so the default cannot produce an invalid value"
    (let [e (err #(retitle note 1))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::title] (keys (:failures e)))))))

(deftest an-unmarked-body-reaches-no-state-a-single-write-could-not
  (let [e (err #(grow-directly note 5))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::read-fits] (keys (:failures e)))))
  (testing "the same body marked is handed a draft, so it may"
    (let [n (grow note 5)]
      (is (rec/sealed? n))
      (is (= 15 (::read n)))
      (is (= 15 (::pages n))))))

(deftest an-unmarked-body-writing-a-draft-only-field-is-refused-as-any-write-is
  (let [e (err #(enlist crew :left-ear))]
    (is (= ::rec/io-limited (:error e)))
    (is (= 'Crew (:record e)))
    (is (= ::members (:key e))))
  (testing "and marked it is handed a draft, which is where that write belongs"
    (let [c (enlist-drafting crew :left-ear)]
      (is (rec/sealed? c))
      (is (= #{:charlie :lyle :left-ear} (::members c))))))

(deftest the-mark-is-read-from-the-method-form-and-from-the-method-name
  ;; Unmarked, this body is refused at its first write, so that both spellings succeed is
  ;; what says the mark was read.
  (let [a (grow note 5)
        b (grow-by-the-name note 5)]
    (is (rec/sealed? a))
    (is (rec/sealed? b))
    (is (= a b))))

(deftest a-drafting-method-still-receives-a-draft-on-the-sealed-type
  (is (= {:draft? true :sealed? false :instance? false :of-type? true :title "a note"}
         (here-drafting note)))
  (testing "and seals the draft the body returns"
    (let [n (grow note 5)]
      (is (rec/sealed? n))
      (is (= 15 (::read n)))))
  (testing "a drafting inside the body passes its draft straight through, so only the wrapper seals"
    ;; The inner form's draft is invalid on its own: had it sealed, the write after it would
    ;; never have run.
    (let [n (grow-nested note 5)]
      (is (rec/sealed? n))
      (is (= 15 (::read n)))
      (is (= 15 (::pages n))))))

(deftest unsealed-in-an-unmarked-body-is-handed-a-sealed-value-and-refuses-it
  (let [e (err #(loosen note))]
    (is (= ::rec/not-a-draft (:error e)))
    (is (rec/sealed? (:value e)) "what it was handed is the value the write produced")
    (is (= "loose" (::title (:value e)))))
  (testing "in a drafting method it hands back a draft owned by no form"
    (let [d (loosen-drafting note)]
      (is (rec/draft? d))
      (is (nil? (internal/owner-of d)))
      (is (= "loose" (::title d))))))

(deftest on-the-draft-type-a-marked-and-an-unmarked-method-are-the-same
  (let [d      (rec/draft note)
        report {:draft? true :sealed? false :instance? false :of-type? true :title "a note"}]
    (is (= report (here d)))
    (is (= report (here-drafting d)))
    (testing "a write answers with a draft either way, and nothing is sealed"
      (doseq [[what x] [["unmarked" (retitle d "x")] ["marked" (retitle-drafting d "x")]]]
        (is (rec/draft? x) what)
        (is (not (rec/sealed? x)) what)
        (is (= "x" (::title x)) what)))
    (testing "and the body runs as written, so a broken invariant is left broken"
      (doseq [[what x] [["unmarked" (grow-directly d 5)] ["marked" (grow d 5)]]]
        (is (rec/draft? x) what)
        (is (= 15 (::read x)) what)
        (is (= 15 (::pages x)) what)))
    (testing "and `unsealed` is unwrapped either way"
      (is (rec/draft? (loosen d)))
      (is (rec/draft? (loosen-drafting d))))))

(deftest recur-works-in-an-unmarked-method-on-both-types
  (is (= [:done "a note"] (step-down note 3)))
  (is (= [:done "a note"] (step-down note 0)))
  (is (= [:done "a note"] (step-down (rec/draft note) 3))))

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
  ;; The draft the wrapper opens inherits the caller's metadata and the sealed value it
  ;; produces carries it out again. `meta` merges the library's own keys over the top of
  ;; it, which is why the comparison is on the caller's key rather than on the whole map.
  (let [m (with-meta led {:tagged 1})
        l (spend m 20)]
    (is (= 1 (:tagged (meta l))))
    (is (= 1 (:tagged (meta (me m)))))
    (is (= (meta led) (dissoc (meta l) :tagged))
        "and what the library adds is the same on the way out as on the way in")))

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
      (is (re-find #"first parameter is the receiver"
                   (compile-message '(recognitum.core/defrecord NoRecv2
                                       [a] {}
                                       recognitum.protocol-test/Described
                                       (describe [] 1))))))))

(deftest a-method-named-for-the-map-interface-does-not-compile
  ;; A method called `keys` or `assoc` would silently replace the one that makes the type
  ;; behave as a map -- the generated type would still compile and quietly stop being one.
  (doseq [[m arity] '[[get 1] [assoc 1] [dissoc 1] [keys 1] [meta 1] [with-meta 1] [empty 1]
                      [cons 2]]]
    (let [e (refusal m arity 'recognitum.protocol-test/Described)]
      (is (= ::rec/reserved-method (:error e)) (str m))
      (is (= m (:method e)) (str m))
      (is (= arity (:arity e)) (str m)))))

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

(deftest a-method-named-for-anything-the-template-defines-does-not-compile
  ;; The template answers some thirty methods, not just the eight of the map interface
  ;; above. `deftype+` merges a body into the template by name and parameter list and lets
  ;; the later definition win, so any of these would take the template's place and still
  ;; compile.
  (doseq [[m arity] '[[seq 1] [count 1] [equiv 2] [hasheq 1] [valAt 2] [valAt 3]
                      [hashCode 1] [equals 2] [iterator 1] [entrySet 1] [coll-reduce 2]
                      [coll-reduce 3] [meta* 1] [get* 1] [assoc* 1]]]
    (let [e (refusal m arity 'recognitum.protocol-test/Described)]
      (is (= ::rec/reserved-method (:error e)) (str m " " arity))
      (is (= m (:method e)) (str m " " arity))
      (is (= arity (:arity e)) (str m " " arity)))))

(deftest a-method-named-for-one-the-expansion-writes-itself-does-not-compile
  ;; The thunk a keyword call site caches on the sealed type, and the two methods a draft's
  ;; verdicts are read and written through.
  (doseq [[m arity] '[[getLookupThunk 2] [verdict-mask 1] [record-verdict! 3]]]
    (let [e (refusal m arity 'recognitum.protocol-test/Described)]
      (is (= ::rec/reserved-method (:error e)) (str m " " arity))
      (is (= m (:method e)) (str m " " arity))
      (is (= arity (:arity e)) (str m " " arity)))))

(deftest the-refusal-carries-everything-it-refuses
  (let [{:keys [reserved reserved-names]}
        (compile-data '(recognitum.core/defrecord Listed
                         [a] {}
                         recognitum.protocol-test/Described
                         (keys [x] 1)))]
    (testing "the map interface a body is written in terms of, under both spellings"
      (is (every? reserved-names '[get assoc dissoc keys meta with-meta empty
                                   get* assoc* dissoc* keys* meta* with-meta* empty*])))
    (testing "every method the template defines, at the arity it defines it at"
      (is (= #{} (into #{} (remove reserved) (template-method-arities)))))
    (testing "the three the expansion writes into the generated bodies"
      (is (every? reserved '[[getLookupThunk 2] [verdict-mask 1] [record-verdict! 3]])))
    (testing "and not toString, which the template leaves to Object as a record does"
      (is (not (contains? reserved-names 'toString)))
      (is (empty? (filter (comp #{'toString} first) reserved))))))

(deftest a-method-the-template-does-not-define-still-installs
  ;; `toString` is the one name in the map and record interfaces the template leaves alone,
  ;; so a body may take it, as on a plain `defrecord`.
  (is (nil? (compiles '(recognitum.core/defrecord Stringy
                         [a]
                         {:checks {:recognitum.protocol-scratch/a int?}}
                         Object
                         (toString [x] (str "a=" (:recognitum.protocol-scratch/a x)))))))
  (is (= "a=1" (str (in-scratch '(->Stringy 1)))))
  (is (= "a=1" (str (in-scratch '(recognitum.core/draft (->Stringy 1)))))))

;;; ------------------- a reserved name at an arity the template leaves open -------------------

(definterface Shelfish
  ;; Declared as an interface rather than a protocol: a protocol would intern `count`, `seq`,
  ;; `values` and `remove` as vars here and shadow the ones the rest of this file calls.
  (count [unit])
  (seq [depth])
  (values [as-of])
  ;; Three parameters rather than two, because `java.util.Map` declares a two-argument
  ;; `remove` returning `boolean`: a method of that name and arity on a type implementing
  ;; `Map` compiles to bytecode the JVM rejects, on a `clojure.core/defrecord` as much as
  ;; here, so the library has nothing to prove at that arity.
  (remove [who reason recorded-at]))

(deftest a-name-the-template-carries-installs-at-an-arity-the-template-leaves-open
  (is (nil? (compiles
             '(recognitum.core/defrecord Shelf
                [items]
                {:checks {:recognitum.protocol-scratch/items vector?}}
                recognitum.protocol_test.Shelfish
                (count [s unit]
                  [unit (count (:recognitum.protocol-scratch/items s))])
                (seq [s depth]
                  [depth (first (:recognitum.protocol-scratch/items s))])
                (values [s as-of]
                  [as-of (:recognitum.protocol-scratch/items s)])
                (remove [s who reason recorded-at]
                  [who reason recorded-at])))))
  (let [sealed (in-scratch '(->Shelf [:a :b :c]))]
    (doseq [[what ^Shelfish v] [["on the sealed value" sealed]
                                ["on its draft" (rec/draft sealed)]]]
      (testing what
        (is (= [:apples 3] (.count v :apples)))
        (is (= [2 :a] (.seq v 2)))
        (is (= [:now [:a :b :c]] (.values v :now)))
        (is (= [:zoe "moved out" :friday] (.remove v :zoe "moved out" :friday)))))
    (testing "and what the same names mean to a map is what it was"
      (is (= 1 (count sealed)) "count of the receiver alone is still the field count")
      (is (= {:recognitum.protocol-scratch/items [:a :b :c]} (into {} (seq sealed)))
          "and seq of it alone is still the entries")
      (is (= 1 (count (rec/draft sealed))))
      (is (= {:recognitum.protocol-scratch/items [:a :b :c]}
             (into {} (seq (rec/draft sealed))))))))

(deftest a-method-at-an-arity-the-template-answers-does-not-compile
  ;; The counterpart: `count` and `seq` of the receiver alone are the template's own.
  (doseq [[m arity] '[[count 1] [seq 1] [values 1] [remove 2]]]
    (let [e (refusal m arity 'recognitum.protocol_test.Shelfish)]
      (is (= ::rec/reserved-method (:error e)) (str m " " arity))
      (is (= m (:method e)) (str m " " arity))
      (is (= arity (:arity e)) (str m " " arity)))))

(deftest a-method-named-for-one-of-the-renamed-seven-does-not-compile-at-any-arity
  ;; The map contract is renamed by name alone, whatever arity the body wrote it at, so none
  ;; of these has an arity of its own the way `count` and `seq` do.
  (doseq [m     '[keys dissoc with-meta get*]
          arity [1 2 3]]
    (let [e (refusal m arity 'recognitum.protocol-test/Described)]
      (is (= ::rec/reserved-method (:error e)) (str m " " arity))
      (is (= m (:method e)) (str m " " arity))
      (is (= arity (:arity e)) (str m " " arity)))))

(deftest invoke-is-the-definitions-own-at-every-arity
  ;; The template declares no `clojure.lang.IFn`, so no arity of `invoke` is spoken for.
  (doseq [arity [2 3]]
    (is (nil? (refusal 'invoke arity 'clojure.lang.IFn)) (str "invoke " arity)))
  (is (nil? (compiles '(recognitum.core/defrecord Sum
                         [tally]
                         {:checks {:recognitum.protocol-scratch/tally int?}}
                         clojure.lang.IFn
                         (invoke [s a] (+ (:recognitum.protocol-scratch/tally s) a))
                         (invoke [s a b] (+ (:recognitum.protocol-scratch/tally s) a b))
                         (invoke [s a b c]
                           (+ (:recognitum.protocol-scratch/tally s) a b c))))))
  (let [sealed (in-scratch '(->Sum 1))]
    (doseq [[what v] [["on the sealed value" sealed] ["on its draft" (rec/draft sealed)]]]
      (testing what
        (is (= 2 (v 1)))
        (is (= 4 (v 1 2)))
        (is (= 7 (v 1 2 3)))))))
