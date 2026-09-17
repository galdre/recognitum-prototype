(ns recognitum.surface-test
  "The public surface of `recognitum.core`. A public name that hands out the model map is a
  constructor that skips the checks -- `:draft-seal` turns a draft into a sealed value and
  runs nothing -- and one that writes the record of which checks pass is another, since
  `seal` runs only what that record does not already say is passing. Privacy is advisory
  and `#'` reaches anything; what it buys is that a qualified symbol naming a private var
  does not compile outside the namespace, so nothing reaches either by accident. The first
  test is the set of public vars, written out, so a name added to `recognitum.core` without
  being added here fails the suite."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]
   [recognitum.internal :as internal])
  (:import
   (recognitum.heist Heist)))

(def ^:private vocabulary
  "Every name this library offers as API, and nothing else.

  Defining and building: `defrecord`, `drafting`, `draft`, `seal`, `clean`. Telling values
  apart: `draft?`, `sealed?`, `of-type?`. Checking, one verb over a check name or a
  certificate name, one value or a group: `check`, `check!`, `check-all`, `check-all!`, and
  the built-in certificate `every-check`, which is a var and not a keyword. Running what
  `check` would skip: `recheck`, `recheck!`, `recheck-all`, `recheck-all!`. Which checks a
  value passes: `passed`, `certificates`. Opting a protocol method's return out of the
  implicit seal: `unsealed`. Reading printed values back: `data-readers`."
  '#{defrecord drafting draft seal clean
     draft? sealed? of-type?
     check check! check-all check-all!
     recheck recheck! recheck-all recheck-all!
     every-check passed certificates
     unsealed data-readers})

(defn- publics [] (set (keys (ns-publics 'recognitum.core))))

;;; ------------------------------ the surface itself ------------------------------

(deftest the-public-surface-is-the-specs-vocabulary-and-nothing-else
  (let [ps (publics)]
    (is (= vocabulary ps))
    (testing "nothing the documents never mention is public"
      (is (empty? (set/difference ps vocabulary))
          "an internal has become part of the surface -- narrow it, or document it"))
    (testing "nothing the documents do mention has gone missing"
      (is (empty? (set/difference vocabulary ps))))))

(deftest the-internals-still-exist-under-their-own-names
  (testing "each is a var of recognitum.core"
    (doseq [nm '[model-of draft-model verdict-mask record-verdict! adopt owner-of
                 owned-here? definition-sections known-options parse-definition
                 resolve-deps reject-duplicate-names! register! register-draft!
                 register-reader! run-checks! run-draft-checks! run-io-checks! fetch-io
                 open dep-values checks-for every-check]]
      (is (var? (ns-resolve 'recognitum.core nm))
          (str nm " is not a var of recognitum.core at all")))))

;;; ------------------------------ what being private buys ------------------------------

(defn- messages
  "Every message down `t`'s cause chain, run together. A compile-time refusal arrives
  wrapped, and what the refusal said is in the wrapping rather than at the top."
  [^Throwable t]
  (->> (iterate (fn [^Throwable x] (.getCause x)) t)
       (take-while some?)
       (map (fn [^Throwable x] (str (.getMessage x))))
       (apply str)))

(defn- compiles-elsewhere?
  "Does `form` compile in a namespace that is not `recognitum.core`?

  A qualified symbol naming a private var is refused by the compiler with `is not public`.
  Anything else the compiler objects to is rethrown, so a typo is not read as proof."
  [form]
  (binding [*ns* (create-ns 'recognitum.surface-test.elsewhere)]
    (refer-clojure)
    (try
      (eval form)
      true
      (catch Exception e
        (if (re-find #"is not public" (messages e))
          false
          (throw e))))))

(deftest a-public-name-compiles-elsewhere-and-that-is-how-this-is-measured
  (is (true? (compiles-elsewhere? '(fn [d] (recognitum.core/seal d))))
      "the probe itself has to be able to say yes, or its noes mean nothing"))

(deftest nothing-public-writes-the-verdict-mask
  (is (false? (compiles-elsewhere? '(fn [d i p] (recognitum.core/record-verdict! d i p))))
      "seal runs only what the record does not say is passing, so a public writer of it is a way past every check")
  (is (false? (compiles-elsewhere? '(fn [d] (recognitum.core/verdict-mask d)))))
  (is (false? (compiles-elsewhere? '(fn [d] (satisfies? recognitum.core/Verdicts d))))))

(deftest nothing-public-hands-out-the-model
  (testing "the model carries :draft-seal, which makes a sealed value and runs nothing"
    (is (false? (compiles-elsewhere? '(fn [x] (recognitum.core/model-of x)))))
    (is (false? (compiles-elsewhere? '(fn [d] (recognitum.core/draft-model d))))))
  (testing "nor does a definition intern one under a name of its own"
    (is (not (contains? (set (keys (ns-publics 'recognitum.heist))) 'Heist-model))
        "the per-type model var is the same hole reached through the defining namespace")))

(deftest nothing-public-runs-or-skips-the-checks-by-hand
  (doseq [form '[(fn [m d is] (recognitum.core/run-checks! m d is))
                 (fn [m d is] (recognitum.core/run-draft-checks! m d is))
                 (fn [m x is] (recognitum.core/run-io-checks! m x is))
                 (fn [m x is] (recognitum.core/fetch-io m x is))
                 (fn [m x t a] (recognitum.core/open m x t a))
                 (fn [d t] (recognitum.core/adopt d t))
                 (fn [c m] (recognitum.core/register! c m))]]
    (is (false? (compiles-elsewhere? form)) (str form " still compiles outside the library"))))

;;; ------------------------------ the forging path itself ------------------------------

(deftest the-draft-under-the-exploit-really-does-fail-its-checks
  (let [d (assoc (rec/draft h/italian-job) ::h/take "garbage")]
    (is (= ::rec/check-failed
           (try (rec/seal d) nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))
        "so anything that seals it is skipping a check, not passing one")))

(deftest the-forging-path-is-not-spellable-in-public-names
  (testing "sealing after marking every check passing"
    (is (false? (compiles-elsewhere?
                 '(fn [d] (doseq [i (range 64)]
                            (try (recognitum.core/record-verdict! d i true)
                                 (catch Throwable _ nil)))
                    (recognitum.core/seal d)))))
    (is (false? (compiles-elsewhere?
                 '(fn [x d] ((:draft-seal (recognitum.core/model-of x)) d)))))))

(deftest the-record-is-reachable-through-the-var-because-privacy-is-advisory
  ;; The suite reaches the same internals through `recognitum.internal`.
  (let [d (assoc (rec/draft h/italian-job) ::h/take "garbage")]
    (dotimes [i 64] (internal/record-verdict! d i true))
    (let [forged (rec/seal d)]
      (is (instance? Heist forged))
      (is (= "garbage" (::h/take forged))
          "so the guarantee rests on the surface"))))
