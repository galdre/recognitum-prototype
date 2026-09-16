(ns recognitum.surface-test
  "The public surface of `recognitum.core`, and what being off it is worth.

  `SPEC.md`'s first paragraph is the whole of what this library claims: a value cannot
  exist unless its checks passed, and there is no constructor that skips them. A public
  name that hands out the model map is such a constructor -- `:draft-seal` turns a draft
  into a sealed value and runs nothing -- and a public name that writes the verdict mask is
  another, because `seal` believes the mask and runs only what the mask does not already
  call passed.

  Clojure's privacy is advisory: `#'` reaches anything, and this suite uses it. What
  privacy buys is not a wall but the absence of a door -- a qualified symbol naming a
  private var does not compile outside the namespace that owns it, so nothing reaches the
  mask by accident, by autocomplete, or by reading the namespace's public names and
  believing they are the API. That is the same standard `clojure.core/defrecord` holds its
  own internals to, and it is what this file pins.

  The first test is the one that matters longest: the set of public vars, written out. A
  name added to `recognitum.core` without being added here fails the suite, so the surface
  cannot drift the way it drifted into seventy-three names to begin with."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]
   [recognitum.internal :as internal])
  (:import
   (recognitum.heist Heist)))

(def ^:private vocabulary
  "Every name `SPEC.md` and `README.md` use as API, and nothing else.

  Defining and building: `defrecord`, `drafting`, `draft`, `seal`, `clean`. Telling values
  apart: `draft?`, `sealed?`, `of-type?`. Checking, one verb over a check name or a
  certificate name, one value or a group: `check`, `check!`, `check-all`, `check-all!`, and
  the built-in certificate `every-check` the README names as a var. Running what the mask
  would have skipped: `recheck`, `recheck!`, `recheck-all`, `recheck-all!`. What a value has
  established: `passed`, `certificates`. Opting a protocol method's return out of the
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
  (testing "narrowing the surface moved nothing and renamed nothing"
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

  This is the question privacy actually answers. A qualified symbol naming a private var is
  refused by the compiler with `is not public`, which is what makes `defn-` a narrowing of
  the surface rather than a note in a docstring. Anything else the compiler objects to is
  rethrown -- a test that read every failure as proof would pass on a typo."
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
      "seal believes the mask, so a public writer of it is a way past every check")
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

(deftest the-forging-path-is-no-longer-spellable-in-public-names
  (testing "the exploit as it was written"
    (is (false? (compiles-elsewhere?
                 '(fn [d] (doseq [i (range 64)]
                            (try (recognitum.core/record-verdict! d i true)
                                 (catch Throwable _ nil)))
                    (recognitum.core/seal d)))))
    (is (false? (compiles-elsewhere?
                 '(fn [x d] ((:draft-seal (recognitum.core/model-of x)) d)))))))

(deftest the-mask-is-still-reachable-through-the-var-because-privacy-is-advisory
  ;; What privacy is worth is exactly this: the hole is still there for anyone who writes
  ;; `#'`, and there is no way to write it that does not say so. The suite reaches its own
  ;; internals the same way, through `recognitum.internal`.
  (let [d (assoc (rec/draft h/italian-job) ::h/take "garbage")]
    (dotimes [i 64] (internal/record-verdict! d i true))
    (let [forged (rec/seal d)]
      (is (instance? Heist forged))
      (is (= "garbage" (::h/take forged))
          "so the guarantee rests on the surface, which is why the surface is pinned above"))))
