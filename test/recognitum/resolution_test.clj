(ns recognitum.resolution-test
  "Compile-time resolution of what an arglist names.

  An arglist holds two kinds of entry: a symbol naming a field of this type, or a keyword
  naming an io-dep of this type. Every one of them is resolved while the macro runs, so a
  dependency that names nothing is a compile error naming the offender rather than an index
  entry under a key no write can ever produce.

  The point is that there is nowhere left for a dependency to be wrong. B1 made the arglist
  the only place a dependency is declared; B2 makes every entry in it resolve or refuse to
  compile."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which interns
  ;; vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.resolution-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

(defn- err [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;;; --------------------------- a symbol names a field, or nothing ---------------------------

(deftest a-symbol-that-is-not-a-declared-field-does-not-compile
  (let [e (compile-data '(recognitum.core/defrecord Stray
                           [a b]
                           {:scratch/c ([z] (int? z))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= 'Stray (:record e)))
    (is (= :scratch/c (:check e)))
    (is (= 'z (:dependency e)) "the offender, exactly as it was written")
    (is (= '[a b] (:fields e)) "and what it could have named")))

(deftest the-message-names-the-offender
  (let [m (compile-message '(recognitum.core/defrecord Stray2
                              [a b]
                              {:scratch/c ([z] (int? z))}))]
    (is (re-find #"\bz\b" m))
    (is (re-find #":scratch/c" m))
    (is (re-find #"Stray2" m))))

(deftest every-entry-is-resolved-not-only-the-first
  (testing "a bad entry after a good one"
    (let [e (compile-data '(recognitum.core/defrecord Later
                             [a b]
                             {:scratch/c ([a z] (< a z))}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= 'z (:dependency e)))))
  (testing "the last entry of a longer arglist"
    (let [e (compile-data '(recognitum.core/defrecord Last
                             [a b]
                             {:scratch/c ([a b z] (< a b z))}))]
      (is (= 'z (:dependency e))))))

(deftest resolution-runs-over-every-check-not-only-the-first
  (let [e (compile-data '(recognitum.core/defrecord Third
                           [a b]
                           {:scratch/one ([a] (int? a))
                            :scratch/two ([b] (int? b))
                            :scratch/three ([nope] (int? nope))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= :scratch/three (:check e)))
    (is (= 'nope (:dependency e)))))

(deftest the-docstring-form-is-resolved-the-same-way
  (let [e (compile-data '(recognitum.core/defrecord Documented
                           [a b]
                           {:scratch/c ("a doc" [a nope] (int? a))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= 'nope (:dependency e)))))

(deftest the-shorthands-field-comes-from-the-check-name-and-must-exist
  ;; `::v pred?` is `::v ([v] (pred? v))`, so a shorthand whose key names no field is the
  ;; same error as any other unresolvable symbol -- the field it would check does not exist.
  (let [e (compile-data '(recognitum.core/defrecord Short
                           [a b]
                           {:scratch/nonesuch int?}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= :scratch/nonesuch (:check e)))
    (is (= 'nonesuch (:dependency e))))
  (testing "while a shorthand over a field it does declare compiles"
    (is (nil? (compiles '(recognitum.core/defrecord ShortOk
                           [a b]
                           {:scratch/a int? :scratch/b int?}))))))

;;; ------------------------- a keyword names an io-dep, or nothing -------------------------

(deftest a-keyword-in-an-arglist-must-name-a-declared-io-dep
  ;; A keyword in an arglist always means an io-dep. None can be declared yet, so every
  ;; keyword is unresolvable -- but it is rejected as an unknown io-dep, not as a stray
  ;; symbol, because which kind of thing it is is visible where it is written.
  (let [e (compile-data '(recognitum.core/defrecord Fetching
                           [a]
                           {:scratch/c ([a :scratch/roster] (= a roster))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= :scratch/roster (:dependency e)))
    (is (= #{} (:io-deps e)) "the type declares none"))
  (testing "and the message says io-dep rather than field"
    (is (re-find #"io-dep"
                 (compile-message '(recognitum.core/defrecord Fetching2
                                     [a]
                                     {:scratch/c ([:scratch/roster] (some? roster))}))))))

;;; -------------------- an entry is one of those two things and nothing else --------------------

(deftest an-arglist-entry-is-a-symbol-or-a-keyword
  (testing "a destructuring map is not a dependency declaration"
    (let [e (compile-data '(recognitum.core/defrecord Destructured
                             [a b]
                             {:scratch/c ([{:keys [a]}] (int? a))}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= '{:keys [a]} (:dependency e)))))
  (testing "nor is a destructuring vector"
    (let [e (compile-data '(recognitum.core/defrecord Vectored
                             [a b]
                             {:scratch/c ([[a b]] (< a b))}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= '[a b] (:dependency e)))))
  (testing "nor anything else that is not a name at all"
    (let [e (compile-data '(recognitum.core/defrecord Stringy
                             [a]
                             {:scratch/c (["a"] true)}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= "a" (:dependency e))))))

(deftest a-rest-arg-is-not-a-dependency
  ;; `&` is a symbol and names no field, so it is refused like any other unresolvable symbol.
  ;; A variadic check would be a check whose dependency set is not known at macro time, which
  ;; is the one thing the arglist rule exists to rule out.
  (let [e (compile-data '(recognitum.core/defrecord Variadic
                           [a b]
                           {:scratch/c ([a & more] (int? a))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= '& (:dependency e)))))

(deftest an-ignored-parameter-is-not-a-dependency-either
  (let [e (compile-data '(recognitum.core/defrecord Ignoring
                           [a b]
                           {:scratch/c ([a _] (int? a))}))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= '_ (:dependency e)))))

;;; --------------------------- a rejected form defines nothing ---------------------------

(deftest a-rejected-form-interns-nothing
  ;; The resolution happens while the macro runs, so a form that fails it never expands --
  ;; there is no half-built type left behind for a later form to pick up.
  (is (some? (compiles '(recognitum.core/defrecord Rejected
                          [a]
                          {:scratch/c ([nope] (int? nope))}))))
  (is (nil? (ns-resolve @scratch '->Rejected)))
  (is (nil? (ns-resolve @scratch 'map->Rejected)))
  (is (nil? (ns-resolve @scratch 'Rejected-model))))

;;; ------------------------------ what still compiles ------------------------------

(rec/defrecord Resolved
  [a b]
  {::a int?
   ::b int?
   ::ordered ("a is below b" [b a] (< a b))})

(deftest an-arglist-that-resolves-still-says-what-it-said
  (let [v (->Resolved 1 2)
        model (rec/model-of v)]
    (is (rec/sealed? v))
    (is (= [[::a] [::b] [::b ::a]] (mapv :deps (:checks model)))
        "resolution rewrites a field symbol into the type's key, in arglist order")
    (is (= [0 2] (get (:index model) ::a)))
    (is (= [1 2] (get (:index model) ::b)))
    (is (= ::rec/check-failed (:error (err #(->Resolved 5 2)))))))

;;; --------------------- the rules, where the macro cannot reach them yet ---------------------

;; `:io-deps` arrives in E1. The resolution itself is written once and used by checks and
;; io-deps alike, so the rules governing io-dep names are exercised here directly against it
;; rather than waiting for a section that can declare one.

(deftest resolve-deps-turns-an-arglist-into-dependency-keys
  (is (= [:t/a :t/roster :t/b]
         (rec/resolve-deps {:record 'T :what :check :name :t/c
                            :fields '{a :t/a b :t/b} :io-deps #{:t/roster}}
                           '[a :t/roster b]))
      "a symbol becomes the field's key; an io-dep name is already one")
  (is (= [] (rec/resolve-deps {:record 'T :what :check :name :t/c
                               :fields '{a :t/a} :io-deps #{}}
                              []))
      "naming nothing is a legal thing to name"))

(deftest resolve-deps-refuses-a-keyword-that-is-not-a-declared-io-dep
  (let [e (err #(rec/resolve-deps {:record 'T :what :check :name :t/c
                                   :fields '{a :t/a} :io-deps #{:t/roster}}
                                  '[:t/vetted]))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= :t/vetted (:dependency e)))
    (is (= #{:t/roster} (:io-deps e)) "and what it could have named")))

(deftest resolve-deps-reports-an-io-dep-under-its-own-kind
  (let [e (err #(rec/resolve-deps {:record 'T :what :io-dep :name :t/vetted
                                   :fields '{a :t/a} :io-deps #{:t/roster}}
                                  '[nope]))]
    (is (= ::rec/unknown-dependency (:error e)))
    (is (= :t/vetted (:io-dep e)) "an io-dep's own arglist is resolved the same way")
    (is (nil? (:check e)))))

(deftest a-name-cannot-be-both-a-check-and-an-io-dep
  (let [e (err #(rec/reject-duplicate-names! 'T [:t/a :t/roster :t/b] #{:t/roster}))]
    (is (= ::rec/duplicate-name (:error e)))
    (is (= 'T (:record e)))
    (is (= :t/roster (:name e)))
    (is (re-find #":t/roster" (try (rec/reject-duplicate-names! 'T [:t/roster] #{:t/roster})
                                   (catch clojure.lang.ExceptionInfo ex (ex-message ex))))))
  (testing "while distinct names are fine"
    (is (nil? (rec/reject-duplicate-names! 'T [:t/a :t/b] #{:t/roster})))
    (is (nil? (rec/reject-duplicate-names! 'T [] #{})))))
