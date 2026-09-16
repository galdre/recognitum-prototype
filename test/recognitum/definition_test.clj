(ns recognitum.definition-test
  "The definition map: `defrecord`'s third position.

  A type used to be written as a name, a vector of fields and a bare map of checks. It is
  now a name, a vector of fields and a *definition* map holding named sections --
  `:options`, `:checks`, `:io-deps`, `:certificates` -- every one of them optional. The
  checks move under `:checks` and nothing else about them changes.

  A section is a declaration, so a section the macro does not recognise is a declaration
  that would mean nothing: a mistyped `:cheks` would define a type with no checks at all,
  which is precisely the failure this library exists to make impossible. Every key of the
  definition map, and of `:options` inside it, is therefore resolved while the macro runs,
  and one that names no section is a compile error naming the offender."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Compile-time rejections are tested by evaluating a whole `defrecord` form, which interns
  ;; vars and defines classes. Do that somewhere other than this namespace.
  (delay (let [n (create-ns 'recognitum.definition-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))
(defn- compile-message [form] (some-> (compiles form) root-cause ex-message))

;;; ------------------------------- checks live under :checks -------------------------------

(rec/defrecord Wrapped
  [a b]
  {:checks
   {::a int?
    ::b-string ([b] (string? b))
    ::a-below-count ("a is below the length of b"
                     [a b] (and (int? a) (string? b) (< a (count b))))}})

(deftest the-checks-section-holds-the-checks
  (is (rec/sealed? (->Wrapped 1 "xx")))
  (testing "all three check forms still mean what they meant unwrapped"
    (let [e (err #(->Wrapped "x" "yy"))]
      (is (= ::rec/check-failed (:error e)))
      (is (= {:doc nil :values {::a "x"} :returned false} (get (:failures e) ::a))))
    (let [e (err #(->Wrapped 1 :nope))]
      (is (= {:doc nil :values {::b :nope} :returned false} (get (:failures e) ::b-string))))
    (let [e (err #(->Wrapped 9 "yy"))]
      (is (= {:doc "a is below the length of b" :values {::a 9 ::b "yy"} :returned false}
             (get (:failures e) ::a-below-count)))))
  (testing "and the field -> checks index is built from them as before"
    (let [model (rec/model-of Wrapped)]
      (is (= [::a ::b-string ::a-below-count] (mapv :name (:checks model))))
      (is (= [::a ::a-below-count]
             (mapv (comp :name (:checks model)) (get (:index model) ::a))))
      (is (= [::b-string ::a-below-count]
             (mapv (comp :name (:checks model)) (get (:index model) ::b)))))))

(deftest a-write-still-runs-the-checks-naming-the-field
  (let [w (->Wrapped 1 "xx")]
    (is (= 0 (::a (assoc w ::a 0))))
    (is (= ::rec/check-failed (:error (err #(assoc w ::a "x")))))
    (is (= ::rec/check-failed (:error (err #(assoc w ::b "")))))
    (is (= ::rec/unknown-field (:error (err #(assoc w :a 1)))))))

;;; ----------------------------- every section is optional -----------------------------

(rec/defrecord Bare [a b] {})

(rec/defrecord Empties
  [a]
  {:options {} :checks {} :io-deps {} :certificates {}})

(deftest an-empty-definition-defines-a-type-with-no-checks
  (let [model (rec/model-of Bare)]
    (is (= [] (vec (:checks model))))
    (is (= {} (:index model))))
  (testing "which is still a validated type in every other respect"
    (is (rec/sealed? (->Bare 1 2)))
    (is (rec/sealed? (map->Bare {::a 1 ::b 2})))
    (is (= 1 (::a (->Bare 1 2))))
    (is (= ::rec/unknown-field (:error (err #(assoc (->Bare 1 2) :a 1)))))
    (is (= ::rec/missing-field (:error (err #(map->Bare {::a 1})))))
    (is (rec/sealed? (rec/drafting [d Bare] (assoc d ::a 1) (-> d (assoc ::a 1) (assoc ::b 2)))))))

(deftest a-section-may-be-present-and-empty
  (is (rec/sealed? (->Empties 1)))
  (is (= [] (vec (:checks (rec/model-of Empties)))))
  (testing "and any subset of the sections may simply be absent"
    (doseq [form ['(recognitum.core/defrecord S1 [a] {:checks {:scratch/a int?}})
                  '(recognitum.core/defrecord S2 [a] {:options {}})
                  '(recognitum.core/defrecord S3 [a] {:io-deps {}})
                  '(recognitum.core/defrecord S4 [a] {:certificates {}})
                  '(recognitum.core/defrecord S5 [a] {})]]
      (is (nil? (compiles form)) (str form " should compile")))))

;;; ---------------------------------- :options ----------------------------------

(rec/defrecord Optioned
  [a]
  {:options {:io-limited true}
   :checks  {::a int?}})

(deftest options-are-carried-on-the-model
  (is (= {:io-limited true} (:options (rec/model-of Optioned))))
  (testing "and an absent section is an empty one, so nothing has to ask which"
    (is (= {} (:options (rec/model-of Bare))))
    (is (= {} (:options (rec/model-of Wrapped))))
    (is (= {} (:options (rec/model-of Empties)))))
  (testing "declaring the option changes nothing yet: no check here reaches IO"
    (is (rec/sealed? (assoc (->Optioned 1) ::a 2)))
    (is (= ::rec/check-failed (:error (err #(assoc (->Optioned 1) ::a "x")))))))

(deftest an-unrecognised-option-does-not-compile
  (let [e (compile-data '(recognitum.core/defrecord Optionless
                           [a]
                           {:options {:io-limitd true} :checks {:scratch/a int?}}))]
    (is (= ::rec/unknown-option (:error e)))
    (is (= 'Optionless (:record e)))
    (is (= :io-limitd (:option e)))
    (is (= #{:io-limited} (:options e))))
  (testing "the message names the offender and what could have been written"
    (let [m (compile-message '(recognitum.core/defrecord Optionless2
                                [a] {:options {:io-limitd true}}))]
      (is (re-find #":io-limitd" m))
      (is (re-find #":io-limited" m)))))

;;; ------------------------------- unrecognised sections -------------------------------

(deftest a-section-the-macro-does-not-know-does-not-compile
  (let [e (compile-data '(recognitum.core/defrecord Mistyped
                           [a]
                           {:cheks {:scratch/a int?}}))]
    (is (= ::rec/unknown-section (:error e)))
    (is (= 'Mistyped (:record e)))
    (is (= :cheks (:section e)))
    (is (= [:options :checks :io-deps :certificates] (vec (:sections e))))
    (testing "silently defining a type with no checks is the bug this refuses"
      (is (nil? (ns-resolve @scratch '->Mistyped)))))
  (testing "the message names the offender and the sections there are"
    (let [m (compile-message '(recognitum.core/defrecord Mistyped2 [a] {:cheks {}}))]
      (is (re-find #":cheks" m))
      (is (re-find #":checks" m)))))

(deftest the-old-spelling-is-refused-rather-than-half-understood
  ;; A bare map of checks is now a definition map whose sections are check names. Say so.
  (let [e (compile-data '(recognitum.core/defrecord Old
                           [a b]
                           {:scratch/a int?
                            :scratch/b ([b] (string? b))}))]
    (is (= ::rec/unknown-section (:error e)))
    (is (= :scratch/a (:section e))))
  (testing "and the message points at the section the checks belong in"
    (is (re-find #":checks"
                 (compile-message '(recognitum.core/defrecord Old2
                                     [a] {:scratch/a int?}))))))

;;; ---------------------------------- malformed shapes ----------------------------------

(deftest the-third-position-must-be-a-map
  (doseq [bad ['(recognitum.core/defrecord NotAMap [a] :checks)
               '(recognitum.core/defrecord NotAMap2 [a] [{:doc "an old invariant"
                                                          :keys #{:scratch/a}
                                                          :pred int?}])
               '(recognitum.core/defrecord NotAMap3 [a] nil)]]
    (let [e (compile-data bad)]
      (is (= ::rec/malformed-definition (:error e)) (str bad))))
  (testing "the old invariants vector is reported as what it is, not picked apart"
    (let [e (compile-data '(recognitum.core/defrecord NotAMap4 [a] [:x]))]
      (is (= ::rec/malformed-definition (:error e)))
      (is (= 'NotAMap4 (:record e)))
      (is (= [:x] (:definition e))))))

(deftest a-section-must-itself-be-a-map
  (doseq [[form section] [['(recognitum.core/defrecord Vectored
                              [a] {:checks [:scratch/a int?]}) :checks]
                          ['(recognitum.core/defrecord Keyworded
                              [a] {:options :io-limited}) :options]
                          ['(recognitum.core/defrecord Nilled
                              [a] {:checks nil}) :checks]
                          ['(recognitum.core/defrecord Setted
                              [a] {:certificates #{:scratch/shape}}) :certificates]]]
    (let [e (compile-data form)]
      (is (= ::rec/malformed-section (:error e)) (str form))
      (is (= section (:section e)) (str form)))))

;;; --------------------------- the sections with nothing behind them ---------------------------

(deftest a-section-with-no-implementation-behind-it-is-accepted-only-empty
  ;; `:io-deps` is part of the form before it is part of the library. Accepting a
  ;; declaration and then ignoring it would be the silent mis-declaration this whole design
  ;; is arranged to prevent, so a populated one is refused until it means something.
  ;; (`:certificates` was in the same position until D3 gave it meaning; it now compiles,
  ;; and `recognitum.certificate-test` owns what it means.)
  (let [e (compile-data '(recognitum.core/defrecord Fetching
                           [a]
                           {:io-deps {:scratch/roster ([a] [a])}}))]
    (is (= ::rec/unsupported-section (:error e)))
    (is (= 'Fetching (:record e)))
    (is (= :io-deps (:section e))))
  (testing "while a populated :certificates now defines something"
    (is (nil? (compiles '(recognitum.core/defrecord Certified
                           [a]
                           {:checks {:scratch/a int?}
                            :certificates {:scratch/shape #{:scratch/a}}})))))
  (testing "a keyword in an arglist is still an io-dep nobody declared"
    (let [e (compile-data '(recognitum.core/defrecord Fetching2
                             [a]
                             {:io-deps {}
                              :checks {:scratch/c ([a :scratch/roster] (int? a))}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= :scratch/roster (:dependency e))))))

;;; ------------------------------ resolution through the wrapper ------------------------------

(deftest checks-are-still-resolved-against-the-fields
  (testing "a symbol naming no field"
    (let [e (compile-data '(recognitum.core/defrecord Stray
                             [a]
                             {:checks {:scratch/c ([a z] (int? a))}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= 'z (:dependency e)))))
  (testing "a shorthand whose key names no field"
    (let [e (compile-data '(recognitum.core/defrecord Shorty
                             [a]
                             {:checks {:scratch/nonesuch int?}}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= 'nonesuch (:dependency e)))))
  (testing "more than one arity"
    (let [e (compile-data '(recognitum.core/defrecord Multi
                             [a]
                             {:checks {:scratch/c (([a] (int? a)) ([a b] (int? a)))}}))]
      (is (= ::rec/malformed-check (:error e))))))

;;; -------------------------------- order among the sections --------------------------------

(rec/defrecord Reordered
  [a b]
  {:certificates {}
   :checks       {::r-b ([b] (string? b))
                  ::r-a ([a] (int? a))}
   :io-deps      {}
   :options      {:io-limited false}})

(deftest the-order-the-sections-are-written-in-does-not-matter
  (is (rec/sealed? (->Reordered 1 "x")))
  (is (= {:io-limited false} (:options (rec/model-of Reordered))))
  (testing "while the order within :checks is still the order failures report in"
    (let [e (err #(->Reordered "x" 1))]
      (is (= [::r-b ::r-a] (keys (:failures e))))
      (is (= 2 (count (:failures e)))))))

;;; ----------------------------------- parse-definition -----------------------------------

(deftest parse-definition-fills-in-the-sections-nobody-wrote
  (is (= {:options {} :checks {} :io-deps {} :certificates {}}
         (rec/parse-definition 'T {})))
  (is (= {:options {:io-limited true} :checks {:t/a 'int?} :io-deps {} :certificates {}}
         (rec/parse-definition 'T {:checks {:t/a 'int?} :options {:io-limited true}})))
  (testing "and refuses everything the macro refuses, since it is what the macro calls"
    (is (= ::rec/malformed-definition (:error (err #(rec/parse-definition 'T "no")))))
    (is (= ::rec/unknown-section (:error (err #(rec/parse-definition 'T {:cheks {}})))))
    (is (= ::rec/malformed-section (:error (err #(rec/parse-definition 'T {:checks [1]})))))
    (is (= ::rec/unknown-option (:error (err #(rec/parse-definition 'T {:options {:nope 1}})))))
    (is (= ::rec/unsupported-section
           (:error (err #(rec/parse-definition 'T {:io-deps {:t/r 'identity}})))))))

(deftest the-sections-are-named-once-and-in-spec-order
  (is (= [:options :checks :io-deps :certificates] (vec rec/definition-sections)))
  (is (= #{:io-limited} (set rec/known-options))))
