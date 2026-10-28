(ns recognitum.record-test
  "Sealed values as maps: closed types, required fields, equality, meta, redefinition, and the
  paths that cannot skip the checks."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.proof :as proof]
   [recognitum.proof.key-class :as key-class]))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(rec/defproven Pair [a ^:optional b ^:optional c]
  rec/checks
  (a [] (int? a))
  (b [] (int? b))
  (c [] (int? c))
  (b-not-below-a [a b] (rec/when-extant b (>= b a)))
  (c-needs-b [] (or (not (rec/extant? c)) (rec/extant? b))))

(def v (map->Pair {::a 0}))

(deftest a-value-reads-and-compares-as-a-map
  (is (= {::a 0} v))
  (is (= v {::a 0}))
  (is (= (hash {::a 0}) (hash v)))
  (is (= #{::a} (set (keys v))))
  (is (nil? (::b v))))

(deftest writes-are-checked-as-one-write-in-any-order
  (is (instance? Pair (merge v {::c 0 ::b 1})))
  (is (instance? Pair (into v {::c 0 ::b 1})))
  (is (instance? Pair (conj v [::b 1])))
  (is (= :recognitum/check-failed (:error (err #(assoc v ::c 1)))) "c needs b")
  (is (= :recognitum/check-failed (:error (err #(dissoc v ::a)))) "a is required")
  (is (identical? v (assoc v ::a 0)) "a write that changes nothing returns the value")
  (is (identical? v (dissoc v ::z))))

(deftest a-type-is-closed
  (is (= :recognitum/undeclared-keys (:error (err #(map->Pair {::a 0 ::z 1})))))
  (is (= :recognitum/undeclared-keys (:error (err #(assoc v ::z 1))))))

(rec/defproven Unread [a b]
  rec/checks
  (a [] (int? a)))

(deftest a-required-field-no-check-reads-must-still-be-present
  (is (instance? Unread (map->Unread {::a 1 ::b nil})))
  (is (= {:error :recognitum/missing-fields :keys #{::b}}
         (select-keys (err #(map->Unread {::a 1})) [:error :keys]))))

(deftest meta-travels-with-the-value
  (let [m (with-meta v {:source :test})]
    (is (instance? Pair m))
    (is (= {:source :test} (meta m)))
    (is (= {:source :test} (meta (assoc m ::b 3))))))

(deftest the-installed-write-needs-a-value-to-start-from
  (let [gate (proof/installed key-class/strategy Pair)]
    (is (thrown? IllegalArgumentException (gate nil {::a 1})))
    (is (thrown? IllegalArgumentException (gate {::a 0 ::b 5} {::a 0 ::b 5 ::c 9})))
    (is (instance? Pair (gate v {::a 0 ::b 1})) "from a value, it is the checked write")))

(deftest redefining-a-type-unproves-its-old-values
  (let [ns' (create-ns 'recognitum.record-test.redefined)
        define #(binding [*ns* ns'] (refer-clojure) (eval '(recognitum.core/defproven R [a] recognitum.core/checks (a [] (int? a)))))
        _ (define)
        old ((ns-resolve ns' 'map->R) {:recognitum.record-test.redefined/a 1})]
    (is (rec/sealed? old))
    (define)
    (is (not (rec/sealed? old)))
    (is (not (rec/proves? old rec/every)))
    (is (rec/sealed? ((ns-resolve ns' 'map->R) {:recognitum.record-test.redefined/a 1})))))

(defn- redefinable
  "A namespace of its own and a fn that evaluates `(defproven R [a] ...)` in it, returning R."
  [ns-sym]
  (let [ns' (create-ns ns-sym)]
    (binding [*ns* ns'] (refer-clojure))
    [ns' #(binding [*ns* ns'] (eval '(recognitum.core/defproven R [a] recognitum.core/checks (a [] (int? a)))))]))

(deftest a-displaced-value-reseals-as-the-current-definition-and-prints
  (let [[ns' define] (redefinable 'recognitum.record-test.displaced)
        k :recognitum.record-test.displaced/a
        old-class (define)
        old (with-meta ((ns-resolve ns' 'map->R) {k 1}) {:from :old})
        new-class (define)]
    (is (not (rec/sealed? old)))
    (is (not (rec/proves? old rec/every)))
    (testing "draft then seal gives a value of the current definition, with the old value's meta"
      (let [d (rec/draft old)]
        (is (= #{} (rec/proven-checks d)) "the current definition's checks have not run on it")
        (let [v' (rec/seal d)]
          (is (instance? new-class v'))
          (is (rec/sealed? v'))
          (is (= {k 1} v'))
          (is (= {:from :old} (meta v'))))))
    (testing "it prints as its tagged literal, and the printer's table keeps no entry for any type's class"
      (is (= "#recognitum.record-test.displaced/R #:recognitum.record-test.displaced{:a 1}" (str old) (pr-str old)))
      (is (nil? (get (methods print-method) old-class)))
      (is (nil? (get (methods print-method) new-class))))
    (testing "the readers name the current definition"
      (is (instance? new-class ((get (rec/data-readers) 'recognitum.record-test.displaced/R) {k 2}))))))

(deftest readers-held-across-a-redefinition-refuse-and-say-to-read-again
  (let [[_ define] (redefinable 'recognitum.record-test.held-readers)
        k :recognitum.record-test.held-readers/a
        tname 'recognitum.record-test.held-readers/R
        _ (define)
        held (get (rec/data-readers) tname)
        _ (define)]
    (is (= [(str tname " was redefined: this constructor is of an earlier definition and makes nothing."
                 " Read again with the current readers: (rec/data-readers)")
            {:error :recognitum/displaced-constructor :type tname}]
           (try (held {k 1}) nil
                (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)]))))
    (is (rec/sealed? ((get (rec/data-readers) tname) {k 1})) "read again, the readers seal")))

(deftest a-draft-of-a-displaced-class-refuses-to-seal
  (let [[ns' define] (redefinable 'recognitum.record-test.held-class)
        k :recognitum.record-test.held-class/a
        tname 'recognitum.record-test.held-class/R
        old-class (define)
        opened-before (into (rec/draft old-class) {k 1})
        new-class (define)
        refusal (fn [d] (try (rec/seal d) nil
                             (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))
        want [(str tname " was redefined: this draft is of an earlier definition and seals nothing."
                   " Make a value of the current definition with recognitum.record-test.held-class/map->R,"
                   " or seal a draft of the class recognitum.record-test.held-class now names R:"
                   " (rec/seal (into (rec/draft R) m))")
              {:error :recognitum/displaced-constructor :type tname}]]
    (is (= want (refusal (into (rec/draft old-class) {k 1}))) "a draft opened from the held class")
    (is (= want (refusal opened-before)) "a draft opened before the redefinition")
    (is (= want (refusal (into (rec/draft old-class) {k :not-an-int}))) "refused before any check runs")
    (testing "the remedies seal"
      (is (rec/sealed? ((ns-resolve ns' 'map->R) {k 1})))
      (is (instance? new-class (rec/seal (into (rec/draft new-class) {k 1})))))))

(deftest a-value-prints-as-its-tagged-literal
  (is (= "#recognitum.record-test/Pair #:recognitum.record-test{:a 0}" (pr-str v))))

(rec/defproven Wide [f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22]
  rec/checks
  (f22 [] (int? f22)))

(deftest the-positional-constructor-takes-more-than-twenty-fields
  (is (instance? Wide (apply ->Wide (range 22))))
  (is (thrown? clojure.lang.ArityException (apply ->Wide (range 21)))))

(deftest naming-a-shape-check-in-a-wait-changes-nothing
  (let [ns' (create-ns 'recognitum.record-test.waits)
        define (fn [waits]
                 (binding [*ns* ns'] (refer-clojure)
                   (eval `(recognitum.core/defproven ~'W [~'a]
                            recognitum.core/checks
                            (~'a [] (~'int? ~'a))
                            (~'positive ~waits (~'pos? ~'a))))))
        failed (fn [W] (try ((ns-resolve ns' 'map->W) {:recognitum.record-test.waits/a "x"}) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (define [])
    (let [unnamed (failed nil)]
      (define '[a])
      (is (= unnamed (failed nil)))
      (is (= #{:recognitum.record-test.waits/positive} (:blocked unnamed))))))

(defn- refusal [form]
  (try (binding [*ns* (the-ns 'recognitum.record-test)] (eval form))
       nil
       (catch clojure.lang.Compiler$CompilerException e (ex-data (ex-cause e)))))

(deftest shape-checks-that-read-each-other-form-a-cycle
  (let [e (refusal '(recognitum.core/defproven Loop [a b]
                      recognitum.core/checks
                      (a [] (int? b))
                      (b [] (int? a))))]
    (is (= :recognitum/refused (:error e)))
    (is (= (str "the waits form a cycle: :recognitum.record-test/a -> :recognitum.record-test/b"
                " -> :recognitum.record-test/a (a check waits on the shape checks of the fields it reads)")
           (:message e))))
  (testing "a cycle of named waits alone says nothing of reads"
    (is (= "the waits form a cycle: :recognitum.record-test/p -> :recognitum.record-test/q -> :recognitum.record-test/p"
           (:message (refusal '(recognitum.core/defproven Loop2 [a]
                                 recognitum.core/checks
                                 (p [q] true)
                                 (q [p] true))))))))
