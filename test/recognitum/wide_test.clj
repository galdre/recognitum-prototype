(ns recognitum.wide-test
  "Types with more fields than a function has parameters, against `clojure.core/defrecord`
  over the same fields.

  A Clojure function takes at most twenty parameters, so a positional factory wider than
  that takes the rest of its arguments in one. Each operation below is asked of a generated
  type and of a `defrecord` twin, as `recognitum.record-test` does at two fields."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

;;; ----------------------------------- the types -----------------------------------

;; One field past the limit, with no checks: every difference from the `defrecord` below is
;; a difference of width alone.
(rec/defrecord Wide21
  [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20]
  {})

(clojure.core/defrecord PlainWide21
  [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20])

;; Five fields past the limit, with a shape validation on the last of them: the checks read
;; the fields as locals, so a check on the last field pins that the arguments past the
;; twentieth reach the names they were declared under.
(rec/defrecord Wide25
  [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
   f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24]
  {:checks {::f24 int?}})

(clojure.core/defrecord PlainWide25
  [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
   f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24])

;; Exactly the limit: every field is a parameter and nothing is left over.
(rec/defrecord Narrow20
  [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19]
  {})

;;; ----------------------------------- helpers -----------------------------------

(def ^:private args21 (vec (range 21)))
(def ^:private args25 (vec (range 25)))

(def ^:private ours21   (apply ->Wide21 args21))
(def ^:private theirs21 (apply ->PlainWide21 args21))
(def ^:private ours25   (apply ->Wide25 args25))
(def ^:private theirs25 (apply ->PlainWide25 args25))

(defn- bare
  "`m` with the defining namespace dropped from its keys, which is how the `defrecord`
  spells them."
  [m]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) m))

(defn- qualified
  "The first `n` fields of the types above, spelled as they declare their keys, each mapped
  to its own index."
  [n]
  (into {} (map (fn [i] [(keyword "recognitum.wide-test" (str "f" i)) i])) (range n)))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- arity-failure
  "What the `clojure.lang.ArityException` `f` throws reports, or nil where it throws none.
  The message carries the factory's name, and `Plain` is dropped from it so that a twin's
  message and ours are the same string when they say the same thing."
  [f]
  (try (f) nil
       (catch clojure.lang.ArityException e
         {:actual (.-actual e) :message (.replace (.getMessage e) "Plain" "")})))

;;; ============================ construction and the map surface ============================

(deftest positional-construction-fills-every-field-as-a-defrecords-does
  (is (= (into {} theirs21) (bare (into {} ours21)) (bare (qualified 21))))
  (is (= (into {} theirs25) (bare (into {} ours25)) (bare (qualified 25))))
  (testing "including the fields past the twentieth, which arrive in one argument"
    (is (= 20 (:f20 theirs21) (::f20 ours21)))
    (is (= [20 21 22 23 24]
           [(:f20 theirs25) (:f21 theirs25) (:f22 theirs25) (:f23 theirs25) (:f24 theirs25)]
           [(::f20 ours25) (::f21 ours25) (::f22 ours25) (::f23 ours25) (::f24 ours25)]))))

(deftest the-positional-factory-refuses-a-wrong-argument-count-as-a-defrecords-does
  (doseq [[width ours theirs] [[21 ->Wide21 ->PlainWide21]
                               [25 ->Wide25 ->PlainWide25]]
          given               [(dec width) (inc width)]]
    (testing (str width " fields, called with " given " arguments")
      (let [args (vec (range given))
            a    (arity-failure #(apply ours args))
            b    (arity-failure #(apply theirs args))]
        (is (= given (:actual a)) "the count reported is the count passed")
        (is (= given (:actual b)) "the defrecord reports the count passed")
        (is (= a b) "and the two describe the same mistake the same way")))))

(deftest map-construction-answers-what-a-defrecord-answers
  (is (= (into {} (map->PlainWide21 (bare (qualified 21))))
         (bare (into {} (map->Wide21 (qualified 21))))))
  (is (= (into {} (map->PlainWide25 (bare (qualified 25))))
         (bare (into {} (map->Wide25 (qualified 25))))))
  (testing "and agrees with what the positional factory built"
    (is (= ours21 (map->Wide21 (qualified 21))))
    (is (= ours25 (map->Wide25 (qualified 25))))))

(deftest writing-a-field-past-the-twentieth-answers-what-a-defrecord-answers
  (testing "assoc"
    (is (= (into {} (assoc theirs21 :f20 :written))
           (bare (into {} (assoc ours21 ::f20 :written)))))
    (is (= (into {} (assoc theirs25 :f22 :written))
           (bare (into {} (assoc ours25 ::f22 :written))))))
  (testing "merge"
    (is (= (into {} (merge theirs25 {:f21 :a :f23 :b}))
           (bare (into {} (merge ours25 {::f21 :a ::f23 :b})))))
    (is (= (class ours25) (class (merge ours25 {::f21 :a})))
        "a merge of declared keys is a value of the type, not a map")))

;;; ================================ what only ours does ================================

(deftest a-wide-value-round-trips-through-the-validating-reader
  (let [back (edn/read-string {:readers (rec/data-readers)} (pr-str ours25))]
    (is (= ours25 back))
    (is (rec/sealed? back))
    (is (= 24 (::f24 back)) "the last field survives the trip")))

(deftest drafting-from-a-wide-type-seals-into-a-value
  (let [built (rec/drafting [d Wide25] (reduce-kv assoc d (qualified 25)))]
    (is (= ours25 built))
    (is (rec/sealed? built))))

(deftest a-shape-validation-on-a-field-past-the-twentieth-reads-that-field
  (let [e (err #(apply ->Wide25 (assoc args25 24 :not-a-number)))]
    (is (= ::rec/check-failed (:error e)))
    (is (= [::f24] (keys (:failures e))) "the check that failed is the one declared on f24")
    (is (= {::f24 :not-a-number} (:values (get (:failures e) ::f24)))
        "and it was given that field's value, not another field's"))
  (testing "while a field in the overage that no check reads takes anything"
    (is (rec/of-type? Wide25 (apply ->Wide25 (assoc args25 23 :not-a-number))))))

;;; ============================ exactly the limit ============================

(deftest a-type-of-twenty-fields-takes-them-all-as-parameters
  (let [[arglist :as arglists] (:arglists (meta #'->Narrow20))]
    (is (= 1 (count arglists)))
    (is (= 20 (count arglist)))
    (is (not-any? #{'&} arglist) "nothing is left over at the limit"))
  (let [x (apply ->Narrow20 (range 20))]
    (is (= (bare (qualified 20)) (bare (into {} x))))
    (is (rec/sealed? x)))
  (testing "while a wider one does take a rest argument"
    (is (= '& (nth (first (:arglists (meta #'->Wide21))) 20)))
    (is (= 22 (count (first (:arglists (meta #'->Wide21))))))))
