(ns recognitum.qualified-keys-test
  "Field keys are qualified by the namespace that defines the type.

  A `Heist` declaring `name` has the key `:recognitum.heist/name`, and a bare `:name` is a
  key it does not declare -- so two types that happen to share a field name share nothing
  else, and a map lifted out of one cannot be quietly poured into another."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as heist])
  (:import
   (recognitum.heist Heist)))

(def job heist/italian-job)

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; Defined here rather than in recognitum.heist, so `::` in this file names its fields --
;; which is the point: the qualifying namespace is the one the type is written in.
(rec/defrecord Point
  [x y]
  {::x int?
   ::y int?
   ::ordered ("x does not exceed y" [x y] (<= x y))})

(def origin (rec/drafting [p Point] (merge p {::x 0 ::y 0})))

;;; ---------------------------------- what the keys are ----------------------------------

(deftest keys-are-qualified-by-the-defining-namespace
  (is (= [:recognitum.qualified-keys-test/x :recognitum.qualified-keys-test/y] (keys origin))
      "spelled out, so an alias cannot make this test vacuous")
  (is (= [::x ::y] (keys origin)))
  (testing "a type defined elsewhere qualifies into its own namespace, not the reader's"
    (is (= [:recognitum.heist/name :recognitum.heist/crew
            :recognitum.heist/take :recognitum.heist/cuts]
           (keys job)))
    (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts] (keys job)))))

(deftest reading-goes-through-the-qualified-key
  (is (= 3000 (::heist/take job)))
  (is (= 3000 (get job ::heist/take)))
  (is (= ["The Italian Job" 3000] (let [{::heist/keys [name take]} job] [name take])))
  (is (contains? job ::heist/cuts))
  (is (= {::heist/name "The Italian Job"
          ::heist/crew #{:charlie :lyle :left-ear}
          ::heist/take 3000
          ::heist/cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}
         (into {} job)))
  (testing "the field values themselves are untouched -- only the field keys are qualified"
    (is (= #{:charlie :lyle :left-ear} (::heist/crew job)))
    (is (= {:charlie 1000} (select-keys (::heist/cuts job) [:charlie])))))

(deftest a-bare-key-is-not-a-field
  (is (nil? (:take job)))
  (is (= ::absent (get job :take ::absent)))
  (is (not (contains? job :take)))
  (is (nil? (:name (into {} job))) "nor after it has been poured into a plain map")
  (testing "writing one is an unknown field rather than a silent qualification"
    (let [e (err #(assoc job :take 4000))]
      (is (= ::rec/unknown-field (:error e)))
      (is (= :take (:key e)) "reported exactly as it was written")
      (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts] (:declared e))))
    (is (= ::rec/unknown-field (:error (err #(merge job {:take 4000})))))))

(deftest the-same-field-name-in-another-namespace-is-a-different-key
  ;; ::name here is :recognitum.qualified-keys-test/name. Heist declares no such thing.
  (is (= ::rec/unknown-field (:error (err #(assoc job ::name "The Sequel")))))
  (is (= ::name (:key (err #(assoc job ::name "The Sequel")))))
  (testing "and the type's own key still works"
    (is (= "The Sequel" (::heist/name (assoc job ::heist/name "The Sequel"))))))

;;; --------------------------- the model and the invariant index ---------------------------

(deftest the-model-carries-qualified-keys-throughout
  (let [model (rec/model-of job)]
    (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts] (:field-order model)))
    (is (= #{::heist/name ::heist/crew ::heist/take ::heist/cuts} (:field-set model)))
    (is (= #{::heist/name ::heist/crew ::heist/take ::heist/cuts}
           (set (keys (:index model))))
        "the field -> checks index is keyed the same way")))

(deftest a-check-arglist-resolves-to-qualified-keys-and-the-index-still-selects
  (let [e (err #(assoc job ::heist/take 4000))
        f (get (:failures e) ::heist/cuts-sum-to-take)]
    (is (= ::rec/check-failed (:error e)))
    (is (= "the cuts add up to the take" (:doc f)))
    (is (= [::heist/take ::heist/cuts] (keys (:values f)))
        "the arglist named bare symbols; the dependencies are the type's own keys"))
  (testing "a type defined in this namespace, whose fields qualify into it"
    (let [e (err #(assoc origin ::x 5))]
      (is (= [::ordered] (keys (:failures e))))
      (is (= [::x ::y] (keys (:values (get (:failures e) ::ordered))))))
    (is (= 0 (::x (assoc origin ::x 0))))))

(deftest dissoc-names-the-qualified-field
  (let [e (err #(dissoc job ::heist/cuts))]
    (is (= ::rec/cannot-dissoc (:error e)))
    (is (= ::heist/cuts (:key e)))
    (is (= (list 'assoc 'm ::heist/cuts nil) (:instead e))
        "the proposed rewrite is one the caller can paste"))
  (testing "a bare key is undeclared, so dissoc of one is the ordinary no-op"
    (is (identical? job (dissoc job :cuts)))))

;;; ------------------------------------- constructors -------------------------------------

(deftest the-positional-constructor-still-takes-bare-symbols
  (let [h (heist/->Heist "Job" #{:a} 10 {:a 10})]
    (is (instance? Heist h))
    (is (= {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10 ::heist/cuts {:a 10}}
           (into {} h))))
  (is (= '([name crew take cuts]) (:arglists (meta (resolve 'recognitum.heist/->Heist))))
      "fields are symbols in the arglist; qualification is a fact about the keys"))

(deftest the-map-constructor-takes-qualified-keys
  (is (instance? Heist (heist/map->Heist {::heist/name "Job" ::heist/crew #{:a}
                                          ::heist/take 10 ::heist/cuts {:a 10}})))
  (let [e (err #(heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10}))]
    (is (= ::rec/missing-field (:error e)))
    (is (= ::heist/cuts (:key e))))
  (is (contains? (:failures (err #(heist/map->Heist {::heist/name "" ::heist/crew #{:a}
                                                     ::heist/take 10 ::heist/cuts {:a 10}})))
                 ::heist/name)))

(deftest the-map-constructor-qualifies-a-bare-field-name
  ;; This is what lets a printed literal, whose keys the tag already qualifies, read back.
  (is (= job (heist/map->Heist {:name "The Italian Job"
                                :crew #{:charlie :lyle :left-ear}
                                :take 3000
                                :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})))
  (testing "and validates what it qualified, exactly as if it had been written qualified"
    (is (contains? (:failures (err #(heist/map->Heist {:name "" :crew #{:a} :take 1 :cuts {:a 1}})))
                   ::heist/name))
    (is (= [::heist/cuts-sum-to-take]
           (keys (:failures (err #(heist/map->Heist {:name "x" :crew #{:a} :take 9 :cuts {:a 1}}))))))
    (is (= ::rec/missing-field
           (:error (err #(heist/map->Heist {:name "x" :crew #{:a} :take 1}))))))
  (testing "a bare key naming no field is taken as given and reported as written"
    (let [e (err #(heist/map->Heist {:name "x" :crew #{:a} :take 1 :cuts {:a 1}
                                     :getaway-car "mini"}))]
      (is (= ::rec/extraneous-keys (:error e)))
      (is (= [:getaway-car] (:keys e)))))
  (testing "a key qualified into some other namespace is taken as given too"
    (let [e (err #(heist/map->Heist {:name "x" :crew #{:a} :take 1 :cuts {:a 1}
                                     ::name "forged"}))]
      (is (= ::rec/extraneous-keys (:error e)))
      (is (= [::name] (:keys e))))))

(deftest the-explicit-spelling-wins-where-a-map-holds-both
  ;; Degenerate input, but it has to resolve the same way every time rather than by seq order.
  (let [h (heist/map->Heist {:name "bare" ::heist/name "qualified"
                             :crew #{:a} :take 1 :cuts {:a 1}})]
    (is (= "qualified" (::heist/name h)))))

;;; ------------------------------------ drafts ------------------------------------

(deftest a-draft-holds-the-same-qualified-keys
  (let [d (rec/draft job)]
    (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts] (keys d)))
    (is (= 3000 (::heist/take d)))
    (is (nil? (:take d)) "and a bare key reads as absent here too"))
  (testing "a bare key written into a draft is an undeclared key, and seal says so"
    (let [e (err #(rec/drafting [x job] (assoc x :take 4000)))]
      (is (= ::rec/extraneous-keys (:error e)))
      (is (= [:take] (:keys e)))))
  (testing "a multi-step update over qualified keys still seals"
    (is (instance? Heist (rec/drafting [x job]
                           (-> x
                               (assoc ::heist/crew (conj (::heist/crew x) :napster))
                               (assoc ::heist/cuts (assoc (::heist/cuts x) :napster 500))
                               (assoc ::heist/take 3500)))))))

;;; ------------------------------ printing and reading ------------------------------

(deftest the-printed-map-drops-the-namespace-the-tag-carries
  (let [s (pr-str job)]
    (is (.startsWith s "#recognitum.heist/Heist{"))
    (let [m (edn/read-string (subs s (count "#recognitum.heist/Heist")))]
      (is (= #{:name :crew :take :cuts} (set (keys m))))
      (is (every? simple-keyword? (keys m))
          "repeating the namespace on every key would be noise the tag already carries")
      (is (= 3000 (:take m))))))

(deftest the-reader-accepts-either-spelling
  (let [rd #(edn/read-string {:readers (rec/data-readers)} %)]
    (testing "bare, as printed"
      (is (= job (rd (pr-str job)))))
    (testing "fully qualified, taken as given"
      (is (= job (rd (str "#recognitum.heist/Heist"
                          "{:recognitum.heist/name \"The Italian Job\""
                          " :recognitum.heist/crew #{:charlie :lyle :left-ear}"
                          " :recognitum.heist/take 3000"
                          " :recognitum.heist/cuts {:charlie 1000 :lyle 1000 :left-ear 1000}}")))))
    (testing "and reading validates whichever spelling it was handed"
      (is (= [::heist/cuts-sum-to-take]
             (keys (:failures (err #(rd (str "#recognitum.heist/Heist"
                                             "{:recognitum.heist/name \"x\" :recognitum.heist/crew #{:a}"
                                             " :recognitum.heist/take 9 :recognitum.heist/cuts {:a 1}}"))))))))))

(deftest a-value-round-trips-through-print-and-read
  (let [h (rec/drafting [x job] (heist/rename x "The Sequel"))]
    (is (= h (edn/read-string {:readers (rec/data-readers)} (pr-str h))))
    (is (= h (read-string (pr-str h))) "and through data_readers.clj as well")))
