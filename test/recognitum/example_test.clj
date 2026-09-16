(ns recognitum.example-test
  "The example model, the demo and the bench: the three files a reader meets before the
  docstrings, and the three the library has been quietly outgrowing.

  Everything the library can do is covered somewhere under `test/`; what is covered here is
  that the worked example *shows* it. `recognitum.heist` is the type the README prints and
  the demo drives, so an io-dep that appears in no example is a facility a reader has no
  path to. These tests pin the example model's own behaviour -- the fetch, the gate that
  stands in front of it, the deduplication, the check judging what came back, the
  certificates, the draft-only writes and the protocol implementations -- rather than
  re-deriving the library's rules, which the other suites do against types of their own.

  The bench half asserts the one thing that makes the comparison mean anything: both sides
  are running the same checks, with the same verdicts, on the same values."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.bench :as bench]
   [recognitum.core :as rec]
   [recognitum.internal :as internal]
   [recognitum.demo :as demo]
   [recognitum.heist :as heist]
   [recognitum.roster :as roster])
  (:import
   (clojure.lang ExceptionInfo)
   (recognitum.bench BenchHeist)
   (recognitum.heist Heist)))

(use-fixtures :each (fn [f] (roster/reset-calls!) (f)))

(def job heist/italian-job)

(defn- err [f]
  (try (f) nil (catch ExceptionInfo e (ex-data e))))

(defn- model [] (internal/model-of Heist))

(defn- check-names [] (mapv :name (:checks (model))))

;;; ------------------------------ the model declares its IO ------------------------------

(deftest the-example-model-has-io-deps-in-it
  ;; The whole IO half of the library was invisible from the example: a reader looking for
  ;; what a fetch costs, or when it happens, had only the docstrings to go on.
  (let [m (model)]
    (is (seq (:io-deps m)) "the example model declares at least one io-dep")
    (is (= [::heist/roster ::heist/vetted] (mapv :name (:io-deps m)))
        "one fetch, and one that names it -- so the graph is more than a single edge")
    (testing "both spellings of a fetch appear, since the reader is what tells them apart"
      (is (true?  (:batched (first (filter #(= ::heist/roster (:name %)) (:io-deps m))))))
      (is (false? (boolean (:batched (first (filter #(= ::heist/vetted (:name %))
                                                    (:io-deps m))))))))
    (testing "and a check is bound to them"
      (is (contains? (set (check-names)) ::heist/crew-is-vouched-for)))))

(deftest the-model-still-declares-the-pure-checks-it-always-had
  (is (= [::heist/name ::heist/crew ::heist/take ::heist/cuts
          ::heist/cuts-sum-to-take ::heist/only-crew-get-cuts ::heist/crew-is-vouched-for]
         (check-names))
      "the IO-bound check is added to the pure ones, not in place of them"))

;;; ----------------------------------- a fetch resolving -----------------------------------

(deftest sealing-a-write-the-fetch-covers-reaches-the-source
  (let [h (rec/drafting [x job] (heist/add-member x :handsome-rob 500))]
    (is (instance? Heist h))
    (is (pos? (roster/call-count)) "the fake source was actually reached")))

(deftest a-fetch-is-made-once-per-validation-call-however-many-things-name-it
  ;; ::vetted names ::roster, so a naive implementation fetches the roster twice: once for
  ;; the check and once to fetch what the check's other dependency is derived from.
  (rec/drafting [x job] (heist/add-member x :handsome-rob 500))
  (is (= 1 (roster/call-count :fetch-rosters)))
  (is (= 1 (roster/call-count :fetch-vetted)))
  (testing "and the next call fetches again, because nothing is cached across calls"
    (rec/drafting [x job] (heist/add-member x :handsome-rob 500))
    (is (= 2 (roster/call-count :fetch-rosters))))
  (testing "while a write nothing IO reads pays for none of it"
    (roster/reset-calls!)
    (is (instance? Heist (rec/drafting [x job] (heist/rename x "The Sequel"))))
    (is (= 0 (roster/call-count))
        "the write cleared the verdict of one pure check and nothing else")))

(deftest the-fetched-value-is-what-the-check-judges
  (testing "a crew the house has vetted passes"
    (is (instance? Heist (rec/drafting [x job] (heist/add-member x :handsome-rob 500)))))
  (testing "one it has on file and has not vetted fails, and the check says so by name"
    (let [e (err #(rec/drafting [x job] (heist/add-member x :mole 500)))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::heist/crew-is-vouched-for] (keys (:failures e))))))
  (testing "someone the house has never heard of is not unvetted, merely unknown"
    (is (instance? Heist (rec/drafting [x job] (heist/add-member x :nobody-in-particular 500))))))

;;; ------------------------------------- the gate -------------------------------------

(deftest a-malformed-crew-never-reaches-the-fetch
  ;; `roster/fetch-rosters` throws on anything that is not a crew, so a gate that leaked
  ;; would show up here as that throw rather than as a verdict.
  (let [e (err #(rec/drafting [x job] (assoc x ::heist/crew "not a crew")))]
    (is (= ::rec/check-failed (:error e)))
    (is (contains? (:failures e) ::heist/crew))
    (is (not (contains? (:failures e) ::heist/crew-is-vouched-for))
        "the IO-bound check has no verdict, because it never ran"))
  (is (= 0 (roster/call-count)) "and nothing was queried"))

(deftest a-check-runs-early-and-waits-only-on-what-its-fetch-names
  ;; The gate is narrow: ::roster names `crew`, so the fetch waits on the checks covering
  ;; `crew` -- both of them, ::only-crew-get-cuts included, which is why `cuts` is written
  ;; here too -- and on nothing else. `name` and `take` have never been written at all,
  ;; and nothing waits for them.
  (let [d (-> (rec/draft Heist)
              (assoc ::heist/crew #{:charlie :lyle})
              (assoc ::heist/cuts {:charlie 1}))]
    (is (true? (rec/check d ::heist/crew-is-vouched-for)))
    (is (= 1 (roster/call-count :fetch-rosters)))
    (is (contains? (rec/passing d) ::heist/crew-is-vouched-for))
    (is (contains? (rec/passing d) ::heist/only-crew-get-cuts)
        "and what gated it is established too, since it was actually run"))
  (testing "and a crew the checks over it reject never reaches the fetch"
    (roster/reset-calls!)
    (let [d (-> (rec/draft Heist)
                (assoc ::heist/crew "not a crew")
                (assoc ::heist/cuts {:charlie 1}))
          e (err #(rec/check d ::heist/crew-is-vouched-for))]
      (is (= ::rec/check-failed (:error e)))
      (is (= [::heist/crew] (keys (:failures e)))
          "the check asked about never ran, so the gate's failure is the answer --
           and ::only-crew-get-cuts, which is the rest of that gate, waits on ::crew too")
      (is (= 0 (roster/call-count))))))

;;; ------------------------------ deduplication across values ------------------------------

(defn- crew-draft [nm crew cuts]
  (-> (rec/draft Heist)
      (assoc ::heist/name nm)
      (assoc ::heist/crew crew)
      (assoc ::heist/take (reduce + 0 (vals cuts)))
      (assoc ::heist/cuts cuts)))

(deftest a-group-is-fetched-in-one-call-and-repeated-arguments-once
  (let [ds [(crew-draft "one"   #{:charlie :lyle} {:charlie 1 :lyle 1})
            (crew-draft "two"   #{:charlie :lyle} {:charlie 2 :lyle 2})
            (crew-draft "three" #{:handsome-rob}  {:handsome-rob 3})]
        rs (rec/check-all ds ::heist/crew-is-vouched-for)]
    (is (= [true true true] rs))
    (is (= 1 (roster/call-count :fetch-rosters))
        "the batched form sees the whole group in one call")
    (let [[[_ rows]] (filter (fn [[f]] (= :fetch-rosters f)) @roster/calls)]
      (is (= 2 (count rows)) "and two distinct crews are two tuples, not three")))
  (testing "and each draft keeps its own verdict, so sealing afterwards is free"
    (roster/reset-calls!)
    (let [ds [(crew-draft "one" #{:charlie :lyle} {:charlie 1 :lyle 1})]]
      (rec/certify-all ds rec/every-check)
      (is (every? #(instance? Heist %) (map rec/seal ds)))
      (is (= 1 (roster/call-count :fetch-rosters))
          "one fetch for the group call, and none at the seal"))))

;;; ------------------------------------ draft-only writes ------------------------------------

(deftest the-example-model-is-io-limited
  (is (true? (get-in (model) [:options :io-limited])))
  (is (= #{::heist/crew} (set (:io-limited-keys (model))))
      "exactly the fields an IO-bound check reads, which here is what the fetch names"))

(deftest a-sealed-write-of-an-io-backed-field-is-refused
  (let [e (err #(assoc job ::heist/crew #{:charlie}))]
    (is (= ::rec/io-limited (:error e)))
    (is (= ::heist/crew (:key e)))
    (is (= [::heist/crew-is-vouched-for] (get (:checks e) ::heist/crew)))
    (is (= 'recognitum.core/drafting (first (:instead e))) "and it names the way to write it"))
  (is (= 0 (roster/call-count)) "nothing is fetched to tell the caller no")
  (testing "the same write inside drafting is ordinary"
    (is (instance? Heist (rec/drafting [x job] (assoc x ::heist/crew #{:charlie :lyle :left-ear}))))))

(deftest every-other-field-is-written-as-before
  (is (= "The Lavender Hill Mob" (::heist/name (assoc job ::heist/name "The Lavender Hill Mob"))))
  (is (= 4000 (::heist/take (merge job {::heist/take 4000
                                        ::heist/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}}))))
  (testing "and they still run their own checks"
    (is (= ::rec/check-failed (:error (err #(assoc job ::heist/name "")))))))

;;; ------------------------------------- certificates -------------------------------------

(deftest the-example-model-names-groups-you-can-run-early
  (is (= [::heist/shape ::heist/payout ::rec/every-check]
         (vec (keys (:certificates (model)))))
      "the built-in one is last, and needs no declaring"))

(deftest a-certificate-is-run-against-an-incomplete-draft
  (let [d (-> (rec/draft Heist)
              (assoc ::heist/name "In Progress")
              (assoc ::heist/crew #{:charlie :lyle})
              (assoc ::heist/take 2)
              (assoc ::heist/cuts {:charlie 1 :lyle 1}))]
    (is (true? (rec/certify d ::heist/shape)))
    (is (contains? (rec/holding d) ::heist/shape))
    (is (= 0 (roster/call-count)) "a purely pure certificate fetches nothing")
    (is (true? (rec/certify d ::heist/payout)))
    (testing "and the IO-bound check is still owed, so the type's own certificate is not held"
      (is (not (contains? (rec/holding d) ::rec/every-check)))
      (is (instance? Heist (rec/seal d)))
      (is (= 1 (roster/call-count :fetch-rosters))
          "the seal runs what the certificates left, and only that"))))

(deftest a-failing-certificate-answers-with-the-checks-that-failed
  (let [d (-> (rec/draft job) (assoc ::heist/take 4000))]
    (is (= [::heist/cuts-sum-to-take] (keys (rec/certify d ::heist/payout))))
    (is (not (contains? (rec/holding d) ::heist/payout)))))

(deftest metadata-answers-the-same-way-from-both-forms
  (is (= (set (check-names)) (::rec/passed (meta job))))
  (is (= #{::heist/shape ::heist/payout ::rec/every-check} (::rec/certificates (meta job))))
  (testing "and a draft of it answers from its own mask"
    (let [d (assoc (rec/draft job) ::heist/take 4000)]
      (is (not (contains? (::rec/passed (meta d)) ::heist/cuts-sum-to-take)))
      (is (contains? (::rec/passed (meta d)) ::heist/name)))))

;;; -------------------------------- protocol implementations --------------------------------

(deftest the-impl-layer-is-a-protocol-installed-on-both-types
  (is (some? (resolve 'recognitum.heist/Job)) "the example model implements a protocol")
  (testing "called on a sealed value, the method seals what it produced"
    (let [h (heist/promote job :lyle 250)]
      (is (instance? Heist h))
      (is (= 3250 (::heist/take h)))
      (is (= 1250 (get (::heist/cuts h) :lyle)))))
  (testing "called on a draft, the body runs as written and hands a draft back"
    (rec/drafting [x job]
      (let [inner (heist/promote x :lyle 250)]
        (is (rec/draft? inner))
        inner)))
  (testing "a method that returns something else passes it through untouched"
    (is (= 3000 (heist/total-payout job)))))

(deftest a-method-passes-through-states-no-sealed-write-could
  ;; This is what the wrapper buys: the body writes three fields one at a time, and every
  ;; intermediate value breaks a check, but the draft absorbs them.
  (let [h (heist/add-member job :handsome-rob 500)]
    (is (instance? Heist h))
    (is (= 3500 (::heist/take h))))
  (testing "while the same first write on a sealed value is refused outright"
    (is (= ::rec/io-limited (:error (err #(assoc job ::heist/crew
                                                 (conj (::heist/crew job) :handsome-rob))))))))

;;; ------------------------------------------ the demo ------------------------------------------

(def ^:private demo-output (delay (with-out-str (demo/run))))

(deftest the-demo-runs-to-the-end
  (is (string? @demo-output))
  (is (str/includes? @demo-output "The Italian Job")))

(deftest the-demo-illustrates-the-io-half
  (let [out @demo-output]
    (doseq [[what token] [["a fetch resolving"        "fetch"]
                          ["the gate"                 "gate"]
                          ["deduplication"            "deduplicat"]
                          ["draft-only writes"        "io-limited"]
                          ["certificates"             "certificate"]
                          ["what a draft has established" "passed"]]]
      (is (str/includes? (str/lower-case out) token)
          (str "the demo says nothing about " what)))))

(deftest the-demo-reaches-the-fake-source
  ;; The delay may have been forced by an earlier test in this namespace, in which case the
  ;; fixture has already cleared the log; force it first, then look.
  @demo-output
  (roster/reset-calls!)
  (with-out-str (demo/run))
  (is (pos? (roster/call-count :fetch-rosters))
      "an io-dep the demo only talks about is not an illustration"))

;;; ------------------------------------------ the bench ------------------------------------------

(defn- hand-verdict
  "What the bench's hand-rolled side says about `m`, a bare-keyed map: `:ok`, `:rejected`,
  or `:threw`.

  In the library's own two phases: each field predicate is that field's shape check, and a
  cross-field predicate runs only where the shape predicates over the fields it reads came
  back true. Every predicate that runs does run, so a throw in one is not hidden by an
  earlier one having already said no -- which is what makes this an oracle for gating
  rather than a restatement of it."
  [m]
  (let [shape (into {}
                    (map (fn [[k p]]
                           [k (try (boolean (p (get m k))) (catch Throwable _ :threw))]))
                    bench/field-preds)
        cross (for [[p ks] [[#(bench/cuts-add-up? m) [:take :cuts]]
                            [#(bench/only-crew? m)   [:crew :cuts]]]
                    :when (every? #(true? (get shape %)) ks)]
                (try (boolean (p)) (catch Throwable _ :threw)))
        rs    (concat (vals shape) cross)]
    (cond (some #{:threw} rs) :threw
          (every? true? rs)   :ok
          :else               :rejected)))

(defn- library-verdict [m]
  (let [qm (into {} (map (fn [[k v]] [(keyword "recognitum.bench" (name k)) v])) m)]
    (try (rec/drafting [x BenchHeist] (into x qm)) :ok
         (catch ExceptionInfo _ :rejected)
         (catch Throwable _ :threw))))

(deftest the-bench-compares-like-with-like
  (is (= 6 (count (:checks (internal/model-of BenchHeist))))
      "four fields and two cross-field checks")
  (is (= 4 (count bench/field-preds)))
  (is (= (count (:checks (internal/model-of BenchHeist)))
         (+ (count bench/field-preds) 2))
      "the hand-rolled side carries one predicate per check and no more")
  (doseq [m [{:name "Job" :crew #{:a :b} :take 3 :cuts {:a 1 :b 2}}
             {:name ""    :crew #{:a :b} :take 3 :cuts {:a 1 :b 2}}
             {:name "Job" :crew #{:a :b} :take 9 :cuts {:a 1 :b 2}}
             {:name "Job" :crew #{:a}    :take 3 :cuts {:a 1 :b 2}}
             {:name "Job" :crew #{:a :b} :take -1 :cuts {:a 1 :b 2}}
             {:name "Job" :crew :not-a-set :take 3 :cuts {:a 1 :b 2}}
             {:name "Job" :crew #{:a :b} :take 3 :cuts "garbage"}]]
    (is (= (hand-verdict m) (library-verdict m))
        (str "the two sides disagree about " (pr-str m)))))

(deftest the-bench-has-no-vocabulary-the-library-no-longer-has
  ;; The hand-rolled side is the library's twin, so it drifts the same way the docs do: the
  ;; checks it mirrors stopped being "invariants" when arglists became the declaration.
  (let [src (slurp "bench/recognitum/bench.clj")]
    (doseq [token ["invariant" "inv-index" "all-invs" ":valid"]]
      (is (not (str/includes? (str/lower-case src) token))
          (str "the bench still says " token)))))
