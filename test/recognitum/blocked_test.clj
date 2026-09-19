(ns recognitum.blocked-test
  "What a failing check blocked, and the one report every form carries.

  A check waits on the shape validations of the fields it names, and an external validation
  waits as well on the pure checks over the fields its fetches read. Where one of those
  fails, the checks behind it do not run and the failing check's entry names them under
  `:blocked`; a failure nothing behind it was waiting on blocks nothing and holds up no
  fetch. Every entry point answers with the same report, and the answering form and the
  throwing form of one call report the same thing about the same value."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.core :as rec :refer [drafting]]))

(defn- err
  "The ex-data of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- msg
  "The message of the ExceptionInfo `f` throws, or nil if it throws nothing."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

;;; ---------------------------------------- the types ----------------------------------------

(def ^:private ran "Every check that ran, in order." (atom []))
(def ^:private fetched "Every io-dep that was fetched, in order." (atom []))

(defn- note! [k] (swap! ran conj k) nil)
(defn- fetch! [k] (swap! fetched conj k) nil)
(defn- times [k] (count (filter #{k} @ran)))

(def ^:private tide
  "Whether the house has turned against every title there is. Mutable, so a shape validation
  can stop holding of a value nothing has written to, which is the only way a sealed value
  reaches a forced pass with a failing shape validation."
  (atom false))

;; `::titled` waits on both shape validations; `::vouched` names no field, so it waits on the
;; pure checks over `crew`, which its fetch reads, and through them on both shape validations
;; as well.
(rec/defrecord Job
  [title crew]
  {:checks
   {::title ("the title is a string" [title]
             (do (note! ::title) (and (string? title) (not @tide))))
    ::crew  ("the crew is a set of keywords" [crew]
             (do (note! ::crew) (and (set? crew) (every? keyword? crew))))
    ::titled ("nobody on the crew is named after the job" [title crew]
              (do (note! ::titled) (not (contains? crew (keyword (str title))))))
    ::vouched ("everyone the house has on file for this crew is vetted" [::roster]
               (do (note! ::vouched) (= roster :vetted)))}

   :io-deps
   {::roster ("who the house has on file for this crew" [crew]
              (do (fetch! ::roster) (if (contains? crew :mole) :unvetted :vetted)))}

   :certificates
   {::shape #{::title ::crew}}})

;; Two external validations over two fields, each behind its own fetch, so a pure failure
;; over one field leaves the other's fetch and check alone.
(rec/defrecord Pair
  [left right]
  {:checks
   {::left  ("left is an int" [left] (do (note! ::left) (int? left)))
    ::right ("right is an int" [right] (do (note! ::right) (int? right)))
    ::left-on-file  ("the house has the left one on file" [::left-row]
                     (do (note! ::left-on-file) (= left-row :on-file)))
    ::right-on-file ("the house has the right one on file" [::right-row]
                     (do (note! ::right-on-file) (= right-row :on-file)))}

   :io-deps
   {::left-row  ([left]  (do (fetch! ::left-row) :on-file))
    ::right-row ([right] (do (fetch! ::right-row) :on-file))}})

(def ^:private job (->Job "The Italian Job" #{:charlie :lyle}))

(use-fixtures :each (fn [f] (reset! ran []) (reset! fetched []) (reset! tide false) (f)))

(defn- draft-of
  "A draft of `Job` with both fields written and nothing recorded as passing."
  [title crew]
  (-> (rec/draft Job) (assoc ::title title) (assoc ::crew crew)))

(defn- pair-of
  "A draft of `Pair` with both fields written."
  [left right]
  (-> (rec/draft Pair) (assoc ::left left) (assoc ::right right)))

(defn- entry
  "The report entry for `cname`, out of an answer or out of the ex-data of a throw."
  [r cname]
  (get (if (contains? r :failures) (:failures r) r) cname))

;;; -------------------- a forced pass over a sealed value gates within itself --------------------

(deftest a-forced-pass-on-a-sealed-value-blocks-what-the-shape-validation-blocks
  ;; The value has not changed and cannot; the world the title is judged against has.
  (reset! tide true)
  (let [r (rec/recheck job rec/every-check)]
    (is (= [::title] (vec (keys r))) "the title no longer holds")
    (is (= 1 (times ::crew)) "the other shape validation ran and passed")
    (is (zero? (times ::titled)) "the invariant validation over that field did not run")
    (is (= [] @fetched) "and nothing was fetched for the external validation")
    (is (= [::titled ::vouched] (:blocked (entry r ::title)))
        "which is what the title's entry says")))

(deftest a-forced-throwing-pass-on-a-sealed-value-blocks-the-same-checks
  (reset! tide true)
  (let [e (err #(rec/recheck! job rec/every-check))]
    (is (= ::rec/check-failed (:error e)))
    (is (zero? (times ::titled)))
    (is (= [] @fetched))
    (is (= [::titled ::vouched] (:blocked (entry e ::title))))))

;;; ------------------------------ the entry points, one at a time ------------------------------

(deftest every-entry-point-that-can-block-a-check-says-what-it-blocked
  (doseq [[label f] [["check"        #(rec/check (draft-of 42 #{:charlie}) rec/every-check)]
                     ["check!"       #(err (fn [] (rec/check! (draft-of 42 #{:charlie})
                                                              rec/every-check)))]
                     ["recheck"      #(rec/recheck (draft-of 42 #{:charlie}) rec/every-check)]
                     ["recheck!"     #(err (fn [] (rec/recheck! (draft-of 42 #{:charlie})
                                                                rec/every-check)))]
                     ["check-all"    #(first (rec/check-all [(draft-of 42 #{:charlie})]
                                                            rec/every-check))]
                     ["check-all!"   #(get (:failures (err (fn [] (rec/check-all!
                                                                   [(draft-of 42 #{:charlie})]
                                                                   rec/every-check))))
                                           0)]
                     ["recheck-all"  #(first (rec/recheck-all [(draft-of 42 #{:charlie})]
                                                              rec/every-check))]
                     ["recheck-all!" #(get (:failures (err (fn [] (rec/recheck-all!
                                                                    [(draft-of 42 #{:charlie})]
                                                                    rec/every-check))))
                                           0)]
                     ["seal"         #(err (fn [] (rec/seal (draft-of 42 #{:charlie}))))]
                     ["drafting"     #(err (fn [] (drafting [x Job]
                                                    (-> x (assoc ::title 42)
                                                        (assoc ::crew #{:charlie})))))]
                     ["->Job"        #(err (fn [] (->Job 42 #{:charlie})))]
                     ["map->Job"     #(err (fn [] (map->Job {::title 42 ::crew #{:charlie}})))]
                     ["merge"        #(err (fn [] (merge job {::title 42 ::crew #{:charlie}})))]]]
    (let [r (f)]
      (is (= [::titled ::vouched] (:blocked (entry r ::title))) label)
      (is (= false (:returned (entry r ::title))) label)))
  (testing "and a write to a sealed value blocks what that write was going to run"
    (let [e (err #(assoc job ::title 42))]
      (is (= [::titled] (:blocked (entry e ::title)))
          "no fetch reads the title, so the external validation was never in this plan"))))

(deftest nothing-is-fetched-for-a-check-a-shape-validation-blocked
  (doseq [[label f] [["seal"  #(err (fn [] (rec/seal (draft-of 42 #{:charlie}))))]
                     ["->Job" #(err (fn [] (->Job 42 #{:charlie})))]
                     ["check" #(rec/check (draft-of 42 #{:charlie}) rec/every-check)]
                     ["merge" #(err (fn [] (merge job {::title 42 ::crew #{:charlie}})))]]]
    (reset! fetched [])
    (f)
    (is (= [] @fetched) label)))

;;; ------------------------------- what belongs in :blocked -------------------------------

(deftest a-failure-that-blocked-nothing-carries-no-blocked-key
  (testing "a lone shape validation, with nothing else in the plan"
    (let [r (rec/check (draft-of 42 #{:charlie}) ::title)]
      (is (= [::title] (vec (keys r))))
      (is (not (contains? (entry r ::title) :blocked)))))
  (testing "an external validation, which nothing waits on"
    (let [r (rec/check (draft-of "The Italian Job" #{:mole}) ::vouched)]
      (is (= [::vouched] (vec (keys r))))
      (is (not (contains? (entry r ::vouched) :blocked)))))
  (testing "an invariant validation the external validation does wait on"
    (let [r (rec/check (draft-of "charlie" #{:charlie}) rec/every-check)]
      (is (= [::titled] (vec (keys r))) "the shape validations passed and the invariant did not")
      (is (= [::vouched] (:blocked (entry r ::titled)))
          "the external validation waits on it through the fields its fetch reads"))))

(deftest blocked-holds-the-names-in-declaration-order
  (let [r (rec/check (draft-of 42 #{:charlie}) rec/every-check)]
    (is (= [::titled ::vouched] (:blocked (entry r ::title)))
        "::titled is declared before ::vouched")))

(deftest blocked-names-only-what-this-call-was-going-to-run
  (testing "a certificate that names neither the invariant nor the external validation"
    (let [r (rec/check (draft-of 42 "not a crew") ::shape)]
      (is (= [::title ::crew] (vec (keys r))))
      (is (not (contains? (entry r ::title) :blocked)))
      (is (not (contains? (entry r ::crew) :blocked)))))
  (testing "a request that reaches the invariant validation and no further"
    (let [r (rec/check (draft-of 42 #{:charlie}) ::titled)]
      (is (= [::titled] (:blocked (entry r ::title)))))))

(deftest a-check-two-failures-blocked-appears-under-both
  (let [r (rec/check (draft-of 42 "not a crew") rec/every-check)]
    (is (= [::title ::crew] (vec (keys r))))
    (is (= [::titled ::vouched] (:blocked (entry r ::title))))
    (is (= [::titled ::vouched] (:blocked (entry r ::crew))))))

;;; ------------------- a failure holds up only what was waiting on it -------------------

(deftest a-failing-check-holds-up-only-the-fetch-that-waits-on-it
  (let [r (rec/check (pair-of "not an int" 2) rec/every-check)]
    (is (= [::left] (vec (keys r))))
    (is (= [::left-on-file] (:blocked (entry r ::left)))
        "the external validation over the other field was waiting on nothing that failed")
    (is (= [::right-row] @fetched) "so only the other field's fetch was made")
    (is (= 1 (times ::right-on-file)) "and the external validation behind it ran")
    (is (zero? (times ::left-on-file)))))

(deftest every-entry-point-holds-up-only-the-fetch-that-waits-on-the-failure
  (doseq [[label f] [["seal"      #(err (fn [] (rec/seal (pair-of "not an int" 2))))]
                     ["->Pair"    #(err (fn [] (->Pair "not an int" 2)))]
                     ["map->Pair" #(err (fn [] (map->Pair {::left "not an int" ::right 2})))]
                     ["check-all" #(first (rec/check-all [(pair-of "not an int" 2)]
                                                         rec/every-check))]]]
    (reset! ran [])
    (reset! fetched [])
    (let [r (f)]
      (is (= [::left-on-file] (:blocked (entry r ::left))) label)
      (is (= [::right-row] @fetched) label)
      (is (= 1 (times ::right-on-file)) label))))

(deftest a-check-a-call-planned-and-did-not-run-is-named-under-a-failure
  ;; `every-check` against a draft with nothing recorded plans every check the type declares,
  ;; so what ran and what a failure is reported as having blocked have to cover them between
  ;; them, whichever side of the failure the fetch sits on.
  (doseq [[label d planned]
          [["the fetch waits on the failure"
            (draft-of 42 #{:charlie})  #{::title ::crew ::titled ::vouched}]
           ["the fetch does not"
            (pair-of "not an int" 2)   #{::left ::right ::left-on-file ::right-on-file}]]]
    (reset! ran [])
    (let [r     (rec/check d rec/every-check)
          named (into #{} (mapcat :blocked) (vals r))]
      (is (= planned (into (set @ran) named)) label))))

;;; --------------------------- the two forms report the same thing ---------------------------

(deftest the-answering-form-and-the-throwing-form-give-the-same-report
  (doseq [[label answer insist]
          [["check"   #(rec/check (draft-of 42 #{:charlie}) rec/every-check)
                      #(rec/check! (draft-of 42 #{:charlie}) rec/every-check)]
           ["recheck" #(rec/recheck (draft-of 42 #{:charlie}) rec/every-check)
                      #(rec/recheck! (draft-of 42 #{:charlie}) rec/every-check)]]]
    (is (= (answer) (:failures (err insist))) label))
  (testing "and the group forms agree value by value"
    (let [ds (fn [] [(draft-of 42 #{:charlie}) (draft-of "fine" #{:charlie})])
          rs (rec/check-all (ds) rec/every-check)
          e  (err #(rec/check-all! (ds) rec/every-check))]
      (is (= (first rs) (get (:failures e) 0)))
      (is (= [0] (:indexes e)) "the value that passed is in neither"))))

;;; ------------------------------------ the thrown message ------------------------------------

(deftest the-message-names-the-checks-the-failure-blocked
  (let [m (msg #(rec/seal (draft-of 42 #{:charlie})))]
    (is (str/includes? m (str "which blocked " ::titled " and " ::vouched)))
    (is (str/includes? m (str ::title)) "after the check that blocked them"))
  (testing "and says nothing about blocking where nothing was blocked"
    (let [m (msg #(rec/check! (draft-of 42 #{:charlie}) ::title))]
      (is (not (str/includes? m "which blocked"))))))
