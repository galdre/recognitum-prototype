(ns recognitum.heist-test
  "The worked example: constructors, writes, reports, drafts and the external check."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]
   [recognitum.house :as house])
  (:import
   (recognitum.heist Heist)))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(def job h/italian-job)

(def ^:private entries
  {::h/name "The Italian Job" ::h/crew #{"Charlie" "Stella" "Lyle"} ::h/take 3000
   ::h/cuts {"Charlie" 1000 "Stella" 1000 "Lyle" 1000}})

;;; ------------------------------------------------------------------ constructors

(deftest map->T-returns-a-value-of-the-class
  (is (instance? Heist job))
  (is (rec/sealed? job))
  (is (= entries job) "reads and compares as a map")
  (is (= "The Italian Job" (::h/name job))))

(deftest map->T-throws-a-report-when-a-check-fails
  (let [e (err #(h/map->Heist (assoc entries ::h/take 2999)))]
    (is (= :recognitum/check-failed (:error e)))
    (is (= 'recognitum.heist/Heist (:type e)))
    (is (= #{::h/cuts-sum-to-take} (set (keys (:failed e)))))
    (is (= {:data false :doc "the cuts add up to the take" :blocked #{::h/only-crew-get-cuts}}
           (::h/cuts-sum-to-take (:failed e))))
    (is (= #{::h/only-crew-get-cuts} (:blocked e)))))

(deftest positional-constructor-takes-every-field
  (is (instance? Heist (h/->Heist "Job" #{"Charlie"} 10 {"Charlie" 10} "Rob")))
  (testing "nil is present, and fails the getaway's shape check"
    (is (contains? (:failed (err #(h/->Heist "Job" #{"Charlie"} 10 {"Charlie" 10} nil))) ::h/getaway))))

(deftest the-tagged-literal-runs-every-check
  (let [readers {'recognitum.heist/Heist h/map->Heist}]
    (is (= job (edn/read-string {:readers readers} (pr-str job))) "prints as the tagged literal")
    (is (= job (edn/read-string {:readers (rec/data-readers)} (pr-str job))))
    (is (= :recognitum/check-failed
           (:error (err #(edn/read-string {:readers readers}
                                          "#recognitum.heist/Heist {:recognitum.heist/name \"\"}")))))))

(deftest no-path-skips-the-checks
  (testing "the class's constructor needs the type's key"
    (is (thrown? Exception (Heist. nil entries))))
  (testing "a missing required field fails its shape check"
    (is (= {:absent ::h/take} (:data (::h/take (:failed (err #(h/map->Heist (dissoc entries ::h/take))))))))))

(deftest a-check-waits-on-the-shape-checks-of-the-fields-it-reads
  (testing "cuts-sum-to-take reads take, so a missing take blocks it"
    (let [e (err #(h/map->Heist (dissoc entries ::h/take)))]
      (is (= #{::h/take} (set (keys (:failed e)))))
      (is (= #{::h/cuts-sum-to-take ::h/only-crew-get-cuts} (:blocked e)))))
  (testing "a field read only through when-extant counts as read"
    (let [e (err #(h/->Heist "Job" #{"Charlie"} 10 {"Charlie" 10} nil))]
      (is (= #{::h/getaway} (set (keys (:failed e)))))
      (is (= #{::h/getaway-not-crew} (:blocked e)))))
  (testing "checking a check runs the shape checks of what it reads"
    (let [d (into (rec/draft Heist) entries)]
      (rec/check d ::h/cuts-sum-to-take)
      (is (= #{::h/take ::h/cuts ::h/cuts-sum-to-take} (rec/proven-checks d))))))

;;; ------------------------------------------------------------------ writes

(deftest a-write-reruns-the-checks-that-read-what-it-changed
  (reset! house/calls [])
  (testing "the getaway is optional; adding one checks it"
    (is (instance? Heist (assoc job ::h/getaway "Handsome Rob")))
    (is (= :recognitum/check-failed (:error (err #(assoc job ::h/getaway "Charlie"))))))
  (is (empty? @house/calls) "the crew did not change, so nobody was vetted again")
  (testing "a new crew member is vetted"
    (is (= #{::h/crew-vetted ::h/only-crew-get-cuts}
           (set (keys (:failed (err #(assoc job ::h/crew #{"Charlie" "Steve"})))))))
    (is (= 1 (count @house/calls)))))

(deftest a-multi-key-write-is-checked-once-on-the-result
  (is (instance? Heist (merge job {::h/take 3500 ::h/cuts {"Charlie" 1500 "Stella" 1000 "Lyle" 1000}})))
  (testing "either key alone fails"
    (is (some? (err #(assoc job ::h/take 3500))))))

(deftest a-value-refuses-a-key-its-type-does-not-declare
  (let [e (err #(assoc job :getaway-car "mini"))]
    (is (= :recognitum/undeclared-keys (:error e)))
    (is (= #{:getaway-car} (:keys e)))))

(deftest a-sealed-value-has-no-empty-value
  (is (thrown? UnsupportedOperationException (empty job))))

;;; ------------------------------------------------------------------ drafts

(deftest a-partial-plan-goes-through-a-draft
  (is (= job (h/plan-from-form entries)))
  (testing "the draft takes the fields in any order and any state"
    (is (= :recognitum/check-failed (:error (err #(h/plan-from-form (dissoc entries ::h/cuts))))))))

(deftest proven-checks-and-concerns-of-a-value
  (is (= (rec/checks-of rec/every Heist) (rec/proven-checks job)))
  (is (= #{::h/name ::h/crew ::h/take ::h/cuts ::h/getaway} (rec/checks-of rec/shape job)))
  (is (= #{::h/crew-vetted} (rec/checks-of (rec/fetching house/vetted) Heist)))
  (is (rec/proves? job rec/every)))
