(ns recognitum.draft-test
  "Drafts: what they accept, what they remember, and what seal runs because of it."
  (:require
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]
   [recognitum.house :as house])
  (:import
   (recognitum.heist Heist)
   (recognitum.record Draft)))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(def ^:private entries
  {::h/name "The Italian Job" ::h/crew #{"Charlie" "Stella" "Lyle"} ::h/take 3000
   ::h/cuts {"Charlie" 1000 "Stella" 1000 "Lyle" 1000}})

(deftest a-draft-of-the-type-holds-and-passes-nothing
  (let [d (rec/draft Heist)]
    (is (rec/draft? d))
    (is (= 0 (count d)))
    (is (= #{} (rec/proven-checks d)))
    (is (not (rec/proves? d rec/every)))))

(deftest a-draft-accepts-any-write
  (let [d (-> (rec/draft Heist) (assoc ::h/take "lots") (assoc :scratch 1))]
    (is (= "lots" (::h/take d)))
    (is (= 1 (:scratch d)))))

(deftest a-draft-of-a-value-passes-everything-and-a-draft-of-a-draft-is-itself
  (let [d (rec/draft h/italian-job)]
    (is (= (rec/checks-of rec/every Heist) (rec/proven-checks d)))
    (is (identical? d (rec/draft d)))
    (testing "sealing it again runs nothing"
      (reset! house/calls [])
      (is (= h/italian-job (rec/seal d)))
      (is (empty? @house/calls)))))

(deftest a-write-forgets-the-checks-that-read-the-key
  (let [d (rec/draft h/italian-job)
        d2 (assoc d ::h/take 3001)]
    (is (= (rec/checks-of rec/every Heist) (rec/proven-checks d)) "the old draft is unchanged")
    (is (= (disj (rec/checks-of rec/every Heist) ::h/take ::h/cuts-sum-to-take)
           (rec/proven-checks d2))
        "only-crew-get-cuts waits on cuts-sum-to-take but does not read take, so it is kept")))

(deftest a-failed-seal-remembers-what-passed
  (let [d (into (rec/draft Heist) (assoc entries ::h/take -1))
        e (err #(rec/seal d))]
    (is (= #{::h/take} (set (keys (:failed e)))))
    (is (= #{::h/cuts-sum-to-take ::h/only-crew-get-cuts} (get-in e [:failed ::h/take :blocked]))
        "directly, and through a check it blocked")
    (is (contains? (rec/proven-checks d) ::h/crew-vetted))
    (testing "the next seal of the mended draft runs only the rest"
      (reset! house/calls [])
      (is (instance? Heist (rec/seal (assoc d ::h/take 3000))))
      (is (empty? @house/calls)))))

(deftest seal-refuses-undeclared-keys-before-running-anything
  (let [d (into (rec/draft Heist) (assoc entries :scratch 1))]
    (reset! house/calls [])
    (is (= {:error :recognitum/undeclared-keys :type 'recognitum.heist/Heist :keys #{:scratch}}
           (select-keys (err #(rec/seal d)) [:error :type :keys])))
    (is (empty? @house/calls))
    (is (= #{} (rec/proven-checks d)))
    (testing "clean and dissoc remove them"
      (is (instance? Heist (rec/seal (rec/clean d))))
      (is (instance? Heist (rec/seal (dissoc d :scratch)))))))

(deftest check-remembers-and-recheck-reruns
  (let [d (into (rec/draft Heist) entries)]
    (is (true? (rec/check d rec/shape)))
    (is (= (rec/checks-of rec/shape Heist) (rec/proven-checks d)))
    (reset! house/calls [])
    (is (true? (rec/check d ::h/crew-vetted)))
    (is (true? (rec/check d ::h/crew-vetted)))
    (is (= 1 (count @house/calls)) "remembered after the first check")
    (is (true? (rec/recheck d ::h/crew-vetted)))
    (is (= 2 (count @house/calls)) "recheck runs it in any case")))

(deftest marks-moved-onto-other-entries-show-nothing
  (let [d (rec/draft h/italian-job)
        forged (Draft. (assoc entries ::h/take -1) (.-cell ^Draft d) nil)]
    (is (= #{} (rec/proven-checks forged)))
    (is (= :recognitum/check-failed (:error (err #(rec/seal forged)))))))

;;; A type whose external check reads a source the tests change under it.

(def ^:private on-file (atom #{0 7}))
(def ^:private hold (atom nil))
(def ^:private source-calls (atom 0))

(defn- files [ks]
  (swap! source-calls inc)
  (let [answer (into {} (for [k ks] [k (contains? @on-file k)]))]
    (when-let [h @hold] (h))
    answer))

(rec/defproven Filed [a b]
  rec/checks
  (a [] (int? a))
  (b [] (int? b))
  (b-not-below-a [] (>= b a))
  (a-known [b-not-below-a] (rec/fetch files a)))

(deftest a-pass-found-before-a-recheck-is-not-written-after-it
  (dotimes [_ 20]
    (reset! on-file #{0 7})
    (let [d (into (rec/draft Filed) {::a 0 ::b 1})
          entered (java.util.concurrent.CountDownLatch. 1)
          release (java.util.concurrent.CountDownLatch. 1)]
      (reset! hold (fn [] (reset! hold nil) (.countDown entered) (.await release 10 java.util.concurrent.TimeUnit/SECONDS)))
      (let [checking (future (rec/check d ::a-known))]
        (is (.await entered 10 java.util.concurrent.TimeUnit/SECONDS) "the check has its yes from the source")
        (reset! on-file #{})
        (is (= #{::a-known} (set (keys (:failed (rec/recheck d ::a-known))))))
        (.countDown release)
        (is (true? @checking))
        (is (not (contains? (rec/proven-checks d) ::a-known))
            "the check's pass, found before the recheck, is not written after it")))))

(deftest recheck-forgets-an-external-check-it-names-even-when-blocked
  (reset! on-file #{0 7})
  (let [d (assoc (rec/draft (map->Filed {::a 0 ::b 1})) ::b -1)]
    (is (contains? (rec/proven-checks d) ::a-known) "writing b keeps the check that reads only a")
    (reset! on-file #{})
    (is (= #{::a-known} (:blocked (rec/recheck d ::a-known))))
    (is (not (contains? (rec/proven-checks d) ::a-known)))
    (testing "the next seal asks the source again"
      (reset! source-calls 0)
      (is (= :recognitum/check-failed (:error (err #(rec/seal (assoc d ::b 1))))))
      (is (= 1 @source-calls))))
  (testing "a recheck that runs it and passes remembers it"
    (reset! on-file #{0 7})
    (let [d (rec/draft (map->Filed {::a 0 ::b 1}))]
      (is (true? (rec/recheck d ::a-known)))
      (is (contains? (rec/proven-checks d) ::a-known)))))

(deftest a-recheck-that-loses-the-race-still-forgets-what-failed
  (dotimes [_ 20]
    (reset! on-file #{0 7})
    (let [d (rec/draft (map->Filed {::a 0 ::b 1}))
          entered (java.util.concurrent.CountDownLatch. 1)
          release (java.util.concurrent.CountDownLatch. 1)]
      (is (contains? (rec/proven-checks d) ::a-known))
      (reset! on-file #{})
      (reset! hold (fn [] (reset! hold nil) (.countDown entered) (.await release 10 java.util.concurrent.TimeUnit/SECONDS)))
      (let [rechecking (future (rec/recheck d ::a-known))]
        (is (.await entered 10 java.util.concurrent.TimeUnit/SECONDS) "the recheck has its no from the source")
        (is (true? (rec/check d ::a)) "another run writes the memory meanwhile")
        (.countDown release)
        (is (= #{::a-known} (set (keys (:failed (deref rechecking 10000 nil))))))
        (is (not (contains? (rec/proven-checks d) ::a-known))
            "the losing recheck still forgets the check that failed")))))
