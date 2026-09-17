(ns recognitum.demo
  "Every facility in the README, run against `recognitum.heist`. Each printed line is a
  form, followed by what it returned or the error it threw. `recognitum.roster` counts
  every call made to it, so the lines that print a call count show what was fetched."
  (:require
   [clojure.edn :as edn]
   [recognitum.heist :as heist]
   [recognitum.roster :as house]
   [recognitum.core :as rec :refer [drafting]])
  (:import
   (recognitum.heist Heist)))

(defn- show [label f]
  (print (format "%-50s" label))
  (try
    (println " =>" (pr-str (f)))
    (catch clojure.lang.ExceptionInfo e
      (println " THREW" (name (:error (ex-data e) :unknown)) "|" (ex-message e)))
    (catch Throwable e
      (println " THREW" (.getSimpleName (class e)) "|" (ex-message e)))))

(defn- section [s] (println (str "\n--- " s " ---")))

(defn- watching
  "Run `f` with the call log cleared, and return the names of the fetches it made."
  [f]
  (house/reset-calls!)
  (try (f) (catch Throwable _ nil))
  (mapv first @house/calls))

(defn- complete
  "A draft of a whole job, built from the type rather than from a value."
  [nm crew cuts]
  (-> (rec/draft Heist)
      (assoc ::heist/name nm)
      (assoc ::heist/crew crew)
      (assoc ::heist/take (reduce + 0 (vals cuts)))
      (assoc ::heist/cuts cuts)))

(defn run []
  (let [job heist/italian-job]
    (section "the value")
    (show "italian-job" (fn [] job))
    (show "(::heist/take job)" (fn [] (::heist/take job)))
    (show "destructuring {::heist/keys [name take]}"
          (fn [] (let [{::heist/keys [name take]} job] [name take])))

    (section "field keys are qualified by the namespace that defines the type")
    (show "(keys job)" (fn [] (keys job)))
    (show "(:take job) -- the bare key" (fn [] (:take job)))
    (show "assoc :take 4000 -- the bare key" (fn [] (assoc job :take 4000)))

    (section "telling the two generated types apart")
    (show "(rec/of-type? Heist job)"          (fn [] (rec/of-type? Heist job)))
    (show "(rec/of-type? Heist (draft job))"  (fn [] (rec/of-type? Heist (rec/draft job))))
    (show "(instance? Heist job)"             (fn [] (instance? Heist job)))
    (show "(instance? Heist (draft job))"     (fn [] (instance? Heist (rec/draft job))))
    (show "(rec/draft? (draft job))"          (fn [] (rec/draft? (rec/draft job))))
    (show "(rec/sealed? job)"                 (fn [] (rec/sealed? job)))

    (section "a shape validation, run on the write")
    (show "assoc ::heist/take \"lots\"" (fn [] (assoc job ::heist/take "lots")))
    (show "assoc :getaway-car \"mini\"" (fn [] (assoc job :getaway-car "mini")))

    (section "dissoc refuses declared fields")
    (show "dissoc ::heist/cuts" (fn [] (dissoc job ::heist/cuts)))

    (section "an invariant validation over two fields, on a single-key write")
    (show "assoc ::heist/take 4000 (cuts now stale)" (fn [] (assoc job ::heist/take 4000)))

    (section "merge is one atomic multi-key write")
    (show "merge {::heist/take 4000 ::heist/cuts {...}}"
          (fn [] (merge job {::heist/take 4000 ::heist/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})))
    (show "merge that still violates" (fn [] (merge job {::heist/take 4000 ::heist/cuts {:charlie 1 :lyle 1 :left-ear 1}})))

    (section "protocol implementations: one body, both types")
    (show "(heist/promote job :lyle 250) -- seals"
          (fn [] (heist/promote job :lyle 250)))
    (show "the same call inside drafting -- stays a draft"
          (fn [] (drafting [h job] (let [inner (heist/promote h :lyle 250)]
                                     (println "\n    inner is a draft?" (rec/draft? inner))
                                     inner))))
    (show "(heist/total-payout job) -- passes through"
          (fn [] (heist/total-payout job)))
    (show "add-member on a sealed value, three writes deep"
          (fn [] (heist/add-member job :handsome-rob 500)))
    (show "...while its first write alone is refused"
          (fn [] (assoc job ::heist/crew (conj (::heist/crew job) :handsome-rob))))
    (show "begin-revision -- unsealed, so a draft comes back"
          (fn [] (let [d (heist/begin-revision job "The Turin Job")]
                   {:draft? (rec/draft? d) :sealed? (rec/sealed? d) :name (::heist/name d)})))
    (show "revise -- it finishes that change, and seals"
          (fn [] (heist/revise job "The Turin Job" :lyle 250)))

    (section "draft writes are unchecked; seal is where it fails")
    (show "drafting + assoc ::heist/take \"nope\""
          (fn [] (drafting [h job] (assoc h ::heist/take "nope"))))
    (show "pay someone who is not on the crew"
          (fn [] (drafting [h job] (-> h
                                       (assoc ::heist/cuts (assoc (::heist/cuts h) :stranger 100))
                                       (assoc ::heist/take 3100)))))
    (show "ex-data from that failure"
          (fn [] (try (drafting [h job] (-> h
                                            (assoc ::heist/cuts (assoc (::heist/cuts h) :stranger 100))
                                            (assoc ::heist/take 3100)))
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))

    (section "reduce over changesets: 3 writes, one round of checks")
    (show "3 changesets, one seal"
          (fn [] (drafting [h job]
                   (reduce heist/apply-changeset h
                           [{:op :add-member :who :napster :cut 500}
                            {:op :add-member :who :handsome-rob :cut 750}
                            {:op :rename :name "The Italian Job (2003)"}]))))
    (show "a changeset that leaves it inconsistent"
          (fn [] (drafting [h job]
                   (-> h (assoc ::heist/crew (conj (::heist/crew h) :mole))
                         (assoc ::heist/cuts (assoc (::heist/cuts h) :mole 400))))))

    (section "which checks a draft records as passing")
    (show "(rec/passed (draft Heist)) -- from the type"
          (fn [] (rec/passed (rec/draft Heist))))
    (show "(rec/passed job) -- a sealed value is one every check passed on"
          (fn [] (rec/passed job)))
    (show "(rec/certificates job) -- so it holds every certificate too"
          (fn [] (rec/certificates job)))
    (show "after one write, the checks reading it are cleared"
          (fn [] (rec/passed (assoc (rec/draft job) ::heist/take 4000))))
    (show "(meta job) -- passed and certificates, from a constant"
          (fn [] (meta job)))
    (show "(meta (assoc (draft job) ::heist/take 4000))"
          (fn [] (meta (assoc (rec/draft job) ::heist/take 4000))))

    (section "checks and certificates on an incomplete draft")
    (let [d (-> (rec/draft Heist)
                (assoc ::heist/name "In Progress")
                (assoc ::heist/crew #{:charlie :lyle}))]
      (show "(rec/check d ::heist/name)"     (fn [] (rec/check d ::heist/name)))
      (show "(rec/check d ::heist/cuts) -- never written, so nil, and judged as nil"
            (fn [] (rec/check d ::heist/cuts)))
      (show "(rec/check d ::heist/cuts-sum-to-take) -- it waits on the shape validations"
            (fn [] (rec/check d ::heist/cuts-sum-to-take)))
      (show "(rec/check d ::heist/shape) -- a certificate name; take and cuts are unwritten"
            (fn [] (rec/check d ::heist/shape)))
      (show "(rec/certificates d)"           (fn [] (rec/certificates d)))
      (show "and now the shape certificate"
            (fn [] (let [d (-> d (assoc ::heist/take 2) (assoc ::heist/cuts {:charlie 1 :lyle 1}))]
                     [(rec/check d ::heist/shape) (rec/certificates d)])))
      (show "a check that fails answers with what it returned, under its own name"
            (fn [] (rec/check (assoc d ::heist/name "") ::heist/name)))
      (show "check! throws the report instead"
            (fn [] (rec/check! (assoc d ::heist/name "") ::heist/name)))
      (show "on a sealed value every check passes, so check runs nothing"
            (fn [] [(rec/check job rec/every-check)
                    (watching #(rec/check job rec/every-check))])))

    (section "io-deps: a fetch resolving, and the check judging what came back")
    (show "who the house has on file for the crew"
          (fn [] (house/fetch-roster (::heist/crew job))))
    (show "adding someone it has vouched for"
          (fn [] (drafting [h job] (heist/add-member h :handsome-rob 500))))
    (show "adding someone it has on file and has not"
          (fn [] (drafting [h job] (heist/add-member h :mole 500))))
    (show "adding someone it has never heard of"
          (fn [] (drafting [h job] (heist/add-member h :nobody-in-particular 500))))

    (section "a malformed crew never reaches the fetch, so its throw never propagates")
    (show "fetch-rosters on something that is not a crew"
          (fn [] (house/fetch-rosters [["not a crew"]])))
    (show "sealing a draft holding that crew"
          (fn [] (drafting [h job] (assoc h ::heist/crew "not a crew"))))
    (show "...and what was fetched while that ran"
          (fn [] (watching #(drafting [h job] (assoc h ::heist/crew "not a crew")))))

    (section "one fetch per validation call, however many things name it")
    (show "::vetted names ::roster, so a seal is two calls not three"
          (fn [] (watching #(drafting [h job] (heist/add-member h :handsome-rob 500)))))
    (show "a write no external validation reads fetches nothing"
          (fn [] (watching #(drafting [h job] (heist/rename h "The Sequel")))))
    (show "and nothing is cached across calls"
          (fn [] (watching #(do (drafting [h job] (heist/add-member h :handsome-rob 500))
                                (drafting [h job] (heist/add-member h :handsome-rob 500))))))

    (section "bulk: one batched call over a group, with deduplication")
    (let [ds (fn [] [(complete "one"   #{:charlie :lyle} {:charlie 1 :lyle 1})
                     (complete "two"   #{:charlie :lyle} {:charlie 2 :lyle 2})
                     (complete "three" #{:handsome-rob}  {:handsome-rob 3})])]
      (show "(rec/check-all ds rec/every-check)"
            (fn [] (rec/check-all (ds) rec/every-check)))
      (show "the batched fetch is one call for the group"
            (fn [] (watching #(rec/check-all (ds) rec/every-check))))
      (show "and the tuples it was handed -- two, not three"
            (fn [] (do (house/reset-calls!)
                       (rec/check-all (ds) rec/every-check)
                       (second (first (filter (fn [[f]] (= :fetch-rosters f)) @house/calls))))))
      (show "one of them is not vouched for"
            (fn [] (rec/check-all [(complete "a" #{:charlie} {:charlie 1})
                                   (complete "b" #{:mole}    {:mole 1})]
                                  ::heist/crew-is-vouched-for)))
      (show "then sealing each costs no IO at all"
            (fn [] (let [group (ds)]
                     (rec/check-all group rec/every-check)
                     (watching #(mapv rec/seal group)))))
      (show "recheck-all asks the house again for the whole group -- one fetch pass"
            (fn [] (let [group (ds)]
                     (rec/check-all group rec/every-check)
                     [(watching #(rec/check-all group rec/every-check))
                      (watching #(rec/recheck-all group rec/every-check))]))))

    (section "io-limited: the fields behind a fetch are draft-only")
    (show "assoc ::heist/crew on a sealed value"
          (fn [] (assoc job ::heist/crew #{:charlie :lyle})))
    (show "...and what it fetched before refusing"
          (fn [] (watching #(assoc job ::heist/crew #{:charlie :lyle}))))
    (show "the :instead form from its ex-data"
          (fn [] (try (assoc job ::heist/crew #{:charlie :lyle})
                      (catch clojure.lang.ExceptionInfo e (:instead (ex-data e))))))
    (show "which is how the write is spelled"
          (fn [] (drafting [h job] (assoc h ::heist/crew #{:charlie :lyle :left-ear}))))
    (show "a field no external validation reads is unaffected"
          (fn [] (assoc job ::heist/name "The Lavender Hill Mob")))

    (section "constructors, both validating")
    (show "(heist/->Heist \"Job\" #{:a} 10 {:a 10})" (fn [] (heist/->Heist "Job" #{:a} 10 {:a 10})))
    (show "->Heist with a bad field" (fn [] (heist/->Heist "" #{:a} 10 {:a 10})))
    (show "->Heist with everything wrong at once" (fn [] (heist/->Heist "" :not-a-set -1 "garbage")))
    (show "map->Heist with a key left out -- it is nil, and the shape validation says so"
          (fn [] (heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10})))
    (show "a shape failure is the whole report -- what waits on it does not run"
          (fn [] (heist/->Heist "Job" #{:a} 10 "garbage")))

    (section "printing: the tag names the reader")
    (show "(pr-str job)" (fn [] (pr-str job)))
    (show "a draft says it is unreadable" (fn [] (pr-str (rec/draft job))))

    (section "reading, via clojure.edn and explicit :readers")
    (show "a literal written with bare keys, qualified by the tag"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 1 :cuts {:a 1}}")))
    (show "edn round trip" (fn [] (= job (edn/read-string {:readers (rec/data-readers)} (pr-str job)))))
    (show "a literal that fails a single-field check"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"\" :crew #{:a} :take 1 :cuts {:a 1}}")))
    (show "a literal that breaks an invariant validation"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 9 :cuts {:a 1}}")))

    (section "reading, via dev/data_readers.clj and plain read-string")
    (show "tag registered at startup?" (fn [] (contains? *data-readers* 'recognitum.heist/Heist)))
    (show "(read-string (pr-str job))" (fn [] (= job (read-string (pr-str job)))))
    (show "clojure.edn ignores data_readers.clj" (fn [] (edn/read-string (pr-str job))))
    (println)))
