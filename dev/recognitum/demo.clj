(ns recognitum.demo
  (:require
   [clojure.edn :as edn]
   [recognitum.heist :as heist]
   [recognitum.core :as rec :refer [drafting]]))

(defn- show [label f]
  (print (format "%-50s" label))
  (try
    (println " =>" (pr-str (f)))
    (catch clojure.lang.ExceptionInfo e
      (println " THREW" (name (:error (ex-data e) :unknown)) "|" (ex-message e)))
    (catch Throwable e
      (println " THREW" (.getSimpleName (class e)) "|" (ex-message e)))))

(defn run []
  (let [job heist/italian-job]
    (println "\n--- the value ---")
    (show "italian-job" (fn [] job))
    (show "(::heist/take job)" (fn [] (::heist/take job)))
    (show "destructuring {::heist/keys [name take]}"
          (fn [] (let [{::heist/keys [name take]} job] [name take])))

    (println "\n--- field keys are qualified by the namespace that defines the type ---")
    (show "(keys job)" (fn [] (keys job)))
    (show "(:take job) -- the bare key" (fn [] (:take job)))
    (show "assoc :take 4000 -- the bare key" (fn [] (assoc job :take 4000)))

    (println "\n--- a check over one field, eager ---")
    (show "assoc ::heist/take \"lots\"" (fn [] (assoc job ::heist/take "lots")))
    (show "assoc :getaway-car \"mini\"" (fn [] (assoc job :getaway-car "mini")))

    (println "\n--- dissoc refuses declared fields ---")
    (show "dissoc ::heist/cuts" (fn [] (dissoc job ::heist/cuts)))

    (println "\n--- a check over two fields, on a single-key write ---")
    (show "assoc ::heist/take 4000 (cuts now stale)" (fn [] (assoc job ::heist/take 4000)))

    (println "\n--- merge is one atomic multi-key write ---")
    (show "merge {::heist/take 4000 ::heist/cuts {...}}"
          (fn [] (merge job {::heist/take 4000 ::heist/cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})))
    (show "merge that still violates" (fn [] (merge job {::heist/take 4000 ::heist/cuts {:charlie 1 :lyle 1 :left-ear 1}})))

    (println "\n--- drafting: impl fns take and return drafts ---")
    (show "add-member :napster 500"
          (fn [] (drafting [h job] (heist/add-member h :napster 500))))
    (show "pay someone who is not on the crew"
          (fn [] (drafting [h job] (-> h
                                       (assoc ::heist/cuts (assoc (::heist/cuts h) :stranger 100))
                                       (assoc ::heist/take 3100)))))
    (show "ex-data from that failure"
          (fn [] (try (drafting [h job] (-> h
                                            (assoc ::heist/cuts (assoc (::heist/cuts h) :stranger 100))
                                            (assoc ::heist/take 3100)))
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))

    (println "\n--- draft writes are unchecked; seal is where it fails ---")
    (show "drafting + assoc ::heist/take \"nope\""
          (fn [] (drafting [h job] (assoc h ::heist/take "nope"))))

    (println "\n--- re-entrancy ---")
    (show "promote standalone (seals)" (fn [] (heist/promote job :lyle 250)))
    (show "promote nested (stays a draft until outer seal)"
          (fn [] (drafting [h job] (-> h (heist/promote :lyle 250) (heist/rename "The Sequel")))))
    (show "what the inner call returns when nested"
          (fn [] (drafting [h job] (let [inner (heist/promote h :lyle 250)]
                                     (println "\n    inner is a draft?" (rec/draft? inner))
                                     inner))))

    (println "\n--- reduce over changesets ---")
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
    (println "\n--- constructors, both validating ---")
    (show "(heist/->Heist \"Job\" #{:a} 10 {:a 10})" (fn [] (heist/->Heist "Job" #{:a} 10 {:a 10})))
    (show "->Heist with a bad field" (fn [] (heist/->Heist "" #{:a} 10 {:a 10})))
    (show "map->Heist missing a key" (fn [] (heist/map->Heist {::heist/name "Job" ::heist/crew #{:a} ::heist/take 10})))

    (println "\n--- printing names the reader you need ---")
    (show "(pr-str job)" (fn [] (pr-str job)))
    (show "a draft says it is unreadable" (fn [] (pr-str (rec/draft job))))

    (println "\n--- reading, via clojure.edn and explicit :readers ---")
    (show "a literal written with bare keys, qualified by the tag"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 1 :cuts {:a 1}}")))
    (show "edn round trip" (fn [] (= job (edn/read-string {:readers (rec/data-readers)} (pr-str job)))))
    (show "a literal that fails a single-field check"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"\" :crew #{:a} :take 1 :cuts {:a 1}}")))
    (show "a literal that breaks a cross-field check"
          (fn [] (edn/read-string {:readers (rec/data-readers)}
                                  "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 9 :cuts {:a 1}}")))

    (println "\n--- reading, via dev/data_readers.clj and plain read-string ---")
    (show "tag registered at startup?" (fn [] (contains? *data-readers* 'recognitum.heist/Heist)))
    (show "(read-string (pr-str job))" (fn [] (= job (read-string (pr-str job)))))
    (show "clojure.edn ignores data_readers.clj" (fn [] (edn/read-string (pr-str job))))
    (println)))
