(ns recognitum.demo
  (:require
   [recognitum.heist :as heist]
   [recognitum.core :as v :refer [drafting]]))

(defn- show [label f]
  (print (format "%-50s" label))
  (try
    (println " =>" (pr-str (f)))
    (catch clojure.lang.ExceptionInfo e
      (println " THREW" (name (:error (ex-data e) :unknown)) "|" (ex-message e)))))

(defn run []
  (let [job heist/italian-job]
    (println "\n--- the value ---")
    (show "italian-job" (fn [] job))
    (show "(:take job)" (fn [] (:take job)))
    (show "destructuring {:keys [name take]}" (fn [] (let [{:keys [name take]} job] [name take])))

    (println "\n--- per-field predicates, eager ---")
    (show "assoc :take \"lots\"" (fn [] (assoc job :take "lots")))
    (show "assoc :getaway-car \"mini\"" (fn [] (assoc job :getaway-car "mini")))

    (println "\n--- dissoc refuses declared fields ---")
    (show "dissoc :cuts" (fn [] (dissoc job :cuts)))

    (println "\n--- invariants on single-key writes ---")
    (show "assoc :take 4000 (cuts now stale)" (fn [] (assoc job :take 4000)))

    (println "\n--- merge is one atomic multi-key write ---")
    (show "merge {:take 4000 :cuts {...}}"
          (fn [] (merge job {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}})))
    (show "merge that still violates" (fn [] (merge job {:take 4000 :cuts {:charlie 1 :lyle 1 :left-ear 1}})))

    (println "\n--- drafting: impl fns take and return drafts ---")
    (show "add-member :napster 500"
          (fn [] (drafting [h job] (heist/add-member h :napster 500))))
    (show "pay someone who is not on the crew"
          (fn [] (drafting [h job] (-> h
                                       (assoc :cuts (assoc (:cuts h) :stranger 100))
                                       (assoc :take 3100)))))
    (show "ex-data from that failure"
          (fn [] (try (drafting [h job] (-> h
                                            (assoc :cuts (assoc (:cuts h) :stranger 100))
                                            (assoc :take 3100)))
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))

    (println "\n--- draft writes are unchecked; seal is where it fails ---")
    (show "drafting + assoc :take \"nope\""
          (fn [] (drafting [h job] (assoc h :take "nope"))))

    (println "\n--- re-entrancy ---")
    (show "promote standalone (seals)" (fn [] (heist/promote job :lyle 250)))
    (show "promote nested (stays a draft until outer seal)"
          (fn [] (drafting [h job] (-> h (heist/promote :lyle 250) (heist/rename "The Sequel")))))
    (show "what the inner call returns when nested"
          (fn [] (drafting [h job] (let [inner (heist/promote h :lyle 250)]
                                     (println "\n    inner is a draft?" (v/draft? inner))
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
                   (-> h (assoc :crew (conj (:crew h) :mole))
                         (assoc :cuts (assoc (:cuts h) :mole 400))))))
    (println)))
