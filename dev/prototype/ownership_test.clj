(ns prototype.ownership-test
  (:require
   [prototype.heist :as heist]
   [prototype.validated-record :as v :refer [draft revising]])
  (:import
   (prototype.heist Heist)))

(defn- show [label f]
  (print (format "%-56s" label))
  (try (let [r (f)] (println " =>" (if (v/draft? r) "DRAFT" "SEALED") (pr-str r)))
       (catch clojure.lang.ExceptionInfo e (println " THREW" (name (:error (ex-data e) :unknown))))))

(defn run []
  (let [job heist/italian-job
        add #(heist/add-member % :napster 500)]
    (println "\n--- construction, both idioms ---")
    (show "(revising [h Heist] ...)"
          (fn [] (revising [h Heist] (merge h {:name "A" :crew #{:x} :take 1 :cuts {:x 1}}))))
    (show "(revising [h (draft Heist)] ...)   <- was broken"
          (fn [] (revising [h (draft Heist)] (merge h {:name "B" :crew #{:x} :take 1 :cuts {:x 1}}))))

    (println "\n--- revision, both idioms ---")
    (show "(revising [h job] (add h))" (fn [] (revising [h job] (add h))))
    (show "(revising [h (draft job)] (add h))  <- was broken"
          (fn [] (revising [h (draft job)] (add h))))

    (println "\n--- nesting still defers to the outermost form ---")
    (show "promote standalone" (fn [] (heist/promote job :lyle 250)))
    (show "promote nested inside revising"
          (fn [] (revising [h job] (-> h (heist/promote :lyle 250) (heist/rename "Sequel")))))
    (show "two nested calls in sequence"
          (fn [] (revising [h job] (-> h (heist/promote :lyle 100) (heist/promote :charlie 100)))))
    (show "inner call's return value, observed mid-body"
          (fn [] (revising [h job]
                   (let [inner (heist/promote h :lyle 250)]
                     (print (str "[inner=" (if (v/draft? inner) "DRAFT" "SEALED") "]"))
                     inner))))

    (println "\n--- a free draft is still sealable by hand ---")
    (show "(v/seal (-> (draft job) (add)))" (fn [] (v/seal (add (draft job)))))
    (show "invalid free draft sealed by hand"
          (fn [] (v/seal (assoc (draft job) :take 99))))
    (println)))
