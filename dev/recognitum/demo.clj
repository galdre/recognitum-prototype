(ns recognitum.demo
  "The Heist example end to end. Each printed line is a form, then what it returned or the
  error it threw; the lines that read the house also print the calls it received.

      clojure -M:dev -m recognitum.demo"
  (:require
   [clojure.edn :as edn]
   [recognitum.core :as rec]
   [recognitum.heist :as h]
   [recognitum.house :as house])
  (:import
   (recognitum.heist Heist)))

(defn- show* [label f]
  (reset! house/calls [])
  (println label)
  (let [out (try (str "  => " (pr-str (f)))
                 (catch clojure.lang.ExceptionInfo e
                   (str "  threw " (:error (ex-data e)) "\n  " (.replace (ex-message e) "\n" "\n  ")))
                 (catch Throwable e
                   (str "  threw " (.getName (class e)) ": " (ex-message e))))]
    (println out)
    (when (seq @house/calls) (println "  house calls:" (pr-str @house/calls)))
    (println)))

(defmacro ^:private show [form] `(show* ~(pr-str form) (fn [] ~form)))

(def ^:private crew-of-three
  {::h/name "The Italian Job"
   ::h/crew #{"Charlie" "Stella" "Lyle"}
   ::h/take 3000
   ::h/cuts {"Charlie" 1000 "Stella" 1000 "Lyle" 1000}})

(defn -main [& _]
  (println "-- building a value runs every check\n")
  (show (h/map->Heist crew-of-three))
  (show (h/map->Heist (assoc crew-of-three ::h/take 2999)))
  (show (h/->Heist "Job" #{"Charlie"} 10 {"Charlie" 10} "Handsome Rob"))
  (show (edn/read-string {:readers (rec/data-readers)}
                         "#recognitum.heist/Heist {:recognitum.heist/name \"\" :recognitum.heist/crew #{\"Steve\"} :recognitum.heist/take 0 :recognitum.heist/cuts {}}"))

  (println "-- a write reruns only the checks that read what it changed\n")
  (let [job h/italian-job]
    (show (assoc job ::h/getaway "Handsome Rob"))
    (show (assoc job ::h/getaway "Charlie"))
    (show (assoc job ::h/crew #{"Charlie" "Stella" "Steve"}))
    (show (merge job {::h/take 4000 ::h/cuts {"Charlie" 2000 "Stella" 1000 "Lyle" 1000}}))
    (show (assoc job :getaway-car "mini"))
    (show (instance? Heist job)))

  (println "-- a draft takes any write, remembers what passes, and seals\n")
  (let [d (into (rec/draft Heist) (assoc crew-of-three ::h/take -1))]
    (show (rec/seal d))
    (show (rec/proven-checks d))
    (show (rec/seal (assoc d ::h/take 3000)))
    (show (h/plan-from-form crew-of-three)))

  (println "-- asking a value or a draft\n")
  (show (rec/checks-of (rec/fetching house/vetted) Heist))
  (let [d (into (rec/draft Heist) crew-of-three)]
    (show (rec/check-all [h/italian-job (into (rec/draft Heist) {::h/crew #{"Steve"}}) d]
                         (rec/union rec/shape (rec/fetching house/vetted))))
    (show (rec/proven-checks d))
    (show (rec/seal d)))
  (show (rec/proves? h/italian-job rec/every)))
