(ns prototype.bench2
  (:require
   [criterium.core :as c]
   [prototype.heist :as heist]
   [prototype.validated-record :as v]))

(def h heist/italian-job)
(def d (v/draft h))
(def model (v/model-of h))
(def backing (into {} h))
(def dirty #{:take})

(defn- ns-per [f] (* 1e9 (first (:mean (c/quick-benchmark (f) {:verbose false})))))

(defn run []
  (doseq [[label f]
          [["full draft assoc"            #(assoc d :name "x")]
           ["  check-field! alone"        #(v/check-field! model :name "x")]
           ["  raw map assoc"             #(assoc backing :name "x")]
           ["  conj onto dirty set"       #(conj dirty :name)]
           ["  predicate call alone"      #(heist/non-blank-string? "x")]
           ["validated assoc (for ref)"   #(assoc h :name "x")]]]
    (println (format "%-30s %8.1f ns" label (ns-per f))))
  (println))
