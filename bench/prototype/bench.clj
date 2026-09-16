(ns prototype.bench
  (:require
   [criterium.core :as c]
   [prototype.heist :as heist]
   [prototype.validated-record :as v :refer [draft revising seal]])
  (:import
   (prototype.heist Heist)))

(defrecord PlainHeist [name crew take cuts])

(def ^:private base
  {:name "The Italian Job" :crew #{:charlie :lyle :left-ear}
   :take 3000 :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})

(def m   base)
(def r   (map->PlainHeist base))
(def h   heist/italian-job)
(def d   (draft h))

(defn- ns-per [f]
  (let [res (c/quick-benchmark (f) {:verbose false})]
    (* 1e9 (first (:mean res)))))

(defn- row [label f] [label (ns-per f)])

(defn run []
  (let [results
        [(row "read  :take           plain map"  #(:take m))
         (row "read  :take           defrecord"  #(:take r))
         (row "read  :take           Heist"      #(:take h))

         (row "write :name           plain map"  #(assoc m :name "x"))
         (row "write :name           defrecord"  #(assoc r :name "x"))
         (row "write :name           Heist"      #(assoc h :name "x"))
         (row "write :name           Draft"      #(assoc d :name "x"))

         (row "merge 2 keys          plain map"  #(merge m {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}}))
         (row "merge 2 keys          defrecord"  #(merge r {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}}))
         (row "merge 2 keys          Heist"      #(merge h {:take 4000 :cuts {:charlie 2000 :lyle 1000 :left-ear 1000}}))

         (row "construct             defrecord"  #(map->PlainHeist base))
         (row "construct             Heist"      #(revising [x Heist] (merge x base)))

         (row "revising, 0 writes    Heist"      #(revising [x h] x))
         (row "revising, 3 changeset Heist"      #(revising [x h]
                                                    (reduce heist/apply-changeset x
                                                            [{:op :add-member :who :a :cut 10}
                                                             {:op :add-member :who :b :cut 20}
                                                             {:op :rename :name "n"}])))
         (row "same 3 ops            defrecord"  #(-> r
                                                      (assoc :crew (conj (:crew r) :a))
                                                      (assoc :cuts (assoc (:cuts r) :a 10))
                                                      (assoc :take (+ (:take r) 10))
                                                      (assoc :crew (conj (:crew r) :b))
                                                      (assoc :cuts (assoc (:cuts r) :b 20))
                                                      (assoc :take (+ (:take r) 20))
                                                      (assoc :name "n")))

         (row "seal only             Heist"      #(seal d))]]
    (println)
    (doseq [[label ns] results]
      (println (format "%-34s %10.1f ns" label ns)))
    (println)))
