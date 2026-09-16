(ns prototype.bench3
  (:require
   [criterium.core :as c]
   [prototype.heist :as heist]
   [prototype.validated-record :as v :refer [revising]])
  (:import
   (prototype.heist Heist)))

(defrecord PlainHeist [name crew take cuts])

(def base {:name "The Italian Job" :crew #{:charlie :lyle :left-ear}
           :take 3000 :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})
(def r (map->PlainHeist base))
(def h heist/italian-job)

(defn- ns-per [f] (* 1e9 (first (:mean (c/quick-benchmark (f) {:verbose false})))))

;; scalar field, cheap predicate
(defn- rename-draft [x i] (assoc x :name (str "n" i)))
(defn- rename-rec   [x i] (assoc x :name (str "n" i)))

;; collection fields, element-wise predicates, growing
(defn- add-draft [x i]
  (let [who (keyword (str "m" i))]
    (-> x (assoc :crew (conj (:crew x) who))
          (assoc :cuts (assoc (:cuts x) who 10))
          (assoc :take (+ (:take x) 10)))))
(defn- add-rec [x i]
  (let [who (keyword (str "m" i))]
    (-> x (assoc :crew (conj (:crew x) who))
          (assoc :cuts (assoc (:cuts x) who 10))
          (assoc :take (+ (:take x) 10)))))

(defn run []
  (println (format "%-6s %12s %12s %12s %12s" "N" "rename/rec" "rename/rev" "add/rec" "add/rev"))
  (doseq [n [1 3 5 10 15]]
    (let [is (vec (range n))]
      (println (format "%-6d %12.0f %12.0f %12.0f %12.0f"
                       n
                       (ns-per #(reduce rename-rec r is))
                       (ns-per #(revising [x h] (reduce rename-draft x is)))
                       (ns-per #(reduce add-rec r is))
                       (ns-per #(revising [x h] (reduce add-draft x is)))))))
  (println))
