(ns prototype.bench
  "defrecord + hand-rolled validation vs. the library, sharing one set of predicates
  and invariants so the validation work is provably identical on both sides."
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [prototype.validated-record :refer [defvalidated revising]]))

;;; ---- the validation code, used by BOTH implementations ----

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(defn cuts-add-up? [h] (= (:take h) (reduce + 0 (vals (:cuts h)))))
(defn only-crew?   [h] (every? (:crew h) (keys (:cuts h))))

(def field-preds {:name non-blank-string? :crew crew-set? :take non-neg-int? :cuts cuts-map?})
(def inv-index   {:take [cuts-add-up?] :cuts [cuts-add-up? only-crew?] :crew [only-crew?]})
(def all-invs    [cuts-add-up? only-crew?])

;;; ---- library version ----

(defvalidated BenchHeist
  [^{:valid non-blank-string?} name
   ^{:valid crew-set?}         crew
   ^{:valid non-neg-int?}      take
   ^{:valid cuts-map?}         cuts]
  [{:doc "cuts add up" :keys #{:take :cuts} :pred cuts-add-up?}
   {:doc "only crew"   :keys #{:crew :cuts} :pred only-crew?}])

;;; ---- hand-rolled version ----

(defrecord PlainHeist [name crew take cuts])

(defn- bad! [k] (throw (ex-info "invalid" {:k k})))

(defn m-assoc [r k v]
  (when-not ((field-preds k) v) (bad! k))
  (let [r' (assoc r k v)]
    (doseq [p (inv-index k)] (when-not (p r') (bad! k)))
    r'))

(defn m-merge [r kvs]
  (doseq [[k v] kvs] (when-not ((field-preds k) v) (bad! k)))
  (let [r' (into r kvs)]
    (doseq [p (distinct (mapcat inv-index (keys kvs)))] (when-not (p r') (bad! nil)))
    r'))

(defn m-validate!
  "The hand-rolled `seal`: check the keys that were written, then their invariants."
  [r dirty]
  (doseq [k dirty] (when-not ((field-preds k) (get r k)) (bad! k)))
  (doseq [p (distinct (mapcat inv-index dirty))] (when-not (p r) (bad! nil)))
  r)

;;; ---- fixtures ----

(def base {:name "The Italian Job" :crew #{:charlie :lyle :left-ear}
           :take 3000 :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})
(def r (map->PlainHeist base))
(def h (revising [x BenchHeist] (into x base)))
(def valid-cuts {:charlie 1000 :lyle 1000 :left-ear 1000})

(defn- ns-per [f] (* 1e9 (first (:mean (c/quick-benchmark (f) {:verbose false})))))

(defn- ns-per-read
  "A single field read runs in a couple of nanoseconds, where call overhead and rounding swamp
  the thing being measured. Take a hundred of them, accumulating into a long so the JIT cannot
  drop the read as dead code, and divide."
  [f]
  (/ (ns-per (fn [] (loop [i 0 acc 0] (if (< i 100) (recur (inc i) (+ acc (long (f)))) acc))))
     100.0))

(defn- line [label a b] (println (format "%-34s %10.1f %10.1f %8.1fx" label a b (/ b a))))

;;; ---- batch helpers ----

(defn- m-add [r i]
  (let [who (keyword (str "m" i))]
    (-> r (assoc :crew (conj (:crew r) who))
          (assoc :cuts (assoc (:cuts r) who 10))
          (assoc :take (+ (:take r) 10)))))

(defn run []
  (println (format "%-34s %10s %10s %8s" "" "manual" "library" "ratio"))
  (line "read :take"            (ns-per-read #(:take r))        (ns-per-read #(:take h)))
  (line "write :name (0 invs)"  (ns-per #(m-assoc r :name "x")) (ns-per #(assoc h :name "x")))
  (line "write :cuts (2 invs)"  (ns-per #(m-assoc r :cuts valid-cuts))
                                (ns-per #(assoc h :cuts valid-cuts)))
  (line "merge 2 keys"          (ns-per #(m-merge r {:take 3000 :cuts valid-cuts}))
                                (ns-per #(merge h {:take 3000 :cuts valid-cuts})))
  (line "construct"             (ns-per #(m-validate! (map->PlainHeist base) [:name :crew :take :cuts]))
                                (ns-per #(revising [x BenchHeist] (into x base))))
  (println)
  (println (format "%-34s %10s %10s %8s" "batch: N renames" "manual" "library" "ratio"))
  (doseq [n [1 3 5 10 15]]
    (let [is (vec (range n))]
      (line (str "  N=" n)
            (ns-per #(m-validate! (reduce (fn [x i] (assoc x :name (str "n" i))) r is) [:name]))
            (ns-per #(revising [x h] (reduce (fn [x i] (assoc x :name (str "n" i))) x is))))))
  (println)
  (println (format "%-34s %10s %10s %8s" "batch: N add-members" "manual" "library" "ratio"))
  (doseq [n [1 3 5 10 15]]
    (let [is (vec (range n))]
      (line (str "  N=" n)
            (ns-per #(m-validate! (reduce m-add r is) [:crew :cuts :take]))
            (ns-per #(revising [x h] (reduce m-add x is))))))
  (println))
