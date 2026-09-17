(ns recognitum.bench
  "A hand-rolled `defrecord` with the same validation against the library, sharing one set
  of field predicates. The invariant validations are written twice: the hand-rolled side
  reads its fields out of a map, the library's are passed the fields they named, and that
  difference is part of what is measured. `recognitum.example-test` asserts that both sides
  reach the same result on the same values. Nothing here fetches: a fetch would dominate
  every row."
  (:require
   [clojure.string :as str]
   [criterium.core :as c]
   [recognitum.core :as rec :refer [drafting]]))

;;; ---- the validation code, shared by both sides ----

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(defn cuts-add-up? [h] (= (:take h) (reduce + 0 (vals (:cuts h)))))
(defn only-crew?   [h] (every? (:crew h) (keys (:cuts h))))

(def field-preds {:name non-blank-string? :crew crew-set? :take non-neg-int? :cuts cuts-map?})

(def invariant-index
  "Field -> the invariant validations reading it, written out by hand; the macro derives the
  same index from the arglists."
  {:take [cuts-add-up?] :cuts [cuts-add-up? only-crew?] :crew [only-crew?]})

;;; ---- the protocol both sides carry ----

(defprotocol Job
  (total-payout [h]    "What the cuts come to.")
  (rename       [h nm] "Retitle the job."))

;;; ---- library version ----

(rec/defrecord BenchHeist
  [name crew take cuts]
  {:checks
   {::name non-blank-string?
    ::crew crew-set?
    ::take non-neg-int?
    ::cuts cuts-map?
    ::cuts-add-up ("cuts add up" [take cuts] (= take (reduce + 0 (vals cuts))))
    ::only-crew   ("only crew"   [crew cuts] (every? crew (keys cuts)))}}

  Job
  (total-payout [h]    (reduce + 0 (vals (::cuts h))))
  (rename       [h nm] (assoc h ::name nm)))

;;; ---- hand-rolled version ----

(defn- bad! [k] (throw (ex-info "invalid" {:k k})))

(defn m-assoc [r k v]
  (when-not ((field-preds k) v) (bad! k))
  (let [r' (assoc r k v)]
    (doseq [p (invariant-index k)] (when-not (p r') (bad! k)))
    r'))

(defrecord PlainHeist [name crew take cuts]
  Job
  (total-payout [h]    (reduce + 0 (vals (:cuts h))))
  ;; The library's `rename` runs the checks reading `:name`; `m-assoc` is that same
  ;; selection, written out.
  (rename       [h nm] (m-assoc h :name nm)))

(defn m-merge [r kvs]
  (doseq [[k v] kvs] (when-not ((field-preds k) v) (bad! k)))
  (let [r' (into r kvs)]
    (doseq [p (distinct (mapcat invariant-index (keys kvs)))] (when-not (p r') (bad! nil)))
    r'))

(defn m-validate!
  "The hand-rolled `seal`: the shape validations of the keys written, then the invariant
  validations reading them. The library selects the same checks from the draft's record of
  which checks pass."
  [r dirty]
  (doseq [k dirty] (when-not ((field-preds k) (get r k)) (bad! k)))
  (doseq [p (distinct (mapcat invariant-index dirty))] (when-not (p r) (bad! nil)))
  r)

;;; ---- fixtures ----

(def valid-cuts {:charlie 1000 :lyle 1000 :left-ear 1000})
(def base  {:name "The Italian Job" :crew #{:charlie :lyle :left-ear}
            :take 3000 :cuts valid-cuts})
(def qbase {::name "The Italian Job" ::crew #{:charlie :lyle :left-ear}
            ::take 3000 ::cuts valid-cuts})
(def r (map->PlainHeist base))
(def h (drafting [x BenchHeist] (into x qbase)))

(defn- ns-per [f] (* 1e9 (first (:mean (c/quick-benchmark (f) {:verbose false})))))

(defn- ns-per-read
  "A single field read runs in a couple of nanoseconds, where call overhead and rounding swamp
  the thing being measured. Take a hundred of them, accumulating into a long so the JIT cannot
  drop the read as dead code, and divide."
  [f]
  (/ (ns-per (fn [] (loop [i 0 acc 0] (if (< i 100) (recur (inc i) (+ acc (long (f)))) acc))))
     100.0))

(defn- line [label a b] (println (format "%-40s %10.1f %10.1f %8.1fx" label a b (/ b a))))

;;; ---- batch helpers ----

(defn- m-add [r i]
  (let [who (keyword (str "m" i))]
    (-> r (assoc :crew (conj (:crew r) who))
          (assoc :cuts (assoc (:cuts r) who 10))
          (assoc :take (+ (:take r) 10)))))

(defn- q-add [r i]
  (let [who (keyword (str "m" i))]
    (-> r (assoc ::crew (conj (::crew r) who))
          (assoc ::cuts (assoc (::cuts r) who 10))
          (assoc ::take (+ (::take r) 10)))))

(defn run []
  (println (format "%-40s %10s %10s %8s" "" "manual" "library" "ratio"))
  (line "read :take"            (ns-per-read #(:take r))        (ns-per-read #(::take h)))
  (line "write :name (0 invariant validations)" (ns-per #(m-assoc r :name "x"))
                                (ns-per #(assoc h ::name "x")))
  (line "write :cuts (2 invariant validations)" (ns-per #(m-assoc r :cuts valid-cuts))
                                (ns-per #(assoc h ::cuts valid-cuts)))
  (line "merge 2 keys"          (ns-per #(m-merge r {:take 3000 :cuts valid-cuts}))
                                (ns-per #(merge h {::take 3000 ::cuts valid-cuts})))
  (line "construct"             (ns-per #(m-validate! (map->PlainHeist base) [:name :crew :take :cuts]))
                                (ns-per #(drafting [x BenchHeist] (into x qbase))))
  (line "read method on a sealed value" (ns-per #(total-payout r)) (ns-per #(total-payout h)))
  (line "write method on a sealed value" (ns-per #(rename r "x")) (ns-per #(rename h "x")))
  (println)
  (println (format "%-40s %10s %10s %8s" "batch: N renames" "manual" "library" "ratio"))
  (doseq [n [1 3 5 10 15]]
    (let [is (vec (range n))]
      (line (str "  N=" n)
            (ns-per #(m-validate! (reduce (fn [x i] (assoc x :name (str "n" i))) r is) [:name]))
            (ns-per #(drafting [x h] (reduce (fn [x i] (assoc x ::name (str "n" i))) x is))))))
  (println)
  (println (format "%-40s %10s %10s %8s" "batch: N add-members" "manual" "library" "ratio"))
  (doseq [n [1 3 5 10 15]]
    (let [is (vec (range n))]
      (line (str "  N=" n)
            (ns-per #(m-validate! (reduce m-add r is) [:crew :cuts :take]))
            (ns-per #(drafting [x h] (reduce q-add x is))))))
  (println))
