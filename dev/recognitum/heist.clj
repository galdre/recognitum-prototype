(ns recognitum.heist
  "A heist: a crew, a take, and how the take is split.

  Two cross-field invariants, both of which a single `assoc` necessarily breaks —
  which is the whole reason drafts exist."
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec :refer [drafting]]))

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(rec/defrecord Heist
  [^{:valid non-blank-string?} name
   ^{:valid crew-set?}         crew
   ^{:valid non-neg-int?}      take
   ^{:valid cuts-map?}         cuts]
  [{:doc  "the cuts add up to the take"
    :keys #{:take :cuts}
    :pred (fn [h] (= (:take h) (reduce + 0 (vals (:cuts h)))))}
   {:doc  "only crew get a cut"
    :keys #{:crew :cuts}
    :pred (fn [h] (every? (:crew h) (keys (:cuts h))))}])

(def italian-job
  (drafting [h Heist]
    (merge h {:name "The Italian Job"
              :crew #{:charlie :lyle :left-ear}
              :take 3000
              :cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})))

;;; Impl-layer fns: take a draft, return a draft. None of them seal.

(defn add-member
  "Put `who` on the crew with `cut`, funded by shrinking everyone else proportionally
  is too clever for a prototype — so just raise the take."
  [h who cut]
  (-> h
      (assoc :crew (conj (:crew h) who))
      (assoc :cuts (assoc (:cuts h) who cut))
      (assoc :take (+ (:take h) cut))))

(defn rename [h s] (assoc h :name s))

(defn apply-changeset
  [h {:keys [op] :as cs}]
  (case op
    :add-member (add-member h (:who cs) (:cut cs))
    :rename     (rename h (:name cs))))

(defn promote
  "Uses `drafting` internally, so it seals when called standalone and passes the draft
  straight through when called inside another `drafting`."
  [h who bonus]
  (drafting [x h]
    (-> x
        (assoc :cuts (update (:cuts x) who + bonus))
        (assoc :take (+ (:take x) bonus)))))
