;; usage: the crew.heist namespace of the example program.
;; expect {:report []}
(ns crew.heist
  (:require [recognitum.core :as rec] [crew.house :as house]))

;; helpers the checks use
(defn non-blank? [s] (and (string? s) (some? (re-find #"\S" s))))
(defn crew-set? [x] (and (set? x) (every? string? x)))
(defn non-neg-int? [x] (and (int? x) (not (neg? x))))
(defn cuts-map? [x] (and (map? x) (every? non-neg-int? (vals x))))
(defn sums-to [total cuts] (= total (reduce + (vals cuts))))

;; instance? is the whole proof.
(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (name    [] (non-blank? name))
  (crew    [] (crew-set? crew))
  (take    [] (non-neg-int? take))
  (cuts    [] (cuts-map? cuts))
  (getaway [] (string? getaway))

  (cuts-sum-to-take [] (sums-to take cuts))
  (only-crew-get-cuts [cuts-sum-to-take] (every? crew (keys cuts)))
  (crew-vetted []                                            ; many keys, one read
    (every? true? (vals (rec/fetch-each house/vetted crew))))
  (getaway-not-crew []
    (rec/when-extant getaway (not (contains? crew getaway)))))

(defn plan-from-form [partial-plan]
  (-> (rec/draft Heist) (into partial-plan) (rec/seal)))      ; partial values through a draft
