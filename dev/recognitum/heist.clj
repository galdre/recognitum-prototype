(ns recognitum.heist
  "The worked example: a heist, its crew, the take and how it is split, and what the house
  has on file about the people taking it."
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec]
   [recognitum.house :as house]))

(defn non-blank? [s] (and (string? s) (not (str/blank? s))))
(defn crew-set? [x] (and (set? x) (seq x) (every? string? x)))
(defn non-neg-int? [x] (and (int? x) (not (neg? x))))
(defn cuts-map? [x] (and (map? x) (every? string? (keys x)) (every? non-neg-int? (vals x))))
(defn sums-to [total cuts] (= total (reduce + (vals cuts))))

(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (name    [] "the job has a name" (non-blank? name))
  (crew    [] "the crew is a set of names" (crew-set? crew))
  (take    [] "the take is a whole number" (non-neg-int? take))
  (cuts    [] "each cut is a whole number" (cuts-map? cuts))
  (getaway [] "the getaway driver is named" (non-blank? getaway))

  (cuts-sum-to-take []
    "the cuts add up to the take"
    (sums-to take cuts))
  (only-crew-get-cuts [cuts-sum-to-take]
    "only the crew gets a cut"
    (every? crew (keys cuts)))
  (crew-vetted []
    "the house has vetted everyone on the crew"
    (every? true? (vals (rec/fetch-each house/vetted crew))))
  (getaway-not-crew []
    "the driver stays out of the vault"
    (rec/when-extant getaway (not (contains? crew getaway)))))

(def italian-job
  (map->Heist {::name "The Italian Job"
               ::crew #{"Charlie" "Stella" "Lyle"}
               ::take 3000
               ::cuts {"Charlie" 1000 "Stella" 1000 "Lyle" 1000}}))

(defn plan-from-form
  "A Heist from a form filled in field by field: any write is accepted until the seal."
  [partial-plan]
  (-> (rec/draft Heist) (into partial-plan) (rec/seal)))
