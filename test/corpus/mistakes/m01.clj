;; mistake 1: a misspelled own check in a wait vector.
;; expect {:report [{:at [10 24] :level :error :names "cuts-sum-to-tak"
;;                   :kondo "Unresolved symbol: cuts-sum-to-tak"}]
;;         :cold :same}
(ns mistakes.m01 (:require [recognitum.core :as rec]))
(defn sums-to [total cuts] (= total (reduce + (vals cuts))))
(rec/defproven Heist [name crew take cuts]
  rec/checks
  (cuts-sum-to-take [] (sums-to take cuts))
  (only-crew-get-cuts [cuts-sum-to-tak] (every? crew (keys cuts))))
