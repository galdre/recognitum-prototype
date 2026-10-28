;; mistake 11: rec/when-extant on a required field (a warning). An optional field is fine.
;; expect {:report [{:at [8 32] :level :warning :names "crew"
;;                   :kondo "crew is a required field: it is always extant"}]
;;         :cold :same}
(ns mistakes.m11b (:require [recognitum.core :as rec]))
(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (named-2 [] (rec/when-extant crew (set? crew)))
  (getaway-ok [] (rec/when-extant getaway (string? getaway))))
