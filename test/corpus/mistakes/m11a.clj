;; mistake 11: rec/extant? on a required field (a warning).
;; expect {:report [{:at [8 35] :level :warning :names "name"
;;                   :kondo "name is a required field: it is always extant"}]
;;         :cold :same}
(ns mistakes.m11a (:require [recognitum.core :as rec]))
(rec/defproven Heist [name crew take cuts ^:optional getaway]
  rec/checks
  (named [] (boolean (rec/extant? name))))
