;; mistake 7: a check body naming a field the type does not have.
;; expect {:report [{:at [8 21] :level :error :names "nmae"
;;                   :kondo "Unresolved symbol: nmae"}]
;;         :cold :same}
(ns mistakes.m07 (:require [recognitum.core :as rec]))
(rec/defproven Heist [name crew take cuts]
  rec/checks
  (name [] (string? nmae)))
