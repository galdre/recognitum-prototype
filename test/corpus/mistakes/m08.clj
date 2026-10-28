;; mistake 8: rec/fetch used as a value in a check body.
;; expect {:report [{:at [8 47] :level :error :names "rec/fetch"
;;                   :kondo "rec/fetch is a macro: use it only in call position"}]
;;         :cold :same}
(ns mistakes.m08 (:require [recognitum.core :as rec] [crew.house :as house]))
(rec/defproven Heist [name crew take cuts]
  rec/checks
  (crew-vetted [] (every? true? (map (partial rec/fetch house/vetted) crew))))
