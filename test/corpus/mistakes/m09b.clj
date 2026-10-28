;; mistake 9: rec/fetch inside a fn form in a check body.
;; expect {:report [{:at [8 35] :level :error :names "rec/fetch"
;;                   :kondo "rec/fetch inside a fn the check body builds: it runs outside the check"}]
;;         :cold :same}
(ns mistakes.m09b (:require [recognitum.core :as rec] [crew.house :as house]))
(rec/defproven Heist [name crew take cuts]
  rec/checks
  (crew-vetted [] (every? (fn [c] (rec/fetch house/vetted c)) crew)))
