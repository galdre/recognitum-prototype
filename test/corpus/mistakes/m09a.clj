;; mistake 9: rec/fetch inside a fn literal in a check body.
;; expect {:report [{:at [8 27] :level :error :names "rec/fetch"
;;                   :kondo "rec/fetch inside a fn the check body builds: it runs outside the check"}]
;;         :cold :same}
(ns mistakes.m09a (:require [recognitum.core :as rec] [crew.house :as house]))
(rec/defproven Heist [name crew take cuts]
  rec/checks
  (crew-vetted [] (every? #(rec/fetch house/vetted %) crew)))
