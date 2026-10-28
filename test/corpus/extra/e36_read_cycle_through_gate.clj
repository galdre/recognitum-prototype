;; extra: a check waits on a bundle bound with a gate on a field whose shape check waits on that check.
;; expect {:report [{:at [11 4] :level :error :names "subtitle"
;;                   :kondo "the waits form a cycle: subtitle -> after-named -> named -> titled -> subtitle (a check waits on the shape checks of the fields it reads)"}]
;;         :cold :same}
(ns extra.e36-read-cycle-through-gate (:require [recognitum.core :as rec]))
(rec/defchecks named [label]
  rec/checks (titled [] (string? label)))
(rec/defproven Note [title ^:optional subtitle]
  ^{:when-extant subtitle} named (label [] title)
  rec/checks
  (subtitle [after-named] (string? subtitle))
  (after-named [extra.e36-read-cycle-through-gate/named] true))
