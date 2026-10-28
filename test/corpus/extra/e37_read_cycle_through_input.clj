;; extra: a check waits on a bundle check whose input clause reads a field whose shape check waits on that check. The hook does not read check bodies for cycles: the macros refuse it at load.
;; expect {:report [{:at [11 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> after-titled -> titled -> title (a check waits on the shape checks of the fields it reads)"}]
;;         :kondo [] :cold :same}
(ns extra.e37-read-cycle-through-input (:require [recognitum.core :as rec]))
(rec/defchecks named [label]
  rec/checks (titled [] (string? label)))
(rec/defproven Note [title]
  named (label [] title)
  rec/checks
  (title [after-titled] (string? title))
  (after-titled [extra.e37-read-cycle-through-input/titled] true))
