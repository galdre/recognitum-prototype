;; extra: a shape check waits on a check that reads its field only inside a for. The hook does not read check bodies for cycles: the macros refuse it at load.
;; expect {:report [{:at [8 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}]
;;         :kondo [] :cold :same}
(ns extra.e30-read-cycle-through-for (:require [recognitum.core :as rec]))
(rec/defproven Note [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (every? some? (for [x [1]] [x title]))))
