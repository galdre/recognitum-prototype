;; extra: a check reads a field only in a destructuring default, and the field's shape check waits on it. The hook does not read check bodies for cycles: the macros refuse it at load.
;; expect {:report [{:at [8 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}]
;;         :kondo [] :cold :same}
(ns extra.e38-read-cycle-through-or-default (:require [recognitum.core :as rec]))
(rec/defproven Note [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (let [{:keys [t] :or {t title}} {}] (< (count t) 80))))
