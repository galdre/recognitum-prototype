;; extra: Object's equals in a type, refused: a value's equality and hash are its map's.
;; expect {:report [{:at [7 3] :level :error :names "equals" :tool :compiler}]
;;         :cold :same}
(ns extra.e17-interface (:require [recognitum.core :as rec]))
(rec/defproven Note [title]
  rec/checks (title [] (string? title))
  Object (equals [_ _o] true))
