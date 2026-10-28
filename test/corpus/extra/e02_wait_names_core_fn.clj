;; extra: a bare wait that names no own check, but names clojure.core/count.
;; expect {:report [{:at [9 9] :level :error :names "count"
;;                   :kondo "count is not a check of this type"}]
;;         :cold :same}
(ns extra.e02-wait-names-core-fn (:require [recognitum.core :as rec]))
(rec/defproven T [a]
  rec/checks
  (one [] true)
  (two [count] true))
