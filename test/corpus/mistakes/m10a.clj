;; mistake 10: rec/fetch called outside a check body.
;; expect {:report [{:at [6 19] :level :error :names "rec/fetch"
;;                   :kondo "rec/fetch is allowed only inside a check body"}]
;;         :cold :same}
(ns mistakes.m10a (:require [recognitum.core :as rec] [app.db :as db]))
(defn lookup [id] (rec/fetch db/collections id))
