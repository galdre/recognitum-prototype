;; mistake 10: rec/fetch used as a value outside any check body.
;; Left to the compiler ("Can't take value of a macro"), as for any macro.
;; expect {:report [{:at [7 38] :level :error :names "rec/fetch" :tool :compiler}]
;;         :kondo []
;;         :cold []}
(ns mistakes.m10b (:require [recognitum.core :as rec] [app.db :as db]))
(defn lookup-all [ids] (map (partial rec/fetch db/collections) ids))
