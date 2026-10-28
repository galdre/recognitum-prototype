;; mistake 3: a misspelled concern on a clause, the concern in the same namespace.
;; expect {:report [{:at [9 4] :level :error :names "permision"
;;                   :kondo "Unresolved symbol: permision"}]
;;         :cold :same}
(ns mistakes.m03a (:require [recognitum.core :as rec] [app.db :as db]))
(rec/defconcern permission "Checks on the acting user's rights.")
(rec/defchecks collection-write [user collection-id]
  rec/checks
  ^::permision
  (may-write [] (db/can-write? user collection-id)))
