;; mistake 17: waits that form a cycle among the checks of a bundle.
;; expect {:report [{:at [8 4] :level :error :names "collection-exists"
;;                   :kondo "the waits form a cycle: collection-exists -> may-write -> collection-exists"}]
;;         :cold :same}
(ns mistakes.m17b (:require [recognitum.core :as rec] [app.db :as db]))
(rec/defchecks collection-write [user collection-id]
  rec/checks
  (collection-exists [may-write] (some? (rec/fetch db/collections collection-id)))
  (may-write [collection-exists] (db/can-write? user (rec/fetch db/collections collection-id))))
