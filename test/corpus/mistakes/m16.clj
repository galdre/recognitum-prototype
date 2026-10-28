;; mistake 16: a bundle bound twice in one type.
;; expect {:report [{:at [10 3] :level :error :names "perms/collection-write"
;;                   :kondo "bundle perms/collection-write is bound twice in this type"}]
;;         :cold :same}
(ns mistakes.m16 (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defproven Card [id collection-id ^:optional acting-user ^:optional stored-collection-id]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] stored-collection-id))
