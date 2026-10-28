;; mistake 17: waits that form a cycle, through a wait on a whole bundle.
;; expect {:report [{:at [11 4] :level :error :names "moves"
;;                   :kondo "the waits form a cycle: moves -> perms/collection-write -> perms/may-write -> moves"}]
;;         :cold []}
(ns mistakes.m17c (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defproven Card [id collection-id ^:optional acting-user]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)
  rec/checks
  (moves [perms/collection-write] true)
  ^:override (perms/may-write [moves] true))
