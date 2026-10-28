;; mistake 17: waits that form a cycle, through a bundle check the type overrides.
;; expect {:report [{:at [11 4] :level :error :names "moves"
;;                   :kondo "the waits form a cycle: moves -> perms/may-write -> moves"}]
;;         :cold :same}
(ns mistakes.m17a (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defproven Card [id collection-id ^:optional acting-user]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)
  rec/checks
  (moves [perms/may-write] true)
  ^:override (perms/may-write [moves] true))
