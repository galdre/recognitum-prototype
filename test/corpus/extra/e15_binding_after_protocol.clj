;; extra: a bundle binding after a protocol.
;; expect {:report [{:at [9 3] :level :error :names "perms/collection-write"
;;                   :kondo "perms/collection-write follows the protocol Owned: the checks sections, rec/checks and the bundle bindings, come before the protocols"}]
;;         :cold :same}
(ns extra.e15-binding-after-protocol (:require [recognitum.core :as rec] [app.perms :as perms]))
(defprotocol Owned (owner [x]))
(rec/defproven Card [id collection-id ^:optional acting-user]
  Owned (owner [_] acting-user)
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id))
