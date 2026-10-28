;; extra: a protocol after a bundle binding.
;; expect {:report [] :cold :same}
(ns extra.e14-protocol-after-binding (:require [recognitum.core :as rec] [app.perms :as perms]))
(defprotocol Owned (owner [x]))
(rec/defproven Card [id collection-id ^:optional acting-user]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)
  Owned
  (owner [_] acting-user))
