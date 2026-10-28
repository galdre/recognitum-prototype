;; usage: a binding gated on an optional field that none of its input clauses reads.
;; expect {:report [] :cold :same}
(ns app.gated-card (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defproven Card [id collection-id ^:optional acting-user ^:optional stored-archived]
  ^{:when-extant stored-archived}
  perms/current-collection-write
  (user          [] acting-user)
  (collection-id [] collection-id))
