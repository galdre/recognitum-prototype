;; usage: the app.perms namespace of the example program.
;; expect {:report []}
(ns app.perms
  (:require [recognitum.core :as rec] [app.db :as db]))

(rec/defconcern permission "Checks on the acting user's rights.")

(rec/defchecks collection-write [user collection-id]
  rec/checks
  (collection-exists []
    "the collection is on file"
    (some? (rec/fetch db/collections collection-id)))

  ^::permission
  (may-write [collection-exists]
    "the acting user may write to the collection"
    (db/can-write? user (rec/fetch db/collections collection-id))))   ; user absent: throws, so fails

(rec/defcopy current-collection-write collection-write
  {collection-exists current-collection-exists
   may-write         current-may-write})
