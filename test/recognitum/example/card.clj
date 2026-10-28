(ns recognitum.example.card
  "The card and dashboard of the example application, as types whose constructors run every
  check."
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec]
   [recognitum.example.db :as db]
   [recognitum.example.perms :as perms]))

(defn non-blank? [s] (and (string? s) (not (str/blank? s))))

(rec/defproven Card [id name collection-id archived
                     ^:optional acting-user
                     ^:optional stored-collection-id
                     ^:optional stored-archived]

  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)

  ^{:when-extant stored-collection-id}
  perms/current-collection-write
  (user          [] acting-user)
  (collection-id [] stored-collection-id)

  rec/checks
  (name          [] (non-blank? name))
  (collection-id [] (pos-int? collection-id))
  (archived      [] (boolean? archived))

  (not-archived-on-edit []
    "an archived card is not edited"
    (or (not (rec/extant? stored-archived)) (not stored-archived)))

  ^::perms/permission
  (moves-only-where-allowed [perms/may-write perms/current-may-write]
    "a move needs write access to both collections"
    true)

  (owner-active [perms/collection-exists]
    "the collection's owner is active"
    (let [coll (rec/fetch db/collections collection-id)]
      (true? (:active? (rec/fetch db/users (:owner-id coll)))))))

(rec/defproven Dashboard [id title collection-id ^:optional acting-user]
  perms/collection-write
  (user          [] acting-user)
  (collection-id [] collection-id)
  rec/checks
  (title [] (non-blank? title))
  ^:override
  (perms/may-write [perms/collection-exists]
    "dashboards also allow the collection's curators"
    (let [coll (rec/fetch db/collections collection-id)]
      (or (db/can-write? acting-user coll) (db/can-curate? acting-user coll)))))
