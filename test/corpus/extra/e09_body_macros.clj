;; extra: a user macro in a check body, and reads inside threading and let forms.
;; expect {:report [] :cold :same}
(ns extra.e09-body-macros (:require [recognitum.core :as rec] [app.db :as db]))
(defmacro unless-blank [s & body] `(if (seq ~s) (do ~@body) false))
(rec/defproven Note [title collection-id]
  rec/checks
  (title [] (unless-blank title (string? title)))
  (collection-on-file []
    (-> (rec/fetch db/collections collection-id) some?))
  (owner-known []
    (let [c (rec/fetch db/collections collection-id)
          u (rec/fetch db/users (:owner-id c))]
      (some? u))))
