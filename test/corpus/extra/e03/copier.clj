;; extra: a copy of a bundle from another namespace. Clean warm and cold.
;; expect {:report [] :cold :same}
(ns extra.e03.copier (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy stored-collection-write perms/collection-write
  {collection-exists stored-collection-exists
   may-write         stored-may-write})
