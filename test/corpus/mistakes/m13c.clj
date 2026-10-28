;; mistake 13: a rec/defcopy map that names a check twice.
;; expect {:report [{:at [9 4] :level :error :names "may-write"
;;                   :kondo "may-write is named twice in the rec/defcopy map"}]
;;         :cold :same}
(ns mistakes.m13c (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy twice-collection-write perms/collection-write
  {collection-exists twice-collection-exists
   may-write         twice-may-write
   may-write         twice-may-write-2})
