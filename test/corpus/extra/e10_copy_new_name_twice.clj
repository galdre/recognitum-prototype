;; extra: a rec/defcopy map that gives two checks the same new name.
;; expect {:report [{:at [8 22] :level :error :names "twice-check"
;;                   :kondo "twice-check is named twice in the rec/defcopy map"}]
;;         :cold :same}
(ns extra.e10-copy-new-name-twice (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy twice-collection-write perms/collection-write
  {collection-exists twice-check
   may-write         twice-check})
