;; mistake 13: a rec/defcopy map that names a check the bundle lacks.
;; expect {:report [{:at [10 4] :level :error :names "may-wrte"
;;                   :kondo "bundle perms/collection-write has no check may-wrte"}
;;                  {:at [9 3] :level :error :names "may-write"
;;                   :kondo "no new name for check may-write of bundle perms/collection-write"}]
;;         :cold []}
(ns mistakes.m13b (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy other-collection-write perms/collection-write
  {collection-exists other-collection-exists
   may-wrte          other-may-write})
