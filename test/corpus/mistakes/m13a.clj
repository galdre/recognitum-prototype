;; mistake 13: a rec/defcopy map that leaves out a check of the bundle.
;; expect {:report [{:at [7 3] :level :error :names "collection-exists"
;;                   :kondo "no new name for check collection-exists of bundle perms/collection-write"}]
;;         :cold []}
(ns mistakes.m13a (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy half-collection-write perms/collection-write
  {may-write half-may-write})
