;; extra: the old :prefix form of rec/defcopy. The stock arity linter reports it,
;; from the library's arglist [new-name bundle names].
;; expect {:report [{:at [7 1] :level :error :names "defcopy"
;;                   :kondo "recognitum.core/defcopy is called with 4 args but expects 3"}]
;;         :cold :same}
(ns extra.e06-copy-prefix-form (:require [recognitum.core :as rec] [app.perms :as perms]))
(rec/defcopy current-collection-write perms/collection-write :prefix current-)
