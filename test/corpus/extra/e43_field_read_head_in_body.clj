;; extra: a check body writes the head the macros write for a field read: refused, and the hook reports it.
;; expect {:report [{:at [8 28] :level :error :names "&field is a name the check engine reserves"
;;                   :kondo "&field is a name the check engine reserves for its reads: a check body may not write it, as a call, a value, a local or a var. To read, write the field's name"}
;;                  {:at [8 28] :level :error :names "&field is a name the check engine reserves" :kondo "Unresolved symbol: &field"}]
;;         :cold :same}
(ns extra.e43-field-read-head-in-body (:require [recognitum.core :as rec]))
(rec/defproven Note [x]
  rec/checks (c [] (some? (&field ::x))))
