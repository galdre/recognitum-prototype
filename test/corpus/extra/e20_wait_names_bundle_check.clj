;; extra: a bare wait that names a bundle check this namespace defines.
;; expect {:report [{:at [11 17] :level :error :names "titled"
;;                   :kondo "titled is not a check of this type: a bundle check is waited on as [bundle titled]"}]
;;         :cold :same}
(ns extra.e20-wait-names-bundle-check (:require [recognitum.core :as rec]))
(rec/defchecks named [label]
  rec/checks (titled [] (string? label)))
(rec/defproven Note [title]
  named (label [] title)
  rec/checks
  (short-title [titled] (< (count title) 80)))
