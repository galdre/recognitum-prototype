;; extra: a bare wait on a bundle of the same namespace; the refusal names the qualified wait.
;; expect {:report [{:at [12 10] :level :error :names "extra.e19-bare-bundle-wait/shape" :tool :compiler}
;;                  {:at [12 10] :level :error :names "shape" :kondo "shape is not a check of this type"}]
;;         :cold :same}
(ns extra.e19-bare-bundle-wait (:require [recognitum.core :as rec]))
(rec/defchecks shape [t]
  rec/checks
  (named [] (string? t)))
(rec/defproven Note [title]
  shape (t [] title)
  rec/checks
  (tidy [shape] (not= "" title)))
