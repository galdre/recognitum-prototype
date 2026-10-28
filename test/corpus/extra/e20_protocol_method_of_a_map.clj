;; extra: a protocol method that a map already has, size: refused when defproven expands, as
;; defrecord refuses it, the message naming rename and extend-protocol as the remedies.
;; expect {:report [{:at [7 1] :level :error :names "size" :tool :compiler}]
;;         :cold :same}
(ns extra.e20-protocol-method-of-a-map (:require [recognitum.core :as rec]))
(defprotocol Measure (size [x]))
(rec/defproven Crate [label weight]
  rec/checks
  (weight [] (pos-int? weight))
  Measure
  (size [_] weight))
