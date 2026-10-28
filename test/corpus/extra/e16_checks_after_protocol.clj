;; extra: rec/checks after a protocol.
;; expect {:report [{:at [9 3] :level :error :names "rec/checks"
;;                   :kondo "rec/checks follows the protocol Describe: the checks sections, rec/checks and the bundle bindings, come before the protocols"}]
;;         :cold :same}
(ns extra.e16-checks-after-protocol (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(rec/defproven Note [title]
  Describe (describe [_] title)
  rec/checks
  (title [] (string? title)))
