;; extra: a method body that calls a name naming nothing.
;; expect {:report [{:at [8 26] :level :error :names "titel" :kondo "Unresolved symbol: titel"}]
;;         :cold :same}
(ns extra.e18-method-body-unresolved (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(rec/defproven Note [title]
  rec/checks (title [] (string? title))
  Describe (describe [_] titel))
