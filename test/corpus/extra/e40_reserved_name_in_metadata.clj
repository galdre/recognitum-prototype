;; extra: a check body writes the engine's read form in metadata: refused, as anywhere else in a body.
;; expect {:report [{:at [7 27] :level :error :names "&field is a name the check engine reserves" :kondo "Unresolved symbol: &field"}]
;;         :cold :same}
(ns extra.e40-reserved-name-in-metadata (:require [recognitum.core :as rec]))
(rec/defproven Note [x]
  rec/checks
  (c [] (= 1 (count ^{:m (&field ::x)} [1]))))
