;; extra: a check body defines a deftype whose method reads a field: refused, since a method sees only its type's own fields and its params.
;; expect {:report [{:at [8 49] :level :error :names "x is used inside a method of deftype Local" :tool :compiler}]
;;         :cold :same}
(ns extra.e39-field-in-deftype-method (:require [recognitum.core :as rec]))
(definterface Pair (pair [a]))
(rec/defproven Note [x]
  rec/checks
  (c [] (do (deftype Local [q] Pair (pair [_ _] x)) true)))
