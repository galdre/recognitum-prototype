;; extra: a type that implements a protocol, an interface and Object's toString, mixed.
;; expect {:report [] :cold :same}
(ns extra.e21-interfaces (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(rec/defproven Note [title ^:optional author]
  rec/checks
  (title [] (string? title))
  Describe
  (describe [_] title)
  Comparable
  (compareTo [_ other] (compare title (::title other)))
  Object
  (toString [_] (str "Note " title)))
