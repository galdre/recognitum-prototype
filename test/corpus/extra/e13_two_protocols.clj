;; extra: a type that implements two protocols, one method with two arities.
;; expect {:report [] :cold :same}
(ns extra.e13-two-protocols (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(defprotocol Measure (weigh [x] [x unit]))
(rec/defproven Crate [label weight]
  rec/checks
  (weight [] (pos-int? weight))
  Describe
  (describe [_] label)
  Measure
  (weigh [_] weight)
  (weigh [this unit] (if (= unit :g) (* 1000 (weigh this)) weight)))
