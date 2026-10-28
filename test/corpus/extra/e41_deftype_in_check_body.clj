;; extra: a check body defines a deftype and passes it a field: its methods read their own field and params, and the check reads x where it builds the value.
;; expect {:report []
;;         :cold :same}
(ns extra.e41-deftype-in-check-body (:require [recognitum.core :as rec]))
(definterface Pair (pair [a]))
(rec/defproven Note [x]
  rec/checks
  (c [] (let [p (do (deftype Local [q] Pair (pair [_ a] [a q]))
                    (extra.e41_deftype_in_check_body.Local. x))]
          (= [0 x] (.pair ^Pair p 0)))))
