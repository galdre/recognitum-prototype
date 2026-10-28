;; extra: a type that implements one protocol after its checks; the methods see the fields.
;; expect {:report [] :cold :same}
(ns extra.e12-one-protocol (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]) (describe-with [x prefix]))
(rec/defproven Note [title ^:optional author]
  rec/checks
  (title [] (string? title))
  Describe
  (describe [_] (if author (str title " by " author) title))
  (describe-with [this prefix] (str prefix (describe this))))
