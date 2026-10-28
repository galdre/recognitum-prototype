;; extra: a definition with protocol implementations inside a top-level do, which evaluates each form as a top-level form.
;; expect {:report []
;;         :cold :same}
(ns extra.e25-protocols-inside-do (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(do
  (rec/defproven Note [title]
    rec/checks (title [] (string? title))
    Describe (describe [_] title))
  (def described describe))
