;; extra: a definition with protocol implementations inside another form. The macros accept it and
;; define it as a top-level one, and the hook reports nothing.
;; expect {:report []
;;         :cold :same}
(ns extra.e26-protocols-not-top-level (:require [recognitum.core :as rec]))
(defprotocol Describe (describe [x]))
(when true
  (rec/defproven Note [title]
    rec/checks (title [] (string? title))
    Describe (describe [_] title)))
