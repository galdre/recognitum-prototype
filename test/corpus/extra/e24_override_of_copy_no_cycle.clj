;; extra: a check waits on a whole bundle, and an override of its copy's check waits on that check: no cycle.
;; expect {:report []
;;         :cold :same}
(ns extra.e24-override-of-copy-no-cycle (:require [recognitum.core :as rec]))
(rec/defchecks named [label]
  rec/checks (titled [] (string? label)))
(rec/defcopy renamed named {titled retitled})
(rec/defproven Note [title subtitle]
  named (label [] title)
  renamed (label [] subtitle)
  rec/checks
  (after-named [extra.e24-override-of-copy-no-cycle/named] (some? title))
  ^:override (extra.e24-override-of-copy-no-cycle/retitled [after-named] (string? subtitle)))
