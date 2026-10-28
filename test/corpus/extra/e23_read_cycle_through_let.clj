;; extra: a shape check waits on a check that reads its field: inside a let, a case or a letfn. The hook does not read check bodies for cycles: the macros refuse it at load.
;; expect {:report [{:at [18 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}
;;                  {:at [22 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}
;;                  {:at [26 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}
;;                  {:at [30 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}
;;                  {:at [34 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}
;;                  {:at [38 4] :level :error :names "title"
;;                   :kondo "the waits form a cycle: title -> short-title -> title (a check waits on the shape checks of the fields it reads)"}]
;;         :kondo [] :cold :same}
(ns extra.e23-read-cycle-through-let (:require [recognitum.core :as rec]))
(rec/defproven Note [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (let [n (count title)] (< n 80))))
(rec/defproven Note2 [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (let [n 80] (< (count title) n))))
(rec/defproven Note3 [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (case 1 1 (some? title) false)))
(rec/defproven Note4 [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (case 0 1 false (some? title))))
(rec/defproven Note5 [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (letfn [(f [] true)] (and (f) (some? title)))))
(rec/defproven Note6 [title]
  rec/checks
  (title [short-title] (string? title))
  (short-title [] (case 0 1 false 2 (some? title) false)))
