;; extra: a shape check waits on checks whose bodies bind locals named after its field, or quote it: no read, no cycle.
;; expect {:report []
;;         :cold :same}
(ns extra.e22-shadowed-field-is-no-read (:require [recognitum.core :as rec]))
(rec/defproven Note [title body]
  rec/checks
  (title [by-let by-fn by-case by-catch by-for by-letfn-param by-letfn-name by-quote by-quote-call]
    (string? title))
  (by-let [] (let [title (str body)] (string? title)))
  (by-fn [] (every? (fn [title] (some? title)) [body]))
  (by-case [] (case body title 1 2))
  (by-catch [] (try (some? body) (catch Exception title (str body) (some? title))))
  (by-for [] (every? some? (for [x [body] :let [title x]] title)))
  (by-letfn-param [] (letfn [(f [title] (some? title))] (f body)))
  (by-letfn-name [] (letfn [(title [] (some? body))] (title)))
  (by-quote [] (= 'title body))
  (by-quote-call [] (= (quote title) body)))
