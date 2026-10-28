;; extra: checks that read fields only inside a fn, a try, a catch, a finally, a reify or a letfn's fn; a fetch inside a try; a bundle input read only inside a fn.
;; expect {:report []
;;         :cold :same}
(ns extra.e27-reads-inside-fn-try-reify (:require [recognitum.core :as rec]))
(defn on-file [ks] (zipmap ks (repeat true)))
(rec/defchecks under-limit [xs lim]
  rec/checks
  (all-under [] (every? #(<= % lim) xs)))
(rec/defproven Scores [scores limit]
  rec/checks
  (scores [] (vector? scores))
  (limit [] (int? limit))
  (by-fn-literal [] (every? #(<= % limit) scores))
  (by-fn [] (every? (fn [s] (<= s limit)) scores))
  (by-for [] (every? true? (for [s scores] (<= s limit))))
  (by-try [] (try (<= (count scores) limit) (catch Exception _ false)))
  (by-catch [] (try (pos? (count "x")) (catch Exception _ (some? limit))))
  (by-finally [] (try true (finally (some? limit))))
  (by-reify [] (= (str limit) (str (reify Object (toString [_] (str limit))))))
  (by-letfn [] (letfn [(under? [s] (<= s limit))] (every? under? scores)))
  (fetch-in-try [] (try (rec/fetch on-file limit) (catch Exception _ false)))
  under-limit
  (xs [] scores)
  (lim [] limit))
