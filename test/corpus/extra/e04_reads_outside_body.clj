;; extra: the four body reads, called outside a check body.
;; expect {:report [{:at [10 24] :level :error :names "rec/fetch-each"
;;                   :kondo "rec/fetch-each is allowed only inside a check body"}
;;                  {:at [11 20] :level :error :names "rec/extant?"
;;                   :kondo "rec/extant? is allowed only inside a check body"}
;;                  {:at [12 23] :level :error :names "rec/when-extant"
;;                   :kondo "rec/when-extant is allowed only inside a check body"}]
;;         :cold :same}
(ns extra.e04-reads-outside-body (:require [recognitum.core :as rec] [app.db :as db]))
(defn lookup-all [ids] (rec/fetch-each db/collections ids))
(defn present? [x] (rec/extant? x))
(defn maybe-check [x] (rec/when-extant x (string? x)))
