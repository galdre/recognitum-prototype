;; usage: a source of the usage program (a stub for app.db).
;; expect {:report []}
(ns app.db)
(def collections "a source" nil)
(def users "a source" nil)
(defn can-write? [_user _coll])
(defn can-curate? [_user _coll])
