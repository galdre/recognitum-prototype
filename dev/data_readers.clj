;; Clojure reads every data_readers.clj at the root of the classpath on startup and merges
;; them into *data-readers*, which is what `read` and `read-string` consult. Each entry maps a
;; tag to a fully-qualified var. Pointing it at the validating constructor is the whole trick:
;; a literal cannot produce a value that construction would have refused.
;;
;; This only reaches `read`/`read-string`. `clojure.edn` never consults *data-readers* -- hand
;; it `(recognitum.core/data-readers)` as :readers instead.
{recognitum.heist/Heist recognitum.heist/map->Heist}
