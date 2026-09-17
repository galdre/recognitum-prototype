;; Merged into `*data-readers*` at startup, which `read` and `read-string` consult and
;; `clojure.edn` does not; `clojure.edn` takes `(recognitum.core/data-readers)` as `:readers`.
;; The tag maps to the validating constructor, so a literal that fails a check throws at read
;; time.
{recognitum.heist/Heist recognitum.heist/map->Heist}
