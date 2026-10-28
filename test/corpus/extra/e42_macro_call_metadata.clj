;; extra: metadata written on a macro call, naming a local and a field: the expansion does not keep it, as the compiler has it, so it is neither evaluated nor a read.
;; expect {:report []
;;         :cold :same}
(ns extra.e42-macro-call-metadata (:require [recognitum.core :as rec]))
(defmacro thunk [] '(fn* [] 1))
(rec/defproven Note [x]
  rec/checks
  (c [] (let [z 1] (and (nil? (meta ^{:a z} (thunk))) (nil? (meta ^{:b x} (thunk)))))))
