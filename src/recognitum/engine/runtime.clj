(ns recognitum.engine.runtime
  "What compiled check bodies call at run time: field reads, parks, the trampoline that runs
  loops, and try regions that span a park.

  Nothing here is part of the library's surface.")

;;; ------------------------------------------------------------- the checked value

(def value-sym
  "The parameter of a compiled check: the value it checks."
  (gensym "value__"))

;;; ------------------------------------------------------------- signals

(defn absent
  "The exception a read of the absent field `k` throws."
  [k]
  (ex-info (str "absent field " k) {:recognitum.checks/absent k}))

(defn fetch-failed
  "The exception a read throws when source `src` failed to answer it, with `cause`, what the
  source threw or answered."
  [src cause]
  (ex-info (str "fetch failed: " src) {:recognitum.checks/fetch-failed src} cause))

(def ^:private none (Object.))

(defn field
  "The value of field `k` in `value`; throws [[absent]] when `value` has no `k`."
  [^clojure.lang.ILookup value k]
  (let [v (.valAt value k none)]
    (if (identical? v none) (throw (absent k)) v)))

;;; ------------------------------------------------------------- parks

(deftype Park [src ks one? frame k])
(deftype Recur [thunk])
(deftype TryExit [v])
(deftype Thrown [t])
(deftype Failure [src cause])

(defn park-one
  "A read of key `x` from `src`, resumed by `k` with the answer."
  [src x k]
  (Park. src x true (clojure.lang.Var/getThreadBindingFrame) k))

(defn park-each
  "A read of every key of `xs` from `src`, resumed by `k` with a map from key to answer. The
  keys are realized here, in the body, so an exception from them is thrown at the read."
  [src xs k]
  (Park. src (into [] (distinct) xs) false (clojure.lang.Var/getThreadBindingFrame) k))

(defn unwrap
  "The answer `a`, or a throw of [[fetch-failed]] when `a` is a Failure."
  [a]
  (if (instance? Failure a)
    (throw (fetch-failed (.-src ^Failure a) (.-cause ^Failure a)))
    a))

(defn new-recur [thunk] (Recur. thunk))

(defn try-exit [v] (TryExit. v))

(defn settle
  "Runs Recur thunks until the result is something else."
  [r]
  (if (instance? Recur r) (recur ((.-thunk ^Recur r))) r))

(defn run-in-try
  "Runs `thunk` inside a try region. Its TryExit goes to `exit` and its throw to `handler`, both
  outside the region; a Park leaves with a continuation that re-enters the region."
  [thunk handler exit]
  (let [r (try (settle (thunk)) (catch Throwable t (Thrown. t)))]
    (cond
      (instance? TryExit r) (exit (.-v ^TryExit r))
      (instance? Park r)    (let [^Park p r, k (.-k p)]
                              (Park. (.-src p) (.-ks p) (.-one? p) (.-frame p)
                                     (fn [a] (run-in-try #(k a) handler exit))))
      (instance? Thrown r)  (handler (.-t ^Thrown r))
      :else (throw (IllegalStateException. (str "try region ended without exit: " r))))))
