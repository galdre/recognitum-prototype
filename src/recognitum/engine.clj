(ns recognitum.engine
  "Check bodies that read sources inline, run in waves over a group of values.

  A check body is ordinary Clojure, its macros expanded. It reads the field whose key is `k`
  with `(&field k)`, asks whether that field is present with `(&extant? k)`, reads a source
  with `(&fetch source key)`, and many keys of one source with `(&fetch-each source keys)`. [[body-form]] gives the form of
  the check a body compiles to; [[define]] makes an engine from a type's checks and the checks
  each waits on; [[run-all]] runs engines over a group of values.

  A run proceeds in waves. Every check that can start runs until it finishes or reaches a read
  whose answer is not yet known, where it parks. Then each source is called once with every
  key parked on it, the parked checks resume, and the next wave begins. Batching changes how
  sources are called, never what a check answers: every check answers what it answers when
  each read calls its source at once with one key. Within one run:

  - a source is asked for a key at most once, so each call passes distinct keys;
  - a source is asked for exactly the keys the checks that run reach;
  - a source is called no more times than the longest chain of reads along any path of waits,
    each `&fetch` or `&fetch-each` one link.

  A source is a function of a vector of distinct keys that returns a map from key to answer.
  A key left out of the map reads as nil; an answer that is a Throwable fails the reads of
  that key. A source that throws an Exception fails the reads of every key it was given; an
  Error propagates. A source that needs smaller calls splits the keys it is given. Sources
  are compared with `=`, so reads of one source share its calls across checks, values and
  engines."
  (:require
   [recognitum.engine.rewrite :as rewrite]
   [recognitum.engine.runtime :as rt])
  (:import
   [clojure.lang Var]
   [java.util ArrayDeque ArrayList HashMap LinkedHashMap LinkedHashSet]
   [recognitum.engine.runtime Failure Park]))

;;; ------------------------------------------------------------- compiling a body

(defn- with-fields
  "`form` with each `(&field k)` a read of field `k` of the checked value and each
  `(&extant? k)` a test of it. Outside a quoted form, those heads are read forms and nothing
  else, so this knows no scope."
  [form]
  (cond
    (seq? form)
    (let [[h k] form]
      (cond (= 'quote h)    form
            (= '&field h)   (with-meta (list `rt/field rt/value-sym k) (meta form))
            (= '&extant? h) (list `contains? rt/value-sym k)
            :else           (let [out (apply list (map with-fields form))]
                              (if (meta form) (with-meta out (meta form)) out))))
    (map-entry? form) (mapv with-fields form)
    (coll? form)      (into (empty form) (map with-fields) form)
    :else             form))

(defn body-form
  "The form of the check that runs `body`, for a macro to place in its expansion. `body` has
  every macro expanded; its reads are `(&field k)`, `(&extant? k)`, `(&fetch src k)` and
  `(&fetch-each src ks)`, each in call position, the last two outside any fn the body builds
  and outside `locking`."
  [body]
  `(fn* [~rt/value-sym] ~(rewrite/rewrite (with-fields body))))

;;; ------------------------------------------------------------- engines

(deftype Engine [ids fns waits dependents])

(defn- order
  "The ids of `checks`, each after every check it waits on. Throws :recognitum.checks/cycle."
  [checks]
  (loop [done [], seen #{}]
    (if (= (count done) (count checks))
      done
      (let [ready (sort-by str (remove seen (filter #(every? seen (:after (checks %))) (keys checks))))]
        (when (empty? ready)
          (let [left (set (remove seen (keys checks)))]
            (throw (ex-info (str "checks wait on one another in a cycle: " left)
                            {:error :recognitum.checks/cycle :checks left}))))
        (recur (into done ready) (into seen ready))))))

(defn define
  "An engine for `checks`, a map `{id {:after #{id ...} :body check}}`. A check runs only once
  every check in its `:after` has passed; one whose wait failed or was blocked is blocked. A
  `:body` is a function of a value, such as the check a [[body-form]] evaluates to.

  Throws ex-info whose :error is :recognitum.checks/unknown-check when a check waits on an id
  `checks` lacks, and :recognitum.checks/cycle when waits form a cycle."
  [checks]
  (doseq [[id {:keys [after]}] checks, w after :when (not (contains? checks w))]
    (throw (ex-info (str "check " id " waits on " w ", which is not a check")
                    {:error :recognitum.checks/unknown-check :check id :wait w})))
  (let [ids (order checks)
        index (zipmap ids (range))
        waits (mapv (fn [id] (int-array (map index (:after (checks id))))) ids)
        deps (mapv (fn [i] (int-array (keep-indexed (fn [j ^ints ws] (when (some #{i} ws) j)) waits)))
                   (range (count ids)))
        fns (object-array (map (comp :body checks) ids))]
    (Engine. ids fns waits deps)))

;;; ------------------------------------------------------------- a run

(def ^:private none (Object.))

(defn- failure-of
  "The failure data of a signal `e` a body let escape, or nil when `e` is no signal."
  [^Exception e]
  (let [d (ex-data e)]
    (cond (contains? d :recognitum.checks/absent)       {:absent (:recognitum.checks/absent d)}
          (contains? d :recognitum.checks/fetch-failed) {:fetch-failed (:recognitum.checks/fetch-failed d)
                                                         :cause (.getCause e)}
          :else                                         nil)))

(deftype Failed [data])

(defn- step
  "Runs `f`, a thunk of a body, to its next Park, or to `::passed` or a Failed. Rethrows what
  the body throws, unless it is a signal."
  [f]
  (try (let [r (rt/settle (f))]
         (cond (instance? Park r) r
               (true? r)          ::passed
               :else              (Failed. r)))
       (catch Exception e
         (if-let [d (failure-of e)] (Failed. d) (throw e)))))

(deftype Slot [^Engine engine value ^objects state ^ints remaining ^objects failures])

(deftype Waiting [^Slot slot ^long c ^Park park ^HashMap answers src])

(defn- answer
  "The answer to `p` from `answers`, a Failure when the source failed a key it asked for, or
  `none` while a key is unanswered."
  [^HashMap answers ^Park p]
  (letfn [(fail [a] (Failure. (.-src p) a))]
    (if (.-one? p)
      (let [x (.-ks p)]
        (if (.containsKey answers x)
          (let [a (.get answers x)] (if (instance? Throwable a) (fail a) a))
          none))
      (loop [xs (seq (.-ks p)), m (transient {}), failed nil]
        (if xs
          (let [x (first xs)]
            (if (.containsKey answers x)
              (let [a (.get answers x)]
                (recur (next xs) (assoc! m x a) (or failed (when (instance? Throwable a) a))))
              none))
          (if failed (fail failed) (persistent! m)))))))

(defn- call-wave!
  "Calls each source once with the distinct keys parked on it that it was not yet asked for."
  [^HashMap caches ^ArrayList waiting]
  (let [wanted (LinkedHashMap.)]
    (dotimes [i (.size waiting)]
      (let [^Waiting w (.get waiting i)
            ^Park p (.-park w)
            ^HashMap answers (.-answers w)
            ^LinkedHashSet ks (or (.get wanted (.-src w))
                                  (let [s (LinkedHashSet.)] (.put wanted (.-src w) s) s))]
        (if (.-one? p)
          (let [x (.-ks p)] (when-not (.containsKey answers x) (.add ks x)))
          (doseq [x (.-ks p)] (when-not (.containsKey answers x) (.add ks x))))))
    (doseq [[f ^LinkedHashSet ks] wanted
            :when (pos? (.size ks))
            :let [ks (vec ks)
                  ^HashMap answers (.get caches f)
                  ans (try (f ks) (catch Exception e e))]]
      (doseq [x ks]
        (.put answers x (if (instance? Throwable ans) ans (get ans x)))))))

(defn run-all
  "Runs `items`, a sequence of `[engine value assumed]`, as one group. `value` is a map from key
  to field value; the checks in `assumed`, a set of ids, count as passed and do not run. One
  result per item, in order: `{:failed {id failure} :blocked #{id ...}}`.

  A check fails with what its body returns, unless that is exactly `true`; with
  `{:absent key}` when it reads an absent field; and with `{:fetch-failed source :cause
  exception}` when a read failed. Each of these two is an ordinary exception inside the body,
  which the body may catch. Anything else a body throws propagates out of `run-all`, which
  then returns nothing for any item. Every check sees the dynamic bindings in force where
  `run-all` was called."
  [items]
  (let [items (vec items)
        home (Var/getThreadBindingFrame)
        caches (HashMap.)
        ready (ArrayDeque.)
        parked (ArrayList.)
        slots (mapv (fn [[^Engine e value assumed]]
                      (let [n (count (.-ids e))]
                        (Slot. e value (object-array n) (int-array n) (object-array n))))
                    items)]
    (letfn [(answers-of ^HashMap [f]
              (or (.get caches f) (let [m (HashMap.)] (.put caches f m) m)))
            (resume [^Park p a]
              (Var/resetThreadBindingFrame (.-frame p))
              (let [r (step #((.-k p) a))]
                (Var/resetThreadBindingFrame home)
                r))
            (handle [^Slot slot c r]
              (loop [r r]
                (if (instance? Park r)
                  (let [^Park p r
                        f (.-src p)
                        answers (answers-of f)
                        a (answer answers p)]
                    (if (identical? a none)
                      (.add parked (Waiting. slot c p answers f))
                      (recur (resume p a))))
                  (finish slot c r))))
            (block [^Slot slot d]
              (let [^objects state (.-state slot)]
                (when (nil? (aget state d))
                  (aset state d ::blocked)
                  (doseq [dd (nth (.-dependents ^Engine (.-engine slot)) d)] (block slot dd)))))
            (finish [^Slot slot c r]
              (let [^objects state (.-state slot)
                    ^ints remaining (.-remaining slot)
                    deps (nth (.-dependents ^Engine (.-engine slot)) c)]
                (if (identical? r ::passed)
                  (do (aset state c ::passed)
                      (doseq [d deps]
                        (let [m (dec (aget remaining d))]
                          (aset remaining d (int m))
                          (when (and (zero? m) (nil? (aget state d))) (.add ready [slot d])))))
                  (do (aset state c ::failed)
                      (aset ^objects (.-failures slot) c (.-data ^Failed r))
                      (doseq [d deps] (block slot d))))))
            (start [^Slot slot c]
              (aset ^objects (.-state slot) c ::running)
              (Var/resetThreadBindingFrame home)
              (let [f (aget ^objects (.-fns ^Engine (.-engine slot)) c)
                    value (.-value slot)
                    r (step #(f value))]
                (Var/resetThreadBindingFrame home)
                (handle slot c r)))
            (drain []
              (loop [] (when-let [[slot c] (.poll ready)] (start slot c) (recur))))]
      (try
        (doseq [[[_ _ assumed] ^Slot slot] (map vector items slots)
                :let [^Engine e (.-engine slot)
                      ids (.-ids e)
                      n (count ids)
                      assumed (if (set? assumed) assumed (set assumed))
                      none-assumed? (empty? assumed)
                      ^objects state (.-state slot)
                      ^ints remaining (.-remaining slot)]]
          (when-not none-assumed?
            (dotimes [c n] (when (contains? assumed (nth ids c)) (aset state c ::passed))))
          (dotimes [c n]
            (let [^ints ws (nth (.-waits e) c)
                  m (areduce ws i m 0 (if (identical? ::passed (aget state (aget ws i))) m (inc m)))]
              (aset remaining c (int m))
              (when (and (nil? (aget state c)) (zero? m)) (.add ready [slot c])))))
        (drain)
        (while (pos? (.size parked))
          (let [batch (ArrayList. parked)]
            (.clear parked)
            (call-wave! caches batch)
            (dotimes [i (.size batch)]
              (let [^Waiting w (.get batch i)
                    p (.-park w)]
                (handle (.-slot w) (.-c w) (resume p (answer (.-answers w) p)))))
            (drain)))
        (finally (Var/resetThreadBindingFrame home))))
    (mapv (fn [^Slot slot]
            (let [ids (.-ids ^Engine (.-engine slot))
                  ^objects state (.-state slot)
                  ^objects failures (.-failures slot)]
              (loop [c 0, failed {}, blocked #{}]
                (if (= c (count ids))
                  {:failed failed :blocked blocked}
                  (let [s (aget state c)]
                    (recur (inc c)
                           (if (identical? s ::failed) (assoc failed (nth ids c) (aget failures c)) failed)
                           (if (identical? s ::blocked) (conj blocked (nth ids c)) blocked)))))))
          slots)))
