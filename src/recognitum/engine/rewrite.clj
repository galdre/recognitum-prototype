(ns recognitum.engine.rewrite
  "The resumable form of an expanded check body. Every subform that contains a read,
  `(&fetch src k)` or `(&fetch-each src ks)`, or a `recur` of a loop that contains one, is
  rewritten into continuation-passing style; every other subform stays as written. A read becomes a Park that
  holds the request and a continuation. A loop that reads becomes a local fn whose `recur`
  returns a Recur thunk to a trampoline. A `try` that reads becomes a call of `run-in-try`,
  which keeps its handler around every continuation taken inside it.

  Continuations are compile-time functions from a value expression to a form. A continuation
  that is not known to be safe is bound to a local join fn before the rewrite enters a scope
  that binds names, so its code never sees the names that scope binds.

  Nothing here is part of the library's surface."
  (:require
   [recognitum.engine.runtime :refer [park-one park-each unwrap new-recur try-exit run-in-try]]))

(defn- read-kind
  "`:one` or `:each` when `form` is a read, else nil."
  [form]
  (when (seq? form)
    (let [h (first form)]
      (cond (= h '&fetch) :one
            (= h '&fetch-each) :each))))

;;; ------------------------------------------------------------- what needs rewriting

(defn- has-read? [form]
  (cond
    (seq? form) (let [op (first form)]
                  (cond (contains? '#{quote fn* reify* deftype*} op) false
                        (read-kind form) true
                        :else (boolean (some has-read? form))))
    (coll? form) (boolean (some has-read? form))
    :else false))

(defn- has-free-recur?
  "A recur not inside a nested loop or fn. Outside a rewritten loop no such recur is ever met,
  since loops that do not read are left whole."
  [form]
  (cond
    (seq? form) (let [op (first form)]
                  (cond (contains? '#{quote fn* reify* deftype* loop*} op) false
                        (= 'recur op) true
                        :else (boolean (some has-free-recur? form))))
    (coll? form) (boolean (some has-free-recur? form))
    :else false))

(defn- needs? [form] (or (has-read? form) (has-free-recur? form)))

;;; ------------------------------------------------------------- continuations

(defn- simple
  "Marks a continuation that places its value exactly once, immediately, in code that is safe in
  any scope."
  ([f] (with-meta f {::simple true}))
  ([f sym] (with-meta f {::simple true ::sym sym})))

(defn- simple? [k] (::simple (meta k)))

(def ^:private done-k (simple (fn [v] v)))
(def ^:private exit-k (simple (fn [v] `(try-exit ~v))))

(defn- with-join
  "(f k') where k' is safe in any scope: k when simple, else a call of a join fn bound around the
  result."
  [k f]
  (if (simple? k)
    (f k)
    (let [j (gensym "join") v (gensym "v")]
      `(let* [~j (fn* [~v] ~(k v))] ~(f (simple (fn [x] (list j x)) j))))))

(defn- runtime-fn
  "[bindings fn-expr]: a runtime fn of one argument for simple continuation k."
  [k]
  (if-let [s (::sym (meta k))]
    [[] s]
    (let [j (gensym "join") v (gensym "v")] [[j `(fn* [~v] ~(k v))] j])))

(defn- constant? [x]
  (or (nil? x) (boolean? x) (number? x) (string? x) (keyword? x) (char? x)))

(defn- bind
  "Evaluates expr now and passes its value to k."
  [expr k]
  (if (or (constant? expr) (symbol? expr) (simple? k))
    (k expr)
    (let [v (gensym "v")] `(let* [~v ~expr] ~(k v)))))

;;; ------------------------------------------------------------- the rewrite

(def ^:private ^:dynamic *loop* nil)

(declare cps)

(defn- cps-do [forms k]
  (let [[a & more] forms]
    (cond (empty? forms) (k nil)
          (empty? more) (cps a k)
          (needs? a) (cps a (fn [_] (cps-do more k)))
          :else `(do ~a ~(cps-do more k)))))

(defn- cps-args
  "Evaluates args in order and passes the vector of value expressions to f. Args after the last
  one that reads stay in place: nothing after them can reorder them."
  [args f]
  (let [args (vec args)
        last-read (or (last (keep-indexed (fn [i a] (when (needs? a) i)) args)) -1)]
    (letfn [(go [i acc]
              (if (> i last-read)
                (f (into acc (subvec args i)))
                (cps (args i) (fn [v] (go (inc i) (conj acc v))))))]
      (go 0 []))))

(defn- cps-bind [bs tail]
  (if (empty? bs)
    (tail)
    (let [[s e & more] bs]
      (cps e (fn [v] `(let* [~s ~v] ~(cps-bind more tail)))))))

(defn- catch? [x] (and (seq? x) (= 'catch (first x))))
(defn- finally? [x] (and (seq? x) (= 'finally (first x))))

(defn- cps-catch [body catches k]
  (with-join k
    (fn [j]
      (let [[jb jf] (runtime-fn j)
            ex (gensym "ex")
            dispatch (reduce (fn [else [_ cls e & h]]
                               `(if (instance? ~cls ~ex) (let* [~e ~ex] ~(cps-do h j)) ~else))
                             `(throw ~ex)
                             (reverse catches))]
        `(let* [~@jb]
           (run-in-try (fn* [] ~(cps-do body exit-k)) (fn* [~ex] ~dispatch) ~jf))))))

(defn- cps-finally [inner fin k]
  (with-join k
    (fn [j]
      (let [ex (gensym "ex") v (gensym "v") fin-fn (gensym "finally") then (gensym "then")]
        `(let* [~fin-fn (fn* [~then] ~(cps-do fin (fn [_] `(~then))))]
           (run-in-try (fn* [] ~(cps inner exit-k))
                       (fn* [~ex] (~fin-fn (fn* [] (throw ~ex))))
                       (fn* [~v] (~fin-fn (fn* [] ~(j v))))))))))

(defn- cps-try [form k]
  (let [[body clauses] (split-with #(not (or (catch? %) (finally? %))) (rest form))
        catches (filter catch? clauses)
        fin (first (filter finally? clauses))]
    (if fin
      (cps-finally (if (seq catches) `(try ~@body ~@catches) `(do ~@body)) (rest fin) k)
      (cps-catch body catches k))))

(defn- cps-loop [[_ bs & body] k]
  (with-join k
    (fn [k']
      (let [lp (gensym "loop")
            syms (take-nth 2 bs)
            params (mapv (fn [_] (gensym "p")) syms)]
        `(let* [~lp (fn* ~lp ~params
                         (let* [~@(interleave syms params)]
                           ~(binding [*loop* lp] (cps-do body k'))))]
           ~(cps-bind bs (fn [] `(~lp ~@syms))))))))

(defn- cps-read [kind [s x] k]
  (let [a (gensym "a") v (gensym "v")
        park (if (= kind :one) `park-one `park-each)]
    `(~park ~s ~x (fn* [~a] (let* [~v (unwrap ~a)] ~(k v))))))

(defn- cps-seq [form k]
  (let [op (first form)]
    (if-let [kind (read-kind form)]
      (cps-args (rest form) (fn [vs] (cps-read kind vs k)))
      (case (when (symbol? op) op)
        do (cps-do (rest form) k)
        if (let [[_ t a b] form]
             (cps t (fn [tv]
                      (if (or (needs? a) (needs? b))
                        (with-join k (fn [k'] `(if ~tv ~(cps a k') ~(cps b k'))))
                        (bind `(if ~tv ~a ~b) k)))))
        let* (let [[_ bs & body] form]
               (with-join k (fn [k'] (cps-bind bs #(cps-do body k')))))
        loop* (cps-loop form k)
        letfn* (let [[_ bs & body] form]
                 (with-join k (fn [k'] `(letfn* ~bs ~(cps-do body k')))))
        recur (let [lp *loop*]
                (cps-args (rest form) (fn [vs] `(new-recur (fn* [] (~lp ~@vs))))))
        try (cps-try form k)
        throw (cps (second form) (fn [v] `(throw ~v)))
        case* (let [[_ e shift mask default imap & more] form]
                (cps e (fn [ev]
                         (with-join k
                           (fn [k']
                             `(case* ~ev ~shift ~mask ~(cps default k')
                                     ~(into (empty imap) (map (fn [[h [t then]]] [h [t (cps then k')]]) imap))
                                     ~@more))))))
        . (let [[_ target member & args] form
                call? (seq? member)
                [m args] (if call? [(first member) (rest member)] [member args])
                keep-target? (symbol? target)]
            (cps-args (if keep-target? args (cons target args))
                      (fn [vs]
                        (let [[t vs] (if keep-target? [target vs] [(first vs) (rest vs)])]
                          (bind (if call? `(. ~t (~m ~@vs)) `(. ~t ~m ~@vs)) k)))))
        new (cps-args (nnext form) (fn [vs] (bind `(new ~(second form) ~@vs) k)))
        set! (cps (nth form 2) (fn [v] (bind `(set! ~(second form) ~v) k)))
        (if (or (symbol? op) (keyword? op))
          (cps-args (rest form) (fn [vs] (bind `(~op ~@vs) k)))
          (cps-args form (fn [[f & vs]] (bind `(~f ~@vs) k))))))))

(defn cps
  "The rewrite of expanded `form`, whose value goes to compile-time continuation k."
  [form k]
  (cond
    (not (needs? form)) (bind form k)
    (seq? form) (cps-seq form k)
    (vector? form) (cps-args form (fn [vs] (bind `(vector ~@vs) k)))
    (map? form) (cps-args (mapcat identity form) (fn [vs] (bind `(hash-map ~@vs) k)))
    (set? form) (cps-args form (fn [vs] (bind `(hash-set ~@vs) k)))
    :else (bind form k)))

(defn rewrite
  "The resumable form of an expanded body."
  [expanded]
  (cps expanded done-k))
