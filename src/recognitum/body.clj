(ns recognitum.body
  "The expansion-time reading of one check body or input clause: [[analyze]] expands its
  macros with its locals in `&env`, refuses the mistakes a body can make, and writes the body
  in a form that says what it reads.

  A check body sees its fields (or a bundle's inputs) as locals. `rec/extant?`,
  `rec/when-extant`, `rec/fetch` and `rec/fetch-each` are legal only inside one, in call
  position, and the reads only outside any fn the body builds and outside `locking`.

  The walk renames every local the body binds to a fresh symbol and writes every field read
  as `(&field key)`, so no field symbol is left in what it returns. What the body reads is
  then a scan of that form, which needs no knowledge of scope.

  Nothing here is part of the public API."
  (:require
   [clojure.set :as set]
   [clojure.string :as str])
  (:import
   [clojure.lang Compiler Compiler$LocalBinding]))

(def marker
  "The local that is in `&env` exactly while a check body is being read. The body macros
  look for it."
  (gensym "recognitum_check_body__"))

(defn absent-read
  "The exception a read of the absent field or input `k` throws."
  [k]
  (ex-info (str "read of absent " k) {:recognitum.checks/absent k}))

;;; ------------------------------------------------------------------ errors

(defn- where [ctx]
  (str (:where ctx) (when-let [l (:line ctx)] (str ", line " l))))

(defn fail!
  "Throws the refusal of a definition, located by `ctx`'s `:where` and `:line`."
  [ctx msg]
  (throw (ex-info (str (where ctx) ": " msg) {:error :recognitum/refused :message msg})))

(defn warn!
  "Prints a warning about a definition to `*err*`, located as [[fail!]] locates a refusal."
  [ctx msg]
  (binding [*out* *err*]
    (println (str "WARNING: " (where ctx) ": " msg))))

;;; ------------------------------------------------------------------ reserved names

(def ^:private reserved
  "Symbol -> what a user writes in its place: the heads [[analyze]] emits, and the read forms
  of earlier versions."
  '{&field                       "the field's name"
    &fetch                       "(rec/fetch ...)"
    &fetch-each                  "(rec/fetch-each ...)"
    &extant?                     "(rec/extant? ...)"
    recognitum.checks/fetch      "(rec/fetch ...)"
    recognitum.checks/fetch-each "(rec/fetch-each ...)"
    recognitum.checks/extant?    "(rec/extant? ...)"})

(defn- refuse-reserved!
  "Refuses `s` when a body writes it: one of the heads [[analyze]] emits (under any
  namespace), or an old read form."
  [ctx s]
  (when (symbol? s)
    (when-let [m (or (reserved s) (reserved (symbol (name s))))]
      (fail! ctx (if (and (namespace s) (reserved s))
                   (str s " is the check engine's read form, not part of the API: write " m)
                   (str s " is a name the check engine reserves for its reads: a check body may"
                        " not write it, as a call, a value, a local or a var. To read, write " m))))))

(defn- parts
  "The metadata of `x` and, where `x` is a collection, its elements."
  [x]
  (cond->> (when (coll? x) (seq x)) (meta x) (cons (meta x))))

(defn- refuse-reserved-in!
  "Refuses a reserved name anywhere in `x`, metadata included: metadata, or a part of a body
  that is neither evaluated nor quoted, such as a class, a member or method name, a var's name,
  a `case` constant or a bound symbol."
  [ctx x]
  (doseq [s (tree-seq #(or (coll? %) (some? (meta %))) parts x)] (refuse-reserved! ctx s)))

;;; ------------------------------------------------------------------ locals for &env

(defn- tag-of
  "The tag the compiler gives a local named `s` (`Compiler.tagOf`)."
  [s]
  (let [t (:tag (meta s))]
    (cond (symbol? t) t
          (string? t) (symbol t)
          (class? t)  (symbol (.getName ^Class t))
          :else       nil)))

(defn- local-binding
  "A compiler local for a macro's `&env`. `num` is positive: the compiler takes local 0 to be
  `this`, and a macro that expands a form headed by local 0 throws."
  [num sym tag arg?]
  (Compiler$LocalBinding. (int num) sym tag nil (boolean arg?) nil))

(defn- add-local
  "`[ctx' s']`: `ctx` with the local `s` in scope, and the symbol the output binds in its place:
  a fresh one carrying `s`'s metadata, or `s` itself when `keep?`. A macro's `&env` holds the
  local under `s`, the name the user wrote."
  ([ctx s tag arg?] (add-local ctx s tag arg? false))
  ([ctx s tag arg? keep?]
   (when-not (simple-symbol? s) (fail! ctx (str "a local is a plain symbol: " (pr-str s))))
   (refuse-reserved-in! ctx s)
   (let [s' (if keep? s (with-meta (gensym (str (name s) "__")) (meta s)))]
     [(-> ctx
          (update :locals assoc s s')
          (update :env assoc s (local-binding (swap! (:num ctx) inc) s tag arg?)))
      s'])))

(defn- add-locals
  "`[ctx' syms']`: [[add-local]] for each of `locals`, each a symbol or `[symbol tag arg?]`."
  [ctx locals]
  (reduce (fn [[c out] l]
            (let [[s tag arg?] (if (vector? l) l [l (tag-of l) false])
                  [c s'] (add-local c s tag arg?)]
              [c (conj out s')]))
          [ctx []] locals))

(defn- param-locals
  "Fn params as the compiler registers them: arguments, the rest param tagged ISeq."
  [params]
  (loop [[p & more] params, rest? false, out []]
    (cond (nil? p) out
          (= '& p) (recur more true out)
          :else    (recur more rest? (conj out [p (if rest? 'clojure.lang.ISeq (tag-of p)) true])))))

(defn- with-params
  "`params` with each param replaced, in order, by one of `syms`; `&` stays."
  [params syms]
  (with-meta (first (reduce (fn [[out [s & more :as ss]] p]
                              (if (= '& p) [(conj out p) ss] [(conj out s) more]))
                            [[] syms] params))
    (meta params)))

;;; ------------------------------------------------------------------ the walk

(def ^:private read-heads
  "The kind of read -> the head of the read form [[analyze]] emits for it. The heads are symbols
  the check engine binds, as `defmacro` binds `&form`: they name no var, and a body never
  writes them."
  '{:field &field, :fetch &fetch, :fetch-each &fetch-each, :extant? &extant?})

(defn- local? [ctx s] (contains? (:locals ctx) s))

(defn- field? [ctx s]
  (and (symbol? s) (nil? (namespace s)) (contains? (:fields ctx) s) (not (local? ctx s))))

(defn- refuse-in-deftype!
  "Refuses a use of field `s` inside a method of a `deftype` the body defines: a method sees
  only its type's fields and its params, not the value the check runs on."
  [ctx s]
  (when-let [t (:in-deftype ctx)]
    (fail! ctx (str s " is used inside a method of deftype " t ", which sees only " t "'s own fields"
                    " and its params, not the " (:field-word ctx) "s of " (:owner ctx) ". Pass " s
                    " to " t " as a field, or use reify, whose methods close over the check body"))))

(defn- field-read
  "The read form of field `s`, carrying the metadata of the symbol written."
  [ctx s]
  (refuse-in-deftype! ctx s)
  (with-meta (list (read-heads :field) (get (:fields ctx) s)) (meta s)))

(defn- may-name-a-var?
  "Whether `s` can resolve to a var: a symbol that names a class, such as `a.b.C`, `[I` or
  `C/m` where `C` is no namespace, cannot."
  [ctx s]
  (if-let [q (namespace s)]
    (let [q (symbol q)]
      (boolean (or (get (ns-aliases (:ns ctx *ns*)) q) (find-ns q))))
    (not (or (str/includes? (name s) ".") (str/starts-with? (name s) "[")))))

(defn- resolve-in
  "What `s` names here: a var, a class the namespace maps, or nil. A symbol that can name no
  var but a class by its full name is not looked up: the lookup would make the class loader of
  the code being compiled record the class, so that a `deftype` of that name in the body,
  compiled again, could not define it."
  [ctx s]
  (when-not (or (local? ctx s) (field? ctx s) (not (may-name-a-var? ctx s)))
    (try (binding [*ns* (:ns ctx *ns*)] (resolve (:env ctx) s)) (catch Exception _ nil))))

(defn- read-kind
  "The kind of read a head symbol `s` names, when it names a body macro."
  [ctx s]
  (when (symbol? s)
    (when-let [v (resolve-in ctx s)]
      (when (var? v) (get (:read-macros ctx) (symbol v))))))

(defn- expand-1 [ctx form]
  (with-bindings {Compiler/LOCAL_ENV (:env ctx)} (macroexpand-1 form)))

(declare walk)

(defn- walk-all [ctx forms] (mapv #(walk ctx %) forms))

(defn- field-list [ctx]
  (str (str/capitalize (:field-word ctx)) "s: " (str/join " " (sort (keys (:fields ctx))))))

(defn- walk-sym [ctx s]
  (refuse-reserved! ctx s)
  (cond
    (local? ctx s) (let [s' (get (:locals ctx) s)] (if (meta s) (with-meta s' (meta s)) s'))
    (field? ctx s) (field-read ctx s)
    :else
    (let [v (resolve-in ctx s)]
      (when (read-kind ctx s)
        (fail! ctx (str s " is a macro: use it only in call position, as (" s " ...)")))
      (when (and (nil? v) (nil? (namespace s)) (not (special-symbol? s))
                 (not (contains? (:env ctx) s)) (not (str/includes? (name s) ".")))
        (fail! ctx (str s " is not a " (:field-word ctx) " of " (:owner ctx)
                        ", a local or a var. " (field-list ctx))))
      s)))

(def ^:private read-names
  "The names of the body macros, which a body writes with the `rec/` alias or refers."
  '#{fetch fetch-each extant? when-extant})

(defn- refuse-unresolved-head!
  "Refuses a call whose head `h` is an unqualified symbol that names no local, field or var
  here, before the engine sees it."
  [ctx h]
  (when (and (nil? (namespace h)) (nil? (resolve-in ctx h))
             (not (contains? (:env ctx) h)) (not (str/includes? (name h) ".")))
    (fail! ctx (if (read-names h)
                 (str "(" h " ...) calls " h ", which names nothing here. To use the body macro, write"
                      " (rec/" h " ...) with recognitum.core required as rec, or refer it")
                 (str "(" h " ...) calls " h ", which is not a " (:field-word ctx) " of " (:owner ctx)
                      ", a local or a var. " (field-list ctx))))))

(defn- arities
  "`[name arities]` of the arguments of a `fn*` form."
  [args]
  (let [[fname args] (if (symbol? (first args)) [(first args) (rest args)] [nil args])]
    [fname (if (vector? (first args)) [args] args)]))

(defn- walk-fn [ctx [op & args]]
  (let [[fname arities] (arities args)
        c (update ctx :in-fn inc)
        [c fname'] (if fname (add-local c fname (tag-of fname) false) [c nil])]
    (list* op (into (if fname [fname'] [])
                    (map (fn [[params & body]]
                           (let [[c ps] (add-locals c (param-locals params))]
                             (list* (with-params params ps) (walk-all c body)))))
                    arities))))

(defn- walk-method
  "One method of a `reify*` or `deftype*`, its params renamed."
  [ctx [mname params & body :as m]]
  (refuse-reserved-in! ctx mname)
  (let [[c ps] (add-locals ctx (param-locals params))]
    (with-meta (list* mname (with-params params ps) (walk-all c body)) (meta m))))

(defn- walk-deftype
  "A `deftype*`: its fields are locals of its methods under their own names, since they name
  the class's fields."
  [ctx [op tname cname tfields & more]]
  (refuse-reserved-in! ctx [tname cname])
  (let [c (reduce (fn [c f]
                    (when (field? c f)
                      (fail! c (str "deftype field " f " shadows the " (:field-word c) " " f
                                    " in a check body: name it otherwise")))
                    (first (add-local c f (tag-of f) false true)))
                  ;; as in the compiler, a method sees no local of the code around the deftype
                  (-> ctx (update :in-fn inc) (assoc :locals {} :in-deftype (symbol (name tname))))
                  tfields)]
    (list* op tname cname tfields
           (loop [[x & xs :as more] more, out []]
             (cond (empty? more) out
                   (keyword? x)  (do (refuse-reserved-in! ctx (first xs))
                                     (recur (rest xs) (conj out x (first xs))))
                   :else         (recur xs (conj out (walk-method c x))))))))

(defn- walk-special
  "A special form. Each local it binds is renamed in its scope. A special form this does not
  name is refused."
  [ctx [op & args :as form]]
  (case op
    quote form
    var (do (refuse-reserved-in! ctx args) form)
    def (let [[nm & more] args]
          (refuse-reserved-in! ctx nm)
          (list* op nm (walk-all ctx more)))
    (let* loop*)
    (let [[bvec & body] args
          [c pairs] (reduce (fn [[c acc] [b i]]
                              (let [i (walk c i)
                                    [c b'] (add-local c b (tag-of b) false)]
                                [c (conj acc b' i)]))
                            [ctx []] (partition 2 bvec))]
      (list* op (with-meta pairs (meta bvec)) (walk-all c body)))
    fn* (walk-fn ctx form)
    letfn*
    (let [[bvec & body] args
          [c names] (add-locals ctx (take-nth 2 bvec))]
      (list* op (with-meta (vec (interleave names (map #(walk c %) (take-nth 2 (rest bvec))))) (meta bvec))
             (walk-all c body)))
    try
    (list* op (mapv (fn [x]
                      (cond (and (seq? x) (= 'catch (first x)))
                            (let [[_ cls e & b] x
                                  _ (refuse-reserved-in! ctx cls)
                                  [c e'] (add-local ctx e (when (symbol? cls) cls) false)]
                              (list* 'catch cls e' (walk-all c b)))
                            (and (seq? x) (= 'finally (first x)))
                            (list* 'finally (walk-all ctx (rest x)))
                            :else (walk ctx x)))
                    args))
    case*
    (let [[ge shift mask default imap & more] args]
      (refuse-reserved-in! ctx (map first (vals imap)))
      (list* op (walk ctx ge) shift mask (walk ctx default)
             (into (empty imap) (map (fn [[k [t th]]] [k [t (walk ctx th)]])) imap)
             more))
    .
    (let [[target member & margs] args]
      (refuse-reserved-in! ctx (if (seq? member) (first member) member))
      (if (seq? member)
        (list op (walk ctx target) (list* (first member) (walk-all ctx (rest member))))
        (list* op (walk ctx target) member (walk-all ctx margs))))
    new (do (refuse-reserved-in! ctx (first args))
            (list* op (first args) (walk-all ctx (rest args))))
    set! (let [[target v] args]
           (when (field? ctx target)
             (fail! ctx (str "set! of " target ": a check body cannot assign a " (:field-word ctx))))
           (list op (walk ctx target) (walk ctx v)))
    reify*
    (let [[ifaces & methods] args
          c (update ctx :in-fn inc)]
      (refuse-reserved-in! ctx ifaces)
      (list* op ifaces (mapv #(walk-method c %) methods)))
    deftype* (walk-deftype ctx form)
    (if do throw recur monitor-enter monitor-exit clojure.core/import*)
    (list* op (walk-all ctx args))
    (fail! ctx (str op " in a check body is not supported: " (pr-str form)))))

(defn- presence
  "`(rec/extant? f)` once checked: the read form the body tests in its place."
  [ctx h [f & more :as args] form]
  (cond
    (or more (empty? args) (not (symbol? f)))
    (fail! ctx (str h " takes one " (:field-word ctx) ": " (pr-str form)))
    (local? ctx f)
    (fail! ctx (str h " on " f ", which is a local here, not a " (:field-word ctx)))
    (not (field? ctx f))
    (fail! ctx (str h " on " f ", which is not a " (:field-word ctx) " of " (:owner ctx)
                    ". " (field-list ctx)))
    :else
    (do (refuse-in-deftype! ctx f)
        (when (contains? (:required ctx) f)
          (warn! ctx (str f " is a required field: it is always extant, so " h " on it is always true")))
        (list (read-heads :extant?) (get (:fields ctx) f)))))

(defn- walk-read [ctx kind [h & args :as form]]
  (case kind
    :extant? (presence ctx h args form)
    :when-extant (let [[f & body] args]
                   (list 'if (presence ctx h (when f [f]) form)
                         (list* 'do (walk-all ctx body))
                         true))
    (:fetch :fetch-each)
    (do
      (when-not (= 2 (count args))
        (fail! ctx (str h " takes a source and " (if (= kind :fetch) "a key" "a collection of keys")
                        ": " (pr-str form))))
      (when-not (:fetch? ctx)
        (fail! ctx (str h " in " (:no-fetch ctx) ": only a check body reads a source")))
      (when (pos? (:in-fn ctx))
        (fail! ctx (str h " inside a fn the check body builds (fn, #(...), for, lazy-seq, delay):"
                        " it runs outside the check. Read outside the fn and close over the answer")))
      (when (:in-lock ctx)
        (fail! ctx (str h " inside locking: a read may wait, and would hold the lock")))
      (list* (read-heads kind) (walk-all ctx args)))))

(defn- walk-seq [ctx form]
  (let [ctx (if-let [l (:line (meta form))] (assoc ctx :line l) ctx)
        [h] form
        _ (refuse-reserved! ctx h)
        same (fn [out] (if (meta form) (with-meta out (merge (meta out) (meta form))) out))]
    ;; A macro's expansion keeps only its own metadata, as in the compiler; any other form
    ;; keeps the metadata written on it.
    (cond
      (not (symbol? h))       (same (apply list (walk-all ctx form)))
      (special-symbol? h)     (same (walk-special ctx form))
      (local? ctx h)          (same (apply list (walk-all ctx form)))
      (field? ctx h)          (same (apply list (walk-all ctx form)))
      :else
      (if-let [kind (read-kind ctx h)]
        (walk-read ctx kind form)
        (let [ctx (cond-> ctx (= #'clojure.core/locking (resolve-in ctx h)) (assoc :in-lock true))
              x (expand-1 ctx form)]
          (if (identical? x form)
            (do (refuse-unresolved-head! ctx h)
                (same (apply list (walk-all ctx form))))
            (walk ctx x)))))))

(defn- walk [ctx form]
  (some->> (meta form) (refuse-reserved-in! ctx))
  (cond
    (symbol? form)    (walk-sym ctx form)
    (seq? form)       (if (empty? form) form (walk-seq ctx form))
    (map-entry? form) (vec (walk-all ctx form))
    (vector? form)    (with-meta (walk-all ctx form) (meta form))
    (map? form)       (with-meta (into {} (map (fn [[k v]] [(walk ctx k) (walk ctx v)])) form) (meta form))
    (set? form)       (with-meta (set (walk-all ctx form)) (meta form))
    :else             form))

;;; ------------------------------------------------------------------ the walk's output

(defn- evaluated
  "The parts of `form`, an output of [[walk]], in positions the compiler evaluates. Left out:
  a quoted form, a var's name, a `def`'s name, a bound symbol or param, the member of `.`,
  the class of `new` or `catch`, a `case` constant, the interfaces and method names of
  `reify*` and `deftype*`, and the head of a special form. Kept: the metadata the compiler
  evaluates, that of a `def`'s name, of a `fn*` or `reify*` form, and of a vector, map or set."
  [form]
  (cond
    (seq? form)
    (let [[op & args] form]
      (if-not (special-symbol? op)
        form
        (case op
          (quote var) nil
          def (cons (meta (first args)) (rest args))
          (let* loop* letfn*) (concat (take-nth 2 (rest (first args))) (rest args))
          fn* (cons (meta form) (mapcat rest (second (arities args))))
          reify* (cons (meta form) (mapcat #(drop 2 %) (rest args)))
          deftype* (mapcat #(drop 2 %) (filter seq? (drop 3 args)))
          try (mapcat (fn [x] (cond (and (seq? x) (= 'catch (first x))) (drop 3 x)
                                    (and (seq? x) (= 'finally (first x))) (rest x)
                                    :else [x]))
                      args)
          case* (let [[ge _ _ default imap] args] (list* ge default (map (comp second val) imap)))
          . (let [[target member & margs] args] (cons target (if (seq? member) (rest member) margs)))
          new (rest args)
          args)))
    (map? form)  (cons (meta form) (mapcat identity form))
    (coll? form) (cons (meta form) (seq form))
    :else        nil))

(defn- read-of
  "`[head arg]` when `x` is a read form [[walk]] wrote."
  [x]
  (when (and (seq? x) (contains? '#{&field &extant? &fetch &fetch-each} (first x)))
    [(first x) (second x)]))

(defn- global-source?
  "Whether a source form names the same object wherever the type is: a constant, or a symbol
  that resolves to a var."
  [ctx src]
  (or (keyword? src) (string? src) (number? src)
      (and (symbol? src) (var? (resolve-in ctx src)))))

(defn analyze
  "Reads `body`, a check body or input clause. `ctx` holds:

  - `:fields`, a map from each field (or input) symbol in scope to its key, and `:required`,
    the set of those always present;
  - `:owner`, the type or bundle, `:field-word` (\"field\" or \"input\"), `:where` and
    `:line`, for messages;
  - `:call-env`, the defining macro's own `&env`;
  - `:read-macros`, a map from the symbol of each body macro's var to the kind of read it is:
    `:fetch`, `:fetch-each`, `:extant?` or `:when-extant`;
  - `:fetch?`, whether the body may read sources; where it may not, `:no-fetch` names it.

  Returns `{:form :reads :sources :fetches?}`: the body with every macro expanded, every
  local it binds renamed to a fresh symbol, and each read written as a read form,
  `(&field key)`, `(&extant? key)`, `(&fetch src k)` or `(&fetch-each src ks)`; the field
  symbols it reads; the source forms that name a var or a constant; and whether it reads any
  source. The last three are a scan of the returned form. Throws on a refused body, on a body
  that writes `&field`, `&fetch`, `&fetch-each` or `&extant?` or an old read form such as
  `recognitum.checks/fetch`, and where a field symbol is left in the returned form."
  [ctx body]
  (let [num (atom 0)
        env (merge (:call-env ctx)
                   (into {} (for [s (concat (sort (keys (:fields ctx))) [marker])]
                              [s (local-binding (swap! num inc) s (tag-of s) false)])))
        c (assoc ctx :num num :env env :locals {} :in-fn 0 :ns *ns*)
        out (walk c body)
        forms (tree-seq coll? evaluated out)
        _ (when-let [s (some #(when (and (symbol? %) (contains? (:fields ctx) %)) %) forms)]
            (fail! c (str s " is used where a check body cannot read it, such as metadata the"
                          " compiler evaluates: " (pr-str body))))
        reads (keep read-of forms)
        by-key (set/map-invert (:fields ctx))
        sources (for [[h src] reads :when (#{'&fetch '&fetch-each} h)] src)]
    {:form out
     :reads (set (for [[h k] reads :when (#{'&field '&extant?} h)] (by-key k)))
     :sources (vec (distinct (filter #(global-source? c %) sources)))
     :fetches? (boolean (seq sources))}))

;;; ------------------------------------------------------------------ input clauses

(def ^:private none (Object.))

(defn read-input
  "In a compiled input clause, the value of field `k` of `value`; throws [[absent-read]] where
  it is absent."
  [value k]
  (let [x (get value k none)]
    (if (identical? x none) (throw (absent-read k)) x)))

(defn- with-reads
  "`form`, an output of [[analyze]], with each field read and presence test a call on the value
  `v`. A read form is only where the walk wrote it, or quoted."
  [v form]
  (cond
    (seq? form)
    (let [[h k] form]
      (cond (= 'quote h)    form
            (= '&field h)   (with-meta `(read-input ~v ~k) (meta form))
            (= '&extant? h) `(contains? ~v ~k)
            :else           (let [out (apply list (map #(with-reads v %) form))]
                              (if (meta form) (with-meta out (meta form)) out))))
    (map-entry? form) (mapv #(with-reads v %) form)
    (coll? form)      (into (empty form) (map #(with-reads v %)) form)
    :else             form))

(defn checked-fn
  "The form of a function of a value that evaluates `form`, an input clause [[analyze]]
  returned, each field read throwing where the field is absent."
  [form]
  (let [v (gensym "value")]
    `(fn [~v] ~(with-reads v form))))
