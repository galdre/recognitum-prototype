(ns recognitum.record
  "The record layer: types whose values read as maps and carry proof that their checks passed,
  drafts that remember which checks pass, checked writes, optional fields and closed types.
  [[define-type]] defines a type from compiled checks, a proof strategy and a check engine,
  and names neither; the rest of this namespace acts on values of any type it defined.

  A **sealed value** is a value of a type's class. The only paths to one run the type's
  checks: [[make]], [[seal]], and a write (`assoc`, `dissoc`, `conj`, `merge`, `into`) on a
  sealed value, which reruns the checks that read a key it changes and seals the result or
  throws. A **draft** is an unchecked map of a type that remembers which checks pass on its
  entries; a write to it forgets the checks that read a key written.

  Every type is closed: a sealed value refuses a key its type does not declare, and a draft
  holding one does not seal. A required field must be present in a sealed value.

  The key that mints a type's values and marks its drafts never leaves the closure
  [[define-type]] builds: it is reachable only through the type's installed write, whose
  every arity runs the checks it stands for."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [recognitum.proof :as proof]
   [recognitum.select :as select]))

;;; ------------------------------------------------------------------ drafts

(deftype ^:private DataKey [data]
  Object
  (equals [_ o] (and (instance? DataKey o) (identical? data (.-data ^DataKey o))))
  (hashCode [_] (System/identityHashCode data)))

(defn- passes
  "The checks a draft's marks show passing on its own entries. Marks are bound to the entries
  they were set for, by identity, so marks moved onto other entries show nothing."
  [type data marks]
  (let [marked? #(proof/marked? (:strategy type) marks %)]
    (if (and (instance? (::draft-class type) marks) (marked? (DataKey. data)))
      (into #{} (filter marked?) (:ids type))
      #{})))

(declare rewrite)

(deftype Draft [data cell mta]
  clojure.lang.IPersistentMap
  (assoc [this k v] (rewrite this (assoc data k v)))
  (assocEx [this k v] (rewrite this (assoc data k v)))
  (without [this k] (rewrite this (dissoc data k)))
  clojure.lang.Associative
  (containsKey [_ k] (contains? data k))
  (entryAt [_ k] (find data k))
  clojure.lang.ILookup
  (valAt [_ k] (get data k))
  (valAt [_ k nf] (get data k nf))
  clojure.lang.Counted
  (count [_] (count data))
  clojure.lang.IPersistentCollection
  (cons [this o] (rewrite this (conj data o)))
  (empty [_] (throw (UnsupportedOperationException. "a draft has no empty value: use (rec/draft T)")))
  (equiv [_ o] (and (instance? Draft o) (= data (.-data ^Draft o))))
  clojure.lang.Seqable
  (seq [_] (seq data))
  java.lang.Iterable
  (iterator [_] (.iterator ^Iterable data))
  clojure.lang.IObj
  (withMeta [_ m] (Draft. data cell m))
  clojure.lang.IMeta
  (meta [_] mta)
  Object
  (hashCode [_] (hash data))
  (equals [this o] (.equiv this o)))

(defn draft?
  "Whether `x` is a draft."
  [x]
  (instance? Draft x))

;;; ------------------------------------------------------------------ types of values

(defn- marks-of ^Object [d] @(.-cell ^Draft d))

(defn- type-of-class
  "The type whose value class or draft class is `c`, as the class itself answers, or nil."
  [^Class c]
  (when-let [g (proof/gate-of c)]
    (let [t (g ::type nil nil)]
      (when (and (map? t) (or (identical? c (::class t)) (identical? c (::draft-class t))))
        t))))

(defn type-of
  "The type `x` belongs to: a type's value class, a sealed value, a draft, or a type itself.
  Nil for anything else."
  [x]
  (cond (class? x)  (type-of-class x)
        (draft? x)  (type-of-class (class (marks-of x)))
        (and (map? x) (contains? x ::class)) x
        :else       (type-of-class (class x))))

(defn- type-of! [x]
  (or (type-of x)
      (throw (ex-info (str "not a value, a draft or a class of a recognitum type: " (pr-str x))
                      {:error :recognitum/not-a-type :value x}))))

(defn- gate
  "The type's installed write. Each arity runs the checks it stands for."
  [type]
  (proof/installed (:strategy type) (::class type)))

(defn- current?
  "Whether `type` is the current definition of its name: no later definition displaced it."
  [type]
  ((gate type) ::current? nil nil))

(defn named-now
  "The type that the namespace of type name `n` maps its name to, if it is a definition of
  `n`, current or not."
  [n]
  (let [c (some-> (find-ns (symbol (namespace n))) (.getMapping (symbol (name n))))]
    (when (class? c)
      (when-let [t (type-of-class c)]
        (when (= n (:name t)) t)))))

(defn- types-in
  "The current types that namespace `n` defines: each its namespace maps its name to."
  [n]
  (for [[s c] (ns-imports n)
        :let [t (type-of-class c)]
        :when (and t (= (:name t) (symbol (str (ns-name n)) (str s))) (current? t))]
    t))

(defn current-types
  "Each type name, to the type it names now: the current definition that the name's namespace
  maps the name to."
  []
  (into {} (for [n (all-ns), t (types-in n)] [(:name t) t])))

(defn refuse-unnamed!
  "Refuses selection `sel` where a keyword in it names no concern or bundle check (see
  [[recognitum.select/named]]) and no check of a current type of the keyword's namespace.
  Returns `sel`."
  [sel]
  (doseq [k (select/keywords sel)]
    (when-not (or (select/named k)
                  (some #(contains? (:ids %) k) (some-> (namespace k) symbol find-ns types-in)))
      (throw (ex-info (str "no concern, bundle check or check of a type is named " k)
                      {:error :recognitum/unknown-name :name k}))))
  sel)

(defn- rewrite [d data] ((gate (type-of d)) ::rewrite d data))

(defn- print-value
  "Prints a value of any type, current or displaced, as its tagged literal."
  [v ^java.io.Writer w]
  (if-let [t (type-of-class (class v))]
    (do (.write w (str "#" (:name t) " "))
        (print-method (into {} (seq v)) w))
    (print-method (into {} (seq v)) w)))

(.addMethod ^clojure.lang.MultiFn print-method recognitum.proof.Proven print-value)

(prefer-method print-method recognitum.proof.Proven clojure.lang.IPersistentMap)
(prefer-method print-method recognitum.proof.Proven java.util.Map)

(defmethod print-method Draft [^Draft d ^java.io.Writer w]
  (.write w (str "#recognitum/draft [" (:name (type-of d)) " "))
  (print-method (.-data d) w)
  (.write w "]"))

;;; ------------------------------------------------------------------ reports

(defn- blocked-by
  "The checks in `blocked` that did not run because `id` failed: those waiting on it, directly
  or through another check in `blocked`."
  [type id blocked]
  (loop [acc #{}]
    (let [more (into acc (filter (fn [b] (some #(or (= id %) (acc %)) (get-in type [:by-id b :waits]))))
                     blocked)]
      (if (= more acc) acc (recur more)))))

(defn- pulled-back
  "For each failed or blocked check that an `except` in `sel` removed but a wait pulled back
  in: the checks that wait on it, and why it ran."
  [type sel picked to-run ids]
  (let [ex (select/excepted type sel)]
    (into {} (for [id ids
                   :when (and (contains? ex id) (not (contains? picked id)))
                   :let [ws (set (for [w to-run :when (contains? (get-in type [:by-id w :waits]) id)] w))
                         names (str/join ", " (sort ws))]]
               [id {:waited-on-by ws
                    :hint (str id " ran although an except removed it, because " names
                               (if (next ws) " wait" " waits") " on it, and a check runs whenever a"
                               " check that waits on it runs. To leave it out, also remove " names ".")}]))))

(defn- report
  "The report of a run: nil where nothing failed or was blocked, else `{:failed :blocked}`, each
  failed check's entry holding its failure `:data`, its `:doc`, and the checks it `:blocked`."
  ([type result] (report type result nil))
  ([type {:keys [failed blocked]} pull]
   (when (or (seq failed) (seq blocked))
     (cond-> {:failed (into {} (for [[id data] failed]
                                 [id (cond-> {:data data :blocked (blocked-by type id blocked)}
                                       (get-in type [:by-id id :doc]) (assoc :doc (get-in type [:by-id id :doc])))]))
              :blocked blocked}
       pull (as-> r (let [pb (pull (concat (keys failed) blocked))]
                      (cond-> r (seq pb) (assoc :pulled-back pb))))))))

(defn- show [x]
  (if (instance? Throwable x)
    (str "#error[" (.getName (class x)) " " (pr-str (ex-message x)) "]")
    (pr-str x)))

(defn- failure-message [type {:keys [failed blocked pulled-back]}]
  (str (:name type) " failed " (count failed) (if (= 1 (count failed)) " check" " checks")
       (when (and (empty? failed) (seq blocked)) (str "; blocked: " (str/join ", " (sort blocked))))
       (apply str (for [[id {:keys [data doc blocked]}] (sort-by key failed)]
                    (str "\n  " id (when doc (str " (" doc ")")) ": " (show data)
                         (when (seq blocked) (str ", which blocked " (str/join ", " (sort blocked)))))))
       (apply str (for [[_ {:keys [hint]}] (sort-by key pulled-back)] (str "\n  " hint)))))

(defn- displaced-refusal
  "The refusal of a write to `v`, or of a method called with it, where `v` is a value of the type
  named `tname` and a later definition has displaced the one `v` belongs to."
  [tname v]
  (ex-info (str tname " was redefined: this value is of an earlier definition, so it is not"
                " sealed and refuses writes and its type's methods. To prove it under the current"
                " definition: (rec/seal (rec/draft v))")
           {:error :recognitum/displaced :type tname :value v}))

(defn- displaced-constructor
  "The refusal of a constructor of the type named `tname`, held from before a later definition
  displaced the one it makes. `remedy`, if given, says what to call instead of the current
  `map->T`."
  [tname remedy]
  (ex-info (str tname " was redefined: this constructor is of an earlier definition and makes"
                " nothing. "
                (or remedy (str "Call the current " (namespace tname) "/map->" (name tname))))
           {:error :recognitum/displaced-constructor :type tname}))

(defn- displaced-draft
  "The refusal to seal a draft of the type named `tname`, opened from the class of a definition
  that a later one displaced."
  [tname]
  (let [n (namespace tname), t (name tname)]
    (ex-info (str tname " was redefined: this draft is of an earlier definition and seals nothing."
                  " Make a value of the current definition with " n "/map->" t ", or seal a draft of"
                  " the class " n " now names " t ": (rec/seal (into (rec/draft " t ") m))")
             {:error :recognitum/displaced-constructor :type tname})))

(defmacro ^:private guard-arities
  "A fn of any number of arguments, at least one, that calls `f` with them while `current` (a
  volatile) holds true, and else calls `refuse` with the first."
  [current refuse f]
  (let [ps (mapv #(symbol (str "p" %)) (range 20))
        more (symbol "more")]
    `(fn ~@(for [n (range 1 21) :let [args (subvec ps 0 n)]]
             (list args `(if (deref ~current) (~f ~@args) (~refuse ~(first args)))))
         ~(list (conj ps '& more) `(if (deref ~current) (apply ~f ~@ps ~more) (~refuse ~(first ps)))))))

(defn- guarded
  "`f`, a method's function, refusing its value once a later definition displaces the type named
  `tname`, which `current` tells."
  [current tname f]
  (let [refuse (fn [v] (throw (displaced-refusal tname v)))]
    (guard-arities current refuse f)))

(defn- check-failed [type rep]
  (ex-info (failure-message type rep)
           (assoc rep :error :recognitum/check-failed :type (:name type))))

(defn- refuse-undeclared! [type ks]
  (when-let [bad (seq (remove (:keys type) ks))]
    (throw (ex-info (str (:name type) " has no field " (str/join ", " (map pr-str (sort-by str bad)))
                         ". Its fields: " (str/join " " (map :key (:fields type))))
                    {:error :recognitum/undeclared-keys :type (:name type) :keys (set bad)}))))

(defn- refuse-missing! [type data]
  (when-let [missing (seq (remove #(contains? data %) (:required type)))]
    (throw (ex-info (str (:name type) " needs the field " (str/join ", " (map pr-str (sort missing))))
                    {:error :recognitum/missing-fields :type (:name type) :keys (set missing)}))))

;;; ------------------------------------------------------------------ running checks

(defn- run
  "The result of running every check of `type` on `data` except those in `assumed`."
  [type data assumed]
  (if (= (count assumed) (count (:ids type)))
    {:failed {} :blocked #{}}
    (first ((:run-all type) [[(:engine type) data assumed]]))))

(defn- passed-in
  "The checks of `to-run` that passed in `result`."
  [to-run {:keys [failed blocked]}]
  (reduce disj (set/difference to-run blocked) (keys failed)))

(defn changed-keys
  "The keys whose entry differs between `old` and `new`: added, removed, or holding a value
  that is not identical."
  [old new]
  (let [none (Object.)
        n (reduce-kv (fn [acc k v] (if (identical? v (get old k none)) acc (conj! acc k)))
                     (transient #{}) new)]
    (persistent! (reduce (fn [acc k] (if (contains? new k) acc (conj! acc k))) n (keys old)))))

(defn bound-check
  "A compiled check of a bundle, run in a type: `run` given the map from each input key to
  what its input clause in `inputs` (a map from input key to a function of the value)
  answers. `inputs` holds the clauses of the inputs the check reads, and each runs only when
  the check runs. An input whose clause reads an absent field is absent."
  [run inputs]
  (fn [value]
    (run (reduce-kv (fn [m k f]
                      (let [x (try (f value)
                                   (catch clojure.lang.ExceptionInfo e
                                     (if (contains? (ex-data e) :recognitum.checks/absent) ::absent (throw e))))]
                        (if (= ::absent x) m (assoc m k x))))
                    {} inputs))))

(defn- present-only
  "`run`, where the field `k` is present; passing where it is absent."
  [k run]
  (fn [value] (if (contains? value k) (run value) true)))

;;; ------------------------------------------------------------------ definition

(defn- run-jobs
  "Runs the `:job` of every entry of `entries` in one call of `run-all`, hands each entry's
  result to its `:done`, and returns a map from each entry's `:i` to its result."
  [run-all entries]
  (if (empty? entries)
    {}
    (let [results (run-all (mapv :job entries))]
      (doseq [[e r] (map vector entries results)] ((:done e) r))
      (zipmap (map :i entries) results))))

(defn waits-of
  "Each check of `checks` (each `{:id :waits :reads :shape-of}`), to every check it waits on:
  those its `:waits` names, and the shape check of each key it `:reads`, other than itself."
  [checks]
  (let [shape-id (into {} (keep (fn [c] (when-let [k (:shape-of c)] [k (:id c)]))) checks)]
    (into {} (for [{:keys [id waits reads]} checks]
               [id (into (set waits) (comp (keep shape-id) (remove #{id})) reads)]))))

(defn- compile-type
  [{:keys [name fields checks]} strategy define run-all]
  (let [optional (set (keep #(when (:optional? %) (:key %)) fields))
        checks (let [w (waits-of checks)] (mapv (fn [c] (assoc c :waits (w (:id c)))) checks))
        by-id (into {} (map (juxt :id identity)) checks)]
    {:name     name
     :fields   (mapv #(select-keys % [:key :sym :optional?]) fields)
     :keys     (set (map :key fields))
     :optional optional
     :required (set (remove optional (map :key fields)))
     :checks   (mapv #(dissoc % :run) checks)
     :by-id    (update-vals by-id #(dissoc % :run))
     :ids      (set (keys by-id))
     :readers  (apply merge-with into {} (for [c checks, k (:reads c)] {k #{(:id c)}}))
     :strategy strategy
     :run-all  run-all
     :engine   (define (into {} (for [{:keys [id waits shape-of gate run]} checks]
                                  [id {:after waits
                                       :body (cond->> run
                                               (contains? optional shape-of) (present-only shape-of)
                                               gate (present-only gate))}])))}))

(defn class-name
  "The name of the class of the values of the type named `type-name`, a qualified symbol, as
  `strategy` names it under every definition of the type."
  ^String [strategy type-name]
  (proof/class-name strategy type-name))

(defn refusal
  "Nil where `strategy` would define a type whose values implement `interfaces` with methods of
  `signatures`, as [[define-type]] takes them, else the strategy's refusal of that definition:
  a map with `:kind`, `:message`, and, by kind, `:method` (`[name arity]`, the name a symbol
  spelled as the signature spells it) or `:interfaces`.
  Asked before the methods' functions exist: each signature is passed with a placeholder
  function. `recognitum.define` gives its own message, at the method's line, for the kind
  `:map-method` where `:method` names a method of the definition; for every other kind it
  gives `:message`."
  [strategy interfaces signatures]
  (proof/refusal strategy {:interfaces (vec interfaces)
                           :methods (into {} (map (fn [sig] [sig (fn [& _])])) signatures)}))

(defn define-type
  "Defines a type and returns it, a map describing it that holds no key. `spec`:

  - `:name`, a qualified symbol, which also names the type's class;
  - `:fields`, each `{:key :sym :optional?}`;
  - `:checks`, each `{:id :waits :run}` and optionally `:concerns`, `:doc`, `:reads` (the keys it
    reads), `:sources` (the sources it reads, compared with `=` by `fetching`), `:fetches?`,
    `:shape-of` (the key of the field it is the shape check of) and `:gate` (a key: where it
    is absent, the check passes without running). `:run` is a compiled check of the engine.
    A check waits on the checks in its `:waits` and on the shape check of each key it reads,
    as [[waits-of]] says. The shape check of an optional field passes where its field is
    absent;
  - `:interfaces`, the interfaces every sealed value implements (a protocol's among them), and
    `:methods`, a map from each method's signature to its function, as the proof strategy's
    definition takes them. A function takes the value first.

  `wiring` names the proof `:strategy` and the engine's `:define` and `:run-all`. Defining a
  type under a name already defined is redefinition: the definition that the name's namespace
  maps the name to is displaced. Its values stay values of their class and are no longer
  proven: a write that changes one, and every method but `toString` called with one, throws
  the refusal `:recognitum/displaced`. [[draft]] of one gives a draft of the type's current
  definition. [[make]] with the displaced definition, and [[seal]] of a draft of its class,
  throw the refusal `:recognitum/displaced-constructor`, so a constructor or a class held from
  before makes nothing."
  [spec {:keys [strategy define run-all]}]
  (let [displaced (named-now (:name spec))
        current (volatile! true)
        h (proof/define-type strategy (:name spec)
                             {:interfaces (vec (:interfaces spec))
                              :methods (into {} (for [[[n ps :as sig] f] (:methods spec)]
                                                  [sig (if (and (= "toString" n) (empty? ps))
                                                         f
                                                         (guarded current (:name spec) f))]))})
        [vc dc] (proof/classes h)
        type (assoc (compile-type spec strategy define run-all) ::class vc ::draft-class dc)
        ids (:ids type)
        marks (fn [data ids]
                (let [m (proof/open h)]
                  (proof/mark! h m (DataKey. data))
                  (doseq [id ids] (proof/mark! h m id))
                  m))
        new-draft (fn [data ids mta] (Draft. data (atom (marks data ids)) mta))
        seal! (fn [data assumed]
                (when-let [rep (report type (run type data assumed))]
                  (throw (check-failed type rep)))
                (refuse-missing! type data)
                (proof/seal h data))
        own-draft (fn [d]
                    (let [m (when (draft? d) (marks-of d))]
                      (when-not (instance? dc m)
                        (throw (ex-info (str "not a draft of " (:name type) ": " (pr-str d))
                                        {:error :recognitum/not-a-draft :type (:name type)})))
                      [(.-data ^Draft d) (passes type (.-data ^Draft d) m) m]))
        ;; A run writes what it found over the memory it started from, less `forget`. Where
        ;; another run has written since, it only forgets what failed and `forget`: a pass it
        ;; found may predate a forget written since.
        remember! (fn remember!
                    ([d m0 known to-run result] (remember! d m0 known to-run result ()))
                    ([^Draft d m0 known to-run result forget]
                     (let [data (.-data d)
                           failed (keys (:failed result))]
                       (when-not (compare-and-set! (.-cell d) m0
                                                   (marks data (reduce disj
                                                                       (into (reduce disj known forget)
                                                                             (passed-in to-run result))
                                                                       failed)))
                         (swap! (.-cell d) #(marks data (reduce disj (passes type data %) (concat failed forget))))))))
        check-group (fn [groups entries]
                      (let [[group & more] groups
                            entries (into entries
                                          (keep (fn [[i d picked]]
                                                  (let [[data known m0] (own-draft d)
                                                        named (set/intersection ids (set picked))
                                                        to-run (set/difference (select/closure type named) known)]
                                                    (when (seq to-run)
                                                      {:i i
                                                       :job [(:engine type) data (set/difference ids to-run)]
                                                       :done #(remember! d m0 known to-run %)}))))
                                          group)]
                        (if-let [[[[_ d]]] (seq more)]
                          ((gate (type-of! d)) ::check-all (vec more) entries)
                          (run-jobs (:run-all type) entries))))
        run-draft (fn [d to-run [data known m0] forget]
                    (let [result (run type data (set/difference ids to-run))]
                      (remember! d m0 known to-run result forget)
                      result))
        write (fn [old entries]
                (when-not (instance? vc old)
                  (throw (ex-info (str "a write starts from a value of " (:name type))
                                  {:error :recognitum/not-a-value :type (:name type)})))
                (when-not @current
                  (throw (displaced-refusal (:name type) old)))
                (let [changed (changed-keys old entries)]
                  (if (empty? changed)
                    old
                    (do (refuse-undeclared! type (filter #(contains? entries %) changed))
                        (seal! (with-meta entries (meta old))
                               (reduce disj ids (mapcat (:readers type) changed)))))))]
    (proof/install-write!
     h
     (fn
       ([old entries] (write old entries))
       ([op x y]
        (case op
          ::make (let [data (proof/plain-map x)]
                   (when-not @current
                     (throw (displaced-constructor (:name type) y)))
                   (refuse-undeclared! type (keys data))
                   (seal! data #{}))
          ::open (new-draft {} #{} nil)
          ::draft-of (if (instance? vc x)
                       (new-draft (proof/plain-map x) ids (meta x))
                       (throw (ex-info (str "not a value of " (:name type)) {:error :recognitum/not-a-value})))
          ::rewrite (let [[data known] (own-draft x)
                          y (proof/plain-map y)
                          changed (changed-keys data y)]
                      (if (empty? changed)
                        x
                        (new-draft y (reduce disj known (mapcat (:readers type) changed)) (meta x))))
          ::clean (let [[data known] (own-draft x)
                        kept (select-keys data (:keys type))]
                    (if (= (count kept) (count data)) x (new-draft kept known (meta x))))
          ::seal (let [[data known m0] (own-draft x)]
                   (when-not @current
                     (throw (displaced-draft (:name type))))
                   (refuse-undeclared! type (keys data))
                   (let [to-run (set/difference ids known)
                         result (run type data known)]
                     (remember! x m0 known to-run result)
                     (when-let [rep (report type result)] (throw (check-failed type rep)))
                     (refuse-missing! type data)
                     (proof/seal h (with-meta data (meta x)))))
          ::check (let [[_ known :as own] (own-draft x)
                        named (set/intersection ids (set y))]
                    (run-draft x (set/difference (select/closure type named) known) own ()))
          ::check-all (check-group x y)
          ::type type
          ::current? @current
          ::displace (do (vreset! current false) nil)
          ::recheck (let [[_ known :as own] (own-draft x)
                          named (set/intersection ids (set y))]
                      ;; a named check that reads a source is forgotten, then remembered only if this run passes it
                      (run-draft x (into named (set/difference (select/closure type named) known)) own
                                 (filter #(:fetches? ((:by-id type) %)) named)))))))
    (when displaced
      ((gate displaced) ::displace nil nil))
    type))

;;; ------------------------------------------------------------------ values

(defn value-class
  "The class of `type`'s sealed values."
  [type]
  (::class type))

(defn make
  "A sealed value of `type` holding the entries of map `m`, after every check passes. Throws
  where `m` holds a key the type does not declare, where a check fails, where a required
  field is missing, or where a later definition has displaced `type`; the ex-data's `:error`
  is `:recognitum/undeclared-keys`, `:recognitum/check-failed`, `:recognitum/missing-fields`
  or `:recognitum/displaced-constructor`. `remedy`, a string, is the displaced refusal's advice
  in place of a call to the current `map->T`."
  ([type m] (make type m nil))
  ([type m remedy] ((gate type) ::make m remedy)))

(defn sealed?
  "Whether `x` is a sealed value of the current definition of its type."
  [x]
  (let [t (type-of x)]
    (boolean (and t (not (draft? x)) (not (class? x)) (current? t)))))

(defn draft
  "A draft: of a type (given its class), holding nothing and passing nothing; of a sealed
  value, holding its entries and passing every check; of a value of a displaced definition, a
  draft of the type's current definition holding its entries and passing nothing; of a draft,
  that draft itself, sharing what it remembers."
  [x]
  (cond (draft? x) x
        (class? x) ((gate (type-of! x)) ::open nil nil)
        :else      (let [t (type-of! x)]
                     (if-let [now (when-not (current? t)
                                    (let [n (named-now (:name t))] (when (and n (current? n)) n)))]
                       (with-meta ((gate now) ::rewrite ((gate now) ::open nil nil) (proof/plain-map x)) (meta x))
                       ((gate t) ::draft-of x nil)))))

(defn clean
  "`d` without the keys its type does not declare, remembering what `d` remembers."
  [d]
  ((gate (type-of! d)) ::clean d nil))

(defn seal
  "The sealed value of draft `d`, after every check it does not remember as passing passes.
  Refuses a draft holding an undeclared key before any check runs. Otherwise `d` remembers
  each check that passed and forgets each that failed, whether or not the seal succeeds.
  Throws as [[make]] does; a draft of a displaced definition's class is refused before any
  check runs. A sealed value is returned as it is."
  [d]
  (cond (draft? d)  ((gate (type-of! d)) ::seal d nil)
        (sealed? d) d
        :else (throw (ex-info (str "not a draft or a sealed value: " (pr-str d))
                              {:error :recognitum/not-a-draft :value d}))))

(defn- passes-of
  "The checks `x` passes, as far as anything shows: every check for a sealed value, what a draft
  remembers."
  [type x]
  (cond (draft? x) (passes type (.-data ^Draft x) (marks-of x))
        (sealed? x) (:ids type)
        :else #{}))

(defn- data-of [x] (if (draft? x) (.-data ^Draft x) x))

(defn- answer [type result sel picked to-run]
  (or (report type result #(pulled-back type sel picked to-run %)) true))

(defn check
  "Runs the checks `sel` names in `x`, and those they wait on, that `x` does not pass already.
  A draft remembers what passed and forgets what failed. Returns true, or a report
  `{:failed {id {:data :doc :blocked}} :blocked #{id}}` with `:pulled-back` where an `except`
  removed a check that a wait then ran."
  [x sel]
  (let [type (type-of! x)
        picked (select/select type sel)]
    (if (draft? x)
      (let [known (passes-of type x)
            to-run (set/difference (select/closure type picked) known)]
        (answer type ((gate type) ::check x picked) sel picked to-run))
      true)))

(defn recheck
  "Runs the checks `sel` names in `x` whether or not `x` passes them, and those they wait on that
  `x` does not pass. A draft remembers as [[check]] does; a sealed value is left as it is.
  Returns as [[check]] does."
  [x sel]
  (let [type (type-of! x)
        picked (select/select type sel)
        known (passes-of type x)
        to-run (into picked (set/difference (select/closure type picked) known))]
    (answer type
            (if (draft? x)
              ((gate type) ::recheck x picked)
              (run type x (set/difference (:ids type) to-run)))
            sel picked to-run)))

(defn check-all
  "Runs, over every value in `xs`, the checks `sel` names in it and those they wait on that it
  does not pass already, in one run of the engine, so values that read the same source, of one
  type or several, share its calls. Each draft remembers what passed and forgets what failed,
  as under [[check]]. Returns a vector with one answer per value, each true or a report as
  [[check]] returns."
  [xs sel]
  (let [plans (mapv (fn [x]
                      (let [type (type-of! x)
                            picked (select/select type sel)
                            known (passes-of type x)]
                        {:x x :type type :picked picked
                         :to-run (set/difference (select/closure type picked) known)}))
                    xs)
        todo (filterv #(seq (:to-run (plans %))) (range (count plans)))
        drafts (filterv #(draft? (:x (plans %))) todo)
        others (vec (for [i todo
                          :let [{:keys [x type to-run]} (plans i)]
                          :when (not (draft? x))]
                      {:i i :job [(:engine type) x (set/difference (:ids type) to-run)] :done (fn [_])}))
        groups (vec (for [is (vals (group-by #(:type (plans %)) drafts))]
                      (mapv (fn [i] (let [{:keys [x picked]} (plans i)] [i x picked])) is)))
        by-index (cond
                   (seq groups) (let [[[_ d]] (first groups)]
                                  ((gate (type-of! d)) ::check-all groups others))
                   (seq others) (run-jobs (:run-all (:type (plans (first todo)))) others)
                   :else {})]
    (mapv (fn [i]
            (let [{:keys [type picked to-run]} (plans i)]
              (if-let [r (by-index i)] (answer type r sel picked to-run) true)))
          (range (count plans)))))

(defn proves?
  "Whether `x` passes every check `sel` names in it, every check those wait on, and the shape
  check of each field it must or does hold. Always true for a sealed value of a type's current
  definition; for a draft, a matter of what it remembers; false for anything else."
  [x sel]
  (if-let [type (when-not (class? x) (type-of x))]
    (or (sealed? x)
        (and (draft? x)
             (set/subset? (select/closure type (into (select/select type sel)
                                                     (select/floor type (data-of x))))
                          (passes-of type x))))
    false))

(defn proven-checks
  "The ids of the checks `x` passes: every check of its type for a sealed value, those a draft
  remembers."
  [x]
  (passes-of (type-of! x) x))

(defn proven-concerns
  "The ids of the concerns whose checks `x` passes, as [[proves?]] answers for each."
  [x]
  (let [type (type-of! x)]
    (set (for [t (distinct (mapcat :concerns (:checks type)))
               :when (proves? x t)]
           t))))

(defn checks-of
  "The ids of the checks `sel` names in the type of `x`: a value, a draft or a type's class."
  [sel x]
  (select/select (type-of! x) sel))
