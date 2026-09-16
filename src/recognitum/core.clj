(ns recognitum.core
  "A record-like type that carries proof of its own validation.

  A write to a validated value checks that field's predicate and every invariant
  depending on it, so such a value is never observable in an invalid state. A draft is
  not a validated value: writes into it check nothing, which is what lets a multi-step
  update pass through states no single write could produce. [[seal]] closes the gap —
  what it returns is validated exactly as if every predicate and every invariant had
  been rerun."
  (:refer-clojure :exclude [defrecord])
  (:require
   [potemkin :as p]))

(defonce ^:private registry (atom {}))

(defn register! "Associate a class with its model map." [cls model] (swap! registry assoc cls model))

(defonce ^:private readers (atom {}))

(defn register-reader! "Associate a print tag with its validating map constructor." [tag ctor]
  (swap! readers assoc tag ctor))

(defn data-readers
  "`{tag -> validating constructor}` for every type defined so far, to hand `clojure.edn/read`
  or `clojure.edn/read-string` as `:readers`.

  There is no global registration, deliberately. `clojure.edn` never consults `*data-readers*`
  -- it takes `:readers` and nothing else -- and it is the reader you want for anything coming
  from outside the process. `alter-var-root` on `*data-readers*` is worse than useless besides:
  `clojure.main` thread-binds that var, so a registration made while loading a namespace is
  invisible to the very thread that loaded it and visible to every other one.

  Reading is validating. A literal whose contents fail a predicate or an invariant throws at
  read time rather than producing a value that lies about itself.

      (edn/read-string {:readers (rec/data-readers)} s)"
  [] @readers)

;;; ------------------------------------------ Checks ------------------------------------------

(defn unknown-key! [model k]
  (throw (ex-info (format "%s has no field %s" (:name model) (pr-str k))
                  {:error ::unknown-field :record (:name model) :key k
                   :declared (:field-order model)})))

(defn invalid-field! [model k v]
  (let [form (get-in model [:fields k :form])]
    (throw (ex-info (format "%s/%s failed %s: %s" (:name model) (name k) form (pr-str v))
                    {:error ::invalid-field :record (:name model)
                     :key k :value v :predicate form}))))

(defn check-value!
  "Run `k`'s predicate against `v`. `k` is assumed declared -- the caller's `case` established it."
  [model k v]
  (when-not (try ((get (:preds model) k) v) (catch Throwable _ false))
    (invalid-field! model k v)))

(defn check-field!
  "Like [[check-value!]] but for a key that may not be declared at all."
  [model k v]
  (if-let [pred (get (:preds model) k)]
    (when-not (try (pred v) (catch Throwable _ false))
      (invalid-field! model k v))
    (unknown-key! model k)))

(deftype Invariant [doc keys pred])

(defn ->invariant
  "Turn a literal {:doc :keys :pred} map into an [[Invariant]], whose fields are read directly
  rather than looked up on every run."
  [{:keys [doc keys pred]}]
  (Invariant. doc keys pred))

(defn build-index
  "`{key -> [invariant ...]}` so a write looks up its invariants instead of scanning them."
  [invariants]
  (reduce (fn [idx inv]
            (reduce #(update %1 %2 (fnil conj []) inv) idx (.-keys ^Invariant inv)))
          {} invariants))

(defn invariant-failed! [model ^Invariant inv m]
  (let [ks (.-keys inv) doc (.-doc inv)]
    (throw (ex-info (format "%s violates invariant: %s" (:name model) doc)
                    {:error ::invariant-violated :record (:name model)
                     :invariant doc :depends-on ks
                     :values (select-keys m ks)}))))

(def ^:dynamic *audit-invariant-keys*
  "When true, every invariant runs against a proxy recording which keys its predicate reads,
  and reading one the invariant does not declare throws [[undeclared-invariant-key!]].

  Bind it in tests, not in production: the proxy costs an allocation per invariant run and a
  layer of indirection per key read.

  It can only speak for invariants that actually run, and an invariant is skipped precisely
  when none of its declared keys were touched -- so it will not catch a bad `:keys` on the
  write that exploits it. Construction touches every field and therefore runs every invariant,
  so building one instance under the audit does catch every mis-declaration in a type."
  false)

(p/def-map-type KeyAuditor [m seen]
  ;; `seen` is a volatile holding the set of keys read so far, or ::all once the predicate has
  ;; asked for the whole map -- at which point nothing can be attributed and the audit abstains.
  (get [_ k default] (vswap! seen #(if (= ::all %) % (conj % k))) (get m k default))
  (assoc [_ k v] (KeyAuditor. (assoc m k v) seen))
  (dissoc [_ k] (KeyAuditor. (dissoc m k) seen))
  (keys [_] (vreset! seen ::all) (keys m))
  (meta [_] (meta m))
  (with-meta [_ mta] (KeyAuditor. (with-meta m mta) seen)))

(defn undeclared-invariant-key! [model ^Invariant inv undeclared]
  (let [doc (.-doc inv) declared (.-keys inv)]
    (throw (ex-info (format "%s invariant %s reads %s but declares only %s -- seal skips an invariant none of whose declared keys were touched, so this one can be skipped on a write that breaks it"
                            (:name model) (pr-str doc) (pr-str (vec (sort undeclared))) (pr-str declared))
                    {:error ::undeclared-invariant-key :record (:name model)
                     :invariant doc :declared declared :also-read (set undeclared)}))))

(defn run-invariant! [model m ^Invariant inv]
  (if-not *audit-invariant-keys*
    (when-not (try ((.-pred inv) m) (catch Throwable _ false))
      (invariant-failed! model inv m))
    (let [seen (volatile! #{})
          ok   (try ((.-pred inv) (KeyAuditor. m seen)) (catch Throwable _ false))
          read @seen]
      ;; A mis-declared `:keys` is a defect in the type, so it outranks a mere data failure.
      (when-not (= ::all read)
        (when-let [undeclared (seq (remove (.-keys inv) read))]
          (undeclared-invariant-key! model inv undeclared)))
      (when-not ok (invariant-failed! model inv m)))))

(defn run-invariants-for!
  "Run the invariants that depend on the single key `k`."
  [model m k]
  (when-let [invs ((:index model) k)]
    (doseq [inv invs] (run-invariant! model m inv))))

(defn check-invariants!
  "Run every invariant depending on a key in `ks`; nil `ks` runs all of them. `ks` is any
  collection -- membership is tested against each invariant's own key set, so a caller never
  has to build one."
  [model m ks]
  (if (nil? ks)
    (doseq [inv (:invariants model)]
      (run-invariant! model m inv))
    (doseq [inv (:invariants model) :when (some (.-keys ^Invariant inv) ks)]
      (run-invariant! model m inv))))

(defn entries-of [o]
  (cond (map? o)                     (seq o)
        (instance? java.util.Map o)  (seq (into {} o))
        (and (vector? o) (= 2 (count o))) [o]
        :else (throw (ex-info "cannot conj this onto a validated value" {:value o}))))

(defn cons-checked
  "Apply every entry, then check invariants once — so `merge` is an atomic multi-key update."
  [model m o]
  (let [es (entries-of o)
        m' (reduce (fn [acc [k v]] (check-field! model k v) (assoc acc k v)) m es)]
    (check-invariants! model m' (map first es))
    m'))

(defn no-dissoc!
  "Declared fields are permanent, so `dissoc` refuses them; where a predicate admits nil,
  `(assoc m k nil)` is how absence is spelled. Undeclared keys dissoc normally."
  [model k]
  (throw (ex-info (format "cannot dissoc %s from %s: declared fields are permanent -- use (assoc m %s nil) if its predicate admits nil"
                          (pr-str k) (:name model) (pr-str k))
                  {:error ::cannot-dissoc :record (:name model) :key k
                   :instead (list 'assoc 'm k nil)})))

;;; ------------------------------------------ Draft ------------------------------------------

(def unset
  "Marks a declared field a draft has never been written to, so `nil` stays a legal value."
  (Object.))

(defonce ^:private draft-registry (atom {}))

(defn register-draft! "Associate a draft class with its model map." [cls model]
  (swap! draft-registry assoc cls model))

(def ^:dynamic *owner*
  "Token identifying the outermost `drafting` currently on the stack, or nil outside one.
  A draft records the token in force when it was created, which is how a nested `drafting`
  tells a draft an enclosing form owns from a free one it must adopt and seal itself."
  nil)

(defn draft? [x] (contains? @draft-registry (class x)))

(defn draft-model [x] (@draft-registry (class x)))

(defn model-of
  "The model for `x`, whether `x` is a validated instance or the type itself."
  [x]
  (let [r @registry] (or (r (class x)) (r x))))

(defn sealed?
  "Is `x` an instance of a registered validated type -- something [[seal]] has already produced?"
  [x] (some? (@registry (class x))))

(defn owner-of [d] ((:draft-owner (draft-model d)) d))

(defn adopt
  "Stamp `d` as owned by `token`, so nested `drafting` forms recognise it."
  ([d token] (adopt (draft-model d) d token))
  ([model d token] ((:draft-with-owner model) d token)))

(defn open
  "The draft a `drafting` works from, already stamped with `token`. A free draft is adopted; a
  validated value or a bare type is opened directly, which skips the extra allocation `adopt`
  would cost."
  [model x token already-draft?]
  (cond
    already-draft?                   (adopt model x token)
    (identical? model (@registry x)) ((:draft-empty model) token)
    :else                            ((:draft-from model) x token)))

(defn draft
  "A draft of `x`: a validated value, a registered type (construction from empty), or a draft.
  A draft created inside a `drafting` records that form as its owner."
  [x]
  (if (draft? x)
    x
    (if-let [model (model-of x)]
      (if (identical? model (@registry x))
        ((:draft-empty model) *owner*)
        ((:draft-from model) x *owner*))
      (throw (ex-info "not a validated value, draft, or registered type" {:value x})))))

(defn extraneous
  "The keys `d` holds that its type does not declare."
  [d]
  (seq (keys ((:draft-ext (draft-model d)) d))))

(defn clean
  "Drop the keys the type does not declare, so `d` can seal. A draft -> draft step: put it at
  the end of a `drafting` body to strip rather than reject."
  [d]
  (when-not (draft? d)
    (throw (ex-info "clean expects a draft" {:value d})))
  (if (extraneous d) ((:draft-clean (draft-model d)) d) d))

(defn- seal-draft [model d]
  ;; Only the touched keys are actually checked, which is equivalent to checking all of
  ;; them: an untouched field still holds a value that already satisfied its predicate,
  ;; and an invariant with no touched key among its own reads only unchanged values.
  ;; That second step is sound only so far as each invariant's `:keys` is accurate.
  (when-let [extra (extraneous d)]
    (throw (ex-info (format "%s has no field(s) %s -- use `clean` to strip them"
                            (:name model) (pr-str (vec extra)))
                    {:error ::extraneous-keys :record (:name model) :keys (vec extra)})))
  (when-let [missing (seq ((:draft-missing model) d))]
    (throw (ex-info (format "%s is missing field %s" (:name model) (first missing))
                    {:error ::missing-field :record (:name model) :key (first missing)})))
  (let [dirty ((:draft-dirty-keys model) d)]
    (doseq [k dirty] (check-value! model k (get d k)))
    (when (seq dirty) (check-invariants! model d dirty)))
  ((:draft-seal model) d))

(defn seal
  "Validate `d` and return the sealed value. What comes back is validated exactly as if
  every field predicate and every invariant had been rerun.

  Idempotent: an already-sealed value passes straight through, so `seal` is a projection and a
  sealed value is a fixed point of it. That is what lets a `drafting` body end in a call that
  may or may not have sealed already, without the caller having to know which."
  ([d]
   (cond
     (draft? d)  (seal (draft-model d) d)
     (sealed? d) d
     :else (throw (ex-info "seal expects a draft or an already-sealed value"
                           {:error ::cannot-seal :value d}))))
  ([model d]
   (if-not (identical? model (draft-model d))
     (if (identical? model (model-of d))
       d
       (throw (ex-info (format "%s: expected a draft or an already-sealed %s, got %s"
                               (:name model) (:name model) (pr-str (class d)))
                       {:error ::cannot-seal :record (:name model) :value d})))
     (seal-draft model d))))

(defn owned-here?
  "Is `x` a draft belonging to a `drafting` form already on the stack?"
  [x]
  (and (draft? x) (some? *owner*) (= *owner* (owner-of x))))

(defmacro drafting
  "Bind `sym` to a draft of `init`, evaluate `body`, and seal the result.

  `init` may be a validated value, a registered type (to build one from nothing), or a
  draft. A draft an enclosing `drafting` owns passes straight through unsealed, so only
  the outermost form validates — that is what lets an impl fn taking and returning a
  draft be called standalone or nested without knowing which. A draft made outside any
  `drafting` is adopted and sealed here."
  [[sym init] & body]
  `(let [init# ~init
         dm#   (draft-model init#)]
     (if (and dm# (some? *owner*) (= *owner* ((:draft-owner dm#) init#)))
       (let [~sym init#] ~@body)
       (let [model# (or dm# (model-of init#))
             token# (Object.)]
         (when-not model#
           (throw (ex-info "not a validated value, draft, or registered type" {:value init#})))
         (binding [*owner* token#]
           (let [~sym (open model# init# token# (some? dm#))]
             (seal model# (do ~@body))))))))

;;; ------------------------------------------ Macro ------------------------------------------

(defmacro defrecord
  "Define a validated map-like type with a real field per declared key, so reads and writes
  are field access rather than map lookups. Each field carries a `^{:valid pred}` predicate;
  `invariants` is a vector of {:doc :keys :pred} maps."
  [tname fields invariants]
  (let [model-sym (symbol (str tname "-model"))
        fsyms     (mapv #(with-meta % nil) fields)
        ks        (mapv keyword fields)
        n         (count fsyms)
        ;; every local is gensymed: a field could be named k, v, o, acc ...
        [ksym vsym dsym osym hsym essym ovsym accsym mtasym mta2sym msym thunksym tgtsym]
        (map gensym ["k" "v" "d" "o" "h" "es" "ov" "acc" "mta" "mta2" "m" "thunk" "target"])
        tgthint   (with-meta tgtsym {:tag tname})
        dname     (symbol (str tname "Draft"))
        [extsym dirtysym ownsym tokensym] (map gensym ["ext" "dirty" "own" "token"])
        dvsym     (with-meta (gensym "d") {:tag dname})
        svsym     (with-meta (gensym "s") {:tag tname})
        dfield    (fn [f] (list '. dvsym (symbol (str "-" f))))
        sfield    (fn [f] (list '. svsym (symbol (str "-" f))))
        dctor     (fn [vals ext dirty own mta] `(new ~dname ~@vals ~ext ~dirty ~own ~mta))
        dfields   (mapv dfield fsyms)
        inv-maps  (vec invariants)
        pred-var  (fn [f] (symbol (str tname "--pred-" f)))
        inv-var   (fn [i] (symbol (str tname "--inv-" i)))
        invp-var  (fn [i] (symbol (str tname "--inv-" i "-pred")))
        ;; resolved at macroexpansion: which invariants does each field appear in?
        invs-for  (fn [k] (filterv #(contains? (:keys (nth inv-maps %)) k) (range (count inv-maps))))
        xsym      (with-meta (gensym "x") {:tag tname})
        params    (conj fsyms mtasym)
        ;; (Heist. a b c mta) with position i replaced by `x`
        ctor-with (fn [i x] `(new ~tname ~@(assoc fsyms i x) ~mtasym))
        field-map (into {} (map (fn [f] [(keyword f) {:form (list 'quote (:valid (meta f)))}])) fields)
        pred-map  (into {} (map (fn [f] [(keyword f) (:valid (meta f))])) fields)
        tagsym    (symbol (str (ns-name *ns*)) (str tname))]
    `(do
       (declare ~model-sym)
       ~@(map (fn [f] `(def ~(with-meta (pred-var f) {:private true}) ~(:valid (meta f)))) fields)
       ~@(mapcat (fn [i]
                   (let [{:keys [doc keys pred]} (nth inv-maps i)]
                     [`(def ~(with-meta (invp-var i) {:private true}) ~pred)
                      `(def ~(with-meta (inv-var i) {:private true})
                         (Invariant. ~doc ~keys ~(invp-var i)))]))
                 (range (count inv-maps)))
       (p/def-map-type ~tname ~params
         (~'get [_# ~ksym ~dsym]
           (case ~ksym ~@(interleave ks fsyms) ~dsym))

         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [i k]
                         [k `(do (check-value! ~model-sym ~k ~vsym)
                                 (let [~hsym ~(ctor-with i vsym)]
                                   (run-invariants-for! ~model-sym ~hsym ~k)
                                   ~hsym))])
                       (range n) ks)
             (unknown-key! ~model-sym ~ksym)))

         ;; A validated value cannot hold an undeclared key, so dissoc of one is the
         ;; ordinary map no-op. Only the declared fields are permanent.
         (~'dissoc [this# ~ksym]
           (case ~ksym
             ~(apply list ks) (no-dissoc! ~model-sym ~ksym)
             this#))
         (~'keys [_#] ~ks)
         (~'meta [_#] ~mtasym)
         (~'with-meta [_# ~mta2sym] (new ~tname ~@fsyms ~mta2sym))

         ;; one allocation for the whole merge, invariants once at the end
         (~'cons [_# ~osym]
           (let [~essym (entries-of ~osym)
                 ~ovsym (reduce (fn [~accsym [~ksym ~vsym]]
                                  (check-field! ~model-sym ~ksym ~vsym)
                                  (assoc ~accsym ~ksym ~vsym))
                                {} ~essym)
                 ~hsym  (new ~tname
                             ~@(map (fn [k f] `(get ~ovsym ~k ~f)) ks fsyms)
                             ~mtasym)]
             (check-invariants! ~model-sym ~hsym (map first ~essym))
             ~hsym))

         ;; A keyword call site caches one of these and reads the field directly, the way a
         ;; record does; without it `(:k x)` pays the full Keyword -> RT -> ILookup -> valAt walk.
         clojure.lang.IKeywordLookup
         (~'getLookupThunk [_# ~ksym]
           (case ~ksym
             ~@(mapcat (fn [[k f]]
                         [k `(reify clojure.lang.ILookupThunk
                               (~'get [~thunksym ~tgtsym]
                                 (if (identical? (class ~tgtsym) ~tname)
                                   (. ~tgthint ~(symbol (str "-" f)))
                                   ~thunksym)))])
                       (map vector ks fsyms))
             nil)))

       ;; The draft: the same real fields, plus a map for keys the type does not declare and a
       ;; bitmask of which declared fields have been written. Extraneous keys set no bit, so
       ;; `clean` only has to empty the map.
       (p/def-map-type ~dname ~(into fsyms [extsym dirtysym ownsym mtasym])
         (~'get [_# ~ksym ~dsym]
           (case ~ksym
             ~@(mapcat (fn [[k f]] [k `(if (identical? ~f unset) ~dsym ~f)]) (map vector ks fsyms))
             (get ~extsym ~ksym ~dsym)))
         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [i k]
                         [k (dctor (assoc fsyms i vsym) extsym `(bit-set ~dirtysym ~i) ownsym mtasym)])
                       (range n) ks)
             ~(dctor fsyms `(assoc ~extsym ~ksym ~vsym) dirtysym ownsym mtasym)))
         (~'dissoc [this# ~ksym]
           (case ~ksym
             ~(apply list ks) (no-dissoc! ~model-sym ~ksym)
             (if (contains? ~extsym ~ksym)
               ~(dctor fsyms `(dissoc ~extsym ~ksym) dirtysym ownsym mtasym)
               this#)))
         (~'keys [_#]
           (concat (cond-> [] ~@(mapcat (fn [[k f]] [`(not (identical? ~f unset)) `(conj ~k)])
                                        (map vector ks fsyms)))
                   (keys ~extsym)))
         (~'meta [_#] ~mtasym)
         (~'with-meta [_# ~mta2sym] ~(dctor fsyms extsym dirtysym ownsym mta2sym))
         (~'cons [this# ~osym]
           (reduce (fn [~accsym [~ksym ~vsym]] (assoc ~accsym ~ksym ~vsym)) this# (entries-of ~osym))))

       (def ~model-sym
         (let [invs# ~(mapv inv-var (range (count inv-maps)))]
           {:name        '~tname
            :fields      ~field-map
            :preds       ~pred-map
            :field-order ~ks
            :invariants  invs#
            :index       (build-index invs#)
            :tag         '~tagsym
            :draft-empty      (fn [~ownsym] ~(dctor (repeat n `unset) {} 0 ownsym nil))
            :draft-from       (fn [~svsym ~ownsym]
                                ~(dctor (mapv sfield fsyms) {} 0 ownsym `(meta ~svsym)))
            :draft-owner      (fn [~dvsym] ~(dfield ownsym))
            :draft-ext        (fn [~dvsym] ~(dfield extsym))
            :draft-with-owner (fn [~dvsym ~tokensym]
                                ~(dctor dfields (dfield extsym) (dfield dirtysym) tokensym (dfield mtasym)))
            :draft-clean      (fn [~dvsym]
                                ~(dctor dfields {} (dfield dirtysym) (dfield ownsym) (dfield mtasym)))
            :draft-dirty-keys (fn [~dvsym]
                                (let [~dirtysym ~(dfield dirtysym)]
                                  (cond-> [] ~@(mapcat (fn [i k] [`(bit-test ~dirtysym ~i) `(conj ~k)])
                                                       (range n) ks))))
            :draft-missing    (fn [~dvsym]
                                (cond-> [] ~@(mapcat (fn [[k f]] [`(identical? ~(dfield f) unset) `(conj ~k)])
                                                     (map vector ks fsyms))))
            :draft-seal       (fn [~dvsym] (new ~tname ~@dfields ~(dfield mtasym)))}))
       ;; `deftype` interns a positional factory that checks nothing -- a public back door
       ;; straight through every guarantee the type exists to make. Replace it with one that
       ;; validates, and unmap the draft's, which nothing outside this file should be calling.
       (defn ~(symbol (str "->" tname))
         ~(str "Construct a validated " tname " from its fields, in declaration order.")
         [~@fsyms]
         ~@(map (fn [k f] `(check-value! ~model-sym ~k ~f)) ks fsyms)
         (let [~hsym (new ~tname ~@fsyms nil)]
           (check-invariants! ~model-sym ~hsym nil)
           ~hsym))

       (defn ~(symbol (str "map->" tname))
         ~(str "Construct a validated " tname " from a map holding exactly its declared keys.\n"
               "  A key it does not declare is an error; a key it declares and the map omits is\n"
               "  reported as :missing-field, so `nil` and absent stay distinct.")
         [~msym]
         (seal ~model-sym (reduce-kv assoc ((:draft-empty ~model-sym) nil) ~msym)))

       (register! ~tname ~model-sym)
       (register-draft! ~dname ~model-sym)
       (register-reader! '~tagsym ~(symbol (str "map->" tname)))

       (ns-unmap *ns* '~(symbol (str "->" dname)))
       ;; A draft is unvalidated working state. `#<` is the reader's own unreadable marker, so
       ;; the printed form says so rather than implying a round trip that should not exist.
       (defmethod print-method ~dname [x# ^java.io.Writer w#]
         (.write w# (str "#<" '~tname " draft " (into (sorted-map) x#) ">")))
       ;; The tag is namespace-qualified, so it escapes the reader's record-literal path and is
       ;; the exact symbol to hand `clojure.edn/read` as a key in `:readers`.
       (defmethod print-method ~tname [x# ^java.io.Writer w#]
         (.write w# (str "#" '~tagsym (into (sorted-map) x#))))
       ~tname)))


