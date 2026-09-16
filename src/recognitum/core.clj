(ns recognitum.core
  "A record-like type that carries proof of its own validation.

  A type declares named checks. A check names, in its arglist, exactly the fields it
  reads, and the body receives those and nothing else -- so the field -> checks index
  is derived from the form rather than declared alongside it, and cannot disagree with
  what the body actually does.

  A check returns `true` -- exactly `true` -- to pass; anything else is a failure, and what
  it returned is the failure data. An exception is neither: a throw is a bug in the check
  rather than a verdict about the data, so it propagates untouched. Nothing stops at the
  first failure either -- every entry point runs everything it was asked to run and throws
  once with all of it, keyed by check name in declaration order.

  A write to a validated value runs the checks naming the field written, so such a value
  is never observable in an invalid state. A draft is not a validated value: writes into
  it check nothing, which is what lets a multi-step update pass through states no single
  write could produce. [[seal]] closes the gap -- what it returns is validated exactly as
  if every check had been rerun.

  Fields become keys qualified by the namespace defining the type, so a bare keyword of
  the same name is a key the type does not declare."
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

  Reading is validating. A literal whose contents fail a check throws at read time rather
  than producing a value that lies about itself.

      (edn/read-string {:readers (rec/data-readers)} s)"
  [] @readers)

;;; ------------------------------------------ Checks ------------------------------------------

(defn unknown-key! [model k]
  (throw (ex-info (format "%s has no field %s" (:name model) (pr-str k))
                  {:error ::unknown-field :record (:name model) :key k
                   :declared (:field-order model)})))

(defn declared-field!
  "Throw unless `k` is a field this type declares."
  [model k]
  (when-not (contains? (:field-set model) k) (unknown-key! model k)))

(defn dep-values
  "What a check was given, keyed by the dependency it was given for, in arglist order.
  Built only where a failure is about to be reported -- the passing path never has to say."
  [check x]
  (apply array-map (mapcat (fn [d] [d (get x d)]) (:deps check))))

(defn failure-entry
  "The `[check-name report]` pair for a check that did not return `true`. `ret` is what it
  returned, which is the failure data; `vals` is what it was given."
  [check ret vals]
  [(:name check) {:doc (:doc check) :values vals :returned ret}])

(defn- failure-str [[cname {:keys [doc values returned]}]]
  (format "%s%s: given %s it returned %s"
          cname (if doc (str " (" doc ")") "") (pr-str values) (pr-str returned)))

(defn checks-failed!
  "Throw one `:check-failed` carrying every failure in `fs`, which is [[failure-entry]] pairs
  in the order the checks ran -- declaration order, everywhere they are run from. `fs` may
  hold `nil` where a check passed, so a call site can emit one expression per check and let
  this work out which of them happened.

  The map is `array-map` applied to the pairs rather than conj-ed together one at a time:
  conj past the eighth entry would promote it to a hash map and lose the very order it is
  reporting in."
  [model fs]
  (let [fs (into [] (remove nil?) fs)]
    (throw (ex-info (str (:name model) " failed "
                         (when (next fs) (str (count fs) " checks -- "))
                         (apply str (interpose "; " (map failure-str fs))))
                    {:error ::check-failed
                     :record (:name model)
                     :failures (apply array-map (into [] cat fs))}))))

(defn checks-for
  "The indices of the checks naming any of `ks`, in declaration order."
  [model ks]
  (into (sorted-set) (mapcat (:index model)) ks))

(defn run-checks!
  "Run the checks at `idxs` against `x`, through the type's own generated runner -- which
  reads the fields each check needs directly rather than looking them up. Only the failure
  path pays for a lookup, to say what the check was given.

  Every one of them runs: a failure does not stop the pass, and what comes back is one throw
  carrying all of them. Only `true` passes, and nothing is caught -- a check that throws is a
  bug in the check rather than a verdict about the data, and \"we could not determine whether
  this is valid\" is a different claim from \"this is invalid\"."
  [model run x idxs]
  (let [cs (:checks model)
        fs (reduce (fn [acc i]
                     (let [ret (run i x)]
                       (if (true? ret)
                         acc
                         (let [c (nth cs i)]
                           (conj acc (failure-entry c ret (dep-values c x)))))))
                   [] idxs)]
    (when (seq fs) (checks-failed! model fs))))

(defn entries-of [o]
  (cond (map? o)                     (seq o)
        (instance? java.util.Map o)  (seq (into {} o))
        (and (vector? o) (= 2 (count o))) [o]
        :else (throw (ex-info "cannot conj this onto a validated value" {:value o}))))

(defn no-dissoc!
  "Declared fields are permanent, so `dissoc` refuses them; where a check admits nil,
  `(assoc m k nil)` is how absence is spelled. Undeclared keys dissoc normally."
  [model k]
  (throw (ex-info (format "cannot dissoc %s from %s: declared fields are permanent -- use (assoc m %s nil) if its checks admit nil"
                          (pr-str k) (:name model) (pr-str k))
                  {:error ::cannot-dissoc :record (:name model) :key k
                   :instead (list 'assoc 'm k nil)})))

(defn qualify-keys
  "Rewrite a bare keyword naming a declared field of `model` into the type's own namespace,
  and leave every other key exactly as it was written.

  That is what lets a printed literal read back: the tag already carries the namespace, so
  the printed map drops it, and the reader -- which is `map->Type` itself -- puts it back.
  Qualifying only what names a field is what keeps the error honest for what does not: a
  stray `:getaway-car` is reported as `:getaway-car` rather than as something the caller
  never wrote. Where a map holds both spellings of one field the qualified entry wins,
  so degenerate input resolves the same way every time rather than by seq order."
  [model m]
  (let [nsp    (:key-ns model)
        fields (:field-set model)
        bare   (reduce-kv (fn [acc k v]
                            (if (simple-keyword? k)
                              (let [q (keyword nsp (name k))]
                                (assoc acc (if (contains? fields q) q k) v))
                              acc))
                          {} m)
        given  (reduce-kv (fn [acc k v] (if (simple-keyword? k) acc (assoc acc k v))) {} m)]
    (conj bare given)))

(defn printable
  "The map a sealed value prints as. A sealed value holds nothing but its declared fields,
  every one of them qualified by the namespace the print tag already names, so repeating
  that namespace on every key would be noise."
  [x]
  (into (sorted-map) (map (fn [[k v]] [(keyword (name k)) v])) x))

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

(defn of-type?
  "Is `x` a value of type `t` -- either form of it?

  `defrecord` generates two classes, so `instance?` asks something narrower than what is
  usually meant by \"is this a Heist\": it holds of a sealed `Heist` and not of a draft of
  that same `Heist`. This is the question about the type. [[draft?]] and [[sealed?]] are the
  questions about the form, of any type, and \"a draft of Heist\" composes from one of each --
  which is the right amount of spelling out for something that specific.

  False for anything the library does not know, `t` itself included: a class is not a value
  of its type, however registered the type is. False, rather than a throw, when `t` names no
  registered type at all -- a predicate that throws is not a predicate.

  A function rather than a predicate generated per type, so a definition interns nothing into
  its namespace that could collide with a name written there by hand."
  [t x]
  (if-let [model (or (@registry t) (@draft-registry t))]
    (let [c (class x)]
      (or (identical? model (@registry c))
          (identical? model (@draft-registry c))))
    false))

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
  ;; Only the checks naming a key the draft wrote are actually run, which is equivalent to
  ;; running all of them: an untouched field still holds the value it had in the validated
  ;; source, and a check none of whose fields were touched reads only unchanged values, so
  ;; its verdict cannot have moved. A check naming no field at all has no verdict to inherit
  ;; from a write, so it runs here regardless.
  (when-let [extra (extraneous d)]
    (throw (ex-info (format "%s has no field(s) %s -- use `clean` to strip them"
                            (:name model) (pr-str (vec extra)))
                    {:error ::extraneous-keys :record (:name model) :keys (vec extra)})))
  (when-let [missing (seq ((:draft-missing model) d))]
    (throw (ex-info (format "%s is missing field %s" (:name model) (first missing))
                    {:error ::missing-field :record (:name model) :key (first missing)})))
  (let [dirty ((:draft-dirty-keys model) d)]
    (run-checks! model (:run-draft model) d
                 (into (checks-for model dirty) (:independent model))))
  ((:draft-seal model) d))

(defn seal
  "Validate `d` and return the sealed value. What comes back is validated exactly as if
  every check had been rerun.

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

(defn unknown-dependency!
  "Report an arglist entry that names nothing this type declares."
  [{:keys [record what fields io-deps] cname :name} entry]
  (let [kind (name what)]
    (throw (ex-info (cond
                      (symbol? entry)
                      (format "%s %s %s names %s, which is not a field of %s -- its fields are %s"
                              record kind (pr-str cname) (pr-str entry) record
                              (pr-str (vec (keys fields))))

                      (keyword? entry)
                      (format "%s %s %s names %s, which is not an io-dep of %s -- %s"
                              record kind (pr-str cname) (pr-str entry) record
                              (if (seq io-deps)
                                (str "its io-deps are " (pr-str io-deps))
                                "it declares no io-deps"))

                      :else
                      (format (str "%s %s %s names %s: an arglist entry is a symbol naming a field "
                                   "or a keyword naming an io-dep, and nothing else")
                              record kind (pr-str cname) (pr-str entry)))
                    (assoc {:error ::unknown-dependency
                            :record record
                            :dependency entry
                            :fields (vec (keys fields))
                            :io-deps io-deps}
                           what cname)))))

(defn resolve-deps
  "Resolve one arglist into the dependency keys it names, in the order it names them.

  The arglist is the dependency declaration, so this is where a declaration either means
  something or the form stops compiling. An entry is a symbol naming a field of this type,
  which resolves to that field's key, or a keyword naming an io-dep of this type, which is
  already its own name. Anything else -- a destructuring form, a rest arg, a parameter named
  `_` -- would be a dependency the macro could not resolve to anything, and is refused with
  the offender named.

  `ctx` is `{:record :what :name :fields :io-deps}`: the type, `:check` or `:io-dep`, the
  name of the thing being resolved, an ordered map of field symbol -> field key, and the set
  of io-dep names. Checks and io-deps are resolved the same way, because they declare
  dependencies the same way."
  [{:keys [fields io-deps] :as ctx} args]
  (mapv (fn [entry]
          (cond
            (symbol? entry)  (or (get fields entry) (unknown-dependency! ctx entry))
            (keyword? entry) (if (contains? io-deps entry)
                               entry
                               (unknown-dependency! ctx entry))
            :else            (unknown-dependency! ctx entry)))
        args))

(defn reject-duplicate-names!
  "A keyword in an arglist means an io-dep and a check never appears in one, so the two
  namespaces only collide where a single name is declared in both places -- at which point
  the reader of an arglist could no longer tell what it is looking at. Refuse it."
  [tname check-names io-dep-names]
  (when-let [dup (some (set io-dep-names) check-names)]
    (throw (ex-info (format "%s declares %s as both a check and an io-dep"
                            tname (pr-str dup))
                    {:error ::duplicate-name :record tname :name dup}))))

(def definition-sections
  "The sections a definition map may hold, in the order `SPEC.md` writes them. Every one of
  them is optional; an absent section is an empty one."
  [:options :checks :io-deps :certificates])

(def known-options
  "What `:options` may hold. `:io-limited` is the only one so far -- the section exists so
  that there can be more."
  #{:io-limited})

(defn parse-definition
  "`defrecord`'s third position -> `{:options :checks :io-deps :certificates}`, with every
  section the form left out filled in empty, so nothing downstream has to ask whether it
  was written.

  Every key is resolved here, while the macro runs, for the same reason every arglist entry
  is: a section the macro does not recognise is a declaration that would mean nothing. A
  mistyped `:cheks` would quietly define a type with no checks at all -- a value asserting
  a validation that never ran, which is the one failure this library exists to make
  impossible -- so it is a compile error naming the offender instead. The same goes for a
  key inside `:options`, and for a bare map of checks written where the definition map now
  goes, whose check names arrive here as sections nobody declared.

  `:io-deps` and `:certificates` are part of the form before they are part of the library.
  An empty one compiles, so a form may be written in its final shape; a populated one is
  refused rather than accepted and ignored, which would be the silent mis-declaration
  again, wearing a different hat."
  [tname definition]
  (when-not (map? definition)
    (throw (ex-info (format (str "%s: defrecord takes a name, a vector of fields and a "
                                 "definition map -- %s is not a map")
                            tname (pr-str definition))
                    {:error ::malformed-definition :record tname :definition definition})))
  (doseq [k (keys definition)]
    (when-not (some #{k} definition-sections)
      (throw (ex-info (format (str "%s declares %s, which is not a section of a definition "
                                   "map -- the sections are %s%s")
                              tname (pr-str k) (pr-str definition-sections)
                              (if (qualified-keyword? k)
                                (str " (a check goes under :checks, which holds the map this "
                                     "position used to take)")
                                ""))
                      {:error ::unknown-section :record tname :section k
                       :sections definition-sections}))))
  (let [sections (reduce (fn [acc s]
                           (let [v (get definition s {})]
                             (when-not (map? v)
                               (throw (ex-info (format "%s: %s is a map, and %s is not one"
                                                       tname s (pr-str v))
                                               {:error ::malformed-section :record tname
                                                :section s :value v})))
                             (assoc acc s v)))
                         {} definition-sections)]
    (doseq [o (keys (:options sections))]
      (when-not (contains? known-options o)
        (throw (ex-info (format (str "%s: :options holds %s, which is not an option -- the "
                                     "options are %s")
                                tname (pr-str o) (pr-str known-options))
                        {:error ::unknown-option :record tname :option o
                         :options known-options}))))
    (doseq [s [:io-deps :certificates]]
      (when (seq (get sections s))
        (throw (ex-info (format (str "%s declares %s, which the library does not implement yet "
                                     "-- the section is accepted only empty, so that nothing is "
                                     "declared and then ignored")
                                tname s)
                        {:error ::unsupported-section :record tname :section s
                         :declared (vec (keys (get sections s)))}))))
    sections))

(defn- parse-check
  "One entry of the `:checks` map -> `{:name :doc :args :fn}`.

  Three forms, told apart by what the value starts with: a string is a docstring, a vector
  is an arglist, and anything else is the shorthand -- a bare function over the single field
  the check's key names."
  [tname cname form]
  (let [bad! (fn [why]
               (throw (ex-info (format "%s check %s %s" tname (pr-str cname) why)
                               {:error ::malformed-check :record tname :check cname :form form})))]
    (cond
      (and (seq? form) (string? (first form)))
      (let [[doc args & body] form]
        (when-not (vector? args) (bad! "has a docstring but no arglist"))
        (when-not (seq body) (bad! "has an arglist but no body"))
        {:name cname :doc doc :args args :fn `(fn ~args ~@body)})

      (and (seq? form) (vector? (first form)))
      (let [[args & body] form]
        (when-not (seq body) (bad! "has an arglist but no body"))
        {:name cname :doc nil :args args :fn `(fn ~args ~@body)})

      (and (seq? form) (seq? (first form)) (vector? (ffirst form)))
      (bad! "gives more than one arity -- the arglist is the dependency list, so there is exactly one")

      :else
      {:name cname :doc nil :args [(symbol (name cname))] :fn form})))

(defmacro defrecord
  "Define a validated map-like type with a real field per declared key, so reads and writes
  are field access rather than map lookups.

  Three positions: the type name, the fields, and a definition map of named sections --

      (rec/defrecord Heist
        [name crew take cuts]
        {:options      {:io-limited true}
         :checks       {::name non-blank-string?}
         :io-deps      {}
         :certificates {}})

  Every section is optional and an absent one is an empty one, but a key that names no
  section at all is a compile error: a mistyped `:cheks` would define a type with no checks,
  and a value asserting a validation that never ran is the one thing here that must not be
  constructible. See [[parse-definition]]. `:io-deps` and `:certificates` are accepted only
  empty until the library implements them.

  `:checks` maps a check name -- a namespace-qualified keyword, unique within the type, and
  what failures report -- to a check, written in one of three forms:

      ::name       non-blank-string?                      ; a bare fn over the field it names
      ::valid-name ([name] (non-blank-string? name))      ; arglist and body
      ::cuts-sum-to-take (\"The cuts add up to the take.\"  ; with a docstring
                          [take cuts] (= take (reduce + 0 (vals cuts))))

  The arglist is the dependency declaration. The body receives exactly what it names,
  positionally, in that order, and nothing else is in scope -- so nothing can depend on
  something it did not name, and the field -> checks index the macro derives from those
  arglists cannot disagree with the code. Exactly one arity: the arity is the dependency set.

  Every entry in an arglist is resolved here, while the macro runs: a symbol names a field
  of this type and a keyword names an io-dep of this type. One that names neither is a
  compile error naming the offender, so a dependency either means something or the form does
  not compile -- there is no way to write a check indexed under a key no write can produce.

  A check passes by returning `true` and nothing else; whatever else it returns is the
  failure data, reported under the check's name alongside its docstring and the fields it
  declared. Checks have no ordering among themselves and every one of them runs, so a
  check spanning two fields sees whatever those fields hold, including shapes the checks
  over them are rejecting in the same pass -- it restates what it needs rather than
  assuming an earlier check stopped anything. An exception is not caught: it is a bug in
  the check rather than a claim about the data, and \"we could not determine whether this
  is valid\" is not \"this is invalid\".

  A field becomes a key qualified by the namespace this form is written in -- a `Heist`
  defined in `my.app.heist` answers to `:my.app.heist/name`, which is `::name` here and
  `::h/name` wherever the namespace is aliased. A bare keyword is a key the type does not
  declare, so two types that happen to share a field name share nothing else."
  [tname fields definition]
  (let [{:keys [options checks]} (parse-definition tname definition)
        model-sym (symbol (str tname "-model"))
        fsyms     (mapv #(with-meta % nil) fields)
        ;; The defining namespace qualifies the keys, so a field named `name` is
        ;; `:this.ns/name` and two types sharing a field name share nothing else.
        key-ns    (str (ns-name *ns*))
        qkey      (fn [f] (keyword key-ns (str f)))
        ks        (mapv qkey fields)
        n         (count fsyms)
        key->sym  (zipmap ks fsyms)
        ;; every local is gensymed: a field could be named k, v, o, acc ...
        [ksym vsym dsym osym hsym essym ovsym accsym mtasym mta2sym msym thunksym tgtsym isym]
        (map gensym ["k" "v" "d" "o" "h" "es" "ov" "acc" "mta" "mta2" "m" "thunk" "target" "i"])
        tgthint   (with-meta tgtsym {:tag tname})
        dname     (symbol (str tname "Draft"))
        [extsym dirtysym ownsym tokensym] (map gensym ["ext" "dirty" "own" "token"])
        dvsym     (with-meta (gensym "d") {:tag dname})
        svsym     (with-meta (gensym "s") {:tag tname})
        dfield    (fn [f] (list '. dvsym (symbol (str "-" f))))
        sfield    (fn [f] (list '. svsym (symbol (str "-" f))))
        dctor     (fn [vals ext dirty own mta] `(new ~dname ~@vals ~ext ~dirty ~own ~mta))
        dfields   (mapv dfield fsyms)
        params    (conj fsyms mtasym)
        ;; (Heist. a b c mta) with position i replaced by `x`
        ctor-with (fn [i x] `(new ~tname ~@(assoc fsyms i x) ~mtasym))
        tagsym    (symbol key-ns (str tname))
        ;; ---- the checks ----
        ;; Ordered, so a rejected arglist can name the fields it could have named in the
        ;; order they were declared rather than in hash order. `array-map` built this way
        ;; stays one at any size; `into` an empty one would not.
        fieldmap  (apply array-map (interleave fsyms ks))
        ;; `:io-deps` arrives in E1. Until then a type declares none, so every keyword in an
        ;; arglist is unresolvable -- which is what it should be, and is reported as an
        ;; unknown io-dep rather than as a stray symbol.
        io-deps   #{}
        parsed    (mapv (fn [[cname form]]
                          (let [c (parse-check tname cname form)]
                            (assoc c :deps (resolve-deps {:record tname :what :check :name cname
                                                          :fields fieldmap :io-deps io-deps}
                                                         (:args c)))))
                        (seq checks))
        _         (reject-duplicate-names! tname (map :name parsed) io-deps)
        nchecks   (count parsed)
        cvar      (fn [i] (symbol (str tname "--check-" i)))
        deps-of   (fn [i] (:deps (nth parsed i)))
        ;; Derived from the arglists, so it cannot be misdeclared: there is nowhere to
        ;; declare it. Sorted vectors, because a sorted set does not survive being embedded
        ;; in emitted code -- the compiler rebuilds a set literal as a hash set.
        index     (reduce (fn [idx i] (reduce #(update %1 %2 (fnil conj []) i) idx (deps-of i)))
                          {} (range nchecks))
        independent (filterv #(empty? (deps-of %)) (range nchecks))
        ;; Run the checks at `idxs` over the expressions `argvs-of` gives for each -- locals
        ;; or direct field reads, so evaluating one twice costs nothing. Every check in the
        ;; group is called before any verdict is read, because a failure does not stop the
        ;; pass; then every one that did not return `true` is reported together. Nothing is
        ;; caught, so a check that throws leaves the `let` before there is anything to
        ;; report, which is the contract: a throw is a bug rather than a verdict. The value
        ;; maps are built only inside the branch that is about to throw.
        run-group (fn [idxs argvs-of]
                    (when (seq idxs)
                      (let [rs (mapv #(gensym (str "r" % "-")) idxs)]
                        `(let [~@(mapcat (fn [i r] [r `(~(cvar i) ~@(argvs-of i))]) idxs rs)]
                           (when-not (and ~@(map (fn [r] `(true? ~r)) rs))
                             (checks-failed!
                              ~model-sym
                              [~@(map (fn [i r]
                                        `(when-not (true? ~r)
                                           (failure-entry (nth (:checks ~model-sym) ~i) ~r
                                                          (array-map
                                                           ~@(interleave (deps-of i)
                                                                         (argvs-of i))))))
                                      idxs rs)]))))))]
    `(do
       (declare ~model-sym)
       ~@(map (fn [i] `(def ~(with-meta (cvar i) {:private true}) ~(:fn (nth parsed i))))
              (range nchecks))

       (p/def-map-type ~tname ~params
         (~'get [_# ~ksym ~dsym]
           (case ~ksym ~@(interleave ks fsyms) ~dsym))

         ;; The checks naming this field are known here, so they are called with the new
         ;; value and the untouched fields directly -- no lookup, and no allocation unless
         ;; they all pass.
         (~'assoc [_# ~ksym ~vsym]
           (case ~ksym
             ~@(mapcat (fn [j k]
                         [k `(do ~(run-group (get index k)
                                             (fn [i] (map (fn [d]
                                                            (if (= d k) vsym (key->sym d)))
                                                          (deps-of i))))
                                 ~(ctor-with j vsym))])
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

         ;; one allocation for the whole merge, each applicable check once at the end
         (~'cons [_# ~osym]
           (let [~essym (entries-of ~osym)
                 ~ovsym (reduce (fn [~accsym [~ksym ~vsym]]
                                  (declared-field! ~model-sym ~ksym)
                                  (assoc ~accsym ~ksym ~vsym))
                                {} ~essym)
                 ~hsym  (new ~tname
                             ~@(map (fn [k f] `(get ~ovsym ~k ~f)) ks fsyms)
                             ~mtasym)]
             (run-checks! ~model-sym (:run-sealed ~model-sym) ~hsym
                          (checks-for ~model-sym (map first ~essym)))
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
         {:name        '~tname
          :key-ns      ~key-ns
          :options     ~options
          :field-order ~ks
          :field-set   ~(set ks)
          :checks      ~(mapv (fn [i]
                                (let [{:keys [name doc deps]} (nth parsed i)]
                                  {:name name :doc doc :deps deps :pred (cvar i)}))
                              (range nchecks))
          :index       ~index
          :independent ~independent
          :tag         '~tagsym
          ;; One runner per type, so a check reached by index is still called with direct
          ;; field access rather than a lookup per dependency.
          :run-sealed  (fn [~isym ~svsym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i) ~@(map (comp sfield key->sym) (deps-of i)))])
                                     (range nchecks))
                           nil))
          :run-draft   (fn [~isym ~dvsym]
                         (case (int ~isym)
                           ~@(mapcat (fn [i]
                                       [i `(~(cvar i) ~@(map (comp dfield key->sym) (deps-of i)))])
                                     (range nchecks))
                           nil))
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
          :draft-seal       (fn [~dvsym] (new ~tname ~@dfields ~(dfield mtasym)))})
       ;; `deftype` interns a positional factory that checks nothing -- a public back door
       ;; straight through every guarantee the type exists to make. Replace it with one that
       ;; validates, and unmap the draft's, which nothing outside this file should be calling.
       (defn ~(symbol (str "->" tname))
         ~(str "Construct a validated " tname " from its fields, in declaration order.")
         [~@fsyms]
         ~(run-group (range nchecks) (fn [i] (map key->sym (deps-of i))))
         (new ~tname ~@fsyms nil))

       (defn ~(symbol (str "map->" tname))
         ~(str "Construct a validated " tname " from a map holding exactly its declared keys.\n"
               "  A key it does not declare is an error; a key it declares and the map omits is\n"
               "  reported as :missing-field, so `nil` and absent stay distinct.\n\n"
               "  Keys may be written qualified or bare: a bare keyword naming a declared field is\n"
               "  qualified into " key-ns ", which is what lets a printed literal -- whose tag\n"
               "  already carries the namespace -- read back through here and be validated.")
         [~msym]
         (seal ~model-sym (reduce-kv assoc ((:draft-empty ~model-sym) nil)
                                     (qualify-keys ~model-sym ~msym))))

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
         (.write w# (str "#" '~tagsym (printable x#))))
       ~tname)))
