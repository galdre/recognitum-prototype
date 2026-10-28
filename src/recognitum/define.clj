(ns recognitum.define
  "What `defproven`, `defchecks`, `defcopy` and `defconcern` expand to: each parses its form,
  refuses the mistakes it can see, and emits a call of the record layer or a `def`.

  Nothing here is part of the public API."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [recognitum.body :as body :refer [fail!]]
   [recognitum.record :as record]
   [recognitum.select :as select]))

;;; ------------------------------------------------------------------ names

(defn- resolve-var [s] (try (let [v (resolve s)] (when (var? v) v)) (catch Exception _ nil)))

(defn- kind-of [v] (when (var? v) (let [x @v] (when (map? x) (:recognitum/kind x)))))

(defn- checks-marker?
  "Whether `s` resolves to the var whose symbol is `marker`, the one that starts a section of
  own checks."
  [marker s]
  (and (symbol? s) (= marker (some-> (resolve-var s) symbol))))

(defn- id-of [nm] (keyword (str *ns*) (str nm)))

(defn- concern-var
  "The var a concern keyword names, where `rec/defconcern` defined it."
  [k]
  (when-let [n (find-ns (symbol (namespace k)))]
    (when-let [v (ns-resolve n (symbol (name k)))]
      (when (:recognitum/concern (meta v)) v))))

(defn- words [xs] (str/join " " xs))

;;; ------------------------------------------------------------------ clauses

(def ^:private plain-meta #{:line :column :end-line :end-column :file :source :override})

(defn- parse-clause
  "`(name [waits] \"doc\"? body...)`, with its concerns and `^:override` flag."
  [ctx clause]
  (let [ctx (assoc ctx :line (:line (meta clause)))]
    (when-not (and (seq? clause) (symbol? (first clause)) (vector? (second clause)))
      (fail! ctx (str "a clause is (name [waits] \"doc\"? body): " (pr-str clause))))
    (let [[nm waits & more] clause
          [doc body] (if (and (string? (first more)) (next more)) [(first more) (rest more)] [nil more])
          concerns (reduce-kv (fn [acc k _]
                            (cond (plain-meta k) acc
                                  (qualified-keyword? k)
                                  (if (concern-var k)
                                    (conj acc k)
                                    (fail! ctx (str "^" k " on " nm " names no concern that rec/defconcern defines")))
                                  :else (fail! ctx (str "unknown flag ^" k " on " nm))))
                          #{} (or (meta clause) {}))]
      (when (empty? body) (fail! ctx (str nm " has no body")))
      {:name nm :waits waits :doc doc :concerns concerns :line (:line (meta clause))
       :override? (boolean (:override (meta clause)))
       :body (if (next body) (cons 'do body) (first body))})))

(defn- topo-check!
  "Refuses waits that form a cycle. `read-wait?`, given a check and a check it waits on, says
  whether the wait comes from a read rather than from the check's wait vector."
  ([ctx ids waits-of] (topo-check! ctx ids waits-of (constantly false)))
  ([ctx ids waits-of read-wait?]
   (let [state (atom {})]
     (letfn [(visit [id path]
               (case (@state id)
                 :done nil
                 :active (let [ring (conj (subvec path (.indexOf ^java.util.List path id)) id)]
                           (fail! ctx (str "the waits form a cycle: " (str/join " -> " ring)
                                           (when (some (fn [[a b]] (read-wait? a b)) (partition 2 1 ring))
                                             " (a check waits on the shape checks of the fields it reads)"))))
                 (do (swap! state assoc id :active)
                     (doseq [w (waits-of id)] (visit w (conj path id)))
                     (swap! state assoc id :done))))]
       (doseq [id ids] (visit id []))))))

(defn- refuse-duplicates! [ctx what names]
  (when-let [d (first (for [[n c] (frequencies names) :when (> c 1)] n))]
    (fail! ctx (str "two " what " are named " d))))

;;; ------------------------------------------------------------------ bundles

(defn check-of
  "The value of the var of check `cname` of `bundle`."
  [bundle cname]
  (assoc (select-keys (get-in bundle [:checks cname]) [:id :name :doc :concerns :waits])
         :recognitum/kind :check :bundle (:name bundle)))

(defn- input-key [s] (keyword (name s)))

(defn- emit-check-vars [bname cs]
  (for [c cs]
    `(def ~(cond-> (:name c) (:doc c) (vary-meta assoc :doc (:doc c)))
       (check-of ~bname '~(:name c)))))

(defn defchecks
  "The expansion of `(rec/defchecks bname [inputs] rec/checks clauses...)`. `opts` holds the
  engine's `:compile-body`, the `:read-macros` [[recognitum.body/analyze]] takes, and
  `:checks-marker`, the symbol of the var that starts a section of own checks."
  [env bname inputs marker clauses {:keys [compile-body read-macros checks-marker]}]
  (let [ctx {:where (str *ns* "/" bname) :read-macros read-macros}
        _ (when-not (and (simple-symbol? bname) (vector? inputs) (every? simple-symbol? inputs))
            (fail! ctx "defchecks takes a name, a vector of plain input symbols, rec/checks and clauses"))
        _ (when-not (checks-marker? checks-marker marker)
            (fail! ctx (str "expected rec/checks after the inputs, got " (pr-str marker))))
        _ (refuse-duplicates! ctx "inputs" inputs)
        parsed (mapv #(parse-clause ctx %) clauses)
        names (set (map :name parsed))
        _ (refuse-duplicates! ctx "checks" (map :name parsed))
        fields (into {} (map (juxt identity input-key)) inputs)
        cs (vec (for [{:keys [name waits body] :as c} parsed
                      :let [cctx (assoc ctx :line (:line c) :fields fields :required #{}
                                        :owner bname :field-word "input" :call-env env :fetch? true)
                            _ (when-not (simple-symbol? name)
                                (fail! cctx (str "a bundle's check is named by a plain symbol: " name)))
                            wait-ids (set (for [w waits]
                                            (if (and (simple-symbol? w) (names w))
                                              (id-of w)
                                              (fail! cctx (str "wait " w " of " name " is not a check of " bname
                                                               ". Its checks: " (words (sort names)))))))
                            {:keys [form reads sources fetches?]} (body/analyze cctx body)]]
                  (assoc c :id (id-of name) :waits wait-ids :input-reads reads
                         :sources sources :fetches? fetches? :run (compile-body form))))
        _ (topo-check! ctx (map :id cs) (into {} (map (juxt :id :waits)) cs))]
    `(do
       (def ~bname
         {:recognitum/kind :bundle
          :name '~(symbol (str *ns*) (str bname))
          :inputs '~inputs
          :order '~(mapv :name cs)
          :checks ~(into {} (for [c cs]
                              [(list 'quote (:name c))
                               {:id (:id c) :name (list 'quote (:name c)) :doc (:doc c) :concerns (:concerns c)
                                :waits (:waits c) :input-reads (list 'quote (:input-reads c))
                                :sources (:sources c) :fetches? (:fetches? c) :run (:run c)}]))})
       ~@(emit-check-vars bname cs)
       (var ~bname))))

(defn copy-bundle
  "`bundle` with its checks renamed by `renames` and their ids in the namespace of `qname`."
  [bundle renames qname]
  (let [rid #(keyword (namespace qname) (str (renames %)))
        id->name (into {} (for [[n c] (:checks bundle)] [(:id c) n]))]
    (assoc bundle
           :name qname
           :order (mapv renames (:order bundle))
           :checks (into {} (for [[n c] (:checks bundle)]
                              [(renames n) (assoc c :name (renames n) :id (rid n)
                                                  :waits (set (map (comp rid id->name) (:waits c))))])))))

(defn defcopy
  "The expansion of `(rec/defcopy new-name bundle {check new-check ...})`."
  [new-name orig renames]
  (let [ctx {:where (str *ns* "/" new-name)}
        ov (resolve-var orig)
        _ (when-not (= :bundle (kind-of ov))
            (fail! ctx (str orig (if ov " is not a bundle" " names no bundle var"))))
        b @ov
        _ (when-not (and (map? renames) (every? simple-symbol? (keys renames)) (every? simple-symbol? (vals renames)))
            (fail! ctx (str "defcopy takes a map from each check of " orig " to its new name: {check new-name ...}")))
        missing (remove (set (keys renames)) (:order b))
        extra (remove (set (:order b)) (keys renames))
        _ (when (or (seq missing) (seq extra))
            (fail! ctx (str "the map must name every check of " orig " exactly once."
                            (when (seq missing) (str " Missing: " (words missing) "."))
                            (when (seq extra) (str " Not checks of " orig ": " (words extra) "."))
                            " Its checks: " (words (:order b)))))
        _ (when-let [d (first (for [[n c] (frequencies (vals renames)) :when (> c 1)] n))]
            (fail! ctx (str "two checks get the same new name: " d)))
        qname (symbol (str *ns*) (str new-name))
        _ (doseq [n (vals renames)]
            (when (= n new-name) (fail! ctx (str "new name " n " is the copy's own name")))
            (when-let [v (resolve-var n)]
              (when (and (= (str *ns*) (namespace (symbol v))) (#{:check :bundle} (kind-of v))
                         (not= (:bundle @v) qname))
                (fail! ctx (str "new name " n " would redefine " (symbol v))))))
        cs (vals (:checks (copy-bundle b renames qname)))]
    `(do
       (def ~new-name (copy-bundle (deref (var ~(:name b))) '~renames '~qname))
       ~@(emit-check-vars new-name cs)
       (var ~new-name))))

(defn defconcern
  "The expansion of `(rec/defconcern name \"doc\")`."
  [tname doc]
  (when-not (and (simple-symbol? tname) (string? doc))
    (fail! {:where (str *ns* "/" tname)} "defconcern takes a name and a docstring"))
  `(def ~(vary-meta tname assoc :doc doc :recognitum/concern true) (select/concern ~(id-of tname) ~doc)))

;;; ------------------------------------------------------------------ types

(defn- protocol-var
  "The var `s` resolves to, where it holds a protocol."
  [s]
  (when-let [v (resolve-var s)] (let [x @v] (when (and (map? x) (:on-interface x)) v))))

(defn- class-named [s] (try (let [c (resolve s)] (when (class? c) c)) (catch Exception _ nil)))

(defn- interface-named
  "The class `s` names, where it may head a section of methods: an interface, or `Object`."
  [s]
  (when-let [^Class c (class-named s)] (when (or (= Object c) (.isInterface c)) c)))

(defn- head-refusal
  "Why `s` heads no section: it names a class that is not an interface, a var that is neither a
  bundle nor a protocol, or nothing."
  [s]
  (cond (class-named s) (str s " is a class, not a protocol or an interface: of the classes, a defproven type"
                             " implements interfaces and Object's toString")
        (resolve-var s) (str s " is not a bundle, a protocol or an interface")
        :else (str s " names no bundle var, no protocol and no interface")))

(defn- parse-sections
  "The sections after the fields: the checks sections (`rec/checks` and the bundle bindings, each
  `{:marker :clauses}`), then the sections of methods, each `{:marker :methods}` with the
  `:protocol` var or the interface `:class` that heads it. A checks section after a section of
  methods is refused."
  [ctx checks-marker forms]
  (loop [items forms, cur nil, out []]
    (if-let [[x & more] (seq items)]
      (if (seq? x)
        (if cur
          (recur more (update cur (if (:clauses cur) :clauses :methods) conj x) out)
          (fail! ctx (str "expected a bundle, rec/checks, a protocol or an interface before " (pr-str x))))
        (let [done (cond-> out cur (conj cur))
              head (some #(when (:methods %) %) done)]
          (cond
            (not (symbol? x))
            (fail! ctx (str "expected a bundle, rec/checks, a protocol, an interface or a clause, got " (pr-str x)))

            (protocol-var x)
            (recur more {:protocol (protocol-var x) :marker x :methods []} done)

            (interface-named x)
            (recur more {:class (interface-named x) :marker x :methods []} done)

            head
            (fail! ctx (if (or (checks-marker? checks-marker x) (= :bundle (kind-of (resolve-var x))))
                         (str x " follows the " (if (:protocol head) "protocol " "interface ") (:marker head)
                              ": the checks sections, rec/checks and the bundle bindings, come before the protocols"
                              (when-not (:protocol head) " and interfaces"))
                         (head-refusal x)))

            (class-named x)
            (fail! ctx (head-refusal x))

            :else
            (recur more {:marker x :clauses []} done))))
      (cond-> out cur (conj cur)))))

(defn- parse-method
  "One method of the section headed by `marker`: `(name [this params...] body...)`, refused where
  `defrecord` refuses it: no first parameter, or parameters other than plain symbols."
  [ctx marker m]
  (let [ctx (assoc ctx :line (:line (meta m)))
        [nm params & body] m]
    (when-not (and (seq? m) (simple-symbol? nm) (vector? params))
      (fail! ctx (str "a method of " marker " is (name [this params...] body...): " (pr-str m))))
    (when (empty? params)
      (fail! ctx (str "method " nm " of " marker " needs a first parameter, for the value")))
    (when-not (and (every? simple-symbol? params) (not-any? #{'&} params))
      (fail! ctx (str "the parameters of method " nm " of " marker " are plain symbols: " (pr-str params))))
    {:name nm :params params :body body :line (:line (meta m)) :marker marker}))

(defn- parameters [n] (if (= 1 n) " parameter" " parameters"))

(defn- parse-protocol
  "A protocol section's methods, each with the signature of its interface's method, refused
  where `defrecord` refuses them: a method the protocol does not declare, and an arity it does
  not declare."
  [ctx {:keys [protocol marker methods]}]
  (let [sigs (:sigs @protocol)]
    (for [m methods
          :let [{:keys [name params line] :as impl} (parse-method ctx marker m)
                ctx (assoc ctx :line line)
                sig (get sigs (keyword (clojure.core/name name)))
                arities (set (map count (:arglists sig)))]]
      (do (when-not sig
            (fail! ctx (str marker " has no method " name ". Its methods: "
                            (words (sort (map clojure.core/name (keys sigs)))))))
          (when-not (arities (count params))
            (fail! ctx (str "method " name " of " marker " takes " (str/join " or " (sort arities))
                            (parameters (apply max arities)) ", the value first, not " (count params))))
          (assoc impl :protocol? true
                      :sig [(munge (clojure.core/name name)) (vec (repeat (dec (count params)) Object))])))))

(def ^:private primitives
  {'long Long/TYPE 'int Integer/TYPE 'double Double/TYPE 'float Float/TYPE 'boolean Boolean/TYPE
   'char Character/TYPE 'byte Byte/TYPE 'short Short/TYPE})

(defn- hint-class
  "The class that the type hint on parameter `p` names, or nil where it has none."
  [ctx nm p]
  (when-let [tag (:tag (meta p))]
    (or (primitives tag)
        (when (symbol? tag) (class-named tag))
        (when (string? tag) (try (Class/forName tag) (catch ClassNotFoundException _ nil)))
        (fail! ctx (str "the hint ^" tag " on parameter " p " of method " nm " names no class")))))

(defn- show-method [^java.lang.reflect.Method m]
  (str (.getName m) "(" (str/join ", " (map #(.getName ^Class %) (.getParameterTypes m))) ")"))

(defn- parse-interface
  "An interface section's methods, each with the parameter types of the one method of the
  interface it implements. Of `Object`, only `toString`. Refuses a method the interface does not
  declare, an arity it does not declare, a method that more than one of the interface's methods
  match after the hints on its parameters, and `equals` and `hashCode`, since a value's equality
  and hash are its map's."
  [ctx {:keys [^Class class marker methods]}]
  (for [m methods
        :let [{:keys [name params line] :as impl} (parse-method ctx marker m)
              ctx (assoc ctx :line line)
              n (munge (clojure.core/name name))
              args (rest params)
              _ (when (#{"equals" "hashCode"} n)
                  (fail! ctx (str "method " n " of " marker ": a value's equality and hash are its map's,"
                                  " so a type gives neither equals nor hashCode")))
              declared (if (= Object class)
                         (if (= "toString" n)
                           [(.getMethod Object "toString" (make-array Class 0))]
                           (fail! ctx (str "method " n " of Object: of Object's methods, a type gives only toString")))
                         (filter #(and (= n (.getName ^java.lang.reflect.Method %))
                                       (not (java.lang.reflect.Modifier/isStatic (.getModifiers ^java.lang.reflect.Method %))))
                                 (.getMethods class)))
              _ (when (empty? declared)
                  (fail! ctx (str marker " declares no method " n ". Its methods: "
                                  (words (sort (distinct (map #(.getName ^java.lang.reflect.Method %)
                                                              (.getMethods class))))))))
              of-arity (filter #(= (count args) (.getParameterCount ^java.lang.reflect.Method %)) declared)
              _ (when (empty? of-arity)
                  (let [arities (sort (distinct (map #(inc (.getParameterCount ^java.lang.reflect.Method %)) declared)))]
                    (fail! ctx (str "method " n " of " marker " takes " (str/join " or " arities)
                                    (parameters (last arities)) ", the value first, not " (count params)))))
              hints (mapv #(hint-class ctx n %) args)
              found (filter (fn [^java.lang.reflect.Method jm]
                              (every? true? (map (fn [h c] (or (nil? h) (= h c))) hints (.getParameterTypes jm))))
                            of-arity)]]
    (case (count found)
      0 (fail! ctx (str "no method " n " of " marker " takes the hinted types: "
                        (words (map show-method of-arity))))
      1 (assoc impl :sig [n (vec (.getParameterTypes ^java.lang.reflect.Method (first found)))])
      (fail! ctx (str "method " n " of " marker ": " (count found) " methods take " (count args)
                      (parameters (count args)) " after the value; hint the parameters' types to choose one: "
                      (words (map show-method found)))))))

(defn- refuse-definition!
  "Throws the proof strategy's refusal `r` of the definition of `tname` with `methods`, located
  at the method it names. A method a value has as a map is refused with its remedies: a
  protocol's is renamed, or extended to the type after the definition; an interface's is left
  out, since the map's method answers it."
  [ctx tname methods {:keys [kind message method]}]
  (let [[mname arity] method
        impl (when method
               (some #(when (= [mname arity] [(symbol (first (:sig %))) (count (second (:sig %)))]) %) methods))
        ctx (cond-> ctx impl (assoc :line (:line impl)))]
    (fail! ctx (if (and (= :map-method kind) impl)
                 (let [{:keys [name marker protocol?]} impl]
                   (str "method " name " of " marker ": as a map, a value of " tname " already has a method " name
                        " of " arity (parameters arity) ", so the type cannot give it: "
                        (if protocol?
                          (str "rename it, or extend " marker " to " tname " with extend-protocol after the"
                               " definition (a method extended from outside is not guarded: it answers on"
                               " values of a displaced definition)")
                          "leave it out, and the map's method answers")))
                 message))))

(defn- hints-own-class
  "A form in `forms` that a type hint names the type `tname` on, or nil."
  [tname classname forms]
  (first (for [f (tree-seq coll? seq forms)
               :let [tag (:tag (meta f))]
               :when (and tag (#{tname (symbol classname)} (if (string? tag) (symbol tag) tag)))]
           f)))

(defn- method-fn
  "The form of the fn that implements one method: called with the value first, the fields bound
  to its entries (nil where absent), the value to the first parameter, and the type's name to
  the value's class. `recur` goes to the method's head with the parameters after the first, as
  in `defrecord`."
  [tname fields key-of {:keys [params body]}]
  (let [[self & args] params
        v (gensym "value")
        gs (mapv #(gensym (str (name %) "_")) args)
        args (map #(cond-> % (primitives (:tag (meta %))) (vary-meta dissoc :tag)) args)]
    `(fn [~v ~@gs]
       (let [~tname (class ~v)
             ~@(mapcat (fn [f] [f `(get ~v ~(key-of f))]) fields)
             ~self ~v]
         (loop [~@(interleave args gs)] ~@body)))))

(defn- binding-gate
  "The field that `^{:when-extant field}` on a bundle var names, refused where it is not an
  optional field."
  [ctx marker fsyms optional]
  (let [mm (dissoc (meta marker) :line :column :end-line :end-column :file)]
    (when-let [bad (seq (remove #{:when-extant} (keys mm)))]
      (fail! ctx (str "unknown flag ^" (first bad) " on the binding of " marker ". Flags: ^{:when-extant field}")))
    (when (contains? mm :when-extant)
      (let [f (:when-extant mm)
            head (str "^{:when-extant " (pr-str f) "} on " marker ": ")]
        (cond
          (not (simple-symbol? f))
          (fail! ctx (str head "name a field, as a plain symbol"))
          (not (fsyms f))
          (fail! ctx (str head f " is not a field of " (:owner ctx) ". Optional fields: " (words (sort optional))))
          (not (optional f))
          (fail! ctx (str head f " is a required field, so it is never absent and the binding never skips."
                          " Optional fields: " (words (sort optional))))
          :else f)))))

(defn- parse-binding
  "A bundle bound in a type: the bundle, its input clauses compiled, and its gate."
  [ctx fctx {:keys [marker clauses]} fsyms optional key-of]
  (let [v (resolve-var marker)
        _ (when-not (= :bundle (kind-of v))
            (fail! ctx (head-refusal marker)))
        b @v
        ins (set (:inputs b))
        parsed (for [c clauses]
                 (let [{:keys [name waits body line]} (parse-clause ctx c)
                       cctx (assoc fctx :line line :fetch? false
                                   :no-fetch (str "input clause " name))]
                   (when-not (ins name)
                     (fail! cctx (str "input clause " name ": bundle " marker " has no input " name
                                      ". Its inputs: " (words (:inputs b)))))
                   (when (seq waits) (fail! cctx (str "input clause " name " takes no waits")))
                   (let [{:keys [form reads]} (body/analyze cctx body)]
                     [name {:reads reads :form form}])))
        given (into {} parsed)]
    (refuse-duplicates! ctx (str "input clauses of " marker) (map first parsed))
    (when-let [missing (seq (remove given (:inputs b)))]
      (fail! ctx (str "no clause binds input " (str/join ", " missing) " of bundle " marker)))
    {:var v :sym (:name b) :bundle b :inputs given
     :gate (some-> (binding-gate fctx marker fsyms optional) key-of)}))

(defn- check-ref
  "What a qualified symbol in a wait or an override names: a check or a bundle bound here."
  [cctx bound tname s what]
  (let [v (resolve-var s)]
    (case (kind-of v)
      :check (let [c @v
                   bv (resolve-var (:bundle c))]
               (when-not (and bv (contains? (:checks @bv) (:name c)))
                 (fail! cctx (str what " " s ": " (:bundle c) " no longer defines it")))
               (when-not (bound (:bundle c))
                 (fail! cctx (str what " " s ": it is not a check of any bundle bound here."
                                  " Its bundle " (:bundle c) " is not bound in " tname)))
               c)
      :bundle (if (bound (:name @v))
                @v
                (fail! cctx (str what " " s ": bundle " (:name @v) " is not bound in " tname)))
      (fail! cctx (str what " " s (if v
                                    ": it is not a check of any bundle bound here, nor a bundle"
                                    ": no such var"))))))

(defn- own-check
  "One clause of the type's own section, compiled."
  [{:keys [fctx fsyms own-names bound gate-of tname key-of compile-body]}
   {:keys [name waits body override? concerns line doc]}]
  (let [cctx (assoc fctx :line line :fetch? true)
        target (cond
                 (and override? (qualified-symbol? name))
                 (let [r (check-ref cctx bound tname name "^:override")]
                   (when-not (= :check (:recognitum/kind r))
                     (fail! cctx (str "^:override " name " names a bundle, not one of its checks")))
                   r)
                 override? (fail! cctx (str "^:override needs a qualified bundle check, got " name))
                 (qualified-symbol? name)
                 (fail! cctx (str name " is qualified: mark it ^:override to replace that bundle check"))
                 :else nil)
        wait-ids (set (mapcat (fn [w]
                                (cond
                                  (simple-symbol? w)
                                  (if (own-names w)
                                    [(key-of w)]
                                    (let [v (resolve-var w)
                                          kind (kind-of v)]
                                      (fail! cctx (str "wait " w " of " name " is not a check of " tname
                                                       ". Its own checks: " (words (sort own-names))
                                                       (when (#{:bundle :check} kind)
                                                         (str ". A wait on a bundle"
                                                              (when (= :check kind) "'s check")
                                                              " is qualified: " (symbol v)))))))
                                  (qualified-symbol? w)
                                  (let [r (check-ref cctx bound tname w "wait")]
                                    (if (= :bundle (:recognitum/kind r))
                                      (map :id (vals (:checks r)))
                                      [(:id r)]))
                                  :else (fail! cctx (str "a wait is a symbol: " (pr-str w)))))
                              waits))
        {:keys [form reads sources fetches?]} (body/analyze cctx body)
        gate (when target (gate-of (:bundle target)))]
    {:id (if target (:id target) (key-of name)) :doc doc
     :concerns (into concerns (:concerns target)) :waits wait-ids :override-of (:bundle target) :gate gate
     :shape-of (when (and (simple-symbol? name) (fsyms name)) (key-of name))
     :reads (cond-> (set (map key-of reads)) gate (conj gate))
     :sources sources :fetches? fetches?
     :run (compile-body form)}))

(defn- emit-factories
  [tname fields kind-doc t key-of]
  (let [m (gensym "m")
        build (into {} (for [f fields] [(key-of f) f]))
        [fixed over] (split-at 20 fields)
        over-sym (gensym "overage")]
    [`(def ~(with-meta (symbol (str "map->" tname))
              {:doc (str "A " tname " holding the entries of map `m`, keyed by field keys. " kind-doc)
               :arglists `'([~'m])})
        (fn [~m] (recognitum.record/make ~t ~m)))
     `(def ~(with-meta (symbol (str "->" tname))
              {:doc (str "A " tname " with every field present, in order; nil is present. " kind-doc)
               :arglists `'([~@(map #(with-meta % nil) fields)])})
        (fn
          ~@(if (seq over)
              `([~@(map #(with-meta % nil) fixed) ~'& ~over-sym]
                (when-not (= (count ~over-sym) ~(count over))
                  (throw (clojure.lang.ArityException. (+ 20 (count ~over-sym)) ~(str "->" tname))))
                (let [[~@(map #(with-meta % nil) over)] ~over-sym]
                  (recognitum.record/make ~t ~build)))
              `([~@(map #(with-meta % nil) fields)] (recognitum.record/make ~t ~build)))))]))


(defn defproven
  "The expansion of `(rec/defproven T [fields] sections... method-sections...)`. `opts` holds
  what [[defchecks]] takes; `:wiring`, the form of the map `recognitum.record/define-type`
  takes as its second argument; and `:strategy`, the proof strategy that form names, which
  answers the class's name and refuses here, at expansion, every definition it would refuse
  when the type is defined.

  The expansion is one form, at the top level or inside another: the methods compile with the
  checks, before the constructors' vars are interned, so a method that does not compile leaves
  nothing. At run time [[defined]] defines the type and imports its class."
  [env tname fields forms {:keys [compile-body read-macros checks-marker wiring strategy]}]
  (let [ctx {:where (str *ns* "/" tname) :call-env env :owner tname :field-word "field"
             :read-macros read-macros}
        _ (when-not (simple-symbol? tname) (fail! ctx "defproven takes a plain symbol as the type's name"))
        _ (when-not (and (vector? fields) (every? simple-symbol? fields) (seq fields))
            (fail! ctx "fields are a vector of plain symbols"))
        _ (refuse-duplicates! ctx "fields" fields)
        fsyms (set fields)
        key-of id-of
        optional (set (filter #(:optional (meta %)) fields))
        field-map (into {} (for [f fields] [f (key-of f)]))
        fctx (assoc ctx :fields field-map :required (set/difference fsyms optional))
        all-sections (parse-sections ctx checks-marker forms)
        sections (filter :clauses all-sections)
        method-secs (remove :clauses all-sections)
        _ (when-let [[h s] (first (for [[h n] (frequencies (map #(or (:protocol %) (:class %)) method-secs))
                                        :when (> n 1)]
                                    [h (some #(when (= h (or (:protocol %) (:class %))) %) method-secs)]))]
            (fail! ctx (if (:protocol s)
                         (str "protocol " (:marker s) " is implemented twice")
                         (str "interface " (:marker s) " is listed twice"))))
        tag-sym (symbol (str *ns*) (str tname))
        classname (record/class-name strategy tag-sym)
        methods (vec (mapcat #(if (:protocol %) (parse-protocol ctx %) (parse-interface ctx %)) method-secs))
        _ (doseq [[sig c] (frequencies (map :sig methods)) :when (> c 1)
                  :let [{:keys [name marker params line]} (some #(when (= sig (:sig %)) %) methods)
                        n (count params)]]
            (fail! (assoc ctx :line line)
                   (str "method " name " of " marker " is written twice with " n (parameters n))))
        _ (doseq [{:keys [name marker params body line]} methods]
            (when-let [f (hints-own-class tname classname (cons params body))]
              (fail! (assoc ctx :line line)
                     (str "method " name " of " marker " hints " tname " on " (pr-str f) ": " tname
                          " has no class while the methods compile. In a method body " tname
                          " is the value's class, (class this), and takes no hint"))))
        interfaces (vec (distinct (for [s method-secs]
                                    (if (:protocol s) (:on-interface @(:protocol s)) (:class s)))))
        _ (some->> (record/refusal strategy interfaces (map :sig methods)) (refuse-definition! ctx tname methods))
        own-secs (filter #(checks-marker? checks-marker (:marker %)) sections)
        _ (when (> (count own-secs) 1) (fail! ctx "rec/checks appears twice"))
        bindings (vec (for [s sections :when (not (checks-marker? checks-marker (:marker s)))]
                        (parse-binding ctx fctx s fsyms optional key-of)))
        _ (refuse-duplicates! ctx "bindings of a bundle" (map :sym bindings))
        bound (set (map :sym bindings))
        own (mapv #(parse-clause ctx %) (mapcat :clauses own-secs))
        _ (refuse-duplicates! ctx "clauses" (map :name own))
        own-checks (mapv #(own-check {:fctx fctx :fsyms fsyms :tname tname :key-of key-of
                                      :own-names (set (filter simple-symbol? (map :name own)))
                                      :bound bound :gate-of (into {} (map (juxt :sym :gate)) bindings)
                                      :compile-body compile-body}
                                     %)
                         own)
        overridden (set (keep #(when (:override-of %) (:id %)) own-checks))
        input-sym (into {} (map-indexed (fn [i b] [(:sym b) (gensym (str "inputs" i))])) bindings)
        bundle-checks
        (vec (for [{:keys [bundle sym inputs gate]} bindings
                   n (:order bundle)
                   :let [c (get-in bundle [:checks n])]
                   :when (not (overridden (:id c)))]
               {:id (:id c) :doc (:doc c) :concerns (:concerns c) :waits (:waits c) :gate gate
                :reads (cond-> (set (map key-of (mapcat #(get-in inputs [% :reads]) (:input-reads c))))
                         gate (conj gate))
                :sources `(get-in @(var ~sym) [:checks '~n :sources])
                :fetches? (:fetches? c)
                :run `(recognitum.record/bound-check (get-in @(var ~sym) [:checks '~n :run])
                                                   (select-keys ~(input-sym sym) ~(mapv input-key (sort (:input-reads c)))))}))
        all (into own-checks bundle-checks)
        _ (refuse-duplicates! ctx "checks" (map :id all))
        known (set (map :id all))
        _ (doseq [c all, w (:waits c)]
            (when-not (known w)
              (fail! ctx (str (:id c) " waits on " w ", which an override or a binding left out"))))
        _ (let [explicit (into {} (map (juxt :id (comp set :waits))) all)]
            (topo-check! ctx (map :id all) (record/waits-of all)
                         (fn [a b] (not (contains? (explicit a) b)))))
        t (gensym "type")
        define-it
        `(recognitum.record/define-type
          {:name '~tag-sym
           :fields ~(mapv (fn [f] {:key (key-of f) :sym (list 'quote f) :optional? (contains? optional f)}) fields)
           :checks ~(mapv #(select-keys % [:id :doc :concerns :waits :gate :reads :sources :fetches? :shape-of :run]) all)
           :interfaces ~interfaces
           :methods ~(into {} (for [m methods] [(:sig m) (method-fn tname fields key-of m)]))}
          ~wiring)
        inputs (mapcat (fn [{:keys [sym inputs]}]
                         [(input-sym sym)
                          (into {} (for [[in {:keys [form]}] inputs]
                                     [(keyword (name in)) (body/checked-fn form)]))])
                       bindings)
        factories (emit-factories tname fields "Runs every check, or throws." t key-of)
        new-vars (set (remove #(.getMapping *ns* %) (map second factories)))]
    `(let [~@inputs
           ~t (recognitum.define/defined ~(:where ctx) '~(ns-name *ns*) '~tname ~classname '~new-vars
                                         (fn [] ~define-it))]
       ~@factories
       (recognitum.record/value-class ~t))))

(defn defined
  "Calls `define`, which defines the type named `tname` in the namespace named `nsym`, and imports
  its class, named `classname`, there under `tname`. Before `define` runs, refuses a name that
  the namespace maps to a var, or to a class of another name, since the import would fail
  there. Where either refuses or `define` throws, the definition leaves nothing: each of
  `new-vars`, the constructors' vars that the definition's form interned, is unmapped, and an
  earlier definition of the type's name stays current. Returns the type."
  [where nsym tname classname new-vars define]
  (let [n (the-ns nsym)
        previous (.getMapping n tname)
        displaced (record/named-now (symbol (str nsym) (str tname)))
        refuse (fn [^Throwable e why]
                 (doseq [s new-vars
                         :let [v (.getMapping n s)]
                         :when (and (var? v) (identical? n (.ns ^clojure.lang.Var v))
                                    (not (.hasRoot ^clojure.lang.Var v)))]
                   (ns-unmap n s))
                 (let [root (when e (last (take-while some? (iterate ex-cause e))))
                       msg (str (or why (ex-message root))
                                (if displaced
                                  (str ". " tname " keeps its earlier definition")
                                  (str ". Nothing is defined under " tname)))
                       refusal (ex-info (str where ": " msg) {:error :recognitum/refused :message msg})]
                   ;; The refusal ends the chain of causes, since the REPL prints the last
                   ;; cause's message; `e` stays with it, suppressed, for its stack trace.
                   (when e (.addSuppressed refusal e))
                   (throw refusal)))
        _ (when (and previous (not (and (class? previous)
                                        (= (.getName ^Class previous) classname))))
            (refuse nil (str tname " already names " (pr-str previous) " in " nsym
                             ", so the type's class cannot take the name")))
        type (try (define) (catch Throwable e (refuse e nil)))]
    (.importClass n tname (record/value-class type))
    type))
