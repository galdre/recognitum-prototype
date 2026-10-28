(ns hooks.recognitum
  "clj-kondo hooks for recognitum's defining macros.
  Each defining hook rewrites its form into plain Clojure that clj-kondo
  analyzes: names become vars or locals, fields become fn parameters, waits and
  concerns become symbol usages. Each check body is wrapped in a call of
  recognitum.core/checks, which the hook on rec/fetch, rec/fetch-each,
  rec/extant? and rec/when-extant finds on the call stack. Bundle shapes and concerns travel between namespaces
  as private marker vars, visible to other hooks through api/ns-analysis (the
  cache) or, within one run, through `registry`.

  A hook sees one form at a time. What a bundle's checks wait on is in its
  defchecks form, which records it in `registry` for this run; a bundle defined
  in another namespace is known by its names alone (the cache), so a cycle
  through what its checks wait on is left to the macros. The cycles reported are
  those of the waits the forms list and the gates the bindings name; check
  bodies are not read for cycles."
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as str]))

;; ---------------------------------------------------------------- helpers

(defn- sym [node]
  (when (api/token-node? node)
    (let [v (api/sexpr node)] (when (symbol? v) v))))

(defn- resolved [node]
  (when-let [s (sym node)] (api/resolve {:name s})))

(defn- rec? [node names]
  (let [r (resolved node)]
    (and (= 'recognitum.core (:ns r)) (contains? names (:name r)))))

(def ^:dynamic *pin*
  "Location for generated nodes: zero width at the form's opening paren, marked
  derived. Without it clj-kondo gives them the span of a neighbouring form, and
  clojure-lsp then finds them under the cursor anywhere in that span."
  nil)

(defn- pin-of [node]
  (let [{:keys [row col]} (meta node)]
    {:row row :col col :end-row row :end-col col
     :derived-location true :derived-name-location true}))

(defn- pinned [n] (if *pin* (with-meta n *pin*) n))
(defn- tok [s] (pinned (api/token-node s)))
(defn- lst [& xs] (pinned (api/list-node (vec xs))))
(defn- vct [xs] (pinned (api/vector-node (vec xs))))
(defn- fresh "A pinned copy of a symbol node." [node] (tok (api/sexpr node)))
(defn- at "A symbol token at node's location." [s node] (with-meta (api/token-node s) (meta node)))
(defn- bare "node without its reader metadata." [node] (assoc node :meta nil))

(defn- finding! [node type msg]
  (api/reg-finding! (assoc (meta node) :type type :message msg)))

(defn- in-body
  "A check body's forms, wrapped in the marker call that body-read looks for."
  [forms]
  (apply lst (tok 'recognitum.core/checks) forms))

(defn- in-check-body? []
  (some #(and (= 'recognitum.core (:ns %)) (= 'checks (:name %))) (api/callstack)))

;; ------------------------------------------- shapes and concerns, by name

(def ^:private input-mark "__rec_input__")
(def ^:private check-mark "__rec_check__")
(def ^:private inputs-unknown "__rec_inputs_unknown")
(def ^:private concern-mark "__rec_concern")

(defonce ^{:doc "[ns name] -> {:inputs #{} :checks #{} :waits {}} for a bundle, with each
  check's waits among the bundle's checks; {:concern true} for a
  concern, {:check true} for a bundle check; filled in this run."}
  registry (atom {}))

(defn- markers [bundle {:keys [inputs checks]}]
  (concat (when (nil? inputs) [(symbol (str bundle inputs-unknown))])
          (for [i inputs] (symbol (str bundle input-mark i)))
          (for [c checks] (symbol (str bundle check-mark c)))))

(defn- marker-defs [ms]
  (concat (for [m ms] (lst (tok 'defn-) (tok m) (vct []) (tok nil)))
          [(vct (map tok ms))]))       ; used, so not reported unused

(defn- cached-names [ns] (set (keys (get (api/ns-analysis ns) :clj))))

(defn- shape-from-cache [ns bundle]
  (let [names (cached-names ns)
        pick (fn [mark] (let [p (str bundle mark)]
                          (set (for [n names :let [s (str n)] :when (str/starts-with? s p)]
                                 (symbol (subs s (count p)))))))
        shape {:inputs (when-not (names (symbol (str bundle inputs-unknown)))
                         (pick input-mark))
               :checks (pick check-mark)}]
    (when (or (seq (:inputs shape)) (seq (:checks shape))) shape)))

(defn- bundle-shape
  "The shape of the bundle named by node, or nil when it cannot be seen."
  [node]
  (when-let [{:keys [ns name]} (resolved node)]
    (let [r (get @registry [ns name])]
      (if (:checks r) r (shape-from-cache ns name)))))

(defn- concern-status
  "For a resolved {:ns :name}: :concern, :not-a-concern (a var that no defconcern defines), or
  nil (cannot say)."
  [{:keys [ns name]}]
  (let [r (get @registry [ns name])
        cached (cached-names ns)]
    (cond (:concern r) :concern
          (cached (symbol (str name concern-mark))) :concern
          r :not-a-concern
          (cached name) :not-a-concern)))

;; ------------------------------------------------------------ body rules

(def ^:private fetches '#{fetch fetch-each})
(def ^:private body-reads '#{fetch fetch-each extant? when-extant})

(def ^:private reserved
  "Symbol -> what a user writes in its place: the heads the macros write for a field read and
  for the read forms, which the check engine binds, and the read forms of earlier versions.
  The macros refuse a check body that writes one."
  '{&field                       "the field's name"
    &fetch                       "(rec/fetch ...)"
    &fetch-each                  "(rec/fetch-each ...)"
    &extant?                     "(rec/extant? ...)"
    recognitum.checks/fetch      "(rec/fetch ...)"
    recognitum.checks/fetch-each "(rec/fetch-each ...)"
    recognitum.checks/extant?    "(rec/extant? ...)"})

(defn- reserved-finding!
  "Report node when it is a symbol the check engine reserves, as the macros refuse it."
  [node]
  (when-let [s (sym node)]
    (when-let [m (or (reserved s) (reserved (symbol (name s))))]
      (finding! node :recognitum/engine-read-form
                (if (and (namespace s) (reserved s))
                  (str s " is the check engine's read form, not part of the API: write " m)
                  (str s " is a name the check engine reserves for its reads: a check body may"
                       " not write it, as a call, a value, a local or a var. To read, write " m))))))

(defn- read?
  "Whether node names one of the body macros in `names`."
  [node names]
  (rec? node names))

(defn- core-head
  "The name of the clojure.core var that heads a list node, or nil."
  [node]
  (when (= :list (api/tag node))
    (let [r (resolved (first (:children node)))]
      (when (= 'clojure.core (:ns r)) (:name r)))))

(def ^:private fn-forms
  "The clojure.core forms whose arguments all run inside a fn the form builds."
  '#{fn fn* lazy-seq lazy-cat delay future bound-fn dosync sync reify})

(defn- in-fn-child?
  "Whether the i-th child of a list headed by clojure.core's `head` runs inside a fn the form
  builds. A letfn's fns are in its binding vector, and its body is outside them; with-redefs
  and with-bindings take their bindings outside the fn that runs the body."
  [head i]
  (cond (fn-forms head) (pos? i)
        (= 'letfn head) (= 1 i)
        ('#{with-redefs with-bindings} head) (> i 1)
        :else false))

(defn- check-body!
  "Report rec/fetch as a value, inside a fn the body builds or inside locking,
  rec/extant? or rec/when-extant on a required field, and a symbol the check engine
  reserves. Quoted forms are data, and are not read."
  [node required in-fn? in-lock?]
  (let [kids (:children node)
        head (core-head node)]
    (cond
      (or (= :quote (api/tag node))
          (and (= :list (api/tag node)) (= 'quote (sym (first kids)))))
      nil

      (sym node)
      (do (reserved-finding! node)
          (when (read? node fetches)
            (finding! node :recognitum/fetch-as-value
                      "rec/fetch is a macro: use it only in call position")))
      (= :fn (api/tag node))                       ; #(...): its children are the call
      (check-body! (with-meta (api/list-node kids) (meta node)) required true in-lock?)

      (and (= :list (api/tag node)) (read? (first kids) fetches))
      (do (cond in-fn?
                (finding! node :recognitum/fetch-in-fn
                          "rec/fetch inside a fn the check body builds: it runs outside the check")
                in-lock?
                (finding! node :recognitum/fetch-in-locking
                          "rec/fetch inside locking: a read may wait, and would hold the lock"))
          (doseq [k (rest kids)] (check-body! k required in-fn? in-lock?)))

      (and (= :list (api/tag node)) (read? (first kids) '#{extant? when-extant}))
      (let [f (second kids)]
        (when (contains? required (sym f))
          (finding! f :recognitum/extant-on-required
                    (str (sym f) " is a required field: it is always extant")))
        (doseq [k (rest kids)] (check-body! k required in-fn? in-lock?)))

      (and (= 'for head) (= :vector (api/tag (second kids))))
      ;; (for [x coll ...] body): only the first collection is read outside the fn
      (do (doseq [[i k] (map-indexed vector (:children (second kids)))]
            (check-body! k required (or in-fn? (not= 1 i)) in-lock?))
          (doseq [k (drop 2 kids)] (check-body! k required true in-lock?)))

      kids
      (let [in-lock? (or in-lock? (= 'locking head))]
        (doseq [[i k] (map-indexed vector kids)]
          (check-body! k required (or in-fn? (in-fn-child? head i)) in-lock?))))))

(defn body-read
  "rec/fetch, rec/fetch-each, rec/extant? and rec/when-extant throw at expansion
  outside a check body. The call is left as it is, so it navigates to the macro."
  [{:keys [node]}]
  (when-not (in-check-body?)
    (let [op (first (:children node))]
      (finding! node :recognitum/read-outside-body
                (str "rec/" (name (sym op)) " is allowed only inside a check body"))))
  nil)

;; ----------------------------------------------------------- clause parse

(defn- clause-parts [clause]
  (let [[nm waits & more] (:children clause)
        body (if (and (api/string-node? (first more)) (next more)) (rest more) more)]
    {:name nm :waits (when (= :vector (api/tag waits)) (:children waits))
     :body body :meta (:meta clause)}))

(defn- meta-entries
  "[key-node value-node] pairs of a node's reader metadata; ^:k gives [k nil]."
  [node]
  (for [m (:meta node)
        [k v] (if (api/map-node? m) (partition 2 (:children m)) [[m nil]])
        :when (api/keyword-node? k)]
    [k v]))

(defn- concern-usages
  "^::c and ^::alias/c on a clause become usages of the concern var, at the
  keyword. A keyword that names a var that no rec/defconcern defines is reported."
  [ns clause]
  (for [[k _] (meta-entries clause) :when (:namespaced? k)
        :let [kw (:k k)
              node (at (symbol (namespace kw) (name kw)) k)
              target (if (namespace kw)
                       (api/resolve {:name (api/sexpr node)})
                       {:ns ns :name (symbol (name kw))})]]
    (do (when (= :not-a-concern (some-> target concern-status))
          (finding! k :recognitum/not-a-concern
                    (str (api/sexpr node) " is not a concern: no rec/defconcern defines it")))
        node)))

(defn- override? [clause]
  (some #(= :override (:k (first %))) (meta-entries clause)))

(defn- sections
  "Split a defining form's tail into [{:bundle node :clauses [...]}... {:own [...]}]."
  [nodes]
  (reduce (fn [acc n]
            (cond
              (rec? n '#{checks}) (conj acc {:checks-token n :own []})
              (api/token-node? n) (conj acc {:bundle n :clauses []})
              (empty? acc) (conj acc {:stray [n]})
              :else (update acc (dec (count acc))
                            (fn [s] (update s (if (:own s) :own :clauses) (fnil conj []) n)))))
          [] nodes))

;; ----------------------------------------------------------------- cycles

(defn- reach
  "The nodes reachable from start by one or more edges."
  [edges start]
  (loop [seen #{} todo (vec (edges start))]
    (if-let [[x & more] (seq todo)]
      (recur (conj seen x) (into (vec more) (when-not (seen x) (edges x))))
      seen)))

(defn- ring
  "A shortest path of edges from start back to start."
  [edges start]
  (loop [paths [[start]] seen #{}]
    (when-let [[path & more] (seq paths)]
      (let [nexts (edges (peek path))]
        (if (some #{start} nexts)
          (conj path start)
          (let [new (remove seen nexts)]
            (recur (into (vec more) (map #(conj path %) new)) (into seen new))))))))

(defn- cycles!
  "Report each cycle of edges once, at the first of clauses ([node name-node]
  pairs, in source order) that lies on it. label gives a node's written name;
  note, given the cycle's nodes, what to add to the message."
  ([clauses edges label] (cycles! clauses edges label (constantly nil)))
  ([clauses edges label note]
  (reduce (fn [done [n at]]
            (let [r (reach edges n)]
              (if (or (done n) (not (contains? r n)))
                done
                (do (finding! at :recognitum/wait-cycle
                              (let [path (ring edges n)]
                                (str "the waits form a cycle: " (str/join " -> " (map label path)) (note path))))
                    (into done (filter #(contains? (reach edges %) n) r))))))
          #{} clauses)))

;; ------------------------------------------------------------------ hooks

(declare defconcern* defcontext* defchecks* defcopy* defproven*)

(defn defconcern [{:keys [node ns]}]
  (binding [*pin* (pin-of node)] (defconcern* node ns)))

(defn- defconcern* [node ns]
  (let [[_ nm doc] (:children node)]
    (swap! registry assoc [ns (sym nm)] {:concern true})
    {:node (apply lst (tok 'do)
                  (lst (tok 'def) nm (or doc (tok nil)))
                  (marker-defs [(symbol (str (sym nm) concern-mark))]))}))

(defn defcontext [{:keys [node]}]
  (binding [*pin* (pin-of node)] (defcontext* node)))

(defn- defcontext* [node]
  (let [[_ nm doc sel] (:children node)]
    {:node (lst (tok 'def) nm (lst (tok 'do) doc sel))}))

(defn defchecks [{:keys [node ns]}]
  (binding [*pin* (pin-of node)] {:node (defchecks* node ns)}))

(defn- defchecks* [node ns]
  (let [[_ nm inputs & tail] (:children node)
        secs (sections tail)
        clauses (map clause-parts (mapcat :own secs))
        named (filter (comp sym :name) clauses)
        checks (set (map (comp sym :name) named))
        ins (set (map sym (:children inputs)))
        shape {:inputs ins
               :checks checks
               :waits (into {} (for [c named] [(sym (:name c)) (set (filter checks (keep sym (:waits c))))]))}]
    (swap! registry assoc [ns (sym nm)] shape)
    (doseq [c (:checks shape)] (swap! registry assoc [ns c] {:check true}))
    (doseq [c clauses] (doseq [b (:body c)] (check-body! b #{} false false)))
    (cycles! (for [c named] [(sym (:name c)) (:name c)]) (:waits shape) str)
    (apply lst (tok 'do)
           (lst (tok 'def) nm (tok nil))
           (concat
            (marker-defs (markers (sym nm) shape))
            (for [c clauses] (lst (tok 'def) (:name c) (tok nil)))
            [(apply lst (tok 'do)
                    (concat (keep :checks-token secs)
                            (mapcat (partial concern-usages ns) clauses)
                            (map #(vct (:waits %)) clauses)))
             (lst (tok 'fn) inputs
                  (vct (concat (map (comp in-body :body) clauses) (map fresh (:children inputs)))))]))))

(defn defcopy [{:keys [node ns]}]
  (binding [*pin* (pin-of node)] (defcopy* node ns)))

(defn- defcopy*
  "(rec/defcopy new-name bundle {check new-check ...}). Each new name is written
  in the source, so it navigates and renames as a var, and the copy needs no
  sight of the bundle to define its vars."
  [node ns]
  (let [[_ nm bundle names & extra] (:children node)]
    (if (or extra (not (api/map-node? names)))
      (do (when-not (or extra (api/map-node? names))
            (finding! (or names node) :recognitum/copy-form
                    "rec/defcopy takes a map {check new-check ...} after the bundle"))
          {:node (lst (tok 'do) (lst (tok 'def) nm (tok nil)) bundle)})
      (let [shape (bundle-shape bundle)
            pairs (partition 2 (:children names))
            bns (some-> (sym bundle) namespace)
            renames (into {} (for [[k v] pairs :when (and (sym k) (sym v))] [(sym k) (sym v)]))
            copy {:inputs (:inputs shape)              ; nil: not known
                  :checks (set (map (comp sym second) pairs))
                  :waits (into {} (for [[c ws] (:waits shape) :when (renames c)]
                                    [(renames c) (set (keep renames ws))]))}
            known? (fn [k] (or (nil? shape) (contains? (:checks shape) (sym k))))]
        (doseq [side [first second]
                :let [nodes (map side pairs)]
                [n seen] (map vector nodes (reductions conj #{} (map sym nodes)))
                :when (seen (sym n))]
          (finding! n :recognitum/copy-duplicate (str (sym n) " is named twice in the rec/defcopy map")))
        (when shape
          (doseq [c (sort (remove (set (map (comp sym first) pairs)) (:checks shape)))]
            (finding! names :recognitum/copy-incomplete
                      (str "no new name for check " c " of bundle " (sym bundle))))
          (doseq [[k _] pairs :when (not (known? k))]
            (finding! k :recognitum/copy-unknown-check
                      (str "bundle " (sym bundle) " has no check " (sym k)))))
        (swap! registry assoc [ns (sym nm)] copy)
        (doseq [c (:checks copy)] (swap! registry assoc [ns c] {:check true}))
        {:node (apply lst (tok 'do)
                      (lst (tok 'def) nm (tok nil))
                      bundle
                      (concat
                       (marker-defs (markers (sym nm) copy))
                       [(vct (for [[k _] pairs :when (known? k)] ; old names: uses of the bundle's checks
                               (at (if bns (symbol bns (str (sym k))) (sym k)) k)))]
                       (for [v (vals (into {} (map (juxt (comp sym second) second)) (reverse pairs)))]
                         (lst (tok 'def) v (tok nil)))))}))))

(defn defproven [{:keys [node ns]}]
  (binding [*pin* (pin-of node)] {:node (defproven* node ns)}))

(defn- when-extant-field
  "The field named by ^{:when-extant field} on a bundle binding, checked against
  the type's optional fields. Returns the field's node when it is a field, for
  navigation and rename."
  [{:keys [bundle]} field-syms optional]
  (when-let [[_ v] (first (filter #(= :when-extant (:k (first %))) (meta-entries bundle)))]
    (let [f (sym v)]
      (cond
        (not (contains? field-syms f))
        (finding! v :recognitum/when-extant-field
                  (str (api/sexpr v) " is not a field of this type"))

        (not (contains? optional f))
        (finding! v :recognitum/when-extant-field
                  (str f " is a required field: :when-extant never skips"))

        :else v))))

(defn- gate-of
  "The field a bundle binding's ^{:when-extant field} names, or nil."
  [bundle]
  (some-> (first (filter #(= :when-extant (:k (first %))) (meta-entries bundle))) second sym))

(defn- wait-cycles!
  "Report cycles among the waits the clauses list and the gates the bindings name. A node is an
  own check by its bare name, a check of a bound bundle by its var, or a bound bundle, which
  waits on each of its checks. A bundle check waits on what its defchecks lists among the
  bundle's checks; an override replaces that. Each check of a binding gated on a field,
  overridden or not, reads that field, and so waits on its shape check. Check bodies are not
  read: a cycle through a read written in a body is left to the macros. What a bundle's checks
  wait on is known only for a bundle whose defchecks this run has seen."
  [own-clauses overrides bound field-syms]
  (let [own (set (keep (comp sym :name) own-clauses))
        shape-checks (set (filter field-syms own))
        override-of (into {} (for [c overrides :let [r (resolved (:name c))] :when r] [r c]))
        bindings (for [[{:keys [bundle]} shape] bound
                       :let [r (resolved bundle)] :when (and r shape)]
                   {:var r :bundle bundle :shape shape
                    :gate (let [g (gate-of bundle)] (when (shape-checks g) g))})
        bundle-of (into {} (for [b bindings] [(:var b) b]))
        check-of (into {} (for [b bindings, c (:checks (:shape b))] [{:ns (:ns (:var b)) :name c} b]))
        node (fn [w] (let [s (sym w)]
                       (cond (simple-symbol? s) (when (own s) s)
                             (qualified-symbol? s) (let [r (resolved w)]
                                                     (when (or (override-of r) (bundle-of r) (check-of r)) r)))))
        clause-node (fn [c] (if (override? c) (resolved (:name c)) (sym (:name c))))
        gate-edges (into {} (for [{:keys [var shape gate]} bindings :when gate, c (:checks shape)]
                              [{:ns (:ns var) :name c} #{gate}]))
        wait-edges (merge
                    (into {} (for [{:keys [var shape]} bindings, c (:checks shape)
                                   :let [n {:ns (:ns var) :name c}] :when (not (override-of n))]
                               [n (for [w (get-in shape [:waits c])] {:ns (:ns var) :name w})]))
                    (into {} (for [c (concat own-clauses overrides) :let [n (clause-node c)] :when n]
                               [n (keep node (:waits c))]))
                    (into {} (for [{:keys [var shape]} bindings]
                               [var (for [c (:checks shape)] {:ns (:ns var) :name c})])))
        label (fn [n] (str (cond (symbol? n) n
                                 (override-of n) (sym (:name (override-of n)))
                                 (bundle-of n) (sym (:bundle (bundle-of n)))
                                 :else (:name n))))]
    (cycles! (keep (fn [c] (when-let [n (clause-node c)] [n (:name c)]))
                   (sort-by #(let [{:keys [row col]} (meta (:name %))] [row col])
                            (concat own-clauses overrides)))
             (merge-with concat wait-edges gate-edges)
             label
             (fn [path]
               (when (some (fn [[a b]] (contains? (get gate-edges a) b)) (partition 2 1 path))
                 " (a check waits on the shape checks of the fields it reads)")))))

(defn- method-clause?
  "Whether node reads as a method implementation, (name [this ...] body...), and not as an
  input clause, whose vector is empty."
  [node]
  (let [[nm params] (:children node)]
    (and (= :list (api/tag node)) (sym nm) (= :vector (api/tag params)) (seq (:children params)))))

(defn- protocol-sections
  "secs with each section that implements a protocol as {:protocol node :methods [...]}: a
  token that names no bundle the hook can see, whose first form is a method, and every
  section after one. A checks section after a protocol, rec/checks or a token that names a
  bundle or whose first form is an input clause, is reported, and kept as it is."
  [secs]
  (first
   (reduce (fn [[out proto] s]
             (let [head (or (:bundle s) (:checks-token s))]
               (cond
                 (and proto (or (:checks-token s) (bundle-shape (:bundle s))
                                (when-let [c (first (:clauses s))] (not (method-clause? c)))))
                 (do (finding! head :recognitum/checks-after-protocol
                               (str (sym head) " follows the protocol " (sym proto)
                                    ": the checks sections, rec/checks and the bundle bindings, come before the protocols"))
                     [(conj out s) proto])

                 (and (:bundle s)
                      (or proto
                          (and (not (bundle-shape (:bundle s))) (method-clause? (first (:clauses s))))))
                 [(conj out {:protocol (:bundle s) :methods (:clauses s)}) (or proto (:bundle s))]

                 :else [(conj out s) proto])))
           [[] nil] secs)))

(defn- defproven* [node ns]
  (let [[_ T fields & tail] (:children node)
        field-nodes (:children fields)
        field-syms (set (map sym field-nodes))
        optional (set (for [f field-nodes
                            :when (some #(= :optional (:k %)) (:meta f))]
                        (sym f)))
        required (apply disj field-syms optional)
        all-secs (protocol-sections (sections tail))
        impls (mapcat (fn [s] (cons (:protocol s) (:methods s))) (filter :protocol all-secs))
        secs (remove :protocol all-secs)
        bundles (filter :bundle secs)
        shapes (map (comp bundle-shape :bundle) bundles)
        clauses (map clause-parts (mapcat :own secs))
        own-clauses (remove override? clauses)
        overrides (filter override? clauses)
        known (when (every? some? shapes)
                (set (for [[{:keys [bundle]} shape] (map vector bundles shapes)
                           :let [bns (:ns (resolved bundle))]
                           c (:checks shape)]
                       {:ns bns :name c})))
        ;; an override must name a check of a bound bundle
        overrides (doall
                   (for [c overrides
                         :let [bad? (and known (not (contains? known (resolved (:name c)))))]]
                     (do (when bad?
                           (finding! (:name c) :recognitum/override-unbound
                                     (str (sym (:name c)) " is not a check of any bundle bound here")))
                         (assoc c :reported bad?))))
        when-extants (doall (keep #(when-extant-field % field-syms optional) bundles))]
    ;; input clauses against the bundle's inputs
    (doseq [[{:keys [bundle clauses]} shape] (map vector bundles shapes)
            :when (:inputs shape)
            :let [named (set (map (comp sym :name clause-parts) clauses))]]
      (doseq [c clauses :let [n (:name (clause-parts c))]
              :when (not (contains? (:inputs shape) (sym n)))]
        (finding! n :recognitum/unknown-input
                  (str "bundle " (sym bundle) " has no input " (sym n))))
      (doseq [i (sort (remove named (:inputs shape)))]
        (finding! bundle :recognitum/missing-input
                  (str "no clause binds input " i " of bundle " (sym bundle)))))
    ;; a bundle bound twice
    (reduce (fn [seen b]
              (let [r (resolved b)]
                (when (contains? seen r)
                  (finding! b :recognitum/bound-twice
                            (str "bundle " (sym b) " is bound twice in this type")))
                (cond-> seen r (conj r))))
            #{} (map :bundle bundles))
    (wait-cycles! own-clauses overrides (map vector bundles shapes) field-syms)
    ;; a bare wait names an own check; one that names a field with no shape check, or a var (a
    ;; bundle check, a function of clojure.core), is reported here; one that names nothing is
    ;; clj-kondo's unresolved symbol
    (let [own (set (map (comp sym :name) own-clauses))]
      (doseq [c clauses, w (:waits c)
              :let [s (sym w)]
              :when (and (simple-symbol? s) (not (own s))
                         (or (field-syms s) (resolved w)))]
        (finding! w :recognitum/unknown-wait
                  (str s " is not a check of this type"
                       (when (:check (some->> (resolved w) ((juxt :ns :name)) (get @registry)))
                         (str ": a bundle check is waited on as [bundle " s "]"))))))
    (doseq [c clauses] (doseq [b (:body c)] (check-body! b required false false)))
    (let [shape-check? #(contains? field-syms (sym (:name %)))
          invariants (remove shape-check? own-clauses)]
      ;; Fields are the outer scope. A shape check's name is a use of its field,
      ;; so the field and its shape check rename together. Invariants are letfn
      ;; locals, seen by the waits but not by the bodies.
      (lst (tok 'do)
           (apply lst (tok 'defrecord) T (vct (map fresh field-nodes)) impls)
           (lst (tok 'fn) fields
                (vct (concat
                      [(apply lst (tok 'letfn)
                              (vct (for [c invariants] (lst (:name c) (vct []) (tok nil))))
                              (concat (keep :checks-token secs)
                                      (map (comp bare :bundle) bundles)
                                      (map :name (remove :reported overrides))
                                      (mapcat (partial concern-usages ns) clauses)
                                      (map #(vct (:waits %)) clauses)
                                      [(vct (map (comp fresh :name) invariants))]))]
                      when-extants
                      (map :name (filter shape-check? own-clauses))
                      (mapcat (comp (partial mapcat (comp :body clause-parts)) :clauses) bundles)
                      (map (comp in-body :body) clauses)
                      (map fresh field-nodes))))))))
