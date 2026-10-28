(ns recognitum.body-test
  "Check bodies: what they see, what a throw does, and the body macros."
  (:require
   [clojure.test :refer [deftest is]]
   [recognitum.core :as rec]))

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defmacro local-tag
  "The tag `&env` gives the local `s`, or nil when `s` is no local."
  [s]
  (list 'quote (some-> (get &env s) (.-tag))))

(defn- on-file [ks] (into {} (map (fn [k] [k (odd? k)])) ks))

(rec/defproven Probe [n ^String s ^:optional o]
  rec/checks
  (n [] (int? n))
  (divides [n] (= 1 (quot 10 (- 10 n))))
  (asserted [n] (do (assert (not= n 99)) true))
  (tagged [] (= 'String (local-tag s)))
  (optional-read [] (if (rec/extant? o) (string? o) true))
  (guarded [] (rec/when-extant o (pos? (count o))))
  (on-file [n] (rec/fetch on-file n))
  (each-on-file [n] (every? true? (vals (rec/fetch-each on-file [n (+ n 2)])))))

(defn- probe [m] (map->Probe (merge {::n 1 ::s "s"} m)))

(deftest a-body-sees-the-fields-as-locals-with-their-tags
  (is (instance? Probe (probe {}))))

(deftest an-exception-from-a-body-propagates
  (is (thrown? ArithmeticException (probe {::n 10})) "out of the constructor")
  (let [v (probe {})
        d (rec/draft v)]
    (is (thrown? ArithmeticException (assoc v ::n 10)) "out of a write")
    (is (thrown? ArithmeticException (rec/seal (assoc d ::n 10))) "out of a seal")
    (is (thrown? ArithmeticException (rec/check (assoc d ::n 10) ::divides)) "out of check")
    (is (thrown? ArithmeticException (rec/recheck (assoc d ::n 10) ::divides)) "out of recheck")
    (is (thrown? ArithmeticException (rec/check-all [(rec/draft Probe) (assoc d ::n 10)] rec/every))
        "out of check-all, with no answer for any value")
    (let [d2 (assoc d ::n 10)
          before (rec/proven-checks d2)]
      (try (rec/seal d2) (catch ArithmeticException _))
      (is (= before (rec/proven-checks d2)) "a seal that threw remembers nothing new"))))

(rec/defproven Catcher [n]
  rec/checks
  (caught [] (try (quot 10 n) true (catch ArithmeticException _ :zero))))

(deftest a-body-may-catch-its-own-exception
  (is (instance? Catcher (map->Catcher {::n 1})))
  (is (= :zero (get-in (err #(map->Catcher {::n 0})) [:failed ::caught :data]))))

(deftest an-error-propagates
  (is (thrown? AssertionError (probe {::n 99}))))

(rec/defproven Careless [a ^:optional b]
  rec/checks
  (reads-a [] (some? a))
  (reads-b [] (some? b)))

(deftest reading-an-absent-field-fails-only-the-check-that-reads-it
  (is (instance? Probe (probe {})) "o is absent, and every check that reads it asks first")
  (let [e (err #(map->Careless {::a 1}))]
    (is (= [::reads-b] (keys (:failed e))))
    (is (= {:absent ::b} (get-in e [:failed ::reads-b :data])))))

(deftest a-source-is-read-inline
  (is (instance? Probe (probe {::n 3})) "3 and 5 are on file")
  (let [e (err #(probe {::n 2}))]
    (is (= #{::on-file ::each-on-file} (set (keys (:failed e)))))
    (is (false? (get-in e [:failed ::on-file :data])))))

(deftest the-body-macros-throw-outside-a-check-body
  (doseq [form ['(recognitum.core/fetch on-file 1)
                '(recognitum.core/fetch-each on-file [1])
                '(recognitum.core/extant? x)
                '(recognitum.core/when-extant x true)]]
    (is (re-find #"can be used only inside a check body"
                 (try (binding [*ns* (the-ns 'recognitum.body-test)] (eval form)) ""
                      (catch clojure.lang.Compiler$CompilerException e (ex-message (ex-cause e)))))
        (pr-str form))))

(defn- refusal
  "The message refusing `form`, evaluated in this namespace, or nil when it is accepted."
  [form]
  (try (binding [*ns* (the-ns 'recognitum.body-test)] (eval form)) nil
       (catch clojure.lang.Compiler$CompilerException e (ex-message (ex-cause e)))))

(deftest a-call-of-a-name-that-names-nothing-is-refused-at-definition
  (is (re-find #"\(fetch-each \.\.\.\) calls fetch-each, which names nothing here\. To use the body macro, write \(rec/fetch-each \.\.\.\)"
               (str (refusal '(rec/defproven Unread [n] rec/checks (known [] (fetch-each on-file [n])))))))
  (is (re-find #"\(frobnicate \.\.\.\) calls frobnicate, which is not a field of Unknown, a local or a var"
               (str (refusal '(rec/defproven Unknown [n] rec/checks (known [] (frobnicate n))))))))

(defn- fetch "A function of this namespace that shares a body macro's name." [m k] (get m k))

(rec/defproven OwnFetch [n]
  rec/checks
  (own [] (= :one (fetch {1 :one} n))))

(deftest a-call-of-a-var-named-like-a-body-macro-calls-the-var
  (is (instance? OwnFetch (map->OwnFetch {::n 1})))
  (is (= #{::own} (rec/checks-of rec/pure OwnFetch))))

(defmacro field-of
  "Expands to the read form the record layer writes for the field whose key is `k`."
  [k]
  (list '&field k))

(deftest the-engine-s-read-forms-are-refused-in-a-body
  (doseq [[body msg] [['(recognitum.checks/fetch on-file n)
                       #"recognitum\.checks/fetch is the check engine's read form, not part of the API: write \(rec/fetch \.\.\.\)"]
                      ['(recognitum.checks/fetch-each on-file [n])
                       #"recognitum\.checks/fetch-each is the check engine's read form, not part of the API: write \(rec/fetch-each \.\.\.\)"]
                      ['(recognitum.checks/extant? n)
                       #"recognitum\.checks/extant\? is the check engine's read form, not part of the API: write \(rec/extant\? \.\.\.\)"]
                      ['(&fetch on-file n)
                       #"&fetch is a name the check engine reserves for its reads: .* To read, write \(rec/fetch \.\.\.\)"]
                      ['(&fetch-each on-file [n])
                       #"&fetch-each is a name the check engine reserves for its reads: .* To read, write \(rec/fetch-each \.\.\.\)"]
                      ['(&extant? n)
                       #"&extant\? is a name the check engine reserves for its reads: .* To read, write \(rec/extant\? \.\.\.\)"]
                      ['(&field ::n)
                       #"&field is a name the check engine reserves for its reads: .* To read, write the field's name"]
                      ['(field-of ::n)
                       #"&field is a name the check engine reserves for its reads: .* To read, write the field's name"]
                      ['(case n &field true false)
                       #"&field is a name the check engine reserves for its reads"]
                      ['(some? &fetch)
                       #"&fetch is a name the check engine reserves for its reads"]
                      ['(map recognitum.checks/fetch [n])
                       #"recognitum\.checks/fetch is the check engine's read form"]
                      ['(. "a" (&field ::n))
                       #"&field is a name the check engine reserves for its reads"]
                      ['(new &field 1)
                       #"&field is a name the check engine reserves for its reads"]
                      ['(do (deftype ReservedType [q] &fetch (m [_] 1)) true)
                       #"&fetch is a name the check engine reserves for its reads"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'Reserved '[n] 'rec/checks (list 'c [] body))))) (pr-str body))))

(deftest a-local-or-var-named-like-an-engine-read-is-refused-in-a-body
  (doseq [body ['(let [&fetch 1] (= 1 &fetch))
                '(let [f (fn [&extant?] &extant?)] (f true))
                '(letfn [(&fetch-each [] true)] (&fetch-each))
                '(try true (catch Exception &fetch false))
                '(let [&field 1] (= 1 &field))
                '(user/&fetch n)]]
    (is (re-find #"is a name the check engine reserves for its reads: a check body may not write it"
                 (str (refusal (list 'rec/defproven 'ReservedLocal '[n] 'rec/checks (list 'c [] body)))))
        (pr-str body))))

(deftest a-quoted-engine-read-form-is-data
  (is (nil? (refusal '(rec/defproven QuotedRead [n] rec/checks (c [] (= 'recognitum.checks/fetch (first '(recognitum.checks/fetch a b)))))))))

(deftest a-read-with-the-wrong-arguments-is-refused
  (doseq [[body msg] [['(rec/fetch on-file) #"rec/fetch takes a source and a key: \(rec/fetch on-file\)"]
                      ['(rec/fetch on-file n n) #"rec/fetch takes a source and a key"]
                      ['(rec/fetch-each on-file) #"rec/fetch-each takes a source and a collection of keys"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'BadFetch '[n] 'rec/checks (list 'c [] body)))))
        (pr-str body))))

(deftest extant-on-anything-but-one-field-is-refused
  (doseq [[body msg] [['(rec/extant?) #"rec/extant\? takes one field: \(rec/extant\?\)"]
                      ['(rec/extant? o n) #"rec/extant\? takes one field"]
                      ['(rec/extant? (str o)) #"rec/extant\? takes one field"]
                      ['(rec/when-extant) #"rec/when-extant takes one field"]
                      ['(let [x o] (rec/extant? x)) #"rec/extant\? on x, which is a local here, not a field"]
                      ['(rec/extant? nothing) #"rec/extant\? on nothing, which is not a field of BadExtant\. Fields: n o"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'BadExtant ['n (with-meta 'o {:optional true})] 'rec/checks (list 'c [] body)))))
        (pr-str body))))

;;; Reads inside a fn, a try or a reify the body builds count as reads.

(rec/defproven Scores [scores limit]
  rec/checks
  (scores [] (vector? scores))
  (limit [] (int? limit))
  (under [] (every? #(<= % limit) scores)))

(deftest a-write-of-a-field-read-only-inside-a-fn-runs-the-check-again
  (let [v (map->Scores {::scores [1 2] ::limit 5})
        e (err #(assoc v ::limit 0))]
    (is (= [::under] (keys (:failed e))) "the write is refused")
    (is (= #{::limit ::under} (rec/checks-of (rec/reading ::limit) Scores)))))

(rec/defproven Tried [n]
  rec/checks
  (n [] (int? n))
  (positive [] (try (pos? n) (catch Exception _ false))))

(deftest a-check-reading-a-field-only-inside-a-try-waits-on-its-shape-check
  (let [e (err #(map->Tried {::n "x"}))]
    (is (= [::n] (keys (:failed e))))
    (is (= #{::positive} (:blocked e)))))

(deftest a-read-inside-a-fn-closes-a-cycle
  (is (= (str "recognitum.body-test/FnCycle: the waits form a cycle: :recognitum.body-test/x"
              " -> :recognitum.body-test/c -> :recognitum.body-test/x (a check waits on the shape"
              " checks of the fields it reads)")
         (refusal '(rec/defproven FnCycle [x]
                     rec/checks
                     (x [c] (int? x))
                     (c [] (every? (fn [y] (= y x)) [1])))))))

(rec/defchecks under-limit [xs lim]
  rec/checks
  (all-under [] (every? #(<= % lim) xs)))

(rec/defproven Capped [items cap]
  under-limit
  (xs [] items)
  (lim [] cap))

(deftest a-bundle-input-read-only-inside-a-fn-is-passed-in
  (is (instance? Capped (map->Capped {::items [1 2] ::cap 5})))
  (is (= [::all-under] (keys (:failed (err #(map->Capped {::items [1 9] ::cap 5}))))))
  (is (= #{::all-under} (rec/checks-of (rec/reading ::cap) Capped))))

(rec/defproven TriedFetch [n]
  rec/checks
  (tried-on-file [] (try (rec/fetch on-file n) (catch Exception _ false))))

(deftest a-fetch-inside-a-try-is-a-read-of-its-source
  (is (= #{::tried-on-file} (rec/checks-of (rec/fetching on-file) TriedFetch)))
  (is (= #{} (rec/checks-of rec/pure TriedFetch)))
  (is (false? (get-in (err #(map->TriedFetch {::n 2})) [:failed ::tried-on-file :data]))))

;;; A body binds its locals under fresh names, and reads a field only where it names one.

(defprotocol Echo (echo [this x]))

(definterface Pair (pair [a]))

(rec/defproven Shadows [x ^String y]
  rec/checks
  (by-let [] (let [x 2] (= 2 x)))
  (by-let-init [] (let [x (inc x)] (= 2 x)))
  (by-loop [] (loop [x 3 n 0] (if (pos? n) (= 3 x) (recur x 1))))
  (by-fn-param [] ((fn [x] (= 4 x)) 4))
  (by-fn-name [] ((fn x [n] (if (pos? n) (x (dec n)) true)) 2))
  (by-letfn [] (letfn [(x [] 5)] (= 5 (x))))
  (by-catch [] (try (throw (ex-info "e" {:v 6})) (catch Exception x (= 6 (:v (ex-data x))))))
  (by-reify [] (= 7 (echo (reify Echo (echo [_ x] x)) 7)))
  (by-deftype [] (do (deftype Local [z] Pair (pair [_ x] [x z]))
                     (= [8 9] (.pair ^Pair ((resolve `->Local) 9) 8))))
  (by-case [] (= :one (case (inc 0) 1 :one 2 :two)))
  (sees-y-tagged [] (= 'String (local-tag y)))
  (sees-a-local-tagged [] (let [^CharSequence y (str 1)] (= 'CharSequence (local-tag y)))))

(deftest a-local-of-each-binding-form-shadows-a-field
  (is (instance? Shadows (map->Shadows {::x 1 ::y "s"})) "each check sees its local, not the field")
  (is (= #{::by-let-init} (rec/checks-of (rec/reading ::x) Shadows))
      "only the let's init reads x")
  (is (= #{} (rec/checks-of (rec/reading ::y) Shadows)) "a macro's look at &env reads nothing")
  (is (= [::by-let-init] (keys (:failed (err #(map->Shadows {::x 5 ::y "s"})))))))

(rec/defchecks tagged [t]
  rec/checks
  (string [] (= 'String t)))

(rec/defproven TaggedInput [^String s]
  tagged
  (t [] (local-tag s)))

(deftest a-macro-in-an-input-clause-sees-the-fields-with-their-tags
  (is (instance? TaggedInput (map->TaggedInput {::s "s"}))))

(declare snap snap-input)

(rec/defproven Snapshot [limit scores]
  rec/checks
  (under [] (do (def snap limit) (every? #(<= % snap) scores))))

(deftest a-write-of-a-field-read-inside-a-def-runs-the-check-again
  (let [v (map->Snapshot {::limit 10 ::scores [5]})]
    (is (= #{::under} (rec/checks-of (rec/reading ::limit) Snapshot)))
    (is (= [::under] (keys (:failed (err #(assoc v ::limit 0))))))))

(rec/defchecks positive [n]
  rec/checks
  (pos [] (pos? n)))

(rec/defproven SnapshotInput [a b]
  positive
  (n [] (do (def snap-input a) snap-input)))

(deftest a-write-of-a-field-an-input-clause-reads-inside-a-def-runs-the-check-again
  (let [v (map->SnapshotInput {::a 1 ::b 0})]
    (is (= #{::pos} (rec/checks-of (rec/reading ::a) SnapshotInput)))
    (is (= [::pos] (keys (:failed (err #(assoc v ::a -1))))))))

(deftest a-field-the-body-uses-where-it-cannot-be-read-is-refused
  (doseq [[body msg] [['(= 1 (count ^{:m x} [1])) #"x is used where a check body cannot read it"]
                      ['(do (set! x 1) true) #"set! of x: a check body cannot assign a field"]
                      ['(do (deftype Other [x]) true) #"deftype field x shadows the field x"]
                      ['(do (def ^{:m x} position-def 1) true) #"x is used where a check body cannot read it"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'Unreadable '[x] 'rec/checks (list 'c [] body))))))
        (pr-str body)))

;;; Every position the compiler evaluates: a field there is read.

(declare position-def)

(def ^:dynamic *position* nil)

(def ^:private ran (atom #{}))

(def ^:private positions
  "Each check's name and an expression that names x in one position only. The first group
  reads x; the second names x where the compiler does not evaluate it."
  '{:reading
    [if-test           (if (= 1 x) 1 2)
     if-then           (if true x 2)
     if-else           (if false 1 x)
     do-statement      (do x nil)
     do-return         (do nil x)
     let-init          (let [a x] a)
     let-body          (let [a 1] [a x])
     loop-init         (loop [a x] a)
     loop-body         (loop [a 1] [a x])
     recur-arg         (loop [a 0] (if (zero? a) (recur x) a))
     letfn-init        (letfn [(f [] x)] (f))
     letfn-body        (letfn [(f [] 1)] [(f) x])
     fn-body           ((fn [] x))
     fn-second-arity   ((fn ([] 1) ([a] [a x])) 1)
     def-init          (do (def position-def x) position-def)
     try-body          (try x (catch Exception _ nil))
     catch-body        (try (throw (Exception.)) (catch Exception _ x))
     finally-body      (try 1 (finally x))
     case-test         (case x 1 :one :other)
     case-default      (case 0 1 :one x)
     case-then         (case 1 1 x :other)
     throw-arg         (try (throw (ex-info "t" {:v x})) (catch Exception e (ex-data e)))
     dot-target        (. x toString)
     dot-arg           (. "1" equals x)
     dot-member-arg    (. "1" (equals x))
     new-arg           (new String (str x))
     set!-value        (binding [*position* 0] (set! *position* x))
     monitor           (try (monitor-enter x) (finally (monitor-exit x)))
     reify-body        (.call ^java.util.concurrent.Callable (reify java.util.concurrent.Callable (call [_] x)))
     map-key           {x 1}
     map-value         {:k x}
     vector-element    [x]
     set-element       #{x}
     call-arg          (identity x)
     extant            (rec/extant? x)]
    :not-reading
    [quoted            'x
     quoted-call       '(inc x)
     case-constant     (case 1 x :x :other)
     shadowed          (let [x 2] x)]})

(defmacro ^:private defpositions
  "A type with fields x, optional so that `rec/extant?` may ask, and y, and one check for each entry of `positions`, which records its run
  in `ran` and evaluates its expression."
  [tname]
  `(rec/defproven ~tname [~(with-meta 'x {:optional true}) ~'y]
     rec/checks
     ~@(for [[nm expr] (partition 2 (concat (:reading positions) (:not-reading positions)))]
         (list nm [] (list 'do (list `swap! `ran `conj (keyword (str *ns*) (str nm))) expr true)))))

(defpositions Positions)

(defn- ids [rows] (set (map #(keyword "recognitum.body-test" (str %)) (take-nth 2 rows))))

(deftest a-field-is-read-in-every-position-the-compiler-evaluates
  (let [reading (ids (:reading positions))
        v (map->Positions {::x 1 ::y 1})]
    (is (= reading (rec/checks-of (rec/reading ::x) Positions)))
    (reset! ran #{})
    (assoc v ::x 2)
    (is (= reading @ran) "a write of x runs again every check that reads it, and no other")
    (reset! ran #{})
    (assoc v ::y 2)
    (is (= #{} @ran) "a write of a field no check reads runs none")))

(defmacro ^:private mkfn "Expands to a fn of no arguments." [] '(fn* [] 1))

(deftest a-macro-call-s-metadata-stays-off-its-expansion
  (is (true? (let [z 1] (nil? (meta ^{:a z} (mkfn))))) "as plain Clojure has it")
  (is (nil? (refusal '(rec/defproven MacroMeta [x] rec/checks (c [] (let [z 1] (nil? (meta ^{:a z} (mkfn)))))))))
  (is (nil? (refusal '(rec/defproven MacroMetaField [x] rec/checks (c [] (nil? (meta ^{:a x} (mkfn))))))))
  (is (= #{} (rec/checks-of (rec/reading :recognitum.body-test/x) (ns-resolve 'recognitum.body-test 'MacroMetaField)))
      "metadata the compiler drops is no read"))

(defmacro ^:private hidden-read
  "Expands to a vector whose metadata holds the read form for `k`."
  [k]
  (with-meta [1] {:m (list '&field k)}))

(deftest a-reserved-name-in-metadata-is-refused
  (doseq [body ['(= 1 (count ^{:m (&field ::x)} [1]))
                '(= 1 (count (hidden-read ::x)))
                '(do (def ^{:m &fetch} position-def 1) true)
                '(let [^{:m &extant?} a 1] (= 1 a))
                '((fn [^{:m &fetch-each} a] (= 1 a)) 1)]]
    (is (re-find #"is a name the check engine reserves for its reads"
                 (str (refusal (list 'rec/defproven 'ReservedMeta '[x] 'rec/checks (list 'c [] body)))))
        (pr-str body))))

(deftest a-field-inside-a-deftype-method-is-refused
  (doseq [[body msg]
          [['(do (deftype InMethod [q] Pair (pair [_ a] x)) true)
            #"x is used inside a method of deftype InMethod, which sees only InMethod's own fields and its params, not the fields of InDeftype\. Pass x to InMethod as a field, or use reify"]
           ['(do (deftype InMethod [q] Pair (pair [_ a] (rec/extant? x))) true)
            #"x is used inside a method of deftype InMethod"]
           ['(do (deftype InMethod [q] Pair (pair [_ a] (fn [] x))) true)
            #"x is used inside a method of deftype InMethod"]
           ['(let [b 1] (deftype InMethod [q] Pair (pair [_ a] b)) true)
            #"Unable to resolve symbol: b in this context"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'InDeftype '[x] 'rec/checks (list 'c [] body)))))
        (pr-str body)))
  (is (nil? (refusal '(rec/defproven InDeftype [x] rec/checks
                        (c [] (let [t (do (deftype Passed [q] Pair (pair [_ a] [a q])) (recognitum.body_test.Passed. x))]
                                (= [0 x] (.pair ^Pair t 0)))))))
      "a field passed in is the deftype's own"))

(deftest a-definition-whose-check-holds-a-deftype-is-evaluated-again
  (let [form '(rec/defproven Again [x] rec/checks (c [] (do (deftype AgainLocal [z]) (some? (recognitum.body_test.AgainLocal. x)))))]
    (dotimes [_ 3] (is (nil? (refusal form))))))

(deftest a-read-the-check-cannot-wait-on-is-refused
  (doseq [[body msg]
          [['(do (locking on-file (rec/fetch on-file n)) true)
            #"rec/fetch inside locking: a read may wait, and would hold the lock"]
           ['(.call ^java.util.concurrent.Callable (reify java.util.concurrent.Callable (call [_] (rec/fetch on-file n))))
            #"rec/fetch inside a fn the check body builds"]
           ['(do (deftype FetchInMethod [q] Pair (pair [_ a] (rec/fetch on-file a))) true)
            #"rec/fetch inside a fn the check body builds"]]]
    (is (re-find msg (str (refusal (list 'rec/defproven 'Waiting '[n] 'rec/checks (list 'c [] body)))))
        (pr-str body))))

(rec/defproven Locals [n]
  rec/checks
  (param-tagged [] (= 'CharSequence ((fn [^CharSequence p] (local-tag p)) "p")))
  (rest-param-tagged [] (= 'clojure.lang.ISeq ((fn [& r] (local-tag r)))))
  (catch-tagged [] (= 'Exception (try (throw (Exception.)) (catch Exception e (local-tag e)))))
  (two-letfns [] (letfn [(f [] n) (g [] (f))] (= n (g))))
  (fn-meta [] (= 1 (:k (meta ^{:k 1} (fn [] n)))))
  (local-source [] (let [s on-file] (rec/fetch s n))))

(deftest a-body-s-locals-keep-their-tags-names-and-metadata
  (is (instance? Locals (map->Locals {::n 1})))
  (is (= #{::two-letfns ::fn-meta ::local-source} (rec/checks-of (rec/reading ::n) Locals)))
  (is (= #{} (rec/checks-of (rec/fetching on-file) Locals)) "a local names no source of the type"))

(rec/defchecks shaped [m k]
  rec/checks
  (has-k [] (= 1 (:k m) (:k k))))

(rec/defproven Shaped [a]
  shaped
  (m [] (meta ^{:k 1} (fn [] a)))
  (k [] {:k a}))

(deftest an-input-clause-keeps-metadata-and-map-literals
  (is (instance? Shaped (map->Shaped {::a 1})))
  (is (= [::has-k] (keys (:failed (err #(map->Shaped {::a 2})))))))
