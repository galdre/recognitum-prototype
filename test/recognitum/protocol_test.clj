(ns recognitum.protocol-test
  "A type's own protocols and interfaces, written after its checks sections: what the methods
  see, how a value implements them, and the forms refused."
  (:require
   [clojure.main]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk]
   [recognitum.core :as rec]))

(defprotocol Describe
  (describe [x])
  (describe-with [x prefix]))

(defprotocol Measure
  (weigh [x] [x unit])
  (count-down [x n]))

(definterface Labelled
  (label [])
  (label [^String prefix])
  (label [^Long n]))

(rec/defproven Crate [label weight ^:optional owner]
  rec/checks
  (label [] (string? label))
  (weight [] (pos-int? weight))
  Describe
  (describe [_] (if owner (str label " of " owner) label))
  (describe-with [this prefix] (str prefix (describe this)))
  Measure
  (weigh [_] weight)
  (weigh [this unit] (if (= unit :g) (* 1000 (weigh this)) weight))
  (count-down [this n] (if (pos? n) (recur (dec n)) [n (instance? Crate this)])))

(def crate (map->Crate {::label "tea" ::weight 2}))

(deftest the-methods-see-the-fields-and-the-value
  (is (= "tea" (describe crate)) "an absent optional field reads as nil")
  (is (= "a tea" (describe-with crate "a ")))
  (is (= "tea of Ann" (describe (assoc crate ::owner "Ann"))) "a written value implements it too")
  (testing "recur goes to the method's head, without the value"
    (is (= [0 true] (count-down crate 3)))))

(deftest a-type-implements-two-protocols-and-a-method-of-two-arities
  (is (satisfies? Describe crate))
  (is (satisfies? Measure crate))
  (is (= 2 (weigh crate)))
  (is (= 2000 (weigh crate :g))))

(deftest a-value-is-an-instance-of-each-protocol-s-interface
  (is (instance? recognitum.protocol_test.Describe crate))
  (is (= "tea" (.describe ^recognitum.protocol_test.Describe crate)) "a direct call of the interface's method")
  (is (not (instance? recognitum.protocol_test.Describe (rec/draft crate)))))

(rec/defproven Tin [label ^:optional owner]
  rec/checks
  (label [] (string? label))
  Labelled
  (label [_] label)
  (label [_ ^String prefix] (str prefix label))
  (label [this ^Long n] [n (instance? Tin this)])
  Comparable
  (compareTo [_ other] (compare label (::label other)))
  Object
  (toString [_] (str "Tin " label)))

(rec/defproven Tin2 [label]
  rec/checks
  (label [] (string? label))
  Labelled
  (label [_ ^"java.lang.String" prefix] (str prefix label)))

(deftest interfaces-and-to-string
  (let [t (->Tin "tea" nil)]
    (is (instance? Labelled t))
    (is (= "tea" (.label ^Labelled t)))
    (is (= "a tea" (.label ^Labelled t "a ")) "overloads of one arity are told apart by the hints")
    (is (= "a tea" (.label ^Labelled (->Tin2 "tea") "a ")) "a hint may name the class by a string")
    (is (= [3 true] (.label ^Labelled t (Long/valueOf 3))) "Tin names the value's class")
    (is (neg? (compare t (->Tin "tin" nil))))
    (is (= "Tin tea" (str t)))
    (is (= "#recognitum.protocol-test/Tin #:recognitum.protocol-test{:label \"tea\", :owner nil}" (pr-str t))
        "printing shows the data, not toString")
    (is (= "Tin tea" (str (assoc t ::owner "Ann"))) "a written value implements them too")
    (is (not (instance? Labelled (rec/draft t))))))

(deftest a-draft-does-not-implement-them
  (is (not (satisfies? Describe (rec/draft crate))))
  (is (not (satisfies? Describe (rec/draft Crate))))
  (is (= "tea" (describe (rec/seal (rec/draft crate))))))

(rec/defchecks named [n]
  rec/checks
  (non-blank [] (and (string? n) (not= "" n))))

(rec/defproven Box [title]
  named
  (n [] title)
  Describe
  (describe [_] title)
  (describe-with [_ prefix] (str prefix title)))

(deftest a-protocol-after-a-bundle-binding
  (is (= "Box: x" (describe-with (->Box "x") "Box: ")))
  (is (thrown? clojure.lang.ExceptionInfo (->Box ""))))

(declare in-fresh-ns some-message reached)

(defn- refusal-message [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest a-redefined-type-implements-its-protocols
  (let [ns' (create-ns (gensym "recognitum.protocol-test.redefined"))
        define #(binding [*ns* ns']
                  (refer-clojure)
                  (eval `(rec/defproven ~'R [~'a] rec/checks (~'a [] (int? ~'a))
                           Describe (~'describe [~'_] (str "R" ~'a)) (~'describe-with [~'_ ~'p] ~'p))))
        k (keyword (str ns') "a")
        make #((ns-resolve ns' 'map->R) {k %})
        old (do (define) (make 1))]
    (define)
    (let [new (make 2)
          way-back "(rec/seal (rec/draft v))"]
      (is (not= (class old) (class new)))
      (is (= "R2" (describe new)))
      (is (not (rec/sealed? old)))
      (testing "a value of the earlier definition refuses its methods and every write, naming the way back"
        (doseq [f [#(describe old) #(describe-with old "p") #(assoc old k 3) #(dissoc old k)
                   #(conj old [k 3]) #(merge old {k 3}) #(into old {k 3})]]
          (let [msg (refusal-message f)]
            (is (and msg (.contains ^String msg (str ns' "/R")) (.contains ^String msg way-back)) msg))))
      (testing "toString and a protocol extended from outside answer"
        (is (string? (str old))))
      (testing "the way back proves it under the current definition"
        (let [again (rec/seal (rec/draft old))]
          (is (rec/sealed? again))
          (is (= (class new) (class again)))
          (is (= "R1" (describe again))))))))

(deftest a-failed-redefinition-restores-the-earlier-methods
  (let [[n [_ refused]]
        (in-fresh-ns '(recognitum.core/defproven S [a]
                        recognitum.core/checks (a [] (int? a))
                        recognitum.protocol-test/Describe (describe [_] [:first a]) (describe-with [_ p] p))
                     '(recognitum.core/defproven S [a]
                        recognitum.core/checks (a [] (int? a))
                        recognitum.protocol-test/Describe (describe [_] nothing-here) (describe-with [_ p] p)))
        v ((ns-resolve n 'map->S) {(keyword (str n) "a") 1})]
    (is (some-message refused "Unable to resolve symbol: nothing-here"))
    (is (rec/sealed? v))
    (is (= [:first 1] (describe v)))))

(defn- refusal [form]
  (try (binding [*ns* (the-ns 'recognitum.protocol-test)] (eval form))
       nil
       (catch clojure.lang.Compiler$CompilerException e (:message (ex-data (ex-cause e))))))

(def ^:private first-checks
  ": the checks sections, rec/checks and the bundle bindings, come before the protocols")

(deftest a-checks-section-after-a-protocol-is-refused
  (is (= (str "named follows the protocol Describe" first-checks)
         (refusal '(recognitum.core/defproven B1 [title]
                     Describe (describe [_] title)
                     named (n [] title)))))
  (is (= (str "rec/checks follows the protocol Describe" first-checks)
         (refusal '(recognitum.core/defproven B2 [title]
                     Describe (describe [_] title)
                     rec/checks (title [] (string? title)))))))

(deftest interface-methods-are-refused-where-the-value-is-a-map
  (doseq [[msg form] (partition 2 [
   "method equals of Object: a value's equality and hash are its map's, so a type gives neither equals nor hashCode"
   '(recognitum.core/defproven I1 [title] rec/checks (title [] (string? title)) Object (equals [_ o] true))
   "method hashCode of Object: a value's equality and hash are its map's, so a type gives neither equals nor hashCode"
   '(recognitum.core/defproven I2 [title] rec/checks (title [] (string? title)) Object (hashCode [_] 0))
   "method notify of Object: of Object's methods, a type gives only toString"
   '(recognitum.core/defproven I3 [title] rec/checks (title [] (string? title)) Object (notify [_]))
   "Labelled declares no method shout. Its methods: label"
   '(recognitum.core/defproven I4 [title] rec/checks (title [] (string? title)) Labelled (shout [_]))
   "method label of Labelled: 2 methods take 1 parameter after the value; hint the parameters' types to choose one: label(java.lang.String) label(java.lang.Long)"
   '(recognitum.core/defproven I5 [title] rec/checks (title [] (string? title)) Labelled (label [_ x] x))
   "method label of Labelled takes 1 or 2 parameters, the value first, not 3"
   '(recognitum.core/defproven I6 [title] rec/checks (title [] (string? title)) Labelled (label [_ x y] x))
   "no method label of Labelled takes the hinted types: label(java.lang.String) label(java.lang.Long)"
   '(recognitum.core/defproven I7 [title] rec/checks (title [] (string? title)) Labelled (label [_ ^Integer x] x))
   "String is a class, not a protocol or an interface: of the classes, a defproven type implements interfaces and Object's toString"
   '(recognitum.core/defproven I8 [title] rec/checks (title [] (string? title)) String (length [_] 0))
   "interface Labelled is listed twice"
   '(recognitum.core/defproven I9 [title] rec/checks (title [] (string? title)) Labelled (label [_] title) Labelled)
   "method label of Labelled is written twice with 1 parameter"
   '(recognitum.core/defproven I10 [title] rec/checks (title [] (string? title)) Labelled (label [_] title) (label [this] 1))
   "method describe of Describe hints B3 on this: B3 has no class while the methods compile. In a method body B3 is the value's class, (class this), and takes no hint"
   '(recognitum.core/defproven B3 [title] rec/checks (title [] (string? title)) Describe (describe [^B3 this] title))
   "method describe of Describe hints B4 on this: B4 has no class while the methods compile. In a method body B4 is the value's class, (class this), and takes no hint"
   '(recognitum.core/defproven B4 [title] rec/checks (title [] (string? title)) Describe (describe [^"B4" this] title))
   "a method of Describe is (name [this params...] body...): (describe this)"
   '(recognitum.core/defproven B5 [title] rec/checks (title [] (string? title)) Describe (describe this))
   "the hint ^Nope on parameter x of method label names no class"
   '(recognitum.core/defproven B6 [title] rec/checks (title [] (string? title)) Labelled (label [_ ^Nope x] x))])]
    (is (= msg (refusal form)))))

(deftest a-refusal-of-the-proof-is-made-at-expansion
  (testing "an interface that drafts implement"
    (let [[n [drafts]]
          (in-fresh-ns '(recognitum.core/defproven D1 [a]
                          recognitum.core/checks (a [] (int? a))
                          java.util.function.Predicate (test [_ x] true)))]
      (is (some-message drafts "Syntax error macroexpanding"))
      (is (some-message drafts (str n "/D1: refused definition: a draft implements this interface")))
      (is (some-message drafts "java.util.function.Predicate"))
      (is (= #{} (reached n 'D1)))))
  (testing "interfaces whose default bodies clash"
    (let [[n [clash]]
          (in-fresh-ns '(recognitum.core/defproven D6 [a]
                          recognitum.core/checks (a [] (int? a))
                          java.util.Iterator (hasNext [_] false) (next [_] nil)
                          java.util.Spliterator (tryAdvance [_ f] false) (trySplit [_] nil)
                          (estimateSize [_] 0) (characteristics [_] 0)))]
      (is (some-message clash "Syntax error macroexpanding"))
      (is (some-message clash (str "the interfaces java.util.Iterator and java.util.Spliterator give default"
                                   " bodies of forEachRemaining")))
      (is (= #{} (reached n 'D6)))))
  (testing "a method of a listed interface that a value has as a map"
    (let [[n [counted]]
          (in-fresh-ns '(recognitum.core/defproven D7 [a]
                          recognitum.core/checks (a [] (int? a))
                          clojure.lang.Counted (count [_] 7)))]
      (is (some-message counted (str "method count of clojure.lang.Counted: as a map, a value of D7 already has a"
                                     " method count of 0 parameters, so the type cannot give it: leave it out,"
                                     " and the map's method answers")))
      (is (= #{} (reached n 'D7))))))

(defn- outranked-at-definition
  "In a new namespace that defines the interface `Shown` and evaluates `before`, expands `form`;
  then gives `Shown` a print entry that print-method prefers to the map's, so the proof refuses
  the definition when it is evaluated, and evaluates it. Returns the namespace, the value of
  `before`, and the messages of the refusal."
  [before form]
  (let [[n [_ before]] (in-fresh-ns '(definterface Shown (shown [])) before)
        expanded (binding [*ns* n] (macroexpand form))
        shown (Class/forName (str (munge (str n)) ".Shown"))]
    (.addMethod ^clojure.lang.MultiFn print-method shown (fn [_ ^java.io.Writer w] (.write w "shown")))
    (try
      (prefer-method print-method shown clojure.lang.IPersistentMap)
      [n before (binding [*ns* n]
                  (try (eval expanded) nil
                       (catch Throwable e (mapv ex-message (take-while some? (iterate ex-cause e))))))]
      (finally (remove-method print-method shown)))))

(deftest a-refusal-of-the-proof-when-the-type-is-defined-leaves-nothing
  (let [[n _ refused] (outranked-at-definition
                       nil
                       '(recognitum.core/defproven D8 [a] recognitum.core/checks (a [] (int? a)) Shown (shown [_] a)))]
    (is (some-message refused "print-method prefers this interface's entry to the map's"))
    (is (some-message refused "Nothing is defined under D8"))
    (is (= #{} (reached n 'D8)))))

(deftest a-protocol-method-a-map-has-is-refused-at-expansion
  (testing "a protocol method with the name and arity of a method a value has as a map, as defrecord refuses it"
    (let [[n [_ size value-at]]
          (in-fresh-ns '(defprotocol Sized (size [x] [x y]) (valAt [x k]))
                       '(recognitum.core/defproven D2 [a]
                          recognitum.core/checks (a [] (int? a))
                          Sized (size [_] 1))
                       '(recognitum.core/defproven D3 [a]
                          recognitum.core/checks (a [] (int? a))
                          Sized (valAt [_ k] k)))]
      (is (some-message size (str n "/D2, line ")))
      (is (some-message size (str "method size of Sized: as a map, a value of D2 already has a method size of"
                                  " 0 parameters, so the type cannot give it: rename it, or extend Sized to D2"
                                  " with extend-protocol after the definition (a method extended from outside"
                                  " is not guarded: it answers on values of a displaced definition)")))
      (is (some-message size "Syntax error macroexpanding") "refused when defproven expands")
      (is (some-message value-at "method valAt of Sized: as a map, a value of D3 already has a method valAt of 1 parameter, so"))
      (is (= #{} (reached n 'D2)))
      (is (= #{} (reached n 'D3)))))
  (testing "the same name at an arity no map method has defines"
    (let [[_ [_ cls answers]]
          (in-fresh-ns '(defprotocol Sized (size [x] [x y]))
                       '(recognitum.core/defproven D4 [a]
                          recognitum.core/checks (a [] (int? a))
                          Sized (size [_ y] [a y]))
                       '(size (map->D4 {(keyword (str *ns*) "a") 1}) 2))]
      (is (class? cls))
      (is (= [1 2] answers))))
  (testing "the remedy: the protocol extended to the type after the definition"
    (let [[_ [_ cls answers]]
          (in-fresh-ns '(defprotocol Sized (size [x]))
                       '(recognitum.core/defproven D5 [a]
                          recognitum.core/checks (a [] (int? a)))
                       '(do (extend-protocol Sized D5 (size [v] [:outside (count v)]))
                            (size (map->D5 {(keyword (str *ns*) "a") 1}))))]
      (is (class? cls))
      (is (= [:outside 1] answers)))))

(deftest code-that-holds-a-refused-definition-fails-to-compile
  (doseq [[tname form msg]
          [['C1 '(recognitum.core/defproven C1 [a]
                   recognitum.core/checks (a [] (int? a))
                   Sized (size [_] 1))
            "method size of Sized: as a map, a value of C1 already has a method size of 0 parameters"]
           ['C2 '(recognitum.core/defproven C2 [a]
                   recognitum.core/checks (a [] (int? a))
                   clojure.lang.Counted (count [_] 7))
            "method count of clojure.lang.Counted: as a map, a value of C2 already has a method count of 0 parameters"]
           ['C3 '(recognitum.core/defproven C3 [a]
                   recognitum.core/checks (a [] (int? a))
                   java.util.Iterator (hasNext [_] false) (next [_] nil)
                   java.util.Spliterator (tryAdvance [_ f] false) (trySplit [_] nil)
                   (estimateSize [_] 0) (characteristics [_] 0))
            "the interfaces java.util.Iterator and java.util.Spliterator give default bodies of forEachRemaining"]]]
    (testing (str "a function whose body defines " tname ", compiled and never called")
      (let [[n [_ compiled]] (in-fresh-ns '(defprotocol Sized (size [x])) (list 'fn [] form))]
        (is (some-message compiled "Syntax error macroexpanding") "compiling the function throws")
        (is (some-message compiled msg))
        (is (= #{} (reached n tname)) "the compiler interned none of the definition's vars")))))

(deftest a-malformed-definition-is-refused
  (let [[n [no-fields twice]]
        (in-fresh-ns '(recognitum.core/defproven E1 [] recognitum.core/checks)
                     '(recognitum.core/defproven E2 [a]
                        recognitum.core/checks (a [] (int? a))
                        recognitum.core/checks (b [] true)))]
    (is (some-message no-fields "fields are a vector of plain symbols"))
    (is (some-message twice "rec/checks appears twice"))
    (is (= #{} (reached n 'E1)))
    (is (= #{} (reached n 'E2)))))

(deftest a-name-that-cannot-take-the-class-leaves-nothing
  (let [[n [_ refused]] (in-fresh-ns '(def W 1)
                                      '(recognitum.core/defproven W [a]
                                         recognitum.core/checks (a [] (int? a))))]
    (is (some-message refused (str "W already names #'" n "/W in " n ", so the type's class cannot take the name")))
    (is (some-message refused "Nothing is defined under W"))
    (is (= 1 @(ns-resolve n 'W)))
    (is (nil? (ns-resolve n 'map->W)))
    (is (nil? (ns-resolve n '->W)))
    (is (thrown? ClassNotFoundException (clojure.lang.RT/classForName (str (munge (str n)) ".W")))
        "the refusal comes before the type is defined")))

(deftest a-name-that-names-a-class-of-another-name-is-refused-before-the-type-is-defined
  (let [[n [refused]] (in-fresh-ns '(recognitum.core/defproven Integer [a]
                                      recognitum.core/checks (a [] (int? a))))]
    (is (some-message refused (str "Integer already names java.lang.Integer in " n
                                   ", so the type's class cannot take the name. Nothing is defined under Integer")))
    (is (identical? Integer (ns-resolve n 'Integer)))
    (is (thrown? ClassNotFoundException (clojure.lang.RT/classForName (str (munge (str n)) ".Integer"))))))

(deftest a-name-that-imports-another-namespace-s-type-revives-nothing
  (testing "b imports a's T, a redefines T, then b defines T: the import fails, and a's earlier T stays displaced"
    (let [def-t '(recognitum.core/defproven T [x] recognitum.core/checks (x [] (int? x)))
          [a] (in-fresh-ns def-t)
          x (keyword (str a) "x")
          make-v1 @(ns-resolve a 'map->T)
          v1 (make-v1 {x 1})
          c1 (class v1)
          [b] (in-fresh-ns (list 'import (symbol (str (munge (str a)) ".T"))))
          _ (binding [*ns* a] (eval def-t))
          refused (binding [*ns* b]
                    (try (eval '(recognitum.core/defproven T [y] recognitum.core/checks (y [] (int? y)))) nil
                         (catch Throwable e (mapv ex-message (take-while some? (iterate ex-cause e))))))]
      (is (some-message refused (str "T already names " (munge (str a)) ".T in " b)))
      (is (some-message refused "Nothing is defined under T") "b had no definition of T to keep")
      (is (identical? c1 (ns-resolve b 'T)) "b still names the class it imported")
      (is (not (rec/sealed? v1)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"was redefined" (assoc v1 x 2)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            (re-pattern (str "\\Q" a "/T was redefined: this constructor is of an earlier"
                                             " definition and makes nothing. Call the current " a "/map->T\\E"))
                            (make-v1 {x 3}))
          "a constructor held from before a's redefinition refuses"))))

(deftest a-constructor-held-across-a-redefinition-refuses
  (let [def-t '(recognitum.core/defproven T [x ^:optional y] recognitum.core/checks (x [] (int? x)))
        [n [_ v]] (in-fresh-ns def-t '(map->T {(keyword (str *ns*) "x") 1}))
        x (keyword (str n) "x")
        map-v1 @(ns-resolve n 'map->T)
        pos-v1 @(ns-resolve n '->T)
        before (pos-v1 2 nil)
        _ (binding [*ns* n] (eval def-t))
        refusal (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))
        tname (symbol (str n) "T")
        want [(str tname " was redefined: this constructor is of an earlier definition and makes nothing."
                   " Call the current " n "/map->T")
              {:error :recognitum/displaced-constructor :type tname}]]
    (is (= want (refusal #(map-v1 {x 1}))) "map->T held from before")
    (is (= want (refusal #(pos-v1 1 nil))) "->T held from before")
    (is (= want (refusal #(map-v1 {x :not-an-int}))) "refused before any check runs")
    (testing "values made before the redefinition are displaced, as before"
      (is (not (rec/sealed? v)))
      (is (not (rec/sealed? before)))
      (is (rec/sealed? (rec/seal (rec/draft v)))))
    (testing "the current constructors seal"
      (is (rec/sealed? ((ns-resolve n 'map->T) {x 1})))
      (is (rec/sealed? ((ns-resolve n '->T) 1 nil))))))

(deftest methods-are-refused-where-defrecord-refuses-them
  (doseq [[msg form] (partition 2 [
   "Describe has no method summary. Its methods: describe describe-with"
   '(recognitum.core/defproven M1 [a] rec/checks (a [] true) Describe (summary [_] a))
   "method describe of Describe takes 1 parameter, the value first, not 2"
   '(recognitum.core/defproven M2 [a] rec/checks (a [] true) Describe (describe [_ x] a))
   "method weigh of Measure takes 1 or 2 parameters, the value first, not 3"
   '(recognitum.core/defproven M3 [a] rec/checks (a [] true) Measure (weigh [_ x y] a))
   "method describe of Describe is written twice with 1 parameter"
   '(recognitum.core/defproven M4 [a] rec/checks (a [] true) Describe (describe [_] a) (describe [this] a))
   "method describe of Describe needs a first parameter, for the value"
   '(recognitum.core/defproven M5 [a] rec/checks (a [] true) Describe (describe [] a))
   "the parameters of method describe-with of Describe are plain symbols: [_ {:keys [p]}]"
   '(recognitum.core/defproven M6 [a] rec/checks (a [] true) Describe (describe-with [_ {:keys [p]}] p))
   "protocol Describe is implemented twice"
   '(recognitum.core/defproven M7 [a] rec/checks (a [] true) Describe (describe [_] a) Describe)
   "Nope names no bundle var, no protocol and no interface"
   '(recognitum.core/defproven M8 [a] rec/checks (a [] true) Describe (describe [_] a) Nope)
   "str is not a bundle, a protocol or an interface"
   '(recognitum.core/defproven M9 [a] rec/checks (a [] true) str (describe [_] a))])]
    (is (= msg (refusal form)))))

(defn- in-fresh-ns
  "Evaluates each form in a new namespace that refers clojure.core. Returns the namespace and,
  for each form, its value or the messages of what it threw."
  [& forms]
  (let [n (create-ns (gensym "recognitum.protocol-test.failed"))]
    (binding [*ns* n]
      (refer-clojure)
      [n (mapv #(try (eval %)
                     (catch Throwable e (mapv ex-message (take-while some? (iterate ex-cause e)))))
               forms)])))

(defn- reached [n tname]
  (set (keep #(ns-resolve n %) [tname (symbol (str "map->" tname)) (symbol (str "->" tname))])))

(defn- some-message [answer text]
  (and (vector? answer) (some #(and % (.contains ^String % text)) answer)))

(deftest a-method-that-does-not-compile-leaves-nothing
  (let [bad '(recognitum.core/defproven F [a]
               recognitum.core/checks (a [] (int? a))
               recognitum.protocol-test/Describe
               (describe [this] (instance? F this))
               (describe-with [_ p] nothing-here))
        good '(recognitum.core/defproven F [a]
                recognitum.core/checks (a [] (int? a))
                recognitum.protocol-test/Describe
                (describe [this] (instance? F this))
                (describe-with [_ p] p))
        [n [refused defined]] (in-fresh-ns bad '(map->F {}))]
    (is (some-message refused "Unable to resolve symbol: nothing-here"))
    (is (some-message defined "Unable to resolve symbol: map->F") "the constructor was never defined")
    (is (= #{} (reached n 'F)))
    (testing "a later definition under the name is a fresh start"
      (binding [*ns* n] (eval good))
      (let [v ((ns-resolve n 'map->F) {(keyword (str n) "a") 1})]
        (is (rec/sealed? v))
        (is (true? (describe v)))
        (is (= 3 (count (reached n 'F))))))))

(deftest a-redefinition-whose-method-does-not-compile-keeps-the-earlier-one
  (let [def-g (fn [body] (list 'recognitum.core/defproven 'G '[a]
                               'recognitum.core/checks '(a [] (int? a))
                               'recognitum.protocol-test/Describe (list 'describe '[_] body)
                               '(describe-with [_ p] p)))
        [n [cls]] (in-fresh-ns (def-g '[:first a]))
        _ (is (class? cls))
        make #((ns-resolve n 'map->G) {(keyword (str n) "a") %})
        old (make 1)
        refused (binding [*ns* n]
                  (try (eval (def-g 'nothing-here)) nil
                       (catch Throwable e (mapv ex-message (take-while some? (iterate ex-cause e))))))]
    (is (some-message refused "Unable to resolve symbol: nothing-here"))
    (is (identical? cls (ns-resolve n 'G)))
    (is (rec/sealed? old) "values of the earlier definition stay sealed")
    (is (= [:first 2] (describe (make 2))))
    (is (rec/sealed? (assoc old (keyword (str n) "a") 3)))))

(deftest a-check-that-does-not-compile-leaves-nothing
  (let [[n [plain with-protocol]]
        (in-fresh-ns '(recognitum.core/defproven H1 [a]
                        recognitum.core/checks (a [] (clojure.string/no-such-fn a)))
                     '(recognitum.core/defproven H2 [a]
                        recognitum.core/checks (a [] (clojure.string/no-such-fn a))
                        recognitum.protocol-test/Describe (describe [_] a)))]
    (is (some-message plain "No such var: clojure.string/no-such-fn"))
    (is (some-message with-protocol "No such var: clojure.string/no-such-fn"))
    (is (= #{} (reached n 'H1)))
    (is (= #{} (reached n 'H2)))))

(deftest a-refused-protocol-section-leaves-nothing
  (let [[n [refused]] (in-fresh-ns '(recognitum.core/defproven K [a]
                                      recognitum.protocol-test/Describe (describe [_] a)
                                      recognitum.core/checks (a [] (int? a))))]
    (is (some-message refused "checks follows the protocol"))
    (is (= #{} (reached n 'K)))))

(def ^:private expansions (atom 0))

(defmacro second-expansion-throws
  "Expands to :once the first time after `expansions` is reset, and throws every later time."
  []
  (if (= 1 (swap! expansions inc)) :once (throw (ex-info "expanded again" {}))))

(deftest a-method-compiles-once
  (reset! expansions 0)
  (let [[n [cls]] (in-fresh-ns '(recognitum.core/defproven E [a]
                                  recognitum.core/checks (a [] (int? a))
                                  recognitum.protocol-test/Describe
                                  (describe [_] (recognitum.protocol-test/second-expansion-throws))
                                  (describe-with [_ p] p)))]
    (is (class? cls))
    (is (= :once (describe ((ns-resolve n 'map->E) {(keyword (str n) "a") 1}))))
    (is (= 1 @expansions))))

(deftest a-definition-inside-another-form-defines
  (let [[n [cls]] (in-fresh-ns '(when true
                                  (recognitum.core/defproven L [a]
                                    recognitum.core/checks (a [] (int? a))
                                    recognitum.protocol-test/Describe (describe [this] [a (instance? L this)]))))]
    (is (class? cls))
    (is (= [1 true] (describe ((ns-resolve n 'map->L) {(keyword (str n) "a") 1}))))
    (is (= 3 (count (reached n 'L))))))

(deftest a-definition-expanded-by-macroexpand-all-defines
  (let [[n [cls]] (in-fresh-ns '(clojure.core/eval
                                 (clojure.walk/macroexpand-all
                                  '(recognitum.core/defproven M [a]
                                     recognitum.core/checks (a [] (int? a))
                                     recognitum.protocol-test/Describe (describe [_] [:m a])))))]
    (is (class? cls))
    (is (= [:m 1] (describe ((ns-resolve n 'map->M) {(keyword (str n) "a") 1}))))))

(deftest a-definition-the-proof-refuses-keeps-the-earlier-definition
  (let [def-u (fn [& more] (list* 'recognitum.core/defproven 'U '[a]
                                  'recognitum.core/checks '(a [] (int? a))
                                  'recognitum.protocol-test/Describe '(describe [_] [:first a])
                                  '(describe-with [_ p] p)
                                  more))
        [n old refused] (outranked-at-definition (list 'do (def-u) '(map->U {(keyword (str *ns*) "a") 1}))
                                                 (def-u 'Shown '(shown [_] a)))
        make #((ns-resolve n 'map->U) {(keyword (str n) "a") %})
        cls (class old)]
    (is (rec/sealed? old))
    (is (some-message refused "print-method prefers this interface's entry to the map's") "refused when the type is defined")
    (is (some-message refused "U keeps its earlier definition") "the refusal, not a second throw from the undo")
    (is (identical? cls (ns-resolve n 'U)))
    (is (= [:first 1] (describe old)))
    (is (rec/sealed? (make 2)))
    (is (rec/sealed? (assoc old (keyword (str n) "a") 3)))))

(deftest the-repl-prints-a-refusal-as-defproven-s-with-its-message
  (let [n (create-ns (gensym "recognitum.protocol-test.printed"))
        printed (binding [*ns* n]
                  (refer-clojure)
                  (try (eval '(recognitum.core/defproven V [a]
                                Object (equals [_ o] true)))
                       nil
                       (catch Throwable e
                         (clojure.main/ex-str (clojure.main/ex-triage (Throwable->map e))))))
        [top message] (str/split-lines (str printed))]
    (is (re-find #"^Syntax error macroexpanding recognitum\.core/defproven at " (str top)))
    (is (re-matches (re-pattern (str "\\Q" n "/V, line \\E\\d+: method equals of Object: a value's equality and"
                                     " hash are its map's, so a type gives neither equals nor hashCode"))
                    (str message)))))
