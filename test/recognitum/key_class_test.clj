(ns recognitum.key-class-test
  "The key-class strategy's definitions: the interfaces a value implements, the functions its
  methods call, and the definitions it refuses."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing]]
   [recognitum.proof :as proof]
   [recognitum.proof.key-class :as key-class]))

(definterface Prims
  (^int add [^int a ^long b ^double c ^boolean d ^char e])
  (^void note [x])
  (^long wide [a b c d e f g h i j k l m n o p q r s t u])
  (twenty [a b c d e f g h i j k l m n o p q r s t]))

(definterface Plain (show []))

(definterface ReturnsString (^String same []))
(definterface ReturnsLong (^Long same []))

(defprotocol Named (label [v]))

(def ^:private noted (atom nil))

(def ^:private definition
  {:interfaces [Prims Plain recognitum.key_class_test.Named clojure.lang.IDeref Object]
   :methods    {'[add 5]      (fn [_ a b c d e] (reset! noted [a b c d e]) 42)
                '[note 1]     (fn [_ x] (reset! noted x) :ignored)
                '[wide 21]    (fn [& xs] (count xs))
                '[twenty 20]  (fn [v & xs] [v xs])
                '[show 0]     (fn [v] [:show v])
                '[label 0]    (fn [v] (:n v))
                '[toString 0] (fn [v] (str "n=" (:n v)))}})

(defn- define [d] (proof/define-type key-class/strategy nil d))

(defn- refusal' [type-name d]
  (let [answer (proof/refusal key-class/strategy d)
        thrown (try (proof/define-type key-class/strategy type-name d) nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (= answer (ex-data thrown)) "the definition refuses exactly what refusal answers")
    (some-> thrown ex-message)))

(defn- refusal [d] (refusal' nil d))

(deftest methods-call-their-functions-with-the-value
  (let [h (define definition)
        v (proof/seal h {:n 1})]
    (is (= 42 (.add ^recognitum.key_class_test.Prims v 1 2 3.0 true \c)))
    (is (= [1 2 3.0 true \c] @noted) "primitive arguments arrive boxed")
    (is (nil? (.note ^recognitum.key_class_test.Prims v :x)))
    (is (= :x @noted))
    (is (= 22 (.wide ^recognitum.key_class_test.Prims v 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
        "the value and 21 arguments, through applyTo")
    (let [[self args] (.twenty ^recognitum.key_class_test.Prims v 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)]
      (is (identical? v self))
      (is (= (range 1 21) args) "the value and 20 arguments, through applyTo"))
    (is (identical? v (second (.show ^recognitum.key_class_test.Plain v))))
    (is (= 1 (label v)) "a protocol placed through its interface")
    (is (thrown? UnsupportedOperationException (deref v)) "a listed method with no function throws")
    (is (= "n=1" (str v)))
    (testing "drafts implement none of the interfaces"
      (is (not-any? #(instance? % (proof/open h)) [Prims Plain recognitum.key_class_test.Named])))))

(deftest printing-shows-the-map
  (let [v (proof/seal (define definition) {:n 1})]
    (is (= {:n 1} (edn/read-string (pr-str v))) "IDeref's print entry does not take over")
    (is (= {:n 1} (binding [*read-eval* true] (read-string (binding [*print-dup* true] (pr-str v))))))))

(deftest values-keep-their-definitions-methods
  (let [old (proof/seal (define definition) {:n 1})
        new (proof/seal (define (assoc-in definition [:methods '[show 0]] (fn [_] :new))) {:n 1})]
    (is (= :show (first (.show ^recognitum.key_class_test.Plain old))))
    (is (= :new (.show ^recognitum.key_class_test.Plain new)))))

(defmacro ^:private are-refused [& pairs]
  `(do ~@(for [[d msg] (partition 2 pairs)]
           `(is (some-> (refusal ~d) (.contains ~msg)) ~(pr-str d)))))

(deftest refused-definitions
  (are-refused
   {:interfaces [String]}                                          "not an interface"
   {:interfaces [Plain] :methods {'[hide 0] identity}}              "no listed interface declares"
   {:interfaces [Prims] :methods {'[add 4] identity}}               "no listed interface declares"
   {:interfaces [java.util.Map] :methods {'[get 1] identity}}       "without the definition"
   {:interfaces [Object] :methods {'[equals 1] identity}}           "equality"
   {:interfaces [Object] :methods {'[hashCode 0] identity}}         "equality"
   {:interfaces [] :methods {'[toString 0] identity}}               "no listed interface declares"
   {:interfaces [Plain] :methods {'[show 0] identity ['show []] identity}} "two signatures"
   {:interfaces [Plain] :methods {'[show 0] 7}}                     "not a function"
   {:interfaces [java.util.function.Consumer java.util.function.IntConsumer]
    :methods {'[accept 1] identity}}                                "more than one method"))

(deftest refusals-are-data-and-the-definition-throws-them
  (let [r #(dissoc (proof/refusal key-class/strategy %) :message)]
    (is (nil? (proof/refusal key-class/strategy definition)))
    (is (nil? (proof/refusal key-class/strategy nil)))
    (is (= {:kind :undeclared :method '[hide 0]} (r {:interfaces [Plain] :methods {'[hide 0] identity}})))
    (is (= {:kind :ambiguous :method '[accept 1]}
           (r {:interfaces [java.util.function.Consumer java.util.function.IntConsumer] :methods {'[accept 1] identity}})))
    (is (= {:kind :equality :method '[equals 1]} (r {:interfaces [Object] :methods {'[equals 1] identity}})))
    (is (= {:kind :equality :method '[hashCode 0]} (r {:interfaces [Object] :methods {['hashCode []] identity}})))
    (is (= {:kind :map-method :method '[get 1]} (r {:interfaces [java.util.Map] :methods {'[get 1] identity}})))
    (is (= {:kind :clash :method '[forEachRemaining 1] :interfaces [java.util.Iterator java.util.Spliterator]}
           (update (r {:interfaces [java.util.Iterator java.util.Spliterator]}) :interfaces (partial sort-by #(.getName ^Class %)))))
    (is (= {:kind :clash :method '[same 0] :interfaces [ReturnsString ReturnsLong]}
           (r {:interfaces [ReturnsString ReturnsLong]})))
    (is (= {:kind :not-an-interface :interfaces [String]} (r {:interfaces [String]})))
    (is (= {:kind :draft-interface :interfaces [java.util.function.Predicate]} (r {:interfaces [java.util.function.Predicate]})))
    (is (= {:kind :duplicate :method '[show 0]} (r {:interfaces [Plain] :methods (array-map '[show 0] identity ['show []] identity)})))
    (is (= {:kind :not-a-function :method '[show 0]} (r {:interfaces [Plain] :methods {'[show 0] 7}})))
    (is (= :malformed (:kind (r {:interfaces [Plain] :methods {'[show] identity}})))))
  (testing "the definition throws the answer, as data and message"
    (let [d {:interfaces [Plain] :methods {'[hide 0] identity}}
          e (try (define d) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= (proof/refusal key-class/strategy d) (ex-data e)))
      (is (= "refused definition: no listed interface declares: [hide 0]" (ex-message e))))))

(deftest the-class-name-comes-from-the-strategy
  (is (= "recognitum.key_class_test.Named_Thing" (proof/class-name key-class/strategy 'recognitum.key-class-test/Named-Thing)))
  (is (nil? (proof/class-name key-class/strategy nil)))
  (dotimes [_ 2]
    (let [[vc] (proof/classes (proof/define-type key-class/strategy 'recognitum.key-class-test/Named-Thing {:interfaces [Plain]}))]
      (is (= (proof/class-name key-class/strategy 'recognitum.key-class-test/Named-Thing) (.getName ^Class vc))))))

(deftest the-constructor-names-the-class-and-the-engine
  (let [[vc] (proof/classes (proof/define-type key-class/strategy 'recognitum.key-class-test/Thing))
        msg  (try (clojure.lang.Reflector/invokeConstructor vc (object-array [nil {}])) nil
                  (catch SecurityException e (.getMessage e)))]
    (is (= (str "recognitum.key_class_test.Thing's constructor takes a key only recognitum holds: "
                "a value of this type is made only through recognitum")
           msg))
    (is (not (.contains ^String msg "map->")))))

(deftest default-methods
  (let [then #(.apply (.andThen ^java.util.function.Function % (reify java.util.function.Function (apply [_ r] r))) :x)]
    (testing "a default with no function keeps its body, which calls the abstract method"
      (let [given (proof/seal (define {:interfaces [java.util.function.Function] :methods {'[apply 1] (fn [_ x] [:applied x])}}) {})
            none  (proof/seal (define {:interfaces [java.util.function.Function]}) {})]
        (is (= [:applied :x] (then given)))
        (is (thrown? UnsupportedOperationException (then none)) "an abstract method with no function throws")))
    (testing "a function replaces a default body"
      (let [v (proof/seal (define {:interfaces [java.util.function.Function]
                                   :methods {'[apply 1] (fn [_ x] x) '[andThen 1] (fn [_ g] (reify java.util.function.Function (apply [_ _] :replaced)))}})
                          {})]
        (is (= :replaced (.apply (.andThen ^java.util.function.Function v (reify java.util.function.Function (apply [_ r] r))) :x)))))
    (testing "two unrelated defaults of one method"
      (is (some-> (refusal {:interfaces [java.util.Iterator java.util.Spliterator]})
                  (.contains "give default bodies of forEachRemaining and the definition gives no function for it")))
      (let [v (proof/seal (define {:interfaces [java.util.Iterator java.util.Spliterator]
                                   :methods {'[forEachRemaining 1] (fn [_ _] nil)}})
                          {})]
        (is (nil? (.forEachRemaining ^java.util.Iterator v nil)) "a function settles which body runs")))
    (testing "the most specific declaration decides: ListIterator makes Iterator's default remove abstract"
      (doseq [ifaces [[java.util.Iterator java.util.ListIterator] [java.util.ListIterator java.util.Iterator]]]
        (let [v (proof/seal (define {:interfaces ifaces}) {})]
          (is (= "no function implements remove"
                 (try (.remove ^java.util.Iterator v) nil (catch UnsupportedOperationException e (.getMessage e))))
              (pr-str ifaces)))))))

(defn- default-interface
  "Defines an interface named `n` whose one method `m`, of descriptor `desc` returning void
  or int, has a default body."
  [^String n ^String m ^String desc]
  (let [cw (clojure.asm.ClassWriter. clojure.asm.ClassWriter/COMPUTE_FRAMES)
        t  (.replace n \. \/)]
    (.visit cw clojure.asm.Opcodes/V1_8
            (bit-or clojure.asm.Opcodes/ACC_PUBLIC clojure.asm.Opcodes/ACC_INTERFACE clojure.asm.Opcodes/ACC_ABSTRACT)
            t nil "java/lang/Object" nil)
    (doto (.visitMethod cw clojure.asm.Opcodes/ACC_PUBLIC m desc nil nil)
      (.visitCode)
      (cond-> (.endsWith desc "I") (.visitInsn clojure.asm.Opcodes/ICONST_0))
      (.visitInsn (if (.endsWith desc "I") clojure.asm.Opcodes/IRETURN clojure.asm.Opcodes/RETURN))
      (.visitMaxs 0 0)
      (.visitEnd))
    (.visitEnd cw)
    (.defineClass (clojure.lang.DynamicClassLoader.) n (.toByteArray cw) nil)))

(deftest defaults-meet-the-maps-own-methods
  (testing "a listed default and an unrelated default of the map's interfaces conflict"
    (let [i (default-interface "recognitum.key_class_test.ForEachPair" "forEach" "(Ljava/util/function/BiConsumer;)V")]
      (is (some-> (refusal {:interfaces [i]}) (.contains "java.util.Map")))))
  (testing "a method the value's class declares wins over every default"
    (let [a (default-interface "recognitum.key_class_test.CountA" "count" "()I")
          b (default-interface "recognitum.key_class_test.CountB" "count" "()I")
          v (proof/seal (define {:interfaces [a b]}) {:x 1 :y 2})]
      (is (= 2 (count v))))))

(definterface NarrowMap
  (^recognitum.key_class_test.NarrowMap without [k]))

(deftest map-methods-are-set-aside-before-return-types
  (is (some? (define {:interfaces [clojure.lang.IPersistentMap]})))
  (is (some? (define {:interfaces [recognitum.key_class_test.NarrowMap]}))))

(definterface Outranked (o []))

(deftest an-outranking-print-entry-refuses-the-definition
  (.addMethod ^clojure.lang.MultiFn print-method recognitum.key_class_test.Outranked (fn [_ ^java.io.Writer w] (.write w "outranked")))
  (try
    (prefer-method print-method recognitum.key_class_test.Outranked clojure.lang.IPersistentMap)
    (let [before [(prefers print-method) (prefers print-dup)]
          msg    (refusal {:interfaces [recognitum.key_class_test.Outranked]})]
      (is (= {:kind :clash :interfaces [recognitum.key_class_test.Outranked]}
             (dissoc (proof/refusal key-class/strategy {:interfaces [recognitum.key_class_test.Outranked]}) :message)))
      (is (= (str "refused definition: print-method prefers this interface's entry to the map's, so a value "
                  "could not print as its map: recognitum.key_class_test.Outranked")
             msg))
      (is (= before [(prefers print-method) (prefers print-dup)]) "a refusal sets no preference"))
    (finally (remove-method print-method recognitum.key_class_test.Outranked))))

(defmacro ^:private many-methods
  "An interface named `nm` with methods m0 ... m(n-1), none taking arguments."
  [nm n]
  `(definterface ~nm ~@(for [i (range n)] (list (symbol (str "m" i)) []))))

(many-methods Many 130)

(deftest a-definition-with-more-than-127-methods
  (let [v (proof/seal (define {:interfaces [Many] :methods {'[m129 0] (fn [_] :last) '[m0 0] (fn [_] :first)}}) {})]
    (is (= :last (.m129 ^recognitum.key_class_test.Many v)))
    (is (= :first (.m0 ^recognitum.key_class_test.Many v)))
    (is (= (repeat 128 UnsupportedOperationException)
           (for [i (range 1 129)]
             (try (clojure.lang.Reflector/invokeInstanceMethod v (str "m" i) (object-array 0)) nil
                  (catch Throwable t (class t)))))
        "each method with no function throws, whatever its place in the table")))

(defn- clone [x]
  (let [bo (java.io.ByteArrayOutputStream.)]
    (with-open [o (java.io.ObjectOutputStream. bo)] (.writeObject o x))
    (with-open [i (java.io.ObjectInputStream. (java.io.ByteArrayInputStream. (.toByteArray bo)))]
      (.readObject i))))

(deftest changes-of-a-value-of-a-new-type
  (let [h (define {:interfaces [java.util.Map]})
        v (proof/seal h (with-meta {:a 1 :b 2} {:m true}))]
    (testing "a change that changes nothing returns the value"
      (is (identical? v (assoc v :a 1)))
      (is (identical? v (dissoc v :z)))
      (is (identical? v (conj v [:a 1]))))
    (testing "with no write installed, a change is a plain map with the value's meta"
      (doseq [w [(assoc v :a 2) (dissoc v :a) (conj v [:c 3])]]
        (is (not (proof/proven? h w)))
        (is (instance? clojure.lang.PersistentArrayMap w))
        (is (= {:m true} (meta w)))))
    (testing "a listed interface that the map already implements keeps the map's methods"
      (is (= 1 (.get ^java.util.Map v :a)))
      (is (= 2 (.size ^java.util.Map v))))
    (testing "serialization yields the plain map"
      (let [c (clone v)]
        (is (= {:a 1 :b 2} c))
        (is (not (proof/proven? h c)))))))

(deftest a-transient-of-a-value-makes-one-checked-write
  (let [h       (define nil)
        written (atom [])
        _       (proof/install-write! h (fn [old entries] (swap! written conj entries) (proof/seal h entries)))
        v       (proof/seal h {:a 1})]
    (testing "into with more entries than an array map holds"
      (let [big (zipmap (range 20) (range 20))
            w   (into v big)]
        (is (= (merge {:a 1} big) w))
        (is (proof/proven? h w))
        (is (= 1 (count @written)))))
    (testing "dissoc! and find on the transient"
      (let [t (transient v)]
        (is (= [:a 1] (find t :a)))
        (is (nil? (find t :z)))
        (is (= {} (persistent! (dissoc! t :a))))
        (is (thrown? IllegalAccessError (assoc! t :b 2)) "a transient is spent by persistent!")))))

(deftest refused-shapes
  (are-refused
   {:interfaces [:not-a-class]}                                     "not {:interfaces"
   {:interfaces [Plain] :methods {'[show] identity}}                "a signature is"
   {:interfaces [Plain] :methods {'[show :x] identity}}             "a signature is"
   {:interfaces [Plain] :methods {['show [:x]] identity}}           "a signature is"))


(deftest refused-return-type-conflicts
  (are-refused
   {:interfaces [ReturnsString ReturnsLong]}                        "differ only in return type"))

(deftest a-definition-without-print-entries-adds-no-preferences
  (let [before (prefers print-method)]
    (define {:interfaces [Plain]})
    (is (= before (prefers print-method)))
    (is (not-any? (set (get (prefers print-method) clojure.lang.IPersistentMap))
                  [Object clojure.lang.IPersistentCollection java.util.Collection])
        "the map's entry is preferred only to entries of listed interfaces' supertypes")))

(gen-interface :name recognitum.key_class_test.SubBiConsumer :extends [java.util.function.BiConsumer])

(deftest drafts-implement-no-listed-interface
  (are-refused
   {:interfaces [java.util.function.BiConsumer]}                    "a draft implements this interface"
   {:interfaces [java.util.function.Predicate]}                     "a draft implements this interface"))

(deftest mark-hands-the-key-only-to-a-draft-of-its-type
  (let [h    (define nil)
        seen (atom [])
        spy  (reify java.util.function.BiConsumer (accept [_ k c] (swap! seen conj k)))
        sub  (proof/seal (define {:interfaces [recognitum.key_class_test.SubBiConsumer]
                                  :methods {'[accept 2] (fn [_ k c] (swap! seen conj k))}})
                         {})
        other (proof/open (define nil))]
    (doseq [x [spy sub other {} nil]]
      (is (thrown? IllegalArgumentException (proof/mark! h x :c)) (pr-str (class x))))
    (is (instance? java.util.function.BiConsumer sub))
    (is (empty? @seen) "no object other than the type's own draft is handed the key")))

(deftest a-refused-definition-leaves-no-class-under-its-name
  (let [def! #(binding [*ns* (the-ns 'recognitum.key-class-test)] (eval '(definterface Redefined (r []))))
        old  (def!)
        _    (def!)
        name "recognitum.key_class_test.Stray"]
    (is (not (identical? old (Class/forName "recognitum.key_class_test.Redefined" false (clojure.lang.DynamicClassLoader.))))
        "the interface was defined again")
    (is (some-> (refusal' 'recognitum.key-class-test/Stray {:interfaces [old]}) (.contains "now names another class")))
    (is (= {:kind :reloaded-interface :interfaces [old]} (dissoc (proof/refusal key-class/strategy {:interfaces [old]}) :message)))
    (is (nil? (try (Class/forName name false (clojure.lang.DynamicClassLoader.)) (catch ClassNotFoundException _ nil))))))

(deftype Imitation$Draft [] java.util.function.Predicate (test [_ _] true))

(deftest marked?-reads-the-marks-of-the-strategys-own-drafts
  (let [tname 'recognitum.key-class-test/Marked
        h     (proof/define-type key-class/strategy tname)
        d     (proof/open h)
        oh    (define nil)
        other (proof/open oh)
        m?    #(proof/marked? key-class/strategy %1 %2)]
    (is (false? (m? d :c)))
    (proof/mark! h d :c)
    (proof/mark! oh other :o)
    (is (true? (m? d :c)))
    (is (false? (m? d :e)))
    (is (true? (m? other :o)) "a draft of another type answers its own marks")
    (proof/define-type key-class/strategy tname)
    (is (true? (m? d :c)) "a draft of an earlier definition answers the same")
    (testing "anything that is not a draft the strategy made answers false"
      (doseq [x [(Imitation$Draft.) (reify java.util.function.Predicate (test [_ _] true))
                 (proof/seal h {:c true}) {:c true} #{:c} nil]]
        (is (false? (m? x :c)) (pr-str (class x)))))))

(deftest the-refused-method-is-named-as-the-signature-spells-it
  (let [r #(:method (proof/refusal key-class/strategy {:interfaces [Plain] :methods {% identity}}))]
    (is (= '[hide-it 0] (r '[hide-it 0])))
    (is (= '[hide_it 0] (r ["hide_it" 0])))
    (is (= '[my.ns/show 0] (r '[my.ns/show 0])) "a qualified name names no method")
    (is (nil? (r ["show" 0])))))

(deftest a-refusal-depends-on-no-function
  (let [d     {:interfaces [:not-a-class] :methods {'[show 0] (fn [_] 1)}}
        blank {:interfaces [:not-a-class] :methods {'[show 0] (fn [& _])}}]
    (is (= (proof/refusal key-class/strategy d) (proof/refusal key-class/strategy blank)))))

(deftest a-type-is-named-by-a-symbol
  (doseq [n [:a.b/T "a.b/T"]]
    (is (thrown? IllegalArgumentException (proof/class-name key-class/strategy n)) (pr-str n))))
