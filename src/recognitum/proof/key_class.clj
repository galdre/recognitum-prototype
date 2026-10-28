(ns recognitum.proof.key-class
  "The key-class proof strategy: each definition of a type emits a final APersistentMap
  subclass T whose one public constructor needs T's key, and a draft class T$Draft whose
  mark needs it too. The handle `define-type` returns holds the key; this namespace holds
  none, so reloading it loses nothing and strands nothing.

    T  extends APersistentMap implements IObj, IEditableCollection, recognitum.proof.Proven
       and each interface its definition lists. final. Holds a private map `m` and a
       private `_meta`. T(Object key, Object data) throws unless key is T.KEY, and takes
       data's meta. Reads forward to `m`. A private static volatile WRITES, set once with
       the key by install(key, writes): while it is set, assoc, assocEx, without and cons
       compute the new entries on the plain map and return WRITES.invoke(this, entries),
       one checked write that seals or throws; asTransient returns WRITES.invoke(this), a
       transient whose persistent! makes one checked write (into). While it is not set,
       every change returns a plain map. A public static gate() returns WRITES, which needs
       no key to call. A no-op assoc, without or cons returns this. empty throws, as on a
       defrecord. withMeta with the same meta returns this. writeReplace serializes a plain
       map; readObject throws. A static create(IPersistentMap) returns its argument, so
       #T{...} reads back as a plain map.
       Each method the definition gives a function, `toString` included, and each
       abstract method of a listed interface that T does not already have, is a
       forwarding method: it calls the function in a private static FNS table, set once by
       claim(fns) when the type is defined, with the value itself and the call's arguments,
       boxed; a slot with no function throws UnsupportedOperationException. The function
       sees no key. A default method with no function keeps its body; two default bodies
       of one method, from interfaces neither of which extends the other, and no function,
       refuse the definition.
    T$Draft implements BiConsumer (accept(key, check): mark) and Predicate (test(check):
       passed?), and none of the listed interfaces: a definition may not list either, nor an
       interface they extend. accept throws unless key is T.KEY. A
       public static gate() returns T.gate(). `define` records on the class, through a
       ClassValue, that it made it, so the keyless `marked?` answers test(check) of its own
       drafts and false of any other object, whatever its class is named.

  T.KEY is a package-private static made in <clinit>. claim(fns) hands it out once, to
  `define-type`. Besides recognitum.proof.Proven, defined by a namespace that is never
  reloaded, and the interfaces a definition lists, only JDK and clojure.lang interfaces
  appear in the classes.

  A value prints as its map, so the text shows its data alone, and reads back as a plain
  map; under *print-dup* it prints as T/create of its map, which reads back plain too. Where
  a listed interface or a supertype of it has a `print-method` or `print-dup` entry, the
  definition prefers the map's entry to it, and is refused, with nothing changed, where
  that entry is already preferred to the map's.

  `refusal` and `define-type` refuse through one analysis of the definition: `define-type`
  throws, as `ex-info` data, exactly the refusal `refusal` answers. The kinds beyond the
  protocol's: `:malformed` (the definition's or a signature's shape), `:not-a-function`,
  `:duplicate` (two signatures of one method), `:not-an-interface`, `:draft-interface`
  (an interface a draft implements) and `:reloaded-interface` (a listed interface whose name
  now loads another class)."
  (:require [clojure.string :as str]
            [recognitum.proof :as proof])
  (:import [clojure.asm ClassWriter Opcodes Type Label MethodVisitor]
           [clojure.lang DynamicClassLoader MultiFn RT]
           [java.lang.reflect Method Modifier]))

(set! *warn-on-reflection* true)

(def ^:private OBJ "java/lang/Object")
(def ^:private OBJ-D "Ljava/lang/Object;")
(def ^:private IPM "clojure/lang/IPersistentMap")
(def ^:private IPM-D "Lclojure/lang/IPersistentMap;")
(def ^:private IFN "clojure/lang/IFn")
(def ^:private IFN-D "Lclojure/lang/IFn;")

(defn- iname [^Class c] (Type/getInternalName c))
(defn- mdesc [^Class c ^String n & ps] (Type/getMethodDescriptor (.getMethod c n (into-array Class ps))))

(defn- throw! [^MethodVisitor mv ^Class ex ^String msg]
  (.visitTypeInsn mv Opcodes/NEW (iname ex))
  (.visitInsn mv Opcodes/DUP)
  (.visitLdcInsn mv msg)
  (.visitMethodInsn mv Opcodes/INVOKESPECIAL (iname ex) "<init>" "(Ljava/lang/String;)V" false)
  (.visitInsn mv Opcodes/ATHROW))

(defn- require-key!
  "if (local[slot] != T.KEY) throw new SecurityException(msg)"
  [^MethodVisitor mv t slot msg]
  (let [ok (Label.)]
    (.visitVarInsn mv Opcodes/ALOAD (int slot))
    (.visitFieldInsn mv Opcodes/GETSTATIC t "KEY" OBJ-D)
    (.visitJumpInsn mv Opcodes/IF_ACMPEQ ok)
    (throw! mv SecurityException msg)
    (.visitLabel mv ok)))

(defn- method ^MethodVisitor [^ClassWriter cw access n desc]
  (doto (.visitMethod cw (int access) n desc nil nil) (.visitCode)))

(defn- end! [^MethodVisitor mv] (.visitMaxs mv 0 0) (.visitEnd mv))

(defn- load-m [^MethodVisitor mv t]
  (.visitVarInsn mv Opcodes/ALOAD 0)
  (.visitFieldInsn mv Opcodes/GETFIELD t "m" IPM-D))

(defn- forward!
  "public R name(args) { return this.m.name(args); } for an interface method of m."
  [cw t ^Class iface n & ps]
  (let [jm   (.getMethod iface n (into-array Class ps))
        desc (Type/getMethodDescriptor jm)
        mv   (method cw Opcodes/ACC_PUBLIC n desc)]
    (load-m mv t)
    (doseq [[i _] (map-indexed vector ps)] (.visitVarInsn mv Opcodes/ALOAD (int (inc i))))
    (.visitMethodInsn mv Opcodes/INVOKEINTERFACE (iname iface) n desc true)
    (.visitInsn mv (if (= Integer/TYPE (.getReturnType jm)) Opcodes/IRETURN
                       (if (= Boolean/TYPE (.getReturnType jm)) Opcodes/IRETURN Opcodes/ARETURN)))
    (end! mv)))

(defn- changing!
  "public IPersistentMap name(args) {
     r = m.name(args);
     if (same-is-this? && r == m) return this;
     w = WRITES; if (w != null) return (IPersistentMap) w.invoke(this, r);
     return plain(r); }"
  [cw t n same-is-this? & ps]
  (let [desc (apply mdesc clojure.lang.IPersistentMap n ps)
        mv   (method cw Opcodes/ACC_PUBLIC n desc)
        r    (inc (count ps))
        w    (+ 2 (count ps))
        other (Label.)
        unhooked (Label.)]
    (load-m mv t)
    (doseq [i (range (count ps))] (.visitVarInsn mv Opcodes/ALOAD (int (inc i))))
    (.visitMethodInsn mv Opcodes/INVOKEINTERFACE IPM n desc true)
    (.visitVarInsn mv Opcodes/ASTORE (int r))
    (when same-is-this?
      (.visitVarInsn mv Opcodes/ALOAD (int r))
      (load-m mv t)
      (.visitJumpInsn mv Opcodes/IF_ACMPNE other)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitLabel mv other))
    (.visitFieldInsn mv Opcodes/GETSTATIC t "WRITES" IFN-D)
    (.visitVarInsn mv Opcodes/ASTORE (int w))
    (.visitVarInsn mv Opcodes/ALOAD (int w))
    (.visitJumpInsn mv Opcodes/IFNULL unhooked)
    (.visitVarInsn mv Opcodes/ALOAD (int w))
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitVarInsn mv Opcodes/ALOAD (int r))
    (.visitMethodInsn mv Opcodes/INVOKEINTERFACE IFN "invoke" (str "(" OBJ-D OBJ-D ")" OBJ-D) true)
    (.visitTypeInsn mv Opcodes/CHECKCAST IPM)
    (.visitInsn mv Opcodes/ARETURN)
    (.visitLabel mv unhooked)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitVarInsn mv Opcodes/ALOAD (int r))
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL t "plain" (str "(" OBJ-D ")" OBJ-D) false)
    (.visitTypeInsn mv Opcodes/CHECKCAST IPM)
    (.visitInsn mv Opcodes/ARETURN)
    (end! mv)))

(defn- no-key-message
  "What T's constructor says when called without the key: the type, by its class name, and
  that a value is made only through the engine."
  [t]
  (str (.replace ^String t \/ \.) "'s constructor takes a key only recognitum holds: a value of this type is made only through recognitum"))

(defn- push-int! [^MethodVisitor mv n]
  (if (<= -128 n 127)
    (.visitIntInsn mv Opcodes/BIPUSH (int n))
    (.visitLdcInsn mv (int n))))

(defn- unbox-desc
  "The descriptor of the static method of RT named `n` taking `c`."
  [^String n ^Class c]
  (Type/getMethodDescriptor (.getMethod RT n (into-array Class [c]))))

(def ^:private casts
  {Integer/TYPE "intCast" Long/TYPE "longCast" Double/TYPE "doubleCast" Float/TYPE "floatCast"
   Short/TYPE "shortCast" Byte/TYPE "byteCast" Character/TYPE "charCast" Boolean/TYPE "booleanCast"})

(defn- forwarder!
  "public R name(P...) {
     f = (IFn) FNS[idx]; if (f == null) throw new UnsupportedOperationException();
     return (R) f.invoke(this, box(p)...); }
  With more than 19 parameters, f.applyTo(ArraySeq.create(this, p...))."
  [^ClassWriter cw t idx {:keys [^String name params ^Class ret]}]
  (let [ptypes (map #(Type/getType ^Class %) params)
        desc   (Type/getMethodDescriptor (Type/getType ret) (into-array Type ptypes))
        mv     (method cw Opcodes/ACC_PUBLIC name desc)
        starts (vec (reductions + 1 (map #(.getSize ^Type %) ptypes)))
        f      (int (peek starts))
        n      (count params)
        ok     (Label.)
        arg!   (fn [i]
                 (let [^Class c (nth params i) ^Type ty (nth ptypes i)]
                   (.visitVarInsn mv (.getOpcode ty Opcodes/ILOAD) (int (nth starts i)))
                   (when (.isPrimitive c)
                     (.visitMethodInsn mv Opcodes/INVOKESTATIC (iname RT) "box"
                                       (Type/getMethodDescriptor (.getMethod RT "box" (into-array Class [c]))) false))))]
    (.visitFieldInsn mv Opcodes/GETSTATIC t "FNS" "[Ljava/lang/Object;")
    (push-int! mv idx)
    (.visitInsn mv Opcodes/AALOAD)
    (.visitVarInsn mv Opcodes/ASTORE f)
    (.visitVarInsn mv Opcodes/ALOAD f)
    (.visitJumpInsn mv Opcodes/IFNONNULL ok)
    (throw! mv UnsupportedOperationException (str "no function implements " name))
    (.visitLabel mv ok)
    (.visitVarInsn mv Opcodes/ALOAD f)
    (.visitTypeInsn mv Opcodes/CHECKCAST IFN)
    (if (< n 20)
      (do (.visitVarInsn mv Opcodes/ALOAD 0)
          (dotimes [i n] (arg! i))
          (.visitMethodInsn mv Opcodes/INVOKEINTERFACE IFN "invoke"
                            (str "(" (apply str (repeat (inc n) OBJ-D)) ")" OBJ-D) true))
      (do (push-int! mv (inc n))
          (.visitTypeInsn mv Opcodes/ANEWARRAY OBJ)
          (.visitInsn mv Opcodes/DUP) (push-int! mv 0) (.visitVarInsn mv Opcodes/ALOAD 0) (.visitInsn mv Opcodes/AASTORE)
          (dotimes [i n]
            (.visitInsn mv Opcodes/DUP) (push-int! mv (inc i)) (arg! i) (.visitInsn mv Opcodes/AASTORE))
          (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/ArraySeq" "create"
                            "([Ljava/lang/Object;)Lclojure/lang/ArraySeq;" false)
          (.visitMethodInsn mv Opcodes/INVOKEINTERFACE IFN "applyTo" "(Lclojure/lang/ISeq;)Ljava/lang/Object;" true)))
    (cond
      (= ret Void/TYPE) (do (.visitInsn mv Opcodes/POP) (.visitInsn mv Opcodes/RETURN))
      (.isPrimitive ret) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC (iname RT) (casts ret) (unbox-desc (casts ret) Object) false)
                             (.visitInsn mv (.getOpcode (Type/getType ret) Opcodes/IRETURN)))
      :else (do (when-not (= ret Object) (.visitTypeInsn mv Opcodes/CHECKCAST (iname ret)))
                (.visitInsn mv Opcodes/ARETURN)))
    (end! mv)))

(defn- value-class-bytes
  "T's bytes. `ifaces` are the listed interfaces T implements; `plan` the methods that
  forward, each to the function at its index in FNS."
  [t ifaces plan]
  (let [cw    (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        super (iname clojure.lang.APersistentMap)
        pub   Opcodes/ACC_PUBLIC
        priv  Opcodes/ACC_PRIVATE
        stat  Opcodes/ACC_STATIC]
    (.visit cw Opcodes/V1_8 (bit-or pub Opcodes/ACC_FINAL Opcodes/ACC_SUPER) t nil super
            (into-array String (concat [(iname clojure.lang.IObj) (iname clojure.lang.IEditableCollection)
                                        (iname recognitum.proof.Proven)]
                                       (map iname ifaces))))
    (doseq [[acc n d] [[(bit-or stat Opcodes/ACC_FINAL) "KEY" OBJ-D]
                       [(bit-or priv stat) "claimed" "Z"]
                       [(bit-or priv stat Opcodes/ACC_VOLATILE) "WRITES" IFN-D]
                       [(bit-or priv stat Opcodes/ACC_VOLATILE) "FNS" "[Ljava/lang/Object;"]
                       [(bit-or priv Opcodes/ACC_FINAL) "m" IPM-D]
                       [(bit-or priv Opcodes/ACC_FINAL) "_meta" IPM-D]]]
      (.visitEnd (.visitField cw (int acc) n d nil nil)))
    ;; static { KEY = new Object(); }
    (doto (method cw stat "<clinit>" "()V")
      (.visitTypeInsn Opcodes/NEW OBJ) (.visitInsn Opcodes/DUP)
      (.visitMethodInsn Opcodes/INVOKESPECIAL OBJ "<init>" "()V" false)
      (.visitFieldInsn Opcodes/PUTSTATIC t "KEY" OBJ-D)
      (.visitInsn Opcodes/RETURN) (end!))
    ;; public static synchronized Object claim(Object[] fns): once; sets FNS
    (let [mv (method cw (bit-or pub stat Opcodes/ACC_SYNCHRONIZED) "claim" (str "([Ljava/lang/Object;)" OBJ-D))
          fresh (Label.)]
      (.visitFieldInsn mv Opcodes/GETSTATIC t "claimed" "Z")
      (.visitJumpInsn mv Opcodes/IFEQ fresh)
      (throw! mv SecurityException "key already claimed")
      (.visitLabel mv fresh)
      (.visitInsn mv Opcodes/ICONST_1)
      (.visitFieldInsn mv Opcodes/PUTSTATIC t "claimed" "Z")
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/PUTSTATIC t "FNS" "[Ljava/lang/Object;")
      (.visitFieldInsn mv Opcodes/GETSTATIC t "KEY" OBJ-D)
      (.visitInsn mv Opcodes/ARETURN)
      (end! mv))
    ;; public static synchronized void install(Object key, Object writes): once, with the key
    (let [mv (method cw (bit-or pub stat Opcodes/ACC_SYNCHRONIZED) "install" (str "(" OBJ-D OBJ-D ")V"))
          fresh (Label.)]
      (require-key! mv t 0 "installing a write takes the type's key")
      (.visitFieldInsn mv Opcodes/GETSTATIC t "WRITES" IFN-D)
      (.visitJumpInsn mv Opcodes/IFNULL fresh)
      (throw! mv SecurityException "writes already installed")
      (.visitLabel mv fresh)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitTypeInsn mv Opcodes/CHECKCAST IFN)
      (.visitFieldInsn mv Opcodes/PUTSTATIC t "WRITES" IFN-D)
      (.visitInsn mv Opcodes/RETURN)
      (end! mv))
    ;; public static IFn gate() { return WRITES; }
    (doto (method cw (bit-or pub stat) "gate" (str "()" IFN-D))
      (.visitFieldInsn Opcodes/GETSTATIC t "WRITES" IFN-D)
      (.visitInsn Opcodes/ARETURN) (end!))
    ;; public T(Object key, Object data): the key is checked before super() runs
    (let [mv (method cw pub "<init>" (str "(" OBJ-D OBJ-D ")V"))
          ok (Label.)]
      (require-key! mv t 1 (no-key-message t))
      (.visitVarInsn mv Opcodes/ALOAD 2)
      (.visitTypeInsn mv Opcodes/INSTANCEOF IPM)
      (.visitJumpInsn mv Opcodes/IFNE ok)
      (throw! mv IllegalArgumentException "data must be a persistent map")
      (.visitLabel mv ok)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL super "<init>" "()V" false)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitVarInsn mv Opcodes/ALOAD 2)
      (.visitTypeInsn mv Opcodes/CHECKCAST IPM)
      (.visitFieldInsn mv Opcodes/PUTFIELD t "m" IPM-D)
      ;; _meta = data instanceof IMeta ? ((IMeta) data).meta() : null
      (let [no-meta (Label.)]
        (.visitVarInsn mv Opcodes/ALOAD 2)
        (.visitTypeInsn mv Opcodes/INSTANCEOF (iname clojure.lang.IMeta))
        (.visitJumpInsn mv Opcodes/IFEQ no-meta)
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (.visitVarInsn mv Opcodes/ALOAD 2)
        (.visitTypeInsn mv Opcodes/CHECKCAST (iname clojure.lang.IMeta))
        (.visitMethodInsn mv Opcodes/INVOKEINTERFACE (iname clojure.lang.IMeta) "meta" (str "()" IPM-D) true)
        (.visitFieldInsn mv Opcodes/PUTFIELD t "_meta" IPM-D)
        (.visitLabel mv no-meta))
      (.visitInsn mv Opcodes/RETURN)
      (end! mv))
    ;; private T(IPersistentMap m, IPersistentMap meta): same data, new meta
    (doto (method cw priv "<init>" (str "(" IPM-D IPM-D ")V"))
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL super "<init>" "()V" false)
      (.visitVarInsn Opcodes/ALOAD 0) (.visitVarInsn Opcodes/ALOAD 1)
      (.visitFieldInsn Opcodes/PUTFIELD t "m" IPM-D)
      (.visitVarInsn Opcodes/ALOAD 0) (.visitVarInsn Opcodes/ALOAD 2)
      (.visitFieldInsn Opcodes/PUTFIELD t "_meta" IPM-D)
      (.visitInsn Opcodes/RETURN) (end!))
    ;; private Object plain(Object r): r with this value's meta, if it has any
    (let [mv (method cw priv "plain" (str "(" OBJ-D ")" OBJ-D))
          some-meta (Label.)]
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/GETFIELD t "_meta" IPM-D)
      (.visitJumpInsn mv Opcodes/IFNONNULL some-meta)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitLabel mv some-meta)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitTypeInsn mv Opcodes/CHECKCAST (iname clojure.lang.IObj))
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/GETFIELD t "_meta" IPM-D)
      (.visitMethodInsn mv Opcodes/INVOKEINTERFACE (iname clojure.lang.IObj) "withMeta"
                        (mdesc clojure.lang.IObj "withMeta" clojure.lang.IPersistentMap) true)
      (.visitInsn mv Opcodes/ARETURN)
      (end! mv))
    ;; reads
    (forward! cw t clojure.lang.ILookup "valAt" Object)
    (forward! cw t clojure.lang.ILookup "valAt" Object Object)
    (forward! cw t clojure.lang.Associative "containsKey" Object)
    (forward! cw t clojure.lang.Associative "entryAt" Object)
    (forward! cw t clojure.lang.Counted "count")
    (forward! cw t clojure.lang.Seqable "seq")
    (forward! cw t Iterable "iterator")
    ;; changes: a plain map, or this when nothing changed
    (changing! cw t "assoc" true Object Object)
    (changing! cw t "assocEx" false Object Object)
    (changing! cw t "without" true Object)
    (changing! cw t "cons" true Object)
    ;; public ITransientCollection asTransient() {
    ;;   w = WRITES; if (w == null) return ((IEditableCollection) m).asTransient();
    ;;   return (ITransientCollection) w.invoke(this); }
    (let [desc (mdesc clojure.lang.IEditableCollection "asTransient")
          mv   (method cw Opcodes/ACC_PUBLIC "asTransient" desc)
          hooked (Label.)]
      (.visitFieldInsn mv Opcodes/GETSTATIC t "WRITES" IFN-D)
      (.visitVarInsn mv Opcodes/ASTORE 1)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitJumpInsn mv Opcodes/IFNONNULL hooked)
      (load-m mv t)
      (.visitTypeInsn mv Opcodes/CHECKCAST (iname clojure.lang.IEditableCollection))
      (.visitMethodInsn mv Opcodes/INVOKEINTERFACE (iname clojure.lang.IEditableCollection) "asTransient" desc true)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitLabel mv hooked)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKEINTERFACE IFN "invoke" (str "(" OBJ-D ")" OBJ-D) true)
      (.visitTypeInsn mv Opcodes/CHECKCAST (iname clojure.lang.ITransientCollection))
      (.visitInsn mv Opcodes/ARETURN)
      (end! mv))
    ;; public IPersistentCollection empty() { throw new UnsupportedOperationException(); }
    (let [mv (method cw pub "empty" (mdesc clojure.lang.IPersistentCollection "empty"))]
      (throw! mv UnsupportedOperationException "a value of a type has no empty value")
      (end! mv))
    ;; meta
    (doto (method cw pub "meta" (str "()" IPM-D))
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitFieldInsn Opcodes/GETFIELD t "_meta" IPM-D)
      (.visitInsn Opcodes/ARETURN) (end!))
    (let [mv (method cw pub "withMeta" (mdesc clojure.lang.IObj "withMeta" clojure.lang.IPersistentMap))
          differ (Label.)]
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/GETFIELD t "_meta" IPM-D)
      (.visitJumpInsn mv Opcodes/IF_ACMPNE differ)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitLabel mv differ)
      (.visitTypeInsn mv Opcodes/NEW t)
      (.visitInsn mv Opcodes/DUP)
      (load-m mv t)
      (.visitVarInsn mv Opcodes/ALOAD 1)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL t "<init>" (str "(" IPM-D IPM-D ")V") false)
      (.visitInsn mv Opcodes/ARETURN)
      (end! mv))
    ;; serialization: out as a plain map; a hand-made stream naming T is refused
    (do
    (doto (method cw priv "writeReplace" (str "()" OBJ-D))
      (.visitVarInsn Opcodes/ALOAD 0)
      (load-m t)
      (.visitMethodInsn Opcodes/INVOKESPECIAL t "plain" (str "(" OBJ-D ")" OBJ-D) false)
      (.visitInsn Opcodes/ARETURN) (end!))
    (doto (method cw priv "readObject" "(Ljava/io/ObjectInputStream;)V")
      (throw! java.io.InvalidObjectException "no key") (end!)))
    ;; public static IPersistentMap create(IPersistentMap m) { return m; }  -- #T{..} reads plain
    (doto (method cw (bit-or pub stat) "create" (str "(" IPM-D ")" IPM-D))
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitInsn Opcodes/ARETURN) (end!))
    ;; the listed interfaces' methods, and toString where given
    (doseq [[i m] (map-indexed vector plan)] (forwarder! cw t i m))
    (.visitEnd cw)
    (.toByteArray cw)))

(defn- draft-class-bytes [d t]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        set-d "Ljava/util/Set;"]
    (.visit cw Opcodes/V1_8 (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER) d nil OBJ
            (into-array String ["java/util/function/BiConsumer" "java/util/function/Predicate"]))
    (.visitEnd (.visitField cw (bit-or Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "marks" set-d nil nil))
    (doto (method cw Opcodes/ACC_PUBLIC "<init>" "()V")
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL OBJ "<init>" "()V" false)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESTATIC "java/util/concurrent/ConcurrentHashMap" "newKeySet"
                        "()Ljava/util/concurrent/ConcurrentHashMap$KeySetView;" false)
      (.visitFieldInsn Opcodes/PUTFIELD d "marks" set-d)
      (.visitInsn Opcodes/RETURN) (end!))
    ;; public void accept(Object key, Object check)
    (let [mv (method cw Opcodes/ACC_PUBLIC "accept" (str "(" OBJ-D OBJ-D ")V"))]
      (require-key! mv t 1 "marking a draft takes the type's key")
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/GETFIELD d "marks" set-d)
      (.visitVarInsn mv Opcodes/ALOAD 2)
      (.visitMethodInsn mv Opcodes/INVOKEINTERFACE "java/util/Set" "add" "(Ljava/lang/Object;)Z" true)
      (.visitInsn mv Opcodes/POP)
      (.visitInsn mv Opcodes/RETURN)
      (end! mv))
    ;; public static IFn gate() { return T.gate(); }
    (doto (method cw (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "gate" (str "()" IFN-D))
      (.visitMethodInsn Opcodes/INVOKESTATIC t "gate" (str "()" IFN-D) false)
      (.visitInsn Opcodes/ARETURN) (end!))
    ;; public boolean test(Object check)
    (doto (method cw Opcodes/ACC_PUBLIC "test" (str "(" OBJ-D ")Z"))
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitFieldInsn Opcodes/GETFIELD d "marks" set-d)
      (.visitVarInsn Opcodes/ALOAD 1)
      (.visitMethodInsn Opcodes/INVOKEINTERFACE "java/util/Set" "contains" "(Ljava/lang/Object;)Z" true)
      (.visitInsn Opcodes/IRETURN) (end!))
    (.visitEnd cw)
    (.toByteArray cw)))

(defonce ^:private counter (java.util.concurrent.atomic.AtomicLong.))

(defn fresh-name
  "A new class name, for a new type. Redefinition reuses the name."
  []
  (str "recognitum.proof.key_class.T" (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong counter)))

(defn class-name
  "The class name for `type-name`, a symbol: `my.ns/T` -> `my.ns.T`. Every definition of the
  type names its value class so. Throws for a name that is not a symbol."
  ^String [type-name]
  (when-not (symbol? type-name)
    (throw (IllegalArgumentException. (str "a type's name is a symbol: " (pr-str type-name)))))
  (munge (.replace (str type-name) \/ \.)))

(defonce ^:private own-drafts
  (proxy [java.lang.ClassValue] []
    (computeValue [_] (java.util.concurrent.atomic.AtomicBoolean.))))

(defn- own-draft!
  "Records, on the class itself, that [[define]] made draft class `dc`."
  [^Class dc]
  (.set ^java.util.concurrent.atomic.AtomicBoolean (.get ^java.lang.ClassValue own-drafts dc) true))

(defn- marked?
  "Whether `draft` is of a draft class [[define]] made and records `check` as passed."
  [draft check]
  (boolean (and (some? draft)
                (.get ^java.util.concurrent.atomic.AtomicBoolean (.get ^java.lang.ClassValue own-drafts (class draft)))
                (.test ^java.util.function.Predicate draft check))))

(defn- refusal-of
  "A refusal of `kind`, whose message says `msg` of `x`, with `more` (`:method`, `:interfaces`)."
  [kind msg x more]
  (merge {:kind kind :message (str "refused definition: " msg ": " (pr-str x))} more))

(defn- refuse
  "Ends the analysis of a definition with [[refusal-of]] its arguments."
  [kind msg x & {:as more}]
  (throw (ex-info "refused" {::refusal (refusal-of kind msg x more)})))

(defn- method-of
  "`[name arity]` of the method named `n` with parameters `params`."
  [n params]
  [(symbol n) (count params)])

(defn- sig-method
  "`[name arity]` of the signature `[nm spec]`, the name spelled as the signature spells it."
  [[nm spec]]
  [(if (symbol? nm) nm (symbol nm)) (if (vector? spec) (count spec) spec)])

(defn- without-functions
  "`definition` as a refusal's message prints it: its signatures without their functions, so
  the words depend on no function."
  [definition]
  (if (and (map? definition) (map? (:methods definition)))
    (update definition :methods (comp vec keys))
    definition))

(def ^:private own
  "The [name parameter-types] of the methods T declares for itself, statics included."
  #{["claim" [(class (object-array 0))]] ["install" [Object Object]] ["gate" []]
    ["create" [clojure.lang.IPersistentMap]] ["plain" [Object]] ["writeReplace" []]
    ["readObject" [java.io.ObjectInputStream]] ["meta" []] ["withMeta" [clojure.lang.IPersistentMap]]
    ["asTransient" []] ["valAt" [Object]] ["valAt" [Object Object]] ["containsKey" [Object]]
    ["entryAt" [Object]] ["count" []] ["seq" []] ["iterator" []] ["assoc" [Object Object]]
    ["assocEx" [Object Object]] ["without" [Object]] ["cons" [Object]] ["empty" []]})

(defn- implemented?
  "Whether T has a method `n` of `params` without the definition: its own or its map's."
  [n params]
  (or (contains? own [n params])
      (try (.getMethod clojure.lang.APersistentMap n (into-array Class params)) true
           (catch NoSuchMethodException _ false))))

(defn- class-implemented?
  "Whether a class T extends, or T itself, declares method `n` of `params`: a body that wins
  over every interface's default."
  [n params]
  (or (contains? own [n params])
      (try (not (.isInterface (.getDeclaringClass (.getMethod clojure.lang.APersistentMap n (into-array Class params)))))
           (catch NoSuchMethodException _ false))))

(defn- interface-methods
  "The instance methods interface `c` declares or inherits, as [[name params] Method] pairs."
  [^Class c]
  (for [^Method m (.getMethods c)
        :when (not (Modifier/isStatic (.getModifiers m)))]
    [[(.getName m) (vec (.getParameterTypes m))] m]))

(defn- members
  "The instance methods `c` declares or inherits, as [[name params] Method] pairs; of
  `Object`, `toString` alone. Refuses a class that is neither an interface nor `Object`."
  [^Class c]
  (cond
    (= c Object)     [[["toString" []] (.getMethod Object "toString" (make-array Class 0))]]
    (.isInterface c) (interface-methods c)
    :else            (refuse :not-an-interface "not an interface" c :interfaces [c])))

(def ^:private map-methods
  "The methods of the interfaces T has as a map, by [name params]: their defaults compete
  with a listed interface's."
  (group-by first (mapcat interface-methods (filter #(.isInterface ^Class %) (supers clojure.lang.APersistentMap)))))

(defn- maximal
  "The methods of `ms` whose declaring interface no other declaring interface of `ms` extends."
  [ms]
  (let [owners (set (map #(.getDeclaringClass ^Method %) ms))]
    (remove (fn [^Method m]
              (let [a (.getDeclaringClass m)]
                (some (fn [^Class b] (and (not= a b) (.isAssignableFrom a b))) owners)))
            ms)))

(def ^:private draft-interfaces
  "The interfaces every draft class implements."
  [java.util.function.BiConsumer java.util.function.Predicate])

(defn- plan
  "The methods of `definition` that T forwards, as [interfaces plan]: each entry
  {:name :params :ret :f}, `:f` nil where no function implements it. Refuses a signature no
  listed interface declares, one that matches more than one method or names a method T has
  without the definition, two signatures of one method, `equals` and `hashCode`, two
  listed methods of one name and parameters with different return types (among the methods
  T does not have as a map), a method with no function whose most specific declarations
  give more than one default body, and an interface that a draft implements.

  A method with no function and no default body among its most specific declarations
  forwards to nothing, and throws; one with exactly one keeps that body."
  [{:keys [interfaces methods] :as definition}]
  (when-not (and (map? definition) (or (nil? interfaces) (sequential? interfaces))
                 (or (nil? methods) (map? methods)) (every? class? interfaces))
    (refuse :malformed "not {:interfaces [class ...] :methods {signature f}}" (without-functions definition)))
  (doseq [^Class i interfaces
          :when (and (not= Object i) (some #(.isAssignableFrom i %) draft-interfaces))]
    (refuse :draft-interface "a draft implements this interface, and a draft implements none of its definition's" i
            :interfaces [i]))
  (let [listed  (group-by first (mapcat members (distinct interfaces)))
        by-sig  (into {} (for [[k pairs] listed
                               :let [ms (distinct (map second pairs))]]
                           (do (when (and (not (apply implemented? k))
                                          (< 1 (count (distinct (map #(.getReturnType ^Method %) ms)))))
                                 (refuse :clash "two listed methods differ only in return type" k
                                         :method (apply method-of k)
                                         :interfaces (vec (distinct (map #(.getDeclaringClass ^Method %) ms)))))
                               [k (first ms)])))
        resolve (fn [[nm spec :as sig]]
                  (when-not (and (vector? sig) (= 2 (count sig)) (or (symbol? nm) (string? nm))
                                 (or (nat-int? spec) (and (vector? spec) (every? class? spec))))
                    (refuse :malformed "a signature is [name arity] or [name [class ...]]" sig))
                  (let [n (str nm)]
                    (when (#{"equals" "hashCode"} n)
                      (refuse :equality "a value's equality is its map's" sig :method (sig-method sig)))
                    (let [found (filter (fn [[[mn ps]]]
                                          (and (= n mn) (if (vector? spec) (= spec ps) (= spec (count ps)))))
                                        by-sig)]
                      (case (count found)
                        0 (refuse :undeclared "no listed interface declares" sig :method (sig-method sig))
                        1 (let [[k] (first found)]
                            (when (and (apply implemented? k) (not= k ["toString" []]))
                              (refuse :map-method "a value has this method without the definition" sig
                                      :method (sig-method sig)))
                            k)
                        (refuse :ambiguous "more than one method matches" sig :method (sig-method sig))))))
        given   (reduce (fn [acc [sig f]]
                          (let [k (resolve sig)]
                            (when-not (or (nil? f) (ifn? f))
                              (refuse :not-a-function "not a function" sig :method (sig-method sig)))
                            (when (contains? acc k)
                              (refuse :duplicate "two signatures name one method" sig :method (sig-method sig)))
                            (assoc acc k f)))
                        {} methods)
        ;; the default bodies a call of `k` could run, where no class T extends declares it
        defaults (fn [k]
                   (if (apply class-implemented? k)
                     []
                     (filter #(.isDefault ^Method %)
                             (maximal (distinct (map second (concat (listed k) (map-methods k))))))))
        _       (doseq [k (keys by-sig)
                        :when (not (contains? given k))
                        :let [ds (defaults k)]
                        :when (< 1 (count ds))]
                  (refuse :clash
                          (str "the interfaces " (str/join " and " (map #(.getName (.getDeclaringClass ^Method %)) ds))
                               " give default bodies of " (first k) " and the definition gives no function for it")
                          k
                          :method (apply method-of k)
                          :interfaces (vec (map #(.getDeclaringClass ^Method %) ds))))
        sigs    (distinct (concat (for [k (keys by-sig)
                                        :when (and (not (apply implemented? k)) (empty? (defaults k)))]
                                    k)
                                  (keys given)))]
    [(vec (remove #(= Object %) (distinct interfaces)))
     (vec (for [[n ps :as k] sigs
                :let [^Method m (by-sig k)]]
            {:name n :params ps :ret (.getReturnType m) :f (given k)}))]))

(defn- handle
  "The handle of one emitted class pair. A reify, not a map: nothing walks into it."
  [^Class vc ^Class dc key]
  (let [ctor       (.getConstructor vc (into-array Class [Object Object]))
        draft-ctor (.getConstructor dc (make-array Class 0))
        install    (.getMethod vc "install" (into-array Class [Object Object]))]
    (reify proof/Handle
      (seal [_ m]
        (.newInstance ctor (object-array [key (proof/plain-map m)])))
      (proven? [_ v] (.isInstance vc v))
      (install-write! [_ write]
        ;; installed hands this function to callers without the key, so a write starts only
        ;; from a value of this definition
        (let [own! (fn [old]
                     (when-not (.isInstance vc old)
                       (throw (IllegalArgumentException. "a write starts from a value of its type"))))]
          (.invoke install nil (object-array [key (fn ([old] (own! old) (proof/checked-transient old write))
                                                      ([old entries] (own! old) (write old entries))
                                                      ([a b c] (write a b c)))])))
        nil)
      (open [_] (.newInstance draft-ctor (object-array 0)))
      (mark! [_ draft check]
        (when-not (.isInstance dc draft) (throw (IllegalArgumentException. "not a draft of this type")))
        (.accept ^java.util.function.BiConsumer draft key check)
        nil)
      (passed? [_ draft check]
        (and (.isInstance dc draft) (.test ^java.util.function.Predicate draft check)))
      (classes [_] [vc dc])
      (capabilities [_] [key]))))

(defn- print-preferences
  "Where `print-method` or `print-dup` has an entry for a listed interface or one of its
  supertypes, the preferences of the map's entry, and of `Proven`'s, over it, as
  [multimethod-name multimethod winner entry]: else a value of both would match two entries
  and print neither."
  [ifaces]
  (for [[nm ^MultiFn mf] [["print-method" print-method] ["print-dup" print-dup]]
        k (keys (methods mf))
        :when (and (class? k) (not= Object k) (not (isa? clojure.lang.IPersistentMap k))
                   (some #(isa? % k) ifaces))
        winner [clojure.lang.IPersistentMap recognitum.proof.Proven]]
    [nm mf winner k]))

(defn- outranked
  "The refusal where multimethod `nm` already prefers interface `k`'s entry to the map's."
  [nm k]
  (refusal-of :clash (str nm " prefers this interface's entry to the map's, so a value could not print as its map") k
              {:interfaces [k]}))

(defn- check-outranked
  "Refuses where one of `prefs` cannot be set because the entry is already preferred over the
  winner. Tried on a copy of each multimethod's preferences, so it changes none."
  [prefs]
  (doseq [[nm group] (group-by first prefs)
          :let [^MultiFn mf (second (first group))
                copy (MultiFn. nm (.-dispatchFn mf) (.-defaultDispatchVal mf) (.-hierarchy mf))]]
    (doseq [[x ys] (.getPreferTable mf), y ys] (.preferMethod copy x y))
    (doseq [[_ _ winner k] group]
      (try (.preferMethod copy winner k)
           (catch IllegalStateException _ (throw (ex-info "refused" {::refusal (outranked nm k)})))))))

(defn- analysis
  "What defining `definition` takes, `{:interfaces :plan :prefs}`, or why it is refused,
  `{:refusal r}`. Defines and sets nothing. Besides [[plan]]'s refusals, refuses a listed
  interface whose name now loads another class (an interface defined again after the
  definition named it), which would leave values outside the one listed, and a print
  preference that cannot be set."
  [definition]
  (try
    (let [[ifaces plan] (plan (or definition {}))
          loader (DynamicClassLoader. (RT/baseLoader))]
      (doseq [^Class i ifaces]
        (when-not (identical? i (try (Class/forName (.getName i) false loader)
                                     (catch ClassNotFoundException _ nil)))
          (refuse :reloaded-interface "the interface's name now names another class" i :interfaces [i])))
      (let [prefs (print-preferences ifaces)]
        (check-outranked prefs)
        {:interfaces ifaces :plan plan :prefs prefs}))
    (catch clojure.lang.ExceptionInfo e
      (if-let [r (::refusal (ex-data e))] {:refusal r} (throw e)))))

(defn- refusal
  "Nil where [[define]] accepts `definition`, else the refusal it throws."
  [definition]
  (:refusal (analysis definition)))

(defn- prefer-print-as-map!
  "Sets `prefs`, which [[analysis]] checked. One set meanwhile in the other direction refuses."
  [prefs]
  (doseq [[nm ^MultiFn mf winner k] prefs]
    (try (.preferMethod mf winner k)
         (catch IllegalStateException _
           (let [r (outranked nm k)] (throw (ex-info (:message r) r)))))))

(defn- define
  "Refuses, defining nothing, where [[analysis]] does, throwing the refusal as `ex-info` data
  with its message. The refusal is checked before T is defined, so it leaves no class under
  the type's name."
  [type-name definition]
  (let [{:keys [refusal interfaces plan prefs]} (analysis definition)
        _      (when refusal (throw (ex-info (:message refusal) refusal)))
        ^String n (if type-name (class-name type-name) (fresh-name))
        t      (.replace n \. \/)
        d      (str t "$Draft")
        loader (DynamicClassLoader. (RT/baseLoader))
        _      (prefer-print-as-map! prefs)
        vc     (.defineClass loader n (value-class-bytes t interfaces plan) nil)]
    (let [dc  (.defineClass loader (.replace d \/ \.) (draft-class-bytes d t) nil)
          _   (own-draft! dc)
          key (.invoke (.getMethod ^Class vc "claim" (into-array Class [(class (object-array 0))]))
                       nil (object-array [(object-array (map :f plan))]))]
      (handle vc dc key))))

(def strategy
  "Each `define-type` emits T and T$Draft into a new loader and claims T's key. The same
  name again is redefinition: a new class pair with a new key. A refused definition throws
  and leaves the type as it was: no handle exists for it, and no key is claimed."
  (reify proof/Strategy
    (installed [_ value-class]
      (.invoke (.getMethod ^Class value-class "gate" (make-array Class 0)) nil (object-array 0)))
    (define-type [_ type-name] (define type-name nil))
    (define-type [_ type-name definition] (define type-name definition))
    (refusal [_ definition] (refusal definition))
    (class-name [_ type-name] (when type-name (class-name type-name)))
    (marked? [_ draft check] (marked? draft check))))
