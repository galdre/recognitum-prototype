(ns recognitum.proof
  "The proof protocol. A strategy defines types; each
  definition is an opaque handle that holds the type's key. Every keyed op takes the handle,
  so the caller keeps the key only by keeping the handle in a closure, never in a var.

  What every strategy guarantees:
  - a value is proven under a handle only if `seal` made it with that handle, or the write
    installed on that handle accepted it;
  - `clojure.core` `assoc`, `dissoc`, `conj`, `merge` and `into` on a value proven now go
    through the installed write, if there is one. `merge` and `into` with one source map
    are one write each, given the whole result; `empty` throws, as on a `defrecord`;
  - with no write installed, every change yields an unproven map;
  - `define-type` again with the same name is redefinition: values of the earlier
    definition are not proven under the new handle;
  - a definition's value class and draft class each have a public static method `gate()`,
    taking nothing and returning the `clojure.lang.IFn` that `installed` returns, so the
    class answers for its definition without any table; [[gate-of]] calls it;
  - a definition's value class implements [[Proven]], an interface with no methods, so one
    entry in a multimethod such as `print-method` serves the values of every definition;
  - a value keeps its definition's methods after a redefinition;
  - a value prints as its map: the text shows its data alone and never reads back proven.

  Not guaranteed: that `with-meta` keeps proof. `seal` takes the meta of the map it seals,
  so a caller sets meta before sealing.

  This namespace is never reloaded: reloading it would orphan every handle.")

(definterface Proven)

(defprotocol Strategy
  (define-type [strategy type-name] [strategy type-name definition]
    "Evaluates a type's definition: a new type and its key. Returns the handle.
    `definition`, `{:interfaces [class ...] :methods {signature f}}`, names the interfaces
    every value of the type implements and the functions behind their methods. A signature
    is `[name arity]`, the arity counting the arguments after the value, or
    `[name [class ...]]`, the parameter types. `f` takes the value and then the method's
    arguments. Of `Object`, `toString` may be given. An abstract method with no `f` throws
    when called; a default method with no `f` keeps its body; a draft implements none of
    the interfaces. Without a definition, the values implement no interface of their own.
    Where [[refusal]] answers a refusal of `definition`, throws `ex-info` with that refusal
    as its data and its `:message` as its message, and defines nothing.")
  (refusal [strategy definition]
    "Nil where `define-type` accepts `definition`, else the refusal it would make: a map with
    `:kind`, `:message`, and, by kind, `:method`, `[name arity]` with the name the signature's
    own spelling as a symbol, neither munged nor demunged, and the arity counting the
    arguments after the value, or `:interfaces`, `[class ...]`. Every strategy refuses a
    signature that names a method no listed interface declares (`:undeclared`), an
    `[name arity]` that matches more than one method (`:ambiguous`), and `equals` or
    `hashCode` (`:equality`). It may refuse a method the value has as a map (`:map-method`),
    a clash of listed interfaces with each other or with the map's, by default bodies,
    return types or print entries (`:clash`), and more, under kinds of its own. Needs no key
    and defines nothing. The answer depends on `definition`, on the classes the listed
    interfaces' names load, and on the print entries, as they are when called.")
  (class-name [strategy type-name]
    "The name of the class of the values of the type named `type-name`, a qualified symbol,
    the same under every definition of the type: `my.ns/T` -> `my.ns.T`. Nil for a nil
    `type-name`, whose class name each definition makes anew.")
  (marked? [strategy draft check]
    "Whether `draft` is a draft that this strategy's `open` made and that records `check`,
    compared by `=`, as passed: what `passed?` answers of it under the handle that opened it,
    also after the type is redefined. False for anything else. Needs no key and changes
    nothing.")
  (installed [strategy value-class]
    "A function that calls the write installed on the definition whose values are of
    `value-class`, or nil when none is installed. It needs no key. Called with one or two
    arguments, it refuses an `old` that is not a value of that definition; called with three,
    it passes them to the write as they are."))

(defprotocol Handle
  (seal [h m]
    "A value of the type holding `m`'s entries and meta, proven under `h`.")
  (proven? [h v]
    "Whether `v` is proven under `h`, the type's current definition.")
  (install-write! [h write]
    "Installs `write`, a (fn [old entries]) that returns a sealed value or throws, as the
    write of every value of the type. Once per handle: a second call throws or does nothing.
    A three-argument arity of `write` is reachable, without the key, through `installed`.")
  (open [h]
    "A new draft of the type, with no marks.")
  (mark! [h draft check]
    "Marks `check` passed on `draft`.")
  (passed? [h draft check]
    "Whether `check` is marked passed on `draft`.")
  (classes [h]
    "`[value-class draft-class]`: the class of every value proven under `h`, and of every draft
    `open` returns. Holding them grants nothing: the key stays in `h`.")
  (capabilities [h]
    "The objects that hold the key, so that a test can confirm no loaded var holds one."))

(defn checked-transient
  "For strategies: a transient over `old`'s entries whose persistent! makes one call
  `(write old entries)`, so `into` is one checked write however many entries it adds."
  [old write]
  (let [t    (volatile! (transient (into {} (seq old))))
        done (volatile! false)
        live (fn [] (when @done (throw (IllegalAccessError. "Transient used after persistent! call"))) @t)]
    (reify
      clojure.lang.ITransientMap
      (assoc [this k v] (vreset! t (assoc! (live) k v)) this)
      (without [this k] (vreset! t (dissoc! (live) k)) this)
      (conj [this o] (vreset! t (conj! (live) o)) this)
      (persistent [_] (let [r (persistent! (live))] (vreset! done true) (write old r)))
      (count [_] (count (live)))
      (valAt [_ k] (get (live) k))
      (valAt [_ k nf] (get (live) k nf))
      clojure.lang.ITransientAssociative2
      (containsKey [_ k] (contains? (live) k))
      (entryAt [_ k] (when (contains? (live) k) (clojure.lang.MapEntry/create k (get (live) k)))))))

(defn plain-map
  "For strategies: `m`'s entries and meta as a plain persistent map. Reads `m` through its
  seq, so no protocol caches `m`'s class: a cache entry would keep a type's class loaded."
  [m]
  (if (or (instance? clojure.lang.PersistentArrayMap m) (instance? clojure.lang.PersistentHashMap m))
    m
    (with-meta (into {} (seq m)) (meta m))))

(defonce ^:private gate-methods
  (proxy [java.lang.ClassValue] []
    (computeValue [^Class c]
      (try (let [m (.getMethod c "gate" (make-array Class 0))]
             (when (and (java.lang.reflect.Modifier/isStatic (.getModifiers m))
                        (= clojure.lang.IFn (.getReturnType m)))
               m))
           (catch NoSuchMethodException _ nil)))))

(defn gate-of
  "The function `installed` returns for the definition whose value class or draft class is
  `c`, found by calling the class's static `gate()`. Nil for any other class, and for a
  definition with no write installed. The lookup of each class's method is cached on the
  class, so it leaves nothing behind that would keep the class loaded."
  [^Class c]
  (when-let [^java.lang.reflect.Method m (.get ^java.lang.ClassValue gate-methods c)]
    (.invoke m nil (object-array 0))))
