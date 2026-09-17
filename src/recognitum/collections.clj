(ns recognitum.collections
  "The template both generated types are built on: a record rather than a map.

  `AbstractRecord` is `potemkin.collections/AbstractMap` with a record's answers in place of
  a map's. `equiv` compares the classes before it compares an entry and `hasheq` mixes the
  class in; `empty` refuses; `clojure.lang.IRecord` and `java.io.Serializable` are declared,
  and `clojure.lang.MapEquivalence` is not, so a plain map holding the same entries is
  unequal to a value of one of these types from either side. `.equals` and `.hashCode` are
  `APersistentMap`'s map contract, as a record's are.

  Everything else is `AbstractMap`'s: `IPersistentMap`, `Associative`, `ILookup`, `Counted`,
  `Seqable`, `IFn`, `IObj`, `java.util.Map`, and iteration. The six-method contract is
  unchanged, so [[def-record-type]] takes the same body `potemkin/def-map-type` does.

  Nothing here is part of the library's surface."
  (:require
   [potemkin.collections :as pc]
   [potemkin.types :as pt]))

(pt/def-abstract-type AbstractRecord

  ;; The six methods the generated types define, under the names `def-record-type` renames
  ;; them to.
  potemkin.collections.PotemkinMap

  ;; What `record?` asks, and what `clojure.walk` asks before it rebuilds a value as
  ;; whatever `empty` returns.
  clojure.lang.IRecord

  java.io.Serializable

  clojure.lang.IPersistentCollection

  ;; Class first, entries second, as a record does it: two unrelated types holding the same
  ;; fields are unequal, and so are a draft and the value it was opened from. A map answers
  ;; the same way from its side because `MapEquivalence` is absent -- `APersistentMap.equiv`
  ;; refuses any `IPersistentMap` that does not carry that marker.
  (equiv [this x]
    (and (identical? (class this) (class x))
         (clojure.lang.Util/equiv (into {} this) (into {} x))))

  (cons [this o]
    (cond
      (map? o)
      (reduce #(apply assoc %1 %2) this o)

      (instance? java.util.Map o)
      (reduce #(apply assoc %1 %2) this (into {} o))

      :else
      (if-let [[k v] (seq o)]
        (assoc this k v)
        this)))

  clojure.lang.IObj
  (withMeta [this mta]
    (potemkin.collections/with-meta* this mta))
  (meta [this]
    (potemkin.collections/meta* this))
  (meta* [this]
    nil)

  clojure.lang.Counted

  (count [this]
    (count (potemkin.collections/keys* this)))

  clojure.lang.Seqable
  (seq [this]
    (seq
     (map
      #(potemkin.PersistentMapProxy$MapEntry. this %)
      (potemkin.collections/keys* this))))

  clojure.core.protocols.CollReduce

  (coll-reduce
    [this f]
    (reduce f (seq this)))

  (coll-reduce
    [this f val]
    (reduce f val (seq this)))

  ;; `emit-defrecord`'s recipe: the class's hash mixed into the unordered hash of the
  ;; entries. Mixed by class name, so it is the same number in the next JVM as in this one.
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (clojure.lang.Util/hasheq (.getName (class this)))
             (hash-unordered-coll (or (seq this) ()))))

  Object
  ;; `emit-defrecord` leaves both of these to `APersistentMap`'s map contract, which is
  ;; class-blind: at the Java level a record equals a bare map holding the same entries, and
  ;; hashes as one. The class-aware answer is in `equiv` and `hasheq` above, which is where
  ;; `=` and `hash` read it.
  (hashCode [this]
    (clojure.lang.APersistentMap/mapHash this))

  (equals [this x]
    (clojure.lang.APersistentMap/mapEquals this x))

  ;; No `toString`: a record does not override it, so `str` of a value is `Object`'s
  ;; identity string. `pr-str` is the readable form.

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k default]
    (potemkin.collections/get* this k default))

  clojure.lang.Associative
  (containsKey [this k]
    (contains? (.keySet this) k))

  (entryAt [this k]
    (when (contains? (.keySet this) k)
      (potemkin.PersistentMapProxy$MapEntry. this k)))

  (assoc [this k v]
    (potemkin.collections/assoc* this k v))

  ;; A record has no empty: the fields are the type. `clojure.walk` asks for this only
  ;; where it has not found `IRecord`, which the template declares above.
  (empty* [this]
    (throw (UnsupportedOperationException.
            (str "Can't create empty: " (.getName (class this))))))

  (empty [this]
    (potemkin.collections/empty* this))

  java.util.Map
  (get [this k]
    (.valAt this k))
  (isEmpty [this]
    (empty? this))
  (size [this]
    (count this))
  (keySet [this]
    (set (potemkin.collections/keys* this)))
  (put [_ _ _]
    (throw (UnsupportedOperationException.)))
  (putAll [_ _]
    (throw (UnsupportedOperationException.)))
  (clear [_]
    (throw (UnsupportedOperationException.)))
  (remove [_ _]
    (throw (UnsupportedOperationException.)))
  (values [this]
    (->> this seq (map second)))
  (entrySet [this]
    (->> this seq set))

  java.util.Iterator
  (iterator [this]
    (clojure.lang.SeqIterator. this))

  clojure.lang.IPersistentMap
  (assocEx [this k v]
    (if (contains? this k)
      (throw (Exception. "Key or value already present"))
      (assoc this k v)))
  (without [this k]
    (potemkin.collections/dissoc* this k))

  potemkin.collections/PotemkinFn

  (invoke [this k]
    (potemkin.collections/get* this k nil))
  (invoke [this k default]
    (potemkin.collections/get* this k default)))

(defmacro def-record-type
  "`potemkin/def-map-type` over [[AbstractRecord]] rather than `AbstractMap`.

  The body is the same six definitions `def-map-type` takes --

    (get [this key default-value])
    (assoc [this key value])
    (dissoc [this key])
    (keys [this])
    (meta [this])
    (with-meta [this meta])

  -- and anything else the type defines or overrides. What differs is the template they are
  merged into."
  [name params & body]
  (let [fns '{get       get*
              assoc     assoc*
              dissoc    dissoc*
              keys      keys*
              empty     empty*
              with-meta with-meta*
              meta      meta*}
        classname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
    `(do
       (pt/deftype+ ~name ~params recognitum.collections/AbstractRecord
         ~@(map
            #(if (sequential? %)
               (list* (get fns (first %) (first %)) (rest %))
               %)
            body))
       ~classname)))
