(ns recognitum.collections
  "The template both generated types are built on: a record rather than a map.

  `potemkin/def-map-type` builds its types on `potemkin.collections/AbstractMap`, which is
  the right template for a map-like thing and the wrong one for a record. It declares
  `clojure.lang.MapEquivalence` -- the marker `APersistentMap.equiv` consults, so a plain
  map answers `true` from *its* side whatever we do on ours -- its `equiv` asks only
  whether the entries match, its `hasheq` is the map's, its `empty` hands back an empty
  instance, and it implements neither `clojure.lang.IRecord` nor `java.io.Serializable`.
  Every one of those is a way for a validated value to be mistaken for the bare map it
  exists to be distinguishable from; the sharpest is `IRecord`, because `clojure.walk`
  reaches for `empty` when it does not find that marker and hands back a
  `PersistentArrayMap` where a value went in.

  `AbstractRecord` is the same mechanism with the record's answers: `equiv` compares nothing
  until the classes match and `hasheq` mixes the class in, `.equals` and `.hashCode` are
  `APersistentMap`'s map contract as a record's are, `empty` refuses as a record's does, and
  the two markers are present. Everything else is `AbstractMap`'s -- `IPersistentMap`,
  `Associative`, `ILookup`, `Counted`, `Seqable`, `IFn`, `IObj`, `java.util.Map`, iteration
  -- and the six-method contract is unchanged, so `def-record-type` takes the same body
  `def-map-type` does.

  Nothing here is part of the library's surface. It is a namespace rather than a block
  inside `recognitum.core` because it is a template and reads as one."
  (:require
   [potemkin.collections :as pc]
   [potemkin.types :as pt]))

(pt/def-abstract-type AbstractRecord

  ;; The six methods the generated types define, under the names `def-record-type` renames
  ;; them to. Potemkin's protocol rather than one of our own: the contract is identical and
  ;; a second copy of it would buy nothing but a second copy.
  potemkin.collections.PotemkinMap

  ;; What `record?` asks, and what `clojure.walk` asks before it considers rebuilding a
  ;; value as whatever `empty` gives it.
  clojure.lang.IRecord

  java.io.Serializable

  clojure.lang.IPersistentCollection

  ;; Class first, entries second, exactly as a record does it. Two unrelated types holding
  ;; the same fields are not equal, a draft is not equal to the value it was opened from,
  ;; and a bare map with the right keys is not a value of this type -- which is the
  ;; sentence the library exists to make true. The map's own `equiv` answers the same way
  ;; from its side because `MapEquivalence` is absent: `APersistentMap.equiv` refuses any
  ;; `IPersistentMap` that does not carry that marker.
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
  ;; entries. Mixed by name rather than by `Class` identity, so it is the same number in
  ;; the next JVM as in this one. `hash-unordered-coll` over the entry seq is what
  ;; `APersistentMap/mapHasheq` computes, spelled without the cast.
  clojure.lang.IHashEq
  (hasheq [this]
    (bit-xor (clojure.lang.Util/hasheq (.getName (class this)))
             (hash-unordered-coll (or (seq this) ()))))

  Object
  ;; `emit-defrecord` leaves both of these to `APersistentMap`'s map contract, class-blind:
  ;; at the Java level a record is a bare map with the same entries, symmetrically, and
  ;; hashes as one. The class-aware answer lives in `equiv` and `hasheq` above, which is
  ;; where `=` and `hash` read it; here the baseline rules, so these are the record's.
  (hashCode [this]
    (clojure.lang.APersistentMap/mapHash this))

  (equals [this x]
    (clojure.lang.APersistentMap/mapEquals this x))

  ;; No `toString`, for the same reason: a record does not override it, so `str` of one is
  ;; `Object`'s identity string. potemkin's template answered with the entries, which is
  ;; readable but unbounded -- `str` is what concatenation and logging call implicitly, and
  ;; a collection field would go into every line that mentions a value. The readable form
  ;; is `pr-str`, one call away.

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

  ;; A record has no empty: the fields are the type, so a value without them is not one.
  ;; `clojure.walk` asks for this only where it has already failed to find `IRecord`, so
  ;; after the marker above it is a refusal nothing in the standard library trips over.
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

  -- and anything else the type wants to define or override. What differs is the template
  they are merged into."
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
