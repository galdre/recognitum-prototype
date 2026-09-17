(ns recognitum.roster
  "An in-memory table standing in for the database an io-dep would reach. An io-dep is an
  ordinary function; what matters is that it is called at most once per validation call,
  only once the pure checks over the fields it names have passed, and that what it returns
  is judged by the checks naming it. A map and an atom show all three.

  `fetch-roster` and `fetch-rosters` throw on anything that is not a set of keywords, so a
  crew whose shape validation has not passed reaching either would be a throw rather than
  a failure report.")

(def table
  "Who the house knows about, and what it knows. Anyone not among the keys is a stranger,
  which is a different thing from someone on file and not vetted."
  {:charlie      {:vetted true}
   :lyle         {:vetted true}
   :left-ear     {:vetted true}
   :handsome-rob {:vetted true}
   :napster      {:vetted true}
   :mole         {:vetted false}})

(def calls
  "Every argument a fetch here has been handed, in order, as `[fn-name arg]` pairs."
  (atom []))

(defn reset-calls! [] (reset! calls []))

(defn call-count
  ([] (count @calls))
  ([f] (count (filter (fn [[g]] (= f g)) @calls))))

(defn fetch-roster
  "Everyone in `crew` the table knows about. Throws on anything that is not a set of
  keywords."
  [crew]
  (swap! calls conj [:fetch-roster crew])
  (when-not (and (set? crew) (every? keyword? crew))
    (throw (IllegalArgumentException. (str "not a crew: " (pr-str crew)))))
  (into #{} (filter table) crew))

(defn fetch-vetted
  "Those of `roster` the table has vetted."
  [roster]
  (swap! calls conj [:fetch-vetted roster])
  (into #{} (filter #(get-in table [% :vetted])) roster))

(defn fetch-rosters
  "The batched form of [[fetch-roster]]: every argument tuple in one call, answered as a map
  keyed by tuple. Keying by tuple is what lets the library deduplicate: a hundred values
  over five crews arrive as five tuples. Throws on anything that is not a crew, as the
  singular form does."
  [rows]
  (swap! calls conj [:fetch-rosters rows])
  (into {}
        (map (fn [[crew :as tuple]]
               (when-not (and (set? crew) (every? keyword? crew))
                 (throw (IllegalArgumentException. (str "not a crew: " (pr-str crew)))))
               [tuple (into #{} (filter table) crew)]))
        rows))
