(ns recognitum.roster
  "An in-memory table standing in for the database an io-dep would really reach.

  Nothing in this library needs real infrastructure to be exercised: an io-dep is an
  ordinary function, and what matters about it is that it is called at most once per
  validation call, that it is called only once the fields it names are known to be
  well-formed, and that what it returns is judged by the checks naming it rather than by
  the library. A map and an atom show all three.

  `fetch-roster` refuses anything that is not a set of keywords, and that refusal is the
  point: it is what the gate exists to keep away from. A crew the checks over it have not
  yet passed never reaches here, so the throw a malformed crew would cause is one nobody
  sees -- and if it were ever seen, the gate would be broken.")

(def table
  "Who the house knows about, and what it knows. The keys are the only people a fetch can
  find; anyone else is a stranger, which is a fact the checks are free to make something
  of -- as is the difference between someone it has on file and has vouched for and
  someone it has on file and has not."
  {:charlie      {:vetted true}
   :lyle         {:vetted true}
   :left-ear     {:vetted true}
   :handsome-rob {:vetted true}
   :napster      {:vetted true}
   :mole         {:vetted false}})

(def calls
  "Every argument a fetch here has been handed, in order -- `[fn-name arg]` pairs. A fetch
  is supposed to happen once per validation call however many checks name it, and never at
  all where the gate refuses; both are counted rather than argued about."
  (atom []))

(defn reset-calls! [] (reset! calls []))

(defn call-count
  ([] (count @calls))
  ([f] (count (filter (fn [[g]] (= f g)) @calls))))

(defn fetch-roster
  "Everyone in `crew` the table knows about.

  Throws on anything that is not a set of keywords. A fetch that is handed garbage is a
  fetch that should never have run, and saying so loudly is worth more here than a nil."
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
  keyed by the tuple it was asked about.

  Keying by the tuple rather than aligning positionally is what lets the library do the
  deduplication rather than the author -- a hundred values referencing five crews arrive
  here as five tuples, and each of them is answered once.

  Throws on anything that is not a crew, for the same reason the singular form does: a fetch
  handed garbage is a fetch the gate should have kept away, and saying so loudly is worth
  more here than a nil."
  [rows]
  (swap! calls conj [:fetch-rosters rows])
  (into {}
        (map (fn [[crew :as tuple]]
               (when-not (and (set? crew) (every? keyword? crew))
                 (throw (IllegalArgumentException. (str "not a crew: " (pr-str crew)))))
               [tuple (into #{} (filter table) crew)]))
        rows))
