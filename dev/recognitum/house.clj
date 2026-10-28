(ns recognitum.house
  "A stand-in for the house's records: who it has vetted. It is in memory and logs every call,
  so a caller can see what a check read and how often.")

(def ^:private on-file
  {"Charlie" true "Stella" true "Lyle" true "Handsome Rob" true "Left Ear" true "Steve" false})

(defonce calls (atom []))

(defn vetted
  "A source: for each name, whether the house has vetted that person. A name it has never
  heard of is not vetted."
  [names]
  (swap! calls conj [:vetted names])
  (into {} (map (fn [n] [n (get on-file n false)])) names))
