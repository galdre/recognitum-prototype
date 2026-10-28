(ns recognitum.example.db
  "In-memory tables that stand in for a database, read through sources that log their calls.")

(defonce calls (atom []))

(def ^:private tables
  {:collections {1 {:owner-id 10 :writers #{"ann"} :curators #{}}
                 2 {:owner-id 11 :writers #{"bob"} :curators #{"cy"}}
                 3 {:owner-id 12 :writers #{"ann" "bob"} :curators #{}}}
   :users {10 {:active? true} 11 {:active? true} 12 {:active? false}}})

(defn- source [table]
  (fn [ks]
    (swap! calls conj [table ks])
    (select-keys (tables table) ks)))

(def collections "A source: collection id to collection." (source :collections))
(def users "A source: user id to user." (source :users))

(defn can-write? [user coll] (contains? (:writers coll) user))
(defn can-curate? [user coll] (contains? (:curators coll) user))
