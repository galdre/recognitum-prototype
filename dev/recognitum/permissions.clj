(ns recognitum.permissions
  "An in-memory permission service: who may refund which order, and how much each person may
  refund. It logs every call, so a caller can see what a check asked and how often.")

(def ^:private on-file #{["dana" :refund 41] ["dana" :refund 42] ["eli" :refund 42]})

(defonce ^:private grants (atom on-file))

(defonce ^:private limits (atom {"dana" 5000 "eli" 1000}))

(defonce calls (atom []))

(defn allowed
  "A source: for each key `[user action order-id]`, whether the user holds that grant."
  [ks]
  (swap! calls conj [:allowed ks])
  (into {} (map (fn [k] [k (contains? @grants k)])) ks))

(defn refund-limit
  "A source: for each user, the largest refund in cents the user may issue. A user with no
  limit on file is absent from the answer."
  [users]
  (swap! calls conj [:refund-limit users])
  (select-keys @limits users))

(defn revoke!
  "Takes the grant `[user action order-id]` away."
  [user action order-id]
  (swap! grants disj [user action order-id])
  nil)

(defn restore!
  "Puts back every grant the service started with."
  []
  (reset! grants on-file)
  nil)
