(ns recognitum.authz
  "Authorization as checks: a refund request whose value proves that the permission service
  allowed it. Each printed line is a form, then what it returned or the error it threw, then
  the calls the permission service received.

      clojure -M:dev -m recognitum.authz"
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec]
   [recognitum.permissions :as perms]
   [recognitum.show :refer [show *logs*]]))

(rec/defconcern access "Checks that ask the permission service.")

(defn non-blank? [s] (and (string? s) (not (str/blank? s))))

(rec/defproven Refund [actor order-id amount ^:optional note]
  rec/checks
  (actor    [] "the actor is named" (non-blank? actor))
  (order-id [] "the order id is a positive whole number" (pos-int? order-id))
  (amount   [] "the amount is a positive whole number of cents" (pos-int? amount))
  (note     [] "the note is text" (string? note))

  ^::access
  (may-refund []
    "the actor holds the refund grant for the order"
    (true? (rec/fetch perms/allowed [actor :refund order-id])))

  ^::access
  (within-limit [may-refund]
    "the amount is within the actor's refund limit"
    (<= amount (or (rec/fetch perms/refund-limit actor) 0))))

(defn request->draft
  "A draft of a Refund from a request: the actor from the session, the rest from the form's
  string parameters. A parameter that does not parse is held as given, so its shape check
  fails on it."
  [{:keys [session params]}]
  (let [number (fn [s] (or (some-> s parse-long) s))]
    (cond-> (assoc (rec/draft Refund)
                   ::actor (:user session)
                   ::order-id (number (get params "order"))
                   ::amount (number (get params "amount")))
      (contains? params "note") (assoc ::note (get params "note")))))

(defn issue!
  "Issues the refund `r`. Throws unless `r` is a sealed Refund."
  [r]
  (when-not (and (instance? Refund r) (rec/sealed? r))
    (throw (ex-info "not a sealed Refund" {:value r})))
  (str "refunded " (::amount r) " cents on order " (::order-id r) " for " (::actor r)))

(defn- request [user params] {:session {:user user} :params params})

(def ^:private dana-refunds-1500
  (request "dana" {"order" "42" "amount" "1500" "note" "damaged in transit"}))

(defn -main [& _]
  (perms/restore!)
  (binding [*logs* {"permission service" perms/calls}]
    (println "-- a request becomes a draft; the seal asks the permission service\n")
    (let [d (request->draft dana-refunds-1500)]
      (show d)
      (show (issue! d))
      (show (issue! (rec/seal d))))

    (println "-- a failed seal reports each failed check and the checks it blocked\n")
    (show (rec/seal (request->draft (request "eli" {"order" "42" "amount" "2500"}))))
    (show (rec/seal (request->draft (request "eli" {"order" "41" "amount" "15.00"}))))
    (show (rec/check (request->draft (request "eli" {"order" "41" "amount" "15.00"})) rec/every))

    (println "-- a draft remembers what passed: the shape checks first, the service at the seal\n")
    (let [d (request->draft (request "eli" {"order" "42" "amount" "800"}))]
      (show (rec/check d rec/shape))
      (show (rec/proven-checks d))
      (show (rec/seal d)))

    (println "-- requests checked in one pass share the service's calls\n")
    (show (rec/check-all [(request->draft dana-refunds-1500)
                          (request->draft (request "eli" {"order" "42" "amount" "900"}))
                          (request->draft (request "eli" {"order" "41" "amount" "900"}))]
                         access))

    (println "-- after a grant is revoked, a recheck asks the service again; a seal does not\n")
    (let [r (rec/seal (request->draft dana-refunds-1500))]
      (show (rec/check r access))
      (show (perms/revoke! "dana" :refund 42))
      (show (rec/recheck r access))
      (show (rec/seal (rec/draft r))))))
