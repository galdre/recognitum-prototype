(ns recognitum.orders
  "An ordinary domain model: an order with lines, addresses and a discount, its fields' shapes,
  its invariants, and a bundle of address checks bound twice. Each printed line is a form, then
  what it returned or the error it threw.

      clojure -M:dev -m recognitum.orders"
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec]
   [recognitum.show :refer [show]]))

(defn non-blank? [s] (and (string? s) (not (str/blank? s))))

(defn line? [x]
  (and (map? x) (non-blank? (:sku x)) (pos-int? (:qty x)) (pos-int? (:price x))))

(defn subtotal [lines] (reduce + (map #(* (:qty %) (:price %)) lines)))

(def postcode-forms
  {"US" #"\d{5}" "DE" #"\d{5}" "NL" #"\d{4} ?[A-Z]{2}" "GB" #"[A-Z]{1,2}\d[A-Z\d]? ?\d[A-Z]{2}"})

;;; ------------------------------------------------------------------ addresses, a bundle

(rec/defchecks postal-address [street postcode country]
  rec/checks
  (street-given []
    "the address has a street"
    (non-blank? street))
  (country-served []
    "the country is one the shop ships to"
    (contains? postcode-forms country))
  (postcode-fits-country [country-served]
    "the postcode has its country's form"
    (and (string? postcode) (boolean (re-matches (postcode-forms country) postcode)))))

;; A type binds a bundle once; a second address binds a copy under new names.
(rec/defcopy billing-address postal-address
  {street-given          billing-street-given
   country-served        billing-country-served
   postcode-fits-country billing-postcode-fits-country})

;;; ------------------------------------------------------------------ the order

(defprotocol Priced (total [x] "What the customer pays, in cents."))

(rec/defproven Order [id customer lines shipping ^:optional billing ^:optional discount]
  postal-address
  (street   [] (:street shipping))
  (postcode [] (:postcode shipping))
  (country  [] (:country shipping))

  ^{:when-extant billing}
  billing-address
  (street   [] (:street billing))
  (postcode [] (:postcode billing))
  (country  [] (:country billing))

  rec/checks
  (id       [] "the id is a positive whole number" (pos-int? id))
  (customer [] "the customer is named" (non-blank? customer))
  (lines    [] "the lines are a vector of sku, quantity and price" (and (vector? lines) (every? line? lines)))
  (shipping [] "the shipping address is a map" (map? shipping))
  (billing  [] "the billing address is a map" (map? billing))
  (discount [] "the discount is a whole number of cents" (nat-int? discount))

  (has-lines []
    "the order has at least one line"
    (boolean (seq lines)))
  (one-line-per-sku []
    "no sku appears on two lines"
    (= (count lines) (count (set (map :sku lines)))))
  (discount-within-subtotal []
    "the discount is no more than the subtotal"
    (rec/when-extant discount (<= discount (subtotal lines))))

  Priced
  (total [_] (- (subtotal lines) (or discount 0))))

(def ^:private address {:street "1 Canal St" :postcode "1012 AB" :country "NL"})

(def ^:private two-lines
  [{:sku "tea" :qty 2 :price 450} {:sku "pot" :qty 1 :price 2900}])

(defn -main [& _]
  (println "-- a value runs every check, and answers its protocol\n")
  (let [o (map->Order {::id 7 ::customer "Ada" ::lines two-lines ::shipping address})]
    (show o)
    (show (total o))
    (show (rec/sealed? o))

    (println "-- a write reruns the checks that read what it changed\n")
    (show (total (assoc o ::discount 500)))
    (show (assoc o ::discount 5000))
    (show (update o ::lines conj {:sku "tea" :qty 1 :price 450}))
    (show (assoc o ::billing {:street "2 Kings Rd" :postcode "SW3 4ND" :country "GB"}))
    (show (assoc o ::billing {:street "" :postcode "12345" :country "FR"}))
    (show (assoc o ::coupon "SPRING")))

  (println "-- a failed constructor reports every failure; blocked checks did not run\n")
  (show (map->Order {::id 0 ::customer "Ada" ::lines [] ::shipping {:street "1 Main St" :postcode "9021" :country "US"}}))
  (show (map->Order {::id 8 ::customer "Ada" ::lines two-lines ::shipping "1 Main St"}))

  (println "-- an order built in steps is a draft until it seals\n")
  (let [d (-> (rec/draft Order) (assoc ::id 9 ::customer "Grace") (assoc ::lines two-lines))]
    (show (rec/check d (rec/except rec/every (rec/reading ::shipping))))
    (show (rec/seal d))
    (show (rec/seal (assoc d ::shipping {:street "5 Hauptstr." :postcode "10115" :country "DE"}))))

  (println "-- asking which checks a selection names\n")
  (show (rec/checks-of postal-address Order))
  (show (rec/checks-of (rec/reading ::discount) Order)))
