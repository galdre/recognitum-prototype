(ns recognitum.heist
  "A heist: a crew, a take, how the take is split, and what the house has on file about
  the people taking it. The worked example: every facility the library has appears here
  once, on one type.

  Four checks are named after fields, so they are those fields' shape validations. Two
  invariant validations span fields and wait on the shape validations of the fields they
  name, so a single `assoc` necessarily breaks one of them. One external validation names
  two io-deps rather than a field, so the fields behind it are `:io-limited` and are
  written under `drafting`.

  `recognitum.roster` stands in for the database. It is in-memory and counts its calls."
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec :refer [drafting]]
   [recognitum.roster :as house]))

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(defprotocol Job
  "Written once and installed on both generated types. On a draft each body runs as written
  and returns a draft; on a sealed `Heist` the same body runs against a draft the wrapper
  opened, and the result is sealed on the way out. So `add-member`, which passes through
  states no single write to a sealed value could produce, needs no `drafting` of its own,
  and `total-payout`, which returns a number, passes through untouched. `begin-revision`
  marks its return `unsealed`, so the wrapper unwraps the marker instead of sealing;
  `revise` calls it on its own draft receiver and returns the finished draft bare."
  (rename       [h nm]        "Retitle the job.")
  (add-member   [h who cut]   "Put `who` on the crew with `cut`, and raise the take to fund it.")
  (promote      [h who bonus] "Raise `who`'s cut by `bonus`, and the take with it.")
  (total-payout [h]           "What the cuts come to.")
  (begin-revision [h title]
    "Open a revision under `title` and return the draft unsealed.

    Called on a sealed `Heist`, what comes back is a draft owned by no `drafting`, to keep
    working on and seal when the revision is done; called on a draft it is the same body
    with nothing to opt out of.")
  (revise [h title who bonus]
    "Retitle the job and promote `who` by `bonus`, in one finished value.

    Inside a method body the receiver is a draft, so `begin-revision` runs the draft-side
    implementation, which unwraps the marker. The draft is returned bare, so called on a
    sealed `Heist` this answers with a sealed `Heist`."))

(rec/defrecord Heist
  [name crew take cuts]

  {:options {:io-limited true}

   :checks
   {::name non-blank-string?
    ::crew crew-set?
    ::take non-neg-int?
    ::cuts cuts-map?

    ;; Invariant validations: neither is named after a field, so each waits on the shape
    ;; validations of the fields it names. `::cuts-sum-to-take` runs only once `::take` and
    ;; `::cuts` have passed, so it restates neither.
    ::cuts-sum-to-take
    ("the cuts add up to the take"
     [take cuts] (= take (reduce + 0 (vals cuts))))

    ::only-crew-get-cuts
    ("only crew get a cut"
     [crew cuts] (every? crew (keys cuts)))

    ;; The external validation: it names two io-deps and no field, so the fields it reads --
    ;; `crew`, through the fetch -- are worked out from the io-deps' arglists. A name the
    ;; house has never heard of is not a problem; one it has on file and has not vouched for
    ;; is.
    ::crew-is-vouched-for
    ("everyone the house has on file for this crew is vetted"
     [::roster ::vetted] (= roster vetted))}

   ;; Written the two ways a fetch can be written. `::roster` is batched: its argument
   ;; tuples arrive together, so one call serves a whole group of drafts, and it answers a
   ;; map keyed by tuple. `::vetted` is singular and names `::roster` rather than a field,
   ;; so the fetches run in dependency order and `::roster` is fetched once for the pair.
   :io-deps
   {::roster ("who the house has on file for this crew"
              [[crew] :as rows] (house/fetch-rosters rows))
    ::vetted ("those of them it has vouched for"
              [::roster] (house/fetch-vetted roster))}

   ;; `::shape` can be run against a half-built value; `::payout` covers the arithmetic.
   :certificates
   {::shape  #{::name ::crew ::take ::cuts}
    ::payout #{::cuts-sum-to-take ::only-crew-get-cuts}}}

  Job
  (rename [h nm] (assoc h ::name nm))

  (add-member [h who cut]
    (-> h
        (assoc ::crew (conj (::crew h) who))
        (assoc ::cuts (assoc (::cuts h) who cut))
        (assoc ::take (+ (::take h) cut))))

  (promote [h who bonus]
    (-> h
        (assoc ::cuts (update (::cuts h) who + bonus))
        (assoc ::take (+ (::take h) bonus))))

  (total-payout [h] (reduce + 0 (vals (::cuts h))))

  (begin-revision [h title] (rec/unsealed (assoc h ::name title)))

  (revise [h title who bonus] (-> (begin-revision h title) (promote who bonus))))

(def italian-job
  (drafting [h Heist]
    (merge h {::name "The Italian Job"
              ::crew #{:charlie :lyle :left-ear}
              ::take 3000
              ::cuts {:charlie 1000 :lyle 1000 :left-ear 1000}})))

(defn apply-changeset
  "Apply one changeset to a draft. An ordinary draft -> draft function, so a reduce over a
  hundred of them is one round of checks rather than a hundred."
  [h {:keys [op] :as cs}]
  (case op
    :add-member (add-member h (:who cs) (:cut cs))
    ;; A changeset is a plain map, not a Heist -- its keys stay bare.
    :rename     (rename h (:name cs))))
