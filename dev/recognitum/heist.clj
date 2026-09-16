(ns recognitum.heist
  "A heist: a crew, a take, how the take is split, and what the house has on file about
  the people taking it.

  The worked example, and it is meant to be worked: every facility the library has appears
  here once, on one type, so that a reader can see what each of them costs in the form
  rather than in a docstring.

  Four of the checks are named after fields, which is the whole of what declares them those
  fields' shape checks; the two that span fields wait on the shape checks of the fields
  they name, so they restate nothing. A single `assoc` necessarily breaks either of those
  two -- which is the whole reason drafts exist. A seventh check is IO-bound: it names an
  io-dep rather than a field, so it judges something fetched from outside the value, the
  fetch waits until the crew it will carry is known to be a crew, and the fields behind it
  are `:io-limited` -- written under `drafting`, where N writes cost one round of checks at
  the seal rather than a query apiece on the way there.

  `recognitum.roster` is the fake standing in for the database. It is in-memory and it
  counts its calls, which is what lets the demo show a fetch happening once rather than
  assert that it does."
  (:require
   [clojure.string :as str]
   [recognitum.core :as rec :refer [drafting]]
   [recognitum.roster :as house]))

(defn non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn crew-set?        [x] (and (set? x) (seq x) (every? keyword? x)))
(defn non-neg-int?     [x] (and (int? x) (not (neg? x))))
(defn cuts-map?        [x] (and (map? x) (every? keyword? (keys x)) (every? non-neg-int? (vals x))))

(defprotocol Job
  "The impl layer, written once and installed on both generated types.

  On a draft each body runs as written and hands a draft back, so these compose; called on
  a sealed `Heist` the same body runs against a draft the wrapper opened and the result is
  sealed on the way out. So `add-member`, which passes through two states no single write
  to a sealed value could produce, needs no `drafting` form of its own -- and
  `total-payout`, which hands back a number, passes through untouched. `begin-revision` is
  the exception that names the rule: it marks its return `unsealed`, so the wrapper unwraps
  the marker instead of sealing, and `revise` is what that is for -- it calls the inner
  stage on its own draft receiver and hands the finished draft back bare."
  (rename       [h nm]        "Retitle the job.")
  (add-member   [h who cut]   "Put `who` on the crew with `cut`, and raise the take to fund it.")
  (promote      [h who bonus] "Raise `who`'s cut by `bonus`, and the take with it.")
  (total-payout [h]           "What the cuts come to.")
  (begin-revision [h title]
    "Open a revision under `title` and hand the draft back unsealed.

    The first of the two stages: the caller asked to start a revision, not to finish one,
    so this opts its return out of the seal. Called on a sealed `Heist` what comes back is
    a free draft, owned by nothing, to keep working on and seal when the revision is
    actually done; called on a draft it is the same body with nothing to opt out of.")
  (revise [h title who bonus]
    "Retitle the job and promote `who` by `bonus`, in one finished value.

    The second stage: inside a method body the receiver is a draft, so `begin-revision`
    hits the draft-side implementation, whose wrapper unwraps the marker and hands back the
    draft this body goes on writing to. Returned bare, so the wrapper seals it -- called on
    a sealed `Heist` this answers with a sealed `Heist`."))

(rec/defrecord Heist
  [name crew take cuts]

  {:options {:io-limited true}

   :checks
   {::name non-blank-string?
    ::crew crew-set?
    ::take non-neg-int?
    ::cuts cuts-map?

    ;; The four above are named after fields, which is what makes them those fields' shape
    ;; checks. These two are not, so they wait on the shape checks of the fields they name:
    ;; `::cuts-sum-to-take` runs only once `::take` and `::cuts` have passed. So it restates
    ;; neither, and cannot be handed a take that is not a number or a cuts that is not a
    ;; map -- which is why a throw out of here would be a bug in the check rather than a
    ;; verdict about the data.
    ::cuts-sum-to-take
    ("the cuts add up to the take"
     [take cuts] (= take (reduce + 0 (vals cuts))))

    ::only-crew-get-cuts
    ("only crew get a cut"
     [crew cuts] (every? crew (keys cuts)))

    ;; The IO-bound one: it names no field at all, only two io-deps, and the fields it
    ;; reads -- `crew`, through the fetch -- are worked out from that rather than declared.
    ;; A name the house has never heard of is not a problem; one it has heard of and has
    ;; not vouched for is.
    ::crew-is-vouched-for
    ("everyone the house has on file for this crew is vetted"
     [::roster ::vetted] (= roster vetted))}

   ;; Written the two ways a fetch can be written. `::roster` is batched: its argument
   ;; tuples arrive together, which is what makes one call serve a whole group of drafts,
   ;; and it answers a map keyed by the tuple so the library can distribute the results.
   ;; `::vetted` is singular, and names `::roster` rather than a field -- so the fetches
   ;; form a graph, run in its order, and `::roster` is fetched once for the pair.
   :io-deps
   {::roster ("who the house has on file for this crew"
              [[crew] :as rows] (house/fetch-rosters rows))
    ::vetted ("those of them it has vouched for"
              [::roster] (house/fetch-vetted roster))}

   ;; Groups you can establish on a draft before it is complete. Holding one is derived
   ;; from which checks pass, so nothing here records anything; `::shape` is what a
   ;; half-built value can be held to, and `::payout` what the arithmetic owes.
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
