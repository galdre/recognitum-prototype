(ns recognitum.cost-test
  "What the library costs, as a multiple of the hand-rolled `defrecord` in
  `recognitum.bench`, the one the README's table was measured against.

  The bounds are several times the measured ratio: a bound tight enough to catch a 20%
  regression fails on a busy machine. What they catch is a fixed cost paid per validation
  call, large enough to swamp the checks it runs.

  The second half pins what a call with nothing to run still does: refuse an undeclared
  key, fetch nothing, and answer from the record of which checks pass."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.bench :as b]
   [recognitum.core :as rec :refer [drafting]]
   [recognitum.roster :as roster])
  (:import
   (recognitum.bench BenchHeist)))

;;; ------------------------------------ the measurement ------------------------------------

(defn- ns-per
  "Nanoseconds per call of `f`, over `iters` calls.

  The last value is kept and inspected so that nothing measured here can be dropped as dead
  code -- every shape below allocates a value whose construction is the whole of what is
  being timed."
  [iters f]
  (let [sink (volatile! nil)
        t0   (System/nanoTime)]
    (dotimes [_ iters] (vreset! sink (f)))
    (let [elapsed (- (System/nanoTime) t0)]
      (when (nil? @sink) (throw (ex-info "the shape being measured produced nothing" {})))
      (/ (double elapsed) iters))))

(defn- ratio
  "`[multiple manual-ns library-ns]`: what `lib` costs as a multiple of what `manual` costs.

  Interleaved, and the best of several trials on each side, because the two are being
  compared to each other rather than to a number: whatever the machine was doing during one
  trial it was doing to both, and the minimum is the trial least of it landed on."
  [manual lib]
  (dotimes [_ 3] (ns-per 2000 manual) (ns-per 2000 lib))
  (let [[m l] (reduce (fn [[m l] _]
                        [(min m (ns-per 5000 manual)) (min l (ns-per 5000 lib))])
                      [Double/MAX_VALUE Double/MAX_VALUE]
                      (range 7))]
    [(/ l m) m l]))

(defn- within?
  [label bound [r m l]]
  (is (<= r bound)
      (format "%s: %.0f ns hand-rolled, %.0f ns library -- %.1fx, and the bound is %.1fx"
              label m l r bound)))

;;; ----------------------------- the multiples the table publishes -----------------------------

;; The fixtures are the benchmark's own. `b/r` is the hand-rolled record, `b/h` the
;; library's value over the same fields, and both sides run the same six checks.

(deftest a-single-write-inside-a-drafting-stays-within-its-multiple
  ;; One write invalidates one check, so nearly everything measured here is the library's
  ;; own overhead. The bound is high because every call site into `recognitum.core` is
  ;; megamorphic by the time the suite has defined forty-odd types, which costs the library
  ;; side roughly twice what the benchmark reads for the same shape and costs the
  ;; hand-rolled side nothing.
  (within? "one rename in a drafting" 12.0
           (ratio #(b/m-validate! (assoc b/r :name "n0") [:name])
                  #(drafting [x b/h] (assoc x ::b/name "n0")))))

(deftest constructing-through-a-drafting-stays-within-its-multiple
  ;; Every check on the type runs on both sides, so this row is the one where the
  ;; comparison is closest to being about the checks themselves.
  (within? "construct" 2.5
           (ratio #(b/m-validate! (b/map->PlainHeist b/base) [:name :crew :take :cuts])
                  #(drafting [x BenchHeist] (into x b/qbase)))))

(deftest a-three-field-update-inside-a-drafting-stays-within-its-multiple
  ;; The benchmark's add-member shape: three writes in one `drafting`, one round of checks
  ;; at the seal against the hand-rolled side's one round at the end.
  (within? "one add-member in a drafting" 2.0
           (ratio #(b/m-validate! (-> b/r
                                      (assoc :crew (conj (:crew b/r) :m0))
                                      (assoc :cuts (assoc (:cuts b/r) :m0 10))
                                      (assoc :take (+ (:take b/r) 10)))
                                  [:crew :cuts :take])
                  #(drafting [x b/h]
                     (-> x
                         (assoc ::b/crew (conj (::b/crew x) :m0))
                         (assoc ::b/cuts (assoc (::b/cuts x) :m0 10))
                         (assoc ::b/take (+ (::b/take x) 10)))))))

(deftest a-merge-onto-a-sealed-value-stays-within-its-multiple
  ;; Both sides run each applicable check once at the end, over the same values, so
  ;; whatever separates them is what the library does around the checks. Nothing here is
  ;; amortized over a batch, and megamorphic call sites cost this shape little, so the bound
  ;; is tighter: a quarter above parity is room for a busy machine, and half again is a data
  ;; structure per write.
  (within? "merge two fields onto a sealed value" 1.4
           (ratio #(b/m-merge b/r {:take 3000 :cuts b/valid-cuts})
                  #(merge b/h {::b/take 3000 ::b/cuts b/valid-cuts}))))

;;; ------------------------- what a call with nothing to run still does -------------------------

(def ^:private runs (atom []))
(defn- ran! [k] (swap! runs conj k) true)
(defn- ran [k] (count (filter #(= k %) @runs)))

(use-fixtures :each (fn [f] (reset! runs []) (roster/reset-calls!) (f)))

(rec/defrecord Tally
  [a b spare]
  {:checks
   {::a ([a] (and (ran! ::a) (int? a)))
    ::b ([b] (and (ran! ::b) (string? b)))}

   :certificates
   {::shape #{::a ::b}}})

;; A field no check mentions, so a draft can be in the state this suite needs: every check
;; passing and a field still unwritten.
(defn- tally [] (-> (rec/draft Tally) (assoc ::a 1) (assoc ::b "xx")))

(deftest a-seal-with-nothing-to-run-runs-nothing
  (let [d (tally)]
    (is (true? (rec/check d ::shape)) "every check the type has now passes")
    (reset! runs [])
    (is (rec/sealed? (rec/seal d))
        "and ::spare, which no check names, has no shape to be the wrong one")
    (is (zero? (count @runs)) "so the seal ran nothing at all")))

(deftest a-seal-with-nothing-to-run-still-refuses-an-undeclared-key
  (let [t (drafting [x Tally] (-> x (assoc ::a 1) (assoc ::b "xx") (assoc ::spare nil)))]
    (reset! runs [])
    (let [e (try (drafting [x t] (assoc x :bogus 1))
                 nil
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :recognitum.core/extraneous-keys (:error e))
          "a draft of a sealed value has every check passing, and an undeclared key is not a check")
      (is (= [:bogus] (:keys e)))
      (is (zero? (count @runs))))))

(deftest a-draft-of-a-sealed-value-seals-again-without-running-anything
  (let [t (drafting [x Tally] (-> x (assoc ::a 1) (assoc ::b "xx") (assoc ::spare nil)))]
    (reset! runs [])
    (is (= t (drafting [x t] x)) "the same value, reached by running nothing")
    (is (zero? (count @runs)))))

(deftest a-check-already-passing-answers-true-without-being-run-again
  (let [d (tally)]
    (is (true? (rec/check d ::a)))
    (is (= 1 (ran ::a)))
    (is (true? (rec/check d ::a)) "answered from the record, and as `true` rather than as a map")
    (is (= 1 (ran ::a)) "still once")))

;;; ------------------------- alignment, where some values have nothing to run -------------------------

(rec/defrecord Party
  [crew]
  {:checks
   {::crew  ([crew] (and (set? crew) (every? keyword? crew)))
    ::known ([crew ::roster] (every? roster crew))}

   :io-deps
   {::roster ([[crew] :as rows] (roster/fetch-rosters rows))}})

(defn- party [crew] (assoc (rec/draft Party) ::crew crew))

(deftest a-group-answers-in-order-with-a-hole-where-a-value-owed-nothing
  (let [ds [(party #{:charlie}) (party #{:stranger}) (party #{:lyle})]]
    (is (= [true] (rec/check-all [(first ds)] ::known))
        "the first value passes on its own, so the group call has nothing to run for it")
    (roster/reset-calls!)
    (let [rs (rec/check-all ds ::known)]
      (is (= [true {::known false} true] rs)
          "index-aligned, with the first value's answer read off its record")
      (is (= 1 (roster/call-count :fetch-rosters))
          "one batched call for the whole group")
      (is (= [[#{:stranger}] [#{:lyle}]]
             (second (first (filter (fn [[f]] (= :fetch-rosters f)) @roster/calls))))
          "and it was handed the tuples of the two values that still owed a fetch, in order"))))

(deftest a-group-where-nothing-is-pending-fetches-nothing
  (let [ds [(party #{:charlie}) (party #{:lyle})]]
    (is (= [true true] (rec/check-all ds ::known)))
    (roster/reset-calls!)
    (is (= [true true] (rec/check-all ds ::known))
        "asked again, and every answer is already recorded")
    (is (zero? (roster/call-count)) "so nothing was fetched a second time")))
