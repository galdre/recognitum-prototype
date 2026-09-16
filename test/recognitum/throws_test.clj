(ns recognitum.throws-test
  "What each public function throws, held against what its docstring says it throws.

  The rule the library follows is that a caller learns what an operation can throw from
  that operation's docstring, and nowhere else: there is no table to consult and no list to
  keep in step with the code. That makes every docstring a claim about the implementation,
  and a claim nothing checks is a claim that rots -- the code learns a new throw, the
  docstring does not, and the first reader to find out is the one whose `catch` did not
  match.

  So the claims are checked. `throws` below is the set of `:error` keywords each public
  function can actually throw, worked out by hand from the call graph and maintained by
  hand: determining it mechanically would mean walking every form a function reaches
  through six layers of private helpers, and a walk that guessed wrong would be worse than
  a map somebody has to edit. The map is the thing this suite keeps in sync with the
  docstrings, and editing it is the deliberate act that adding a throw is supposed to be.

  Compile-time refusals are not in it. Those are thrown while the macro runs, by a form
  that will never produce a value, and their messages are the documentation -- there is no
  caller holding a value to catch them around."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]))

(def ^:private throws
  "Public var -> the `:error` keywords a call to it can throw.

  Hand-maintained, and meant to be. What a function reaches is the whole private call graph
  under it -- `check` reaches the fetch, so it reaches the two ways a batched fetch can
  misbehave; `drafting` reaches `seal`, so it throws everything a seal throws -- and that is
  a fact about the implementation rather than about the form, which is exactly why a reader
  needs telling and why a static walk over the source form would not find it.

  A function that cannot throw has an empty set, which is a claim too: it says the docstring
  has nothing to say about errors because there is nothing to say."
  {'certificates #{::rec/not-a-value}
   'check        #{::rec/not-a-value ::rec/unknown-check
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'check!       #{::rec/not-a-value ::rec/unknown-check ::rec/check-failed
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'check-all    #{::rec/not-a-value ::rec/unknown-check
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'check-all!   #{::rec/not-a-value ::rec/unknown-check ::rec/check-failed
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'clean        #{::rec/not-a-draft}
   'data-readers #{}
   'defrecord    #{}
   'draft        #{::rec/cannot-draft}
   'draft?       #{}
   'drafting     #{::rec/cannot-draft ::rec/cannot-seal ::rec/extraneous-keys
                   ::rec/check-failed ::rec/malformed-batch ::rec/incomplete-batch}
   'every-check  #{}
   'of-type?     #{}
   'passed       #{::rec/not-a-value}
   'recheck      #{::rec/not-a-value ::rec/unknown-check
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'recheck!     #{::rec/not-a-value ::rec/unknown-check ::rec/check-failed
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'recheck-all  #{::rec/not-a-value ::rec/unknown-check
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'recheck-all! #{::rec/not-a-value ::rec/unknown-check ::rec/check-failed
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'seal         #{::rec/cannot-seal ::rec/extraneous-keys ::rec/check-failed
                   ::rec/malformed-batch ::rec/incomplete-batch}
   'sealed?      #{}
   'unsealed     #{::rec/not-a-draft}})

(def ^:private generated-throws
  "The same, for the two constructors a definition writes. Their docstrings are written by
  the macro, so they are the one place a docstring is generated rather than typed -- and the
  batch-shape errors appear in them only for a type whose io-deps are batched, which
  `Heist`'s are."
  {'->Heist    #{::rec/check-failed ::rec/malformed-batch ::rec/incomplete-batch}
   'map->Heist #{::rec/check-failed ::rec/extraneous-keys
                 ::rec/malformed-batch ::rec/incomplete-batch}})

(defn- doc-of [ns-sym nm]
  (:doc (meta (ns-resolve ns-sym nm))))

(defn- unmentioned
  "Those of `es` the docstring `doc` does not name, as the short spelling a reader writes."
  [doc es]
  (remove #(str/includes? (str doc) (str ":" (name %))) (sort es)))

(deftest the-map-covers-every-public-var-and-nothing-else
  ;; A name added to the library without being added here would be a function whose
  ;; docstring nothing holds to anything.
  (is (= (set (keys (ns-publics 'recognitum.core))) (set (keys throws)))
      "every public var needs an entry, even if the entry is the empty set")
  (testing "and the two generated constructors exist under the names claimed for them"
    (doseq [nm (keys generated-throws)]
      (is (var? (ns-resolve 'recognitum.heist nm)) (str nm)))))

;; Candidate for deletion. This test asserts a docstring mentions every :error its function can
;; throw. It exists so part-of-speech drift -- the code learning a new throw, the docstring not --
;; gets caught. If it becomes a nuisance, delete it rather than weaken it.
(deftest every-docstring-names-every-error-its-function-can-throw
  (doseq [[nm es] throws]
    (let [doc (doc-of 'recognitum.core nm)]
      (is (some? doc) (str "recognitum.core/" nm " has no docstring at all"))
      (is (empty? (unmentioned doc es))
          (str "recognitum.core/" nm " can throw "
               (str/join ", " (unmentioned doc es))
               " and its docstring does not say so"))))
  (testing "including the constructors the macro writes the docstrings for"
    (doseq [[nm es] generated-throws]
      (let [doc (doc-of 'recognitum.heist nm)]
        (is (some? doc) (str "recognitum.heist/" nm " has no docstring at all"))
        (is (empty? (unmentioned doc es))
            (str "recognitum.heist/" nm " can throw "
                 (str/join ", " (unmentioned doc es))
                 " and its docstring does not say so"))))))

(deftest the-map-is-about-errors-this-library-actually-has
  ;; The hand-maintained side of a hand-maintained map: a keyword invented here would make
  ;; the test above demand a docstring say something untrue.
  (let [src (slurp (clojure.java.io/file "src/recognitum/core.clj"))]
    (doseq [e (into (sorted-set) cat (concat (vals throws) (vals generated-throws)))]
      (is (str/includes? src (str "{:error ::" (name e)))
          (str "core.clj throws no " e " anywhere")))))

(deftest the-heist-is-still-the-type-these-claims-are-about
  ;; `->Heist` reaches a batched fetch only because `Heist` declares one. If that stopped
  ;; being true the generated docstrings would stop carrying the batch errors, and the map
  ;; above would be wrong rather than the macro.
  (is (= ::rec/check-failed
         (:error (try (h/->Heist "" #{:charlie} 0 {}) nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(defn- err [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest every-refusal-carries-an-error-key-to-dispatch-on
  ;; Four refusals used to throw an `ex-info` with nothing but `:value` in it, which is a
  ;; throw a caller can only catch by class and read by message. A caller dispatches on
  ;; `:error` or it dispatches on prose.
  (is (= ::rec/cannot-draft (:error (err #(rec/draft 5)))))
  (is (= ::rec/cannot-draft (:error (err #(rec/drafting [x 5] x)))))
  (is (= ::rec/not-a-draft (:error (err #(rec/clean 5)))))
  (testing "and a merge of something that is not entries names the type it was refused for"
    (let [e (err #(conj h/italian-job 5))]
      (is (= ::rec/cannot-conj (:error e)))
      (is (= 'Heist (:record e))))))
