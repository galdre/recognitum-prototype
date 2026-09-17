(ns recognitum.throws-test
  "What each public function throws, held against what its docstring says it throws.

  `throws` is the set of `:error` keywords each public function can throw, worked out from
  the call graph and maintained by hand; adding a throw to the library means adding it
  there. Compile-time refusals are not in it: those are thrown while the macro runs, by a
  form that never produces a value."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]
   [recognitum.heist :as h]))

(def ^:private throws
  "Public var -> the `:error` keywords a call to it can throw, the private call graph under
  it included: `check` reaches the fetch, so it throws what a batched fetch can throw, and
  `drafting` seals, so it throws what `seal` throws. An empty set claims the function cannot
  throw."
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
  "The same, for the two constructors the macro writes docstrings for. The batch-shape
  errors appear in them only for a type whose io-deps are batched, as `Heist`'s are."
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
  (is (= (set (keys (ns-publics 'recognitum.core))) (set (keys throws)))
      "every public var needs an entry, even if the entry is the empty set")
  (testing "and the two generated constructors exist under the names claimed for them"
    (doseq [nm (keys generated-throws)]
      (is (var? (ns-resolve 'recognitum.heist nm)) (str nm)))))

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
  ;; A keyword invented here would make the test above demand that a docstring say something
  ;; untrue.
  (let [src (slurp (clojure.java.io/file "src/recognitum/core.clj"))]
    (doseq [e (into (sorted-set) cat (concat (vals throws) (vals generated-throws)))]
      (is (str/includes? src (str "{:error ::" (name e)))
          (str "core.clj throws no " e " anywhere")))))

(deftest the-heist-is-still-the-type-these-claims-are-about
  ;; `generated-throws` names `Heist`'s constructors, so `Heist` has to be a type whose
  ;; constructors validate.
  (is (= ::rec/check-failed
         (:error (try (h/->Heist "" #{:charlie} 0 {}) nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(defn- err [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest every-refusal-carries-an-error-key-to-dispatch-on
  (is (= ::rec/cannot-draft (:error (err #(rec/draft 5)))))
  (is (= ::rec/cannot-draft (:error (err #(rec/drafting [x 5] x)))))
  (is (= ::rec/not-a-draft (:error (err #(rec/clean 5)))))
  (testing "and a merge of something that is not entries names the type it was refused for"
    (let [e (err #(conj h/italian-job 5))]
      (is (= ::rec/cannot-conj (:error e)))
      (is (= 'Heist (:record e))))))
