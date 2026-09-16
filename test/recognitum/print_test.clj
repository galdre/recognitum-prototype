(ns recognitum.print-test
  "What a validated value prints as, and what reading it back does."
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [recognitum.audit-fixture :as fixture]
   [recognitum.core :as rec]
   [recognitum.heist :as heist]))

(use-fixtures :once fixture/audit)

(def job heist/italian-job)

(defn- err [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest the-tag-is-namespace-qualified
  ;; Non-namespaced tags are reserved for Clojure, and a tag whose *name* contains a dot is
  ;; routed to the reader's record-literal path, which needs a static create this type has no
  ;; way to provide. Qualifying by the defining namespace avoids both.
  (is (.startsWith (pr-str job) "#recognitum.heist/Heist{"))
  (is (contains? (rec/data-readers) 'recognitum.heist/Heist)))

(deftest a-value-round-trips-through-edn
  (let [back (edn/read-string {:readers (rec/data-readers)} (pr-str job))]
    (is (= job back))
    (is (instance? (class job) back))
    (is (rec/sealed? back)))
  (testing "and the tag printed is exactly the key :readers needs"
    (is (= job (edn/read-string {:readers {'recognitum.heist/Heist heist/map->Heist}} (pr-str job))))))

(deftest reading-is-validating
  ;; The reader goes through map->Heist, so a literal cannot smuggle in a value that would
  ;; have been rejected had it been constructed.
  (let [read-bad #(edn/read-string {:readers (rec/data-readers)} %)]
    (is (contains? (:failures (err #(read-bad "#recognitum.heist/Heist{:name \"\" :crew #{:a} :take 1 :cuts {:a 1}}")))
                   :recognitum.heist/name))
    (is (= [:recognitum.heist/cuts-sum-to-take]
           (keys (:failures (err #(read-bad "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 9 :cuts {:a 1}}"))))))
    (is (= ::rec/extraneous-keys
           (:error (err #(read-bad "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 1 :cuts {:a 1} :z 2}")))))
    (is (= ::rec/missing-field
           (:error (err #(read-bad "#recognitum.heist/Heist{:name \"x\" :crew #{:a} :take 1}")))))))

(deftest a-draft-prints-as-explicitly-unreadable
  ;; #< is the reader's own unreadable marker. A draft is unvalidated working state; a printed
  ;; form implying it round-trips would be claiming something untrue.
  (let [s (pr-str (rec/draft job))]
    (is (.startsWith s "#<Heist draft "))
    (is (thrown? RuntimeException (edn/read-string {:readers (rec/data-readers)} s)))
    (is (thrown? RuntimeException (read-string s)))))

(deftest the-library-does-not-register-for-you
  ;; recognitum.core never touches *data-readers*. This project's dev/data_readers.clj does,
  ;; which is the consumer half of the story and the only mechanism that actually works --
  ;; alter-var-root at load time would be masked by clojure.main's thread binding.
  (testing "dev/data_readers.clj puts the tag in *data-readers* at startup"
    (is (contains? *data-readers* 'recognitum.heist/Heist))
    (is (= job (read-string (pr-str job))))
    (is (= ::rec/check-failed
           (:error (err #(read-string "#recognitum.heist/Heist{:name \"\" :crew #{:a} :take 1 :cuts {:a 1}}"))))
        "and read-string validates too"))
  (testing "clojure.edn never consults it, whatever data_readers.clj says"
    (is (thrown? RuntimeException (edn/read-string (pr-str job))))
    (is (= job (edn/read-string {:readers (rec/data-readers)} (pr-str job)))))
  (testing "a type whose namespace has not been loaded is a placeholder, not a reader"
    (is (contains? (rec/data-readers) 'recognitum.heist/Heist)
        "data-readers only ever holds types actually defined in this process")))
