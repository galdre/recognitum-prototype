(ns recognitum.audit-removal-test
  "The invariant-key audit is gone, and stays gone.

  It existed to catch an invariant reading a key its `:keys` did not declare -- a
  declaration that could disagree with the body it described. There is no such
  declaration any more: a check's dependencies *are* its arglist, the body sees exactly
  what the arglist names and nothing else, so the disagreement the audit looked for is not
  expressible. A proxy watching for it would be watching for nothing.

  So this namespace asserts an absence. It is written against the files rather than the
  API because that is where the remains would be: a dynamic var nothing reads, a fixture
  binding it, an alias running the suite under it, and a README section telling people to
  use all three."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(defn- root-cause [^Throwable t] (if-let [c (.getCause t)] (recur c) t))

(def ^:private scratch
  ;; Evaluating a whole `defrecord` form interns vars and defines classes. Do that somewhere
  ;; other than this namespace.
  (delay (let [n (create-ns 'recognitum.audit-removal-scratch)]
           (binding [*ns* n] (eval '(clojure.core/refer-clojure)))
           n)))

(defn- compiles
  "Evaluate `form` in a scratch namespace; the Throwable it threw, or nil."
  [form]
  (binding [*ns* @scratch] (try (eval form) nil (catch Throwable t t))))

(defn- compile-data [form] (some-> (compiles form) root-cause ex-data))

(def ^:private this-file
  "This namespace names every token it forbids, so it is the one file exempt from the scan."
  "test/recognitum/audit_removal_test.clj")

(def ^:private forbidden
  "Every spelling the audit had. Searched for as plain text, so a mention in a docstring or
  a comment counts -- a var nothing reads is exactly the kind of thing that survives by
  being mentioned nowhere but its own definition."
  ["*audit-invariant-keys*"
   "KeyAuditor"
   "undeclared-invariant-key"
   "validated-record.audit"
   "audit-fixture"
   "audit_fixture"
   "recognitum.audit-test"
   "test-audit"])

(defn- source-files
  "Every Clojure file the project ships or tests with, this one excepted."
  []
  (->> ["src" "test" "dev" "bench"]
       (mapcat #(file-seq (io/file %)))
       (filter #(.isFile ^java.io.File %))
       (filter #(re-find #"\.cljc?$" (.getName ^java.io.File %)))
       (remove #(= this-file (str/replace (.getPath ^java.io.File %) "\\" "/")))))

(defn- hits
  "[path line-number line] for every line of `files` containing one of `forbidden`."
  [files]
  (for [^java.io.File f files
        [n line] (map-indexed (fn [i l] [(inc i) l]) (str/split-lines (slurp f)))
        :when (some #(str/includes? line %) forbidden)]
    [(.getPath f) n line]))

;;; ------------------------------- the var and the machinery -------------------------------

(deftest the-audit-var-is-gone
  ;; It was left inert by B1 rather than removed, so "it does nothing" is not the assertion
  ;; that matters -- it has to not be there, or the fixture and the alias have something to
  ;; bind and the README has something to recommend.
  (is (nil? (ns-resolve 'recognitum.core '*audit-invariant-keys*))))

(deftest nothing-in-the-core-namespace-mentions-the-audit
  (testing "no interned var names it"
    (is (empty? (->> (ns-interns 'recognitum.core)
                     keys
                     (map name)
                     (filter #(str/includes? (str/lower-case %) "audit"))))))
  (testing "nor does anything it refers out"
    (is (empty? (->> (ns-publics 'recognitum.core)
                     keys
                     (map name)
                     (filter #(str/includes? (str/lower-case %) "audit")))))))

(deftest the-recording-proxy-is-gone
  ;; The auditor was a type; a leftover class would still be loadable even with no var
  ;; pointing at it.
  (doseq [c ["recognitum.core.KeyAuditor" "recognitum.core.KeyAuditorProxy"]]
    (is (thrown? ClassNotFoundException (Class/forName c))
        (str c " should no longer exist"))))

;;; ------------------------------------ the test harness ------------------------------------

(deftest the-fixture-namespace-is-gone
  (testing "no file on the classpath"
    (is (nil? (io/resource "recognitum/audit_fixture.clj")))
    (is (nil? (io/resource "recognitum/audit_test.clj"))))
  (testing "and requiring it fails"
    (is (thrown? Exception (require 'recognitum.audit-fixture :reload)))))

(deftest nothing-in-the-tree-mentions-the-audit
  ;; Four suites used `fixture/audit` as a `:once` fixture. They have to stop, and stopping
  ;; is not optional -- a require of a deleted namespace does not load.
  (let [offenders (hits (source-files))]
    (is (empty? offenders)
        (str "audit remnants still in the tree:\n"
             (str/join "\n" (map (fn [[p n l]] (str p ":" n ": " (str/trim l))) offenders))))))

(deftest the-test-audit-alias-is-gone
  (let [f (io/file "deps.edn")]
    (is (.exists f) "test must run from the repository root")
    (testing "no alias declares it"
      (let [aliases (:aliases (edn/read-string (slurp f)))]
        (is (contains? aliases :test) "the ordinary suite is still there")
        (is (not (contains? aliases :test-audit)))
        (testing "and no surviving alias turns the audit on by system property"
          (is (empty? (for [[k v] aliases
                            opt (:jvm-opts v)
                            :when (str/includes? opt "audit")]
                        [k opt]))))))
    (testing "nor does a comment still advertise it"
      (is (not (str/includes? (str/lower-case (slurp f)) "audit"))))))

;;; --------------------------------------- the README ---------------------------------------

(deftest the-readme-no-longer-documents-the-audit
  (let [readme (slurp (io/file "README.md"))]
    (testing "the section, the var, the alias and the error row are all gone"
      (doseq [token ["*audit-invariant-keys*"
                     ":test-audit"
                     ":undeclared-invariant-key"
                     "Checking your `:keys`"]]
        (is (not (str/includes? readme token))
            (str "README still mentions " token))))
    (testing "and the word itself does not survive somewhere else in it"
      (is (not (str/includes? (str/lower-case readme) "audit"))))
    (testing "while the rest of the document is intact"
      (is (str/includes? readme "## Errors"))
      (is (str/includes? readme "clojure -X:test")))))

;;; ------------------------------ why there is nothing to audit ------------------------------

(deftest the-bug-the-audit-hunted-does-not-compile
  ;; This is what replaced the audit, and it is not instrumentation: a check body reaching a
  ;; field its arglist does not name is a compile error, so there is no run-time moment at
  ;; which a proxy could watch for it.
  (let [t (compiles '(recognitum.core/defrecord Peeker
                       [a b]
                       {:scratch/peek ([a] (int? b))}))]
    (is (some? t) "reading an undeclared field must not compile")
    (is (re-find #"Unable to resolve symbol: b" (ex-message (root-cause t)))))
  (testing "and a dependency the type does not declare is refused by name"
    (let [e (compile-data '(recognitum.core/defrecord Stranger
                             [a]
                             {:scratch/c ([a z] (int? a))}))]
      (is (= ::rec/unknown-dependency (:error e)))
      (is (= 'z (:dependency e))))))
