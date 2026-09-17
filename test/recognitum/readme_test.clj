(ns recognitum.readme-test
  "The README, held to what a test can check: every type definition in it compiles, the
  errors it names are ones the library throws, the commands it gives are aliases this
  project has, and the facilities it has to cover are mentioned."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [recognitum.core :as rec]))

(def ^:private readme (delay (slurp (io/file "README.md"))))

(def ^:private core-source (delay (slurp (io/file "src/recognitum/core.clj"))))

(defn- mentions? [token] (str/includes? @readme token))

;;; ------------------------------------ what it is called ------------------------------------

(deftest the-readme-names-the-library-the-library-is-called
  (is (str/starts-with? @readme "# recognitum")
      "the title is the library's name"))

;;; ------------------------------- every definition in it compiles -------------------------------

(def ^:private scratch
  (delay (let [n (create-ns 'recognitum.readme-scratch)]
           (binding [*ns* n]
             (eval '(clojure.core/refer-clojure))
             (eval '(clojure.core/require '[recognitum.core :as rec])))
           n)))

(defn- code-blocks
  "The contents of every fenced ```clojure block in `md`."
  [md]
  (->> (str/split md #"```")
       (keep-indexed (fn [i s] (when (odd? i) s)))
       (filter #(str/starts-with? % "clojure"))
       (mapv #(subs % (count "clojure")))))

(defn- eval-block
  "Read and evaluate `src` form by form, as `load` would, so that an `ns` form in it takes
  effect for what follows and `::` keywords resolve where the block says they do. Returns
  the Throwable it threw, or nil."
  [src]
  (binding [*ns* @scratch]
    (let [rdr (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. src))]
      (try
        (loop []
          (let [form (read {:eof ::eof :read-cond :allow} rdr)]
            (when-not (= ::eof form)
              (eval form)
              (recur))))
        nil
        (catch Throwable t t)))))

(defn- first-line [s] (str/trim (first (remove str/blank? (str/split-lines s)))))

(deftest every-type-definition-in-the-readme-compiles
  (let [blocks (filter #(str/includes? % "defrecord") (code-blocks @readme))]
    (is (seq blocks) "the README has to show how a type is defined")
    (doseq [b blocks]
      (let [t (eval-block b)]
        (is (nil? t) (str "README block `" (first-line b) "` does not compile: "
                          (some-> t ex-message)))))))

(deftest the-worked-example-shows-every-section
  (let [b (first (filter #(str/includes? % "defrecord") (code-blocks @readme)))]
    (doseq [section [":checks" ":io-deps" ":certificates" ":options"]]
      (is (str/includes? b section)
          (str "the worked example shows no " section " section")))))

;;; ---------------------------------------- the errors ----------------------------------------

(defn- errors-section
  "The `## Errors` section, up to whatever heading follows it."
  []
  (str (first (str/split (str (second (str/split @readme #"## Errors"))) #"\n## "))))

(deftest the-errors-section-states-the-one-shape
  (let [s (errors-section)]
    (is (seq s) "the README says what a refusal looks like")
    (is (str/includes? s ":error") "the key a caller dispatches on")
    (is (str/includes? s ":record") "the key that says which type refused")
    (is (str/includes? s ":failures") "and where the failures themselves are")
    (is (re-find #"(?s)shape validations.*invariant validations.*external validations" s)
        "and the order failures come back in: shape validations, then invariant validations, then external validations")
    (is (str/includes? s "`check`")
        "the same keys check answers with, so a reader learns one shape rather than two")
    (is (str/includes? s "docstring")
        "and where to find what any one operation throws")))

(deftest every-error-the-errors-section-names-is-one-the-library-throws
  (let [named (distinct (map second (re-seq #":recognitum\.core/([a-z-]+)" (errors-section))))]
    (is (seq named) "the section shows at least one error under its real name")
    (doseq [e named]
      (is (str/includes? @core-source (str "{:error ::" e))
          (str "core.clj never throws ::" e)))))

(deftest the-readme-says-the-error-keys-are-namespaced
  ;; `(= :check-failed (:error (ex-data e)))` is false for everything the library throws, so
  ;; the README has to show the qualified spelling.
  (is (mentions? ":recognitum.core/check-failed")))

;;; ------------------------------- the facilities it has to cover -------------------------------

(deftest the-readme-covers-the-draft-level-facility
  (doseq [token ["check!" "recheck" "recheck!" "passed" "certificates"
                 "check-all" "check-all!" "recheck-all"]]
    (is (mentions? token) (str "README says nothing about " token))))

(deftest the-readme-covers-telling-the-two-types-apart
  (doseq [token ["of-type?" "draft?" "sealed?" "instance?"]]
    (is (mentions? token) (str "README says nothing about " token))))

(deftest the-readme-covers-the-io-half
  (doseq [token [":io-deps" ":as rows" "{:io-limited true}" ":certificates"]]
    (is (mentions? token) (str "README says nothing about " token))))

(deftest the-readme-covers-what-meta-answers
  (doseq [token [":recognitum.core/passed" ":recognitum.core/certificates"]]
    (is (mentions? token) (str "README says nothing about " token))))

(deftest the-readme-covers-protocol-implementations
  (doseq [token ["unsealed" "protocol"]]
    (is (mentions? token) (str "README says nothing about " token))))

(deftest the-check-contract-is-stated
  ;; A truthy return that is not `true` is a failure, and nothing says so at run time.
  (is (or (mentions? "exactly `true`") (mentions? "`true` — exactly"))
      "the README has to say that a check passes by returning true and nothing else"))

;;; ----------------------------------------- the commands -----------------------------------------

(deftest every-command-the-readme-gives-is-one-this-project-has
  (let [aliases (set (keys (:aliases (edn/read-string (slurp (io/file "deps.edn"))))))
        named   (->> (re-seq #"clojure -[XM]:([a-zA-Z-]+)" @readme)
                     (map (comp keyword second))
                     set)]
    (is (seq named) "the README has to say how to run the thing")
    (doseq [a named]
      (is (contains? aliases a) (str "README gives clojure -X" a ", which deps.edn has not")))))

;;; ------------------------------------ the performance table ------------------------------------

(deftest the-performance-table-says-when-it-was-measured
  (let [perf (second (str/split @readme #"## What it costs"))]
    (is (some? perf) "the README reports what the library costs")
    (is (re-find #"\d{4}-\d{2}-\d{2}" (str perf)) "with the date it was taken")
    (is (str/includes? (str/lower-case (str perf)) "re-measure")
        "and how to re-measure it")))

(defn- perf-section []
  (second (str/split @readme #"## What it costs")))

(defn- perf-rows
  "The data rows of the performance table: each one `[label cell cell]`."
  []
  (->> (str/split-lines (str (perf-section)))
       (map str/trim)
       ;; past the header row and its separator, so the column names are not read as a
       ;; measurement of something called "hand-rolled".
       (drop-while #(not (re-find #"^\|\s*-" %)))
       rest
       (take-while #(str/starts-with? % "|"))
       (map #(mapv str/trim (rest (str/split % #"\|"))))
       (remove #(every? str/blank? %))))

(deftest the-performance-table-reports-every-shape-the-benchmark-measures
  ;; `recognitum.bench` prints the single operations, a batch of renames, and a batch of
  ;; add-members.
  (let [perf (str/lower-case (str (perf-section)))]
    (doseq [shape ["read" "merge" "construct" "rename" "add-member" "drafting"]]
      (is (str/includes? perf shape)
          (str "the performance table measures no " shape)))))

(deftest every-row-of-the-performance-table-carries-two-measurements
  (let [rows (perf-rows)]
    (is (<= 8 (count rows))
        "one row per shape the benchmark measures, both batch families included")
    (doseq [[label & cells] rows]
      (is (= 2 (count cells)) (str "row " (pr-str label) " is not a two-column comparison"))
      (doseq [c cells]
        (is (re-find #"\d" c) (str "row " (pr-str label) " has a cell with no figure in it"))
        (is (str/includes? c "ns") (str "row " (pr-str label) " has a cell with no unit on it"))))))
