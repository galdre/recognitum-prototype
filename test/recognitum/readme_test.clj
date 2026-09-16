(ns recognitum.readme-test
  "The README, held to the same standard as the code it describes.

  Documentation drifts silently: nothing fails when a file says the library still takes
  `^{:valid pred}` fields, and the only reader who finds out is the one who tried it. The
  history here is exactly that -- the prose was left behind by four separate changes before
  anyone counted the damage. So the claims the README makes that can be mechanically
  checked are checked: that every type definition in it compiles, that the errors it
  documents are the errors the library throws, that the commands it gives are commands this
  project has, and that the facilities it omits mentioning are not the ones a reader would
  most need told about.

  It cannot check that the prose is any good. It can check that it is not false."
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
      "the title still carried the name the project had before the migration"))

;;; -------------------------------- nothing that no longer exists --------------------------------

(deftest the-readme-documents-nothing-the-library-deleted
  ;; Every token here named a real facility once. Each is now a thing a reader would try
  ;; and find missing, which is worse than an omission: it is a false claim with an
  ;; example attached.
  (doseq [token ["^{:valid" "{:valid " ":invalid-field" ":invariant-violated"
                 ":depends-on" "defvalidated" "validated-record"]]
    (is (not (mentions? token)) (str "README still documents " token)))
  (testing "and the word invariant, which the check model replaced wholesale"
    (is (not (str/includes? (str/lower-case @readme) "invariant"))
        "checks are the only kind of check there is; a cross-field one is still a check")))

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
  ;; A README example is a promise that this is what you type. The cheapest way to keep
  ;; that promise honest is to type it.
  (let [blocks (filter #(str/includes? % "defrecord") (code-blocks @readme))]
    (is (seq blocks) "the README has to show how a type is defined")
    (doseq [b blocks]
      (let [t (eval-block b)]
        (is (nil? t) (str "README block `" (first-line b) "` does not compile: "
                          (some-> t ex-message)))))))

(deftest the-worked-example-is-the-one-the-spec-describes
  (let [b (first (filter #(str/includes? % "defrecord") (code-blocks @readme)))]
    (doseq [section [":checks" ":io-deps" ":certificates" ":options"]]
      (is (str/includes? b section)
          (str "the worked example shows no " section " section")))))

;;; ---------------------------------------- the errors ----------------------------------------

(defn- error-rows
  "The `:error` values the README's error table lists, in the order it lists them."
  []
  (let [table (second (str/split @readme #"## Errors"))
        lines (take-while #(not (str/starts-with? % "## ")) (str/split-lines (or table "")))
        ;; past the header row and its separator, so `:error` -- the column's name -- is
        ;; not read as one of the errors it heads.
        rows  (rest (drop-while #(not (re-find #"^\|\s*-" (str/trim %))) lines))]
    (->> rows
         (keep #(second (re-find #"^\|\s*`:(\S+)`" (str/trim %))))
         (mapv keyword))))

(deftest the-error-table-lists-exactly-what-the-library-throws
  (is (= #{:check-failed :unknown-field :extraneous-keys :missing-field
           :cannot-dissoc :cannot-seal :io-limited}
         (set (error-rows)))
      "every error a caller can dispatch on, and nothing that is no longer thrown")
  (testing "and each one is a keyword the source actually throws"
    (doseq [e (error-rows)]
      (is (str/includes? @core-source (str "::" (name e)))
          (str "core.clj never throws ::" (name e))))))

(deftest the-readme-says-the-error-keys-are-namespaced
  ;; The table writes them short for readability, which is a trap unless the text says so:
  ;; `(= :check-failed (:error (ex-data e)))` is false for every value the library throws.
  (is (mentions? ":recognitum.core/check-failed")))

;;; ------------------------------- the facilities it has to cover -------------------------------

(deftest the-readme-covers-the-draft-level-facility
  ;; The half of the library the README has said least about: running checks before the
  ;; value is finished is most of why drafts carry a verdict at all.
  (doseq [token ["check!" "certify" "certify!" "passing" "holding"
                 "check-all" "certify-all"]]
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
  ;; A check that returns a truthy non-true is read as a failure. Nothing warns you at run
  ;; time, so the README is where a reader has to be told.
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
  ;; It was measured against an implementation two phases ago. Keeping the numbers is
  ;; defensible; keeping them undated is not.
  (let [perf (second (str/split @readme #"## What it costs"))]
    (is (some? perf) "the README still reports what the library costs")
    (is (re-find #"\d{4}-\d{2}-\d{2}" (str perf)) "with the date it was taken")
    (is (str/includes? (str/lower-case (str perf)) "re-measure")
        "and a standing warning not to trust it to three figures")))

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

(deftest the-performance-table-does-not-disclaim-its-own-numbers
  ;; A table that says its own figures were measured against an implementation two phases
  ;; ago is not a measurement; it is an apology for one. Either re-run the benchmark or
  ;; delete the table.
  (let [perf (str/lower-case (str (perf-section)))]
    (doseq [token ["taken before" "predate" "before checks became"]]
      (is (not (str/includes? perf token))
          (str "the performance table still disclaims its own numbers: " token)))))

(deftest the-performance-table-reports-every-shape-the-benchmark-measures
  ;; `recognitum.bench` prints three sections -- the single operations, a batch of renames,
  ;; and a batch of add-members -- and the table used to carry one row out of the last two.
  ;; The rows a table leaves out are the ones a reader cannot check, and the batched shapes
  ;; are where the library is doing something a hand-rolled version has to be written to
  ;; match.
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
