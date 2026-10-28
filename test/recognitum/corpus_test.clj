(ns recognitum.corpus-test
  "Loads every file of the corpus under `test/corpus` and holds the macros to the expectation in
  its header: a file that reports nothing loads; a file that reports a mistake fails to load
  with a message naming a token the header names, or, for a mistake the macros only warn
  about, loads and prints a warning naming it."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def ^:private root (io/file "test/corpus"))

(def ^:private warned-only
  "The mistakes the macros report with a warning, and so load."
  #{"mistakes/m11a.clj" "mistakes/m11b.clj"})

(defn- header [text]
  (let [lines (take-while #(str/starts-with? % ";;") (str/split-lines text))
        joined (str/join "\n" (map #(subs % 2) lines))]
    (edn/read-string (subs joined (+ (str/index-of joined "expect") (count "expect"))))))

(defn- corpus-files []
  (sort (for [f (file-seq root)
              :when (str/ends-with? (.getName ^java.io.File f) ".clj")
              :let [rel (str (.relativize (.toPath (.getAbsoluteFile root)) (.toPath (.getAbsoluteFile f))))]
              :when (not= rel "corpus_check.bb")]
          rel)))

(defn- load-text
  "Loads `text` as the file `rel`. Returns `{:thrown e :err s}`: what loading threw, if anything,
  and what it printed to `*err*`."
  [rel text]
  (let [err (java.io.StringWriter.)
        thrown (binding [*err* err]
                 (try (Compiler/load (java.io.StringReader. text) rel rel) nil
                      (catch Throwable t t)))]
    {:thrown thrown :err (str err)}))

(defn- messages [^Throwable t]
  (str/join "\n" (keep #(.getMessage ^Throwable %) (take-while some? (iterate #(.getCause ^Throwable %) t)))))

(defn- names?
  "Whether `text` names `token`. A token written with the `rec/` alias is also named by its
  full name, as the compiler prints it."
  [text token]
  (or (str/includes? text token)
      (and (str/starts-with? token "rec/")
           (str/includes? text (str "recognitum.core/" (subs token 4))))))

(deftest every-corpus-file-loads-or-is-refused-as-its-header-says
  (doseq [rel (corpus-files)
          :let [text (slurp (io/file root rel))
                {:keys [report]} (header text)
                tokens (map :names report)]]
    (testing rel
      (let [{:keys [thrown err]} (load-text rel text)]
        (cond
          (empty? report)
          (is (nil? thrown) (some-> thrown messages))

          (warned-only rel)
          (do (is (nil? thrown) (some-> thrown messages))
              (is (some #(names? err %) tokens) err))

          :else
          (do (is (some? thrown) "loading succeeded")
              (when thrown
                (is (some #(names? (messages thrown) %) tokens) (messages thrown)))))))))
