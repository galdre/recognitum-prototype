#!/usr/bin/env bb
;; Runs clj-kondo over the shared corpus and compares each file's findings with
;; the `;; expect {...}` map in its header.
;;
;; Header keys:
;;   :report  [{:at [line col] :level :error|:warning :names "token" :kondo "message"} ...]
;;            the tool-neutral expectation; :kondo is clj-kondo's exact message.
;;            An entry with :tool :compiler is not expected from clj-kondo.
;;   :kondo   optional: overrides what clj-kondo is expected to report (as :report entries).
;;   :cold    :same, or entries: what clj-kondo reports when the file is linted
;;            alone with a fresh cache that holds only recognitum.core.
;;
;; Warm: the whole corpus, linted twice with one cache, with the library's sources. Cold: each
;; file alone, with a fresh cache that holds only the library. The hook and its config are the
;; ones the library exports.
;; Usage: bb test/corpus/corpus_check.bb
(require '[babashka.process :refer [shell]]
         '[babashka.fs :as fs]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root (str (fs/parent (fs/absolutize *file*))))
(def project (str (fs/parent (fs/parent root))))
(def library [(str project "/src")])
(def config-dir
  (let [d (fs/create-temp-dir)]
    (spit (str d "/config.edn")
          (pr-str {:config-paths [(str project "/resources/clj-kondo.exports/recognitum/recognitum")]
                   :output {:exclude-files ["/src/"]}}))
    (str d)))

(defn header [f]
  (let [lines (take-while #(str/starts-with? % ";;") (str/split-lines (slurp (str root "/" f))))
        text (str/join "\n" (map #(subs % 2) lines))
        i (str/index-of text "expect")]
    (edn/read-string (subs text (+ i (count "expect"))))))

(defn findings [cache paths]
  (let [{:keys [out]} (apply shell {:dir root :out :string :continue true}
                             "clj-kondo" "--cache-dir" (str cache) "--config-dir" config-dir
                             "--config" "{:output {:format :edn}}"
                             "--lint" paths)]
    (group-by :filename (:findings (edn/read-string out)))))

(defn as-set [fs]
  (set (for [{:keys [row col level message]} fs] [row col level message])))

(defn expected [entries]
  (set (for [{[row col] :at :keys [level kondo tool]} entries
             :when (not= tool :compiler)]
         [row col level kondo])))

(def files (sort (map #(str (fs/relativize root %)) (fs/glob root "**.clj"))))

(def warm
  (let [cache (fs/create-temp-dir)]
    (findings cache (into library ["usage" "mistakes" "extra"]))
    (findings cache (into library ["usage" "mistakes" "extra"]))))

(defn cold [f]
  (let [cache (fs/create-temp-dir)]
    (findings cache library)
    (get (findings cache (conj library f)) f)))

(def failures (atom 0))

(defn compare! [label f want got]
  (if (= want got)
    (println "ok  " label f)
    (do (swap! failures inc)
        (println "FAIL" label f)
        (doseq [x (sort (remove got want))] (println "     missing   " x))
        (doseq [x (sort (remove want got))] (println "     unexpected" x)))))

(doseq [f files]
  (let [h (header f)
        want (expected (or (:kondo h) (:report h)))
        got-warm (as-set (get warm f))]
    (compare! "warm" f want got-warm)
    (compare! "cold" f (if (= :same (:cold h)) want (expected (:cold h))) (as-set (cold f)))))

(println (if (zero? @failures) "all files as expected" (str @failures " mismatches")))
(System/exit (if (zero? @failures) 0 1))
