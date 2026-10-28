(ns recognitum.layers-test
  "The dependency rule of `design/layers.edn`, checked against every namespace under `src`,
  `dev` and `test/recognitum`. A namespace depends on another when it requires it, names one of
  its vars by a qualified symbol (quoted or syntax-quoted forms included), or imports a class
  it defines. A namespace that names another's qualified keywords without depending on it
  knows that vocabulary; that is reported, not refused."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import
   [clojure.lang LispReader LispReader$Resolver LineNumberingPushbackReader]
   [java.io File]))

(def ^:private roots ["src" "dev" "test/recognitum"])

(defn- clj-files []
  (sort (for [r roots, ^File f (file-seq (io/file r))
              :when (and (.isFile f) (str/ends-with? (.getName f) ".clj"))]
          (str f))))

;;; ------------------------------------------------------------------ the ns form

(defn- libspecs
  "[[lib opts] ...] for one :require or :use clause, expanding prefix lists."
  [clause]
  (mapcat (fn spec [x]
            (cond (symbol? x) [[x {}]]
                  (and (sequential? x) (symbol? (first x)))
                  (let [[lib & more] x]
                    (if (or (list? x) (and (seq more) (not (keyword? (first more)))))
                      (for [m more, [l o] (spec m)] [(symbol (str lib "." l)) o])
                      [[lib (apply hash-map more)]]))
                  :else []))
          (rest clause)))

(defn- ns-info
  "What the ns form says: the name, the namespaces it loads, its aliases, the vars it refers
  by name, and the classes it imports."
  [[_ nm & clauses]]
  (let [clauses (filter seq? clauses)
        specs (mapcat libspecs (filter #(#{:require :use} (first %)) clauses))
        alias-only? (fn [o] (and (:as-alias o) (not-any? o [:as :refer :only])))]
    {:ns nm
     :requires (set (for [[l o] specs :when (not (alias-only? o))] l))
     :aliases (into {} (for [[l o] specs, a [(:as o) (:as-alias o)] :when a] [a l]))
     :refers (into {} (for [[l o] specs, r (let [r (or (:refer o) (:only o))] (when (sequential? r) r))]
                        [r l]))
     :imports (set (for [c clauses :when (= :import (first c)), spec (rest c)]
                     (if (symbol? spec)
                       (symbol (str/replace (str spec) #"\.[^.]+$" ""))
                       (first spec))))}))

;;; ------------------------------------------------------------------ the forms

(defn- resolver [{:keys [ns aliases refers]}]
  (reify LispReader$Resolver
    (currentNS [_] ns)
    (resolveClass [_ s] (when (str/includes? (name s) ".") s))
    (resolveAlias [_ a] (get aliases a))
    (resolveVar [_ s] (when-let [l (get refers s)] (symbol (str l) (name s))))))

(defn- read-all [path]
  (with-open [r (LineNumberingPushbackReader. (io/reader path))]
    (let [opts {:eof ::eof :read-cond :allow}
          form (binding [*read-eval* false] (read opts r))
          info (when (and (seq? form) (= 'ns (first form))) (ns-info form))]
      (when info
        (binding [*read-eval* false
                  *default-data-reader-fn* tagged-literal
                  *reader-resolver* (resolver info)]
          (assoc info :forms (doall (take-while #(not= ::eof %) (repeatedly #(read opts r))))))))))

(defn- class-ns
  "The namespace a class symbol such as `recognitum.record.Draft` names, by its package."
  [s]
  (when-let [[_ pkg] (and (nil? (namespace s)) (re-matches #"(.+)\.[A-Z][^.]*" (name s)))]
    (symbol (str/replace pkg "_" "-"))))

(defn- scan
  "{:ns :deps :keywords} of one file: the namespaces it depends on, and the namespaces whose
  qualified keywords it names."
  [path]
  (when-let [{:keys [ns requires aliases imports forms]} (read-all path)]
    (let [atoms (filter #(or (symbol? %) (keyword? %)) (tree-seq coll? seq forms))
          qual (fn [x] (when-let [n (namespace x)] (get aliases (symbol n) (symbol n))))]
      {:ns ns
       :deps (-> (set/union requires
                            (set (keep #(if (symbol? %) (or (qual %) (class-ns %))) atoms))
                            (set (map #(symbol (str/replace (str %) "_" "-")) imports)))
                 (disj ns))
       :keywords (-> (set (keep #(when (keyword? %) (qual %)) atoms)) (disj ns))})))

;;; ------------------------------------------------------------------ the rule

(defn report
  "The check: `:violations` (dependencies the design forbids), `:undeclared` (namespaces it
  does not name), `:knowledge` (keywords named without a dependency), and whether the wiring
  set is small enough."
  []
  (let [{:keys [layers users wiring]} (edn/read-string (slurp "design/layers.edn"))
        allowed (merge layers users)
        scans (keep scan (clj-files))
        project (set (map :ns scans))]
    {:violations (vec (for [{:keys [ns deps]} scans
                            :when (and (not (wiring ns)) (contains? allowed ns))
                            d (sort (set/intersection deps project))
                            :let [ok (get allowed ns)]
                            :when (or (not (ok d)) (and (wiring d) (not (users ns))))]
                        (str ns " -> " d (when (ok d) " (only code that uses the library may depend on wiring)"))))
     :undeclared (vec (sort (for [{:keys [ns]} scans :when (not (or (wiring ns) (contains? allowed ns)))] ns)))
     :knowledge (vec (for [{:keys [ns deps keywords]} scans
                           :when (contains? allowed ns)
                           k (sort (set/intersection keywords project))
                           :when (not (or ((get allowed ns) k) (deps k)))]
                       (str ns " names keywords of " k)))
     :wiring-size-ok (<= (count wiring) 2)}))

(deftest the-dependency-rule
  (let [{:keys [violations undeclared knowledge wiring-size-ok]} (report)
        to-fix (set (keys (:to-fix (edn/read-string (slurp "design/layers.edn")))))]
    (testing "every dependency is one the design allows, or one listed to be removed"
      (is (= [] (remove to-fix violations))))
    (testing "every dependency listed to be removed is still there"
      (is (= #{} (set/difference to-fix (set violations)))))
    (testing "every namespace has a place in the design"
      (is (= [] undeclared)))
    (testing "at most two modules may name concrete backends"
      (is wiring-size-ok))
    (doseq [k knowledge] (println "layers: knowledge:" k))))
