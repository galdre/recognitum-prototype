(ns recognitum.show
  "Prints a form, then what it returned or the error it threw, then the calls each logged
  source received while it ran.")

(def ^:dynamic *logs*
  "A map from a label to an atom holding a vector of calls. [[show]] empties each atom before
  the form runs and prints what it holds after."
  {})

(defn show* [label f]
  (doseq [a (vals *logs*)] (reset! a []))
  (println label)
  (println (try (str "  => " (pr-str (f)))
                (catch clojure.lang.ExceptionInfo e
                  (str "  threw " (some-> (:error (ex-data e)) (str "\n  "))
                       (.replace (ex-message e) "\n" "\n  ")))
                (catch Throwable e
                  (str "  threw " (.getName (class e)) ": " (ex-message e)))))
  (doseq [[label a] *logs* :when (seq @a)]
    (println (str "  " label ":") (pr-str @a)))
  (println))

(defmacro show
  "Runs `form` and prints it as [[show*]] does."
  [form]
  `(show* ~(pr-str form) (fn [] ~form)))
