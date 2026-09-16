(ns prototype.audit-fixture
  "Lets the whole suite run under the invariant `:keys` audit.

  Every type the suite constructs gets audited, which is the wholesale form of the idiom
  README recommends: construction touches every field, so it runs every invariant, so it
  exposes every mis-declaration in the type. A test that cares about the audit's own state
  binds the var itself and is unaffected by this."
  (:require
   [prototype.validated-record :as v]))

(def auditing?
  "True when the JVM was started with -Dvalidated-record.audit=true, which `:test-audit` does."
  (= "true" (System/getProperty "validated-record.audit")))

(defn audit
  "A `:once` fixture putting the suite in whichever mode the JVM was started for."
  [f]
  (binding [v/*audit-invariant-keys* auditing?] (f)))
