(ns recognitum.reports
  "Reading a validation report in a test. The answering and throwing forms carry the same
  report: check name -> `{:doc :values :returned}`, and `:blocked` where that failure kept
  other checks from running. Where a test is about which check failed and what it returned,
  it reads the report through here; where it is about the report itself, it reads the report
  whole.")

(defn returned
  "What each check in the report `r` returned, keyed by check name. `true` passes through,
  since that is the whole answer where nothing failed."
  [r]
  (if (true? r) r (update-vals r :returned)))

(defn returned-all
  "[[returned]] of each answer in `rs`, the vector a group call answers with."
  [rs]
  (mapv returned rs))
