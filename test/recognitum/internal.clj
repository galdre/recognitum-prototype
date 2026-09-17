(ns recognitum.internal
  "The private vars of `recognitum.core` the suite reaches through `#'`, one `def` each, so
  what the tests reach around is visible in one place. It lives under `test/`, so no
  consumer can require it."
  (:require
   [recognitum.core]))

;;; ------------------------------------- the model map -------------------------------------

(def model-of
  "The model for a validated instance or the type itself."
  @#'recognitum.core/model-of)

(def draft-model
  "The model for a draft, or nil."
  @#'recognitum.core/draft-model)

;;; ------------------------------ which checks a draft passes ------------------------------

(def verdict-mask
  "The record of which checks pass on a draft, as a bitmask."
  @#'recognitum.core/verdict-mask)

(def record-verdict!
  "Record the check at an index as passing or not on a draft."
  @#'recognitum.core/record-verdict!)

;;; --------------------------------------- ownership ---------------------------------------

(def adopt
  "A copy of a draft owned by a token."
  @#'recognitum.core/adopt)

(def owner-of
  "The token a draft records as its owner."
  @#'recognitum.core/owner-of)

(def owned-here?
  "Is this a draft an enclosing `drafting` owns?"
  @#'recognitum.core/owned-here?)

;;; --------------------------------- what the macro parses ---------------------------------

(def definition-sections
  "The sections a definition map may hold, in order."
  @#'recognitum.core/definition-sections)

(def known-options
  "What `:options` may hold."
  @#'recognitum.core/known-options)

(def parse-definition
  "A definition map -> its sections, every absent one filled in empty."
  @#'recognitum.core/parse-definition)

(def resolve-deps
  "One arglist -> the dependency keys it names, in order."
  @#'recognitum.core/resolve-deps)

(def reject-duplicate-names!
  "Refuse a name declared as both a check and an io-dep."
  @#'recognitum.core/reject-duplicate-names!)
