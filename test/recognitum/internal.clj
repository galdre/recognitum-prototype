(ns recognitum.internal
  "The library's internals, for the suite that tests them.

  `recognitum.core`'s public surface is the vocabulary a caller writes, and nothing else:
  the model map, the verdict mask and the macro's own parsing are private, because a public
  name that hands out any of them is a way past the checks -- the model carries an
  unvalidated draft -> sealed constructor, and writing the mask makes `seal` believe a check
  that never ran.

  Privacy in Clojure is advisory, and a var is reachable through itself whatever its
  metadata says. That is the whole of this namespace: one `#'` per internal the suite
  exercises, gathered here rather than spelled out at fifty call sites, so that what the
  tests reach around is visible in one place and adding to it is a deliberate act.

  It lives under `test/`, so nothing a consumer has on its classpath can require it."
  (:require
   [recognitum.core]))

;;; ------------------------------------- the model map -------------------------------------

(def model-of
  "The model for a validated instance or the type itself."
  @#'recognitum.core/model-of)

(def draft-model
  "The model for a draft, or nil."
  @#'recognitum.core/draft-model)

;;; ------------------------------------ the verdict mask ------------------------------------

(def verdict-mask
  "The mask of checks currently passing on a draft."
  @#'recognitum.core/verdict-mask)

(def record-verdict!
  "Write a verdict into a draft's mask."
  @#'recognitum.core/record-verdict!)

;;; --------------------------------------- ownership ---------------------------------------

(def adopt
  "Stamp a draft as owned by a token."
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
