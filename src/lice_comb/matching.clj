;
; Copyright © 2021 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.matching
  "The core matching functionality within lice-comb. Matching is provided for
  three categories of input, and uses a different process for each:

  1. License names
  2. License uris
  3. License texts

  Each matching fn has two variants:

  1. A 'simple' version that returns a set of SPDX expressions (`String`s)
  2. An 'info' version that returns an 'expressions map' containing metadata
     describing how the determination was made (including source, confidence,
     the matching strategy, etc.)

  An expressions map has this structure:

  * key:   an SPDX expression (`String`)
  * value: a sequence of 'fragment info' maps

  Each fragment info map has this structure:

  * `:fragment` (`String`, mandatory):
  * `:match-type` (either `:declared` or `:concluded`, mandatory):
    Whether this identifier was unambiguously declared within the input or was
    instead concluded by lice-comb (see [the SPDX FAQ](https://wiki.spdx.org/view/SPDX_FAQ)
    for more detail on the definition of these two terms).
  * `:confidence` (one of: `:high`, `:medium`, `:low`, only provided when
    `:type` = `:concluded`):
    Indicates the approximate confidence lice-comb has in its conclusions for
    this particular fragment.
  * `:confidence-explanations` (a set of keywords, optional):
    Describes the associated `:confidence` measure (only provided when `:type` =
    `:concluded`).
  * `:strategy` (a `String`, mandatory):
    The strategy lice-comb used to detect this particular fragment.
  * `:source` (a sequence of `String`s):
    The list of sources used to arrive at this portion of the SPDX expression,
    starting from the most general (the input) through to the most specific
    (the smallest subset of the input that was used to make this
    determination)."
  (:require [clojure.string                :as s]
            [spdx.identifiers              :as si]
            [spdx.licenses                 :as sl]
            [lice-comb.impl.spdx           :as spdx]
            [lice-comb.impl.parsing.parser :as lcip]))

(defn lice-comb-license-ref?
  "Is the given id one of lice-comb's custom LicenseRefs?"
  [^CharSequence id]
  (spdx/lice-comb-license-ref? id))

(defn public-domain?
  "Is the given id lice-comb's custom 'public domain' LicenseRef?"
  [^CharSequence id]
  (spdx/public-domain? id))

(defn proprietary-commercial?
  "Is the given id lice-comb's custom 'proprietary / commercial' LicenseRef?"
  [^CharSequence id]
  (spdx/proprietary-commercial? id))

(defn unidentified?
  "Is the given id a lice-comb custom 'unidentified' LicenseRef?"
  [^CharSequence id]
  (spdx/unidentified? id))

(defn unidentified->name
  "Returns a human readable name for the given lice-comb custom 'unidentified'
  LicenseRef. Returns `nil` if id is not a lice-comb custom 'unidentified'
  LicenseRef."
  [^CharSequence id]
  (spdx/unidentified->human-readable-name id))

(defn id->name
  "Returns a human readable name of the given license or exception identifier;
  either the official SPDX license or exception name or, if the id is a
  lice-comb specific LicenseRef, a lice-comb specific name. Returns `id`
  verbatim if unable to determine a name. Returns `nil` if `id` is blank."
  [^CharSequence id]
  (when-not (s/blank? id)
    (cond (si/listed-id? id)           (:name (si/id->info id))
          (sl/special-form? id)        (sl/canonicalise id)
          (public-domain? id)          "Public domain"
          (proprietary-commercial? id) "Proprietary/commercial"
          (unidentified? id)           (unidentified->name id)
          :else                        id)))

(defn text->expressions-info
  "Returns an expressions map for `text` (a `String`, `File`, or anything that's
  supported by `clojure.java.io/reader`). Returns `nil` if no expressions were
  found in it.

  Notes:

  * this function uses the SPDX matching guidelines (via clj-spdx).
    See [the SPDX specification](https://spdx.github.io/spdx-spec/v3.0.1/annexes/license-matching-guidelines-and-templates/)
    for details
  * the caller is expected to open & close a `Reader` or `InputStream` passed to
    this function (e.g. using `clojure.core/with-open`)
  * you cannot pass a `String` representation of a filename to this method - you
    should use `clojure.java.io/file` (or similar) first"
  [text]
  (lcip/match-text text))

(defn text->expressions
  "Returns a set of SPDX expressions (`String`s) for `text`. See
  [[text->expressions-info]] for details."
  [text]
  (some-> (text->expressions-info text)
          keys
          set))

(defn uri->expressions-info
  "Returns an expressions map for `uri` (a `String`, `URL`, or `URI`), or
  `nil` if no expressions were found or `uri` is `nil`.
  `attempt-download-and-match?` (default `false`) controls whether URIs that
  point to a limited number of allow-listed hosts will have their content
  downloaded and SPDX matching attempted."
  ([uri] (uri->expressions-info uri false))
  ([uri ^Boolean attempt-download-and-match?]
   (when uri
     (lcip/parse-uri uri attempt-download-and-match?))))

(defn uri->expressions
  "Returns a set of SPDX expressions (`String`s) for `uri`.
  `attempt-download-and-match?` (default `false`) controls whether URIs that
  point to a limited number of allow-listed hosts will have their content
  downloaded and SPDX matching attempted."
  ([uri] (uri->expressions uri false))
  ([uri ^Boolean attempt-download-and-match?]
   (some-> (uri->expressions-info uri attempt-download-and-match?)
           keys
           set)))

(defn name->expressions-info
  "Returns an expressions map for `n`ame (a `String`), or `nil` if `n` is
  `clojure.string/blank?`.  `attempt-download-and-match?` (default `false`)
  controls whether URIs found in a name and that point to a limited number of
  allow-listed hosts will have their content downloaded and SPDX matching
  attempted."
  ([^CharSequence n] (name->expressions-info n false))
  ([^CharSequence n ^Boolean attempt-download-and-match?]
   (when-not (s/blank? n)
     (lcip/parse-name n attempt-download-and-match?))))

(defn name->expressions
  "Returns a set of SPDX expressions (`String`s) for `n`ame.
  `attempt-download-and-match?` (default `false`) controls whether URIs found in
  a name and that point to a limited number of allow-listed hosts will have
  their content downloaded and SPDX matching attempted."
  ([^CharSequence n] (name->expressions n false))
  ([^CharSequence n ^Boolean attempt-download-and-match?]
   (some-> (name->expressions-info n attempt-download-and-match?)
           keys
           set)))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning `nil`. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method may have a substantial performance cost."
  []
  (spdx/init!)
  nil)
