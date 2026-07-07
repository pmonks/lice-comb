;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.version-series
  "Functionality related to 'version series' of licenses. A 'version series' is
  a group of licenses or exceptions that share the same base identifier, with
  one or more versions comprising some kind of linear history.

  Notes:

  * this is _not_ an official SPDX concept (yet; see [[https://github.com/spdx/spdx-3-model/issues/1182]]
    which would make this namespace superfluous)
  * this namespace is not part of the public API of lice-comb and may change
    without notice"
  (:require [clojure.string                 :as s]
            [spdx.identifiers               :as si]
            [wreck.api                      :as re]
            [rencg.api                      :as ncg]
            [lice-comb.impl.spdx            :as lcis]
            [lice-comb.impl.regex-fragments :as ref]
            [lice-comb.impl.version-number  :as vernum]))

; These are public because lice-comb.impl.regex.licenses and lice-comb.impl.id-detection.default depend on knowing what these values are
(def placeholder-ver  "${VER}")
(def placeholder-oool "${OOOL}")

; Regexes for matching consecutive version and OOOL placeholders
(def ^:private re-ver-oool      (re/join (re/esc placeholder-ver) #"[\s\-]*" (re/esc placeholder-oool)))
(def ^:private re-word-then-ver (re/join (re/+lb #"\w") (re/esc placeholder-ver)))

; We have our own versions of these regex fragments here, because these elements in the SPDX license list are more carefully controlled than the many variations that exist out in the real world
(def ^:private re-version-group     (re/ncg "version" (re/alt-grp #"\d{4}\-?\d{2}\-?\d{2}" #"\d{4}" #"\d{2}" #"\d+\.\d+(?:\.\d+)*") #"\w?"))
(def ^:private re-oool-in-id        (re/fgrp "i" #"(?:\+|or-later|only)"))
(def ^:private re-oool-in-name      (re/fgrp "i" #"(?<!\w)(?:\+| or later| only)(?!\w)"))
(def ^:private re-version-series    (re/join #"(?<prefix>.*?)-" re-version-group #"(?:\+|(?:-(?<suffix>.*)))?"))
(def ^:private re-opt-version-label (re/opt-grp ref/version-label ref/ows))

; Only public for the unit tests
(defn id->version-series
  "Returns the 'version-series' (a `String`) of `id` (an SPDX license or
  exception identifier), or `nil` if it doesn't belong to a version series."
  [^String id]
  (when-not (s/blank? id)
    (cond
      ; Special cases - licenses
      (= id "W3C")                                                         "W3C"       ; Id doesn't include a version, even though its name does!
      (= id "Libpng")                                                      "libpng"    ; Id doesn't include a version and has different case to other members of the version series
      (s/starts-with? id "AGPL-1.0")                                       "Affero"    ; Unrelated to AGPL-3.0 (despite having the same ID pattern)
      (s/starts-with? id "BSD-")                                           nil         ; e.g. to properly handle BSD-3-Clause-No-Nuclear-License-2014 (2014 is detected as a version number)
      (s/starts-with? id "LZMA-SDK-")                                      "LZMA-SDK"  ; Has multiple versions in one of the ids
      ; Special cases - exceptions
      (= id "Autoconf-exception-generic-3.0")                              nil         ; Name clarifies that this is NOT a version number: "Autoconf generic exception for GPL-3.0"
      (and (s/starts-with? id "GPL-3.0-")  (s/ends-with? id "-exception")) nil         ; e.g. GPL-3.0-389-ds-base-exception, GPL-3.0-interface-exception, GPL-3.0-linking-exception, etc.
      (and (s/starts-with? id "LGPL-3.0-") (s/ends-with? id "-exception")) nil         ; e.g. LGPL-3.0-linking-exception
      (and (s/starts-with? id "QPL-1.0-")  (s/ends-with? id "-exception")) nil         ; e.g. QPL-1.0-INRIA-2004-exception
      ; Default regex based version series identification
      :else
        (when-let [m (ncg/re-matches re-version-series id)]
          (let [prefix (get m "prefix")
                suffix (-> (get m "suffix" "")    ; Strip off `or-later` and `only` at the end of the suffix
                           (s/replace #"\-?only\z"     "")
                           (s/replace #"\-?or-later\z" ""))]
            (if (s/blank? suffix)
              prefix
              (str prefix "/" suffix)))))))

;####TODO: CONSIDER MOVING THIS FN AND id-sorter TO lice-comb.impl.spdx AND MAKE PUBLIC
(defn- parse-suffix-from-id
  "Parses `id` into a thruple containing:

  1. the base id
  2. the exact suffix (-or-later, +, -only) - may be `nil`
  3. a suffix type (:or-later, :only) - may be `nil`"
  [^String id]
  (let [id-without-suffix    (s/replace id #"(?:\-or\-later|\+|\-only)\z" "")
        [suffix suffix-type] (cond
                               (s/ends-with? id "-or-later") ["-or-later" :or-later]
                               (s/ends-with? id "-only")     ["-only"     :only]
                               (s/ends-with? id "+")         ["+"         :or-later])]
    [id-without-suffix suffix suffix-type]))

(defn- id-sorter
  "A comparator for sorting SPDX identifiers.

  Notes:

  * identifiers are sorted case _in_sensitively
  * takes suffixes of the same identifier into account, putting `-or-later`/`+`
    first, then unadorned identifier, then `-only`
  * does _not_ differentiate between license ids and exceptions ids, which is
    important for correctly handling identifiers that have changed type (e.g.
    `SHL`)"
  [^String a ^String b]
  (let [a                                         (s/lower-case (if (= "W3C" a) "W3C-20021231" a))  ; Special case for the (highly irregular) W3C identifier
        b                                         (s/lower-case (if (= "W3C" b) "W3C-20021231" b))  ; Ditto
        [a-without-suffix a-suffix a-suffix-type] (parse-suffix-from-id a)
        [b-without-suffix b-suffix b-suffix-type] (parse-suffix-from-id b)]
    (cond
      (not= a-without-suffix b-without-suffix) (compare a b)  ; Unrelated ids (not in the same version series), so naive compare is fine
      (= a-suffix b-suffix)                    0
      (= a-suffix-type :only)                  1
      (= b-suffix-type :only)                  -1
      (= a-suffix-type :or-later)              (if (= b-suffix "-or-later") 1 -1)     ; This is to ensure -or-later vs + are sorted correctly
      (= b-suffix-type :or-later)              (if (= a-suffix "-or-later") -1 1))))  ; Ditto

(defn- defaults
  "Returns a tuple containing:

  1. a function that can be used to find the default id, version, etc. within
     the given version series. This function assumes it's executed against a
     sorted sequence of values (ids, versions, etc.), sorted using [[id-sorter]].
  2. an or-later? flag, which indicates whether the default version or id should
     have an or later suffix."
  [series-id]
  (case series-id
    "GPL"                [first true]
    "LGPL"               [first true]
    "AGPL"               [first true]
    "GFDL"               [first true]
    "GFDL/invariants"    [first true]
    "GFDL/no-invariants" [first true]
    [last false]))        ; Most version series default to latest version

(defn- default-id
  "Returns the default SPDX identifier within the version series identified by
  `series-id`, using `series-ids`."
  [series-id series-ids]
  (case series-id
    "W3C"    "W3C"
    "libpng" "Libpng"
    (let [[default or-later?] (defaults series-id)]
      (lcis/canonicalise-id-or-expression (str (default series-ids) (when or-later? "+"))))))

;####TODO: MERGE THIS WITH default-id
(defn best-id
  "Returns the 'best' id, canonicalised, in sequence `ids` within the version
  series identified by `series-id`.  Notably, this function works with subsets
  of all of the ids in that version series; for example:
  `(best-id \"Apache\" [\"Apache-1.0\" \"Apache-1.1\"])` would return `\"Apache-1.1\"`."
  [series-id ids]
  (if (= 1 (count ids))
    (first ids)
    (let [[default or-later?] (defaults series-id)]
      (lcis/canonicalise-id-or-expression (str (default ids) (when or-later? "+"))))))

(defn- id-formats
  "Returns a set of unique id formats in the given version series (identified
  by `ids-in-series`, a sequence of license identifiers that MUST be in the same
  version series).  The result is a sequence of the id format(s) for the series
  (usually a singleton, though there are a very few version series that have
  more than one id format).  Returns `nil` if `ids-in-series` is `nil` or empty."
  [ids-in-series]
  (some->> (seq ids-in-series)
           (map (fn [id]
                  (if-let [id-version (get (ncg/re-matches re-version-series id) "version")]
                    ; Id has a version, so replace version elements with placeholders
                    (-> id
                        (s/replace-first (vernum/exact-regex id-version) (s/re-quote-replacement placeholder-ver))   ; The license's version
                        (s/replace       re-oool-in-id                   (s/re-quote-replacement placeholder-oool))  ; "only" or "or later"
                        (s/replace       re-ver-oool                     (s/re-quote-replacement placeholder-ver)))  ; Collapse consecutive version and OOOL
                    ; Id has no versions in it (e.g. Libpng, W3C), so return it verbatim
                    id)))
           distinct))

(defn irregular-ids
  "Returns a summary of version series' that have 'irregular' ids (i.e. ids that
  are not consistent across all versions of the series), using the provided set
  of SPDX listed `ids` (default: all SPDX listed identifiers).

  The result is a map where the keys are series' ids, and the values are a
  sequence of 'id formats' in that series.

  Notes:

  * This function is not used by lice-comb itself, but is useful for maintenance
    purposes, especially as new versions of the SPDX license list are released
    that may include new irregular ids."
  ([] (irregular-ids (si/ids)))
  ([ids]
   (when (seq ids)
     (let [groups (dissoc (group-by id->version-series ids) nil)]
       (apply merge
         (filter
           identity
           (map
             #(let [series-id  (key %)
                    series-ids (sort id-sorter (val %))
                    id-formats (id-formats series-ids)]
                (when (> (count id-formats) 1)
                  {series-id id-formats}))
             groups)))))))

(defn- name-formats
  "Returns a set of unique name formats in the given version series (identified
  by `ids-in-series`, a sequence of license identifiers that MUST be in the same
  version series).  The result is a sequence of the name format(s) for the
  series (usually a singleton, though there are a very few version series that
  have more than one name pattern).  Returns `nil` if `ids-in-series` is `nil`
  or empty."
  [ids-in-series]
  (some->> (seq ids-in-series)
           (map (fn [id]
             (let [nm         (:name (si/id->info id))
                   id-version (if (= id "W3C")
                                "20021231"  ; Special case for the (highly irregular) W3C identifier
                                (get (ncg/re-matches re-version-series id) "version"))]
               (if id-version
                 (let [re-exact-version-and-label (when id-version (re/fgrp "i" (re/-lb #"\A\d{0,4}") #"[,\s]*" re-opt-version-label (vernum/exact-regex id-version)))]
                    ; Name has a version, so replace version elements with placeholders
                   (-> nm
                       (s/replace-first re-exact-version-and-label (s/re-quote-replacement placeholder-ver))            ; The license's version
                       (s/replace       re-word-then-ver           (s/re-quote-replacement (str " " placeholder-ver)))  ; Part of the comma / whitespace handling for SHL-0.51
                       (s/replace       re-oool-in-name            (s/re-quote-replacement placeholder-oool))           ; "only" or "or later"
                       (s/replace       re-ver-oool                (s/re-quote-replacement placeholder-ver))))          ; Collapse consecutive version and OOOL
                 ; Name has no versions in it (e.g. name of Libpng), so return it verbatim
                 nm))))
           distinct))

(defn irregular-names
  "Returns a summary of version series' that have 'irregular' names (i.e. names
  that are not consistent across all versions of the series), using the provided
  set of SPDX listed `ids` (default: all SPDX listed identifiers).

  The result is a map where the keys are series' ids, and the values are a
  sequence of 'name patterns' in that series.

  Notes:

  * This function is not used by lice-comb itself, but is useful for maintenance
    purposes, especially as new versions of the SPDX license list are released
    that may include new irregular names."
  ([] (irregular-names (si/ids)))
  ([ids]
   (when (seq ids)
     (let [groups (dissoc (group-by id->version-series ids) nil)]
       (apply merge
         (filter
           identity
           (map
             #(let [series-id    (key %)
                    series-ids   (sort id-sorter (val %))
                    name-formats (name-formats series-ids)]
                (when (> (count name-formats) 1)
                  {series-id name-formats}))
             groups)))))))

(defn mixed-type-version-series
  "Returns a summary of version series' that contain 'mixed' identifiers (i.e. a
  mix of license and exception identifiers), using the provided set of SPDX
  listed `ids` (default: all SPDX listed identifiers).

  The result is a map where the keys are series' ids, and the values are a
  sequence of identifiers in that series.

  Notes:

  * This function is not used by lice-comb itself, but is useful for maintenance
    purposes, especially as new versions of the SPDX license list are released
    that may include new irregular names."
  ([] (mixed-type-version-series (si/ids)))
  ([ids]
   (when (seq ids)
     (let [groups (dissoc (group-by id->version-series ids) nil)]
       (apply merge
         (filter
           identity
           (map
             #(let [series-id  (key %)
                    series-ids (sort id-sorter (val %))
                    id-types   (map (fn [id] (:type (si/id->info id))) series-ids)]
                (when (> (count (distinct id-types)) 1)
                  {series-id (map (fn [id id-type] [id id-type]) series-ids id-types)}))
             groups)))))))

(defn version-series
  "Processes `ids` (default: all SPDX listed identifiers) and returns a map
  with these keys (both optional):

  * `:version-series` - a map of the version series' identified in `ids`
  * `:unversioned-ids` - a set of ids that are not members of a version series

  The version series' map is keyed by the version series id, and each value is
  also a map that represents that specific version series, containing these
  keys:

  * `:series-id` - an identifier for this series (note: not an official SPDX
    concept)
  * `:ids` - sequence of ids (`String`s) in the series, from oldest to newest
  * `:names` - sequence of names (`String`s) in the series, from oldest to
    newest
  * `:versions` - a sequence of versions (`String`s) in the series, from oldest
    to newest
  * `:default-id` - the default identifier in the series (a `String`),
    canonicalised
  * `:id-formats` - a sequence of [clojure.core/format] strings for constructing
    identifiers in this version series. The format string will contain a single
    `%s` entry where the version number (as a `String`) will go.
  * `:id-formats` - a sequence of identifier format `String`s for the series,
    with version element(s) replaced with placeholders
  * `:name-formats` - a sequence of name format `String`s for the series, with
    with version element(s) replaced with placeholders
  * `:version-type` - a keyword representing the type of versioning used in this
    version series; one of: :semver, :year2, :year4, :date
  * `:version-suffix?` - a boolean indicating whether the version numbers in
    this version series can have a single letter suffix

  Returns `nil` if `ids` is `nil` or empty."
  ([] (version-series (si/ids)))
  ([ids]
   (when (seq ids)
     (let [groups          (group-by id->version-series ids)
           unversioned-ids (seq (get groups nil))
           version-series  (into {}
                             (map
                               #(let [series-id           (key %)
                                      series-ids          (sort id-sorter (val %))
                                      series-versions     (distinct (filter identity (map (fn [id] (get (ncg/re-matches re-version-series id) "version")) series-ids)))
                                      series-names        (map (comp :name si/id->info) series-ids)]
                                  [series-id (merge {:series-id    series-id
                                                     :ids          series-ids
                                                     :names        series-names
                                                     :default-id   (default-id series-id series-ids)
                                                     :versions     series-versions
                                                     :id-formats   (id-formats series-ids)
                                                     :name-formats (name-formats series-ids)}
                                                    (vernum/metadata series-versions))])
                               (dissoc groups nil)))]
       (merge (when     unversioned-ids         {:unversioned-ids (set unversioned-ids)})
              (when-not (empty? version-series) {:version-series  version-series}))))))
