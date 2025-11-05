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
  a group of licenses that share the same base identifier, with one or more
  versions.

  Notes:

  * this is _not_ an official SPDX concept, though see [[https://github.com/spdx/license-list-XML/issues/2805]]
  * this namespace is not part of the public API of lice-comb and may change
    without notice
  * GPL id variants are not supported - the assumption being that this
    functionality is not used with them, thanks to lice-comb.impl.substitutions.gnu"
  (:require [clojure.string         :as s]
            [spdx.identifiers       :as si]
            [wreck.api              :as re]
            [rencg.api              :as rencg]
;####TODO: CONSIDER RATIONALISING VERSION MATCHING REGEXES?
            [lice-comb.impl.regexes :as lcir]
            [lice-comb.impl.utils   :as lciu]))

(def ^:private re-version-group   (re/ncg "version" #"\d{8}|\d{4}|\d{2}|\d+\.\d+(?:\.\d+)*\w?"))

(def ^:private re-version-in-id   (re/join #"(?<=-)" re-version-group))
(def ^:private re-oool-in-id      (re/flags-grp "i" #"(?<=-)(?:\+|or-later|only)\z"))

(def ^:private re-version-series  (re/join (re/ncg "prefix" #".*?")
                                           re-version-in-id
                                           (re/opt (re/alt-grp (re/esc "+") (re/grp (re/esc "-") (re/ncg "suffix" #".*"))))))

(def ^:private re-version-label   (re/flags-grp "i" #"(?<!\w)(?:(v|ver|versions?)?\s*)?"))
(def ^:private re-version-in-name (re/flags-grp "i" #"(?<!\w)(?:(v|ver|versions?)?\s*)?(?<version>\d{8}|\d{4}|\d{2}|\d+\.\d+(?:\.\d+)*\w?)"))
(def ^:private re-oool-in-name    (re/flags-grp "i" #"(?<!\w)(?:\+|or later|only)(?!\w)"))


(def ^:private re-version-series  #"(?<prefix>.*?)-(?<version>\d{8}|\d{4}|\d{2}|\d+\.\d+(?:\.\d+)*\w?)(?:\+|(?:-(?<suffix>.*)))?")

;####TODO: consider whether having a configurable `not-versioned` value is helpful
;####TODO: consider making private
(defn id->version-series
  "Returns the 'version-series' of `id` (an SPDX license or exception
  identifier), or `not-versioned` (default: `nil`) if it doesn't belong to a
  version series."
  ([^String id] (id->version-series nil id))
  ([not-versioned ^String id]
   (when-not (s/blank? id)
     (cond
       ; Special cases
       (s/starts-with? id "LZMA-SDK-")                                      not-versioned  ; Has a version range in one of the ids, which is a pain to process in the regex
       (s/starts-with? id "BSD")                                            not-versioned  ; e.g. to properly handle BSD-3-Clause-No-Nuclear-License-2014
       (and (s/starts-with? id "GPL-3.0-")  (s/ends-with? id "-exception")) not-versioned  ; e.g. GPL-3.0-389-ds-base-exception
       (and (s/starts-with? id "LGPL-3.0-") (s/ends-with? id "-exception")) not-versioned  ; e.g. LGPL-3.0-linking-exception
       (= id "Autoconf-exception-generic-3.0")                              not-versioned  ; See the name for why
       ; Default regex based version series identification
       :else
         (if-let [m (rencg/re-matches-ncg re-version-series id)]
           (let [prefix (get m "prefix")
                 suffix (-> (get m "suffix" "")    ; Strip off `or-later` and `only` at the end of the suffix
                            (s/replace #"\-?only\z" "")
                            (s/replace #"\-?or-later\z" ""))]
             (if (s/blank? suffix)
               prefix
               (str prefix "/" suffix)))
           not-versioned)))))

;####TODO: consider making private
(defn version-series-member?
  "Is `id` a member of a version series?

  Notes:

  * returns `true` even for singleton version series' (e.g. `Hippocratic-2.1`)."
  [^String id]
  (boolean (id->version-series id)))

;####TODO: consider whether having configurable grouping behaviour for non-version-series ids is helpful
;####TODO: possibly redundant
(defn ids->version-series
  "Turns `ids` into a map of 'version series', where each key is a version
  series name (`String`) as defined by [[id->version-series]], and each value is
  a sequence of SPDX identifiers in that version series, in ascending order
  (oldest version first).

  Ids that don't belong to a version series are included in the result, with
  `group-unversioned-ids?` (default: `true`) controlling whether they're grouped
  under the key `nil`, or stored separately with themselves as the key."
  ([ids] (ids->version-series true ids))
  ([group-unversioned-ids? ids]
   (when (seq ids)
     (let [result (lciu/mapfonv sort (group-by #(id->version-series (if group-unversioned-ids? nil %) %) ids))]
       (when-not (empty? result)
         result)))))

;####TODO: consider whether having configurable grouping behaviour for non-version-series ids is helpful
;####TODO: possibly redundant
(defn id-infos->version-series
  "Turns `id-infos` (a sequence of id-info maps) into a map of 'version series',
  where each key is a version series name (`String`) and each value is a
  sequence of id-info maps in that version series, in ascending order (oldest
  version first).

  id-infos that don't belong to a version series are included in the result,
  with `group-unversioned-ids?` (default: `true`) controlling whether they're
  grouped under the key `nil`, or stored separately with themselves as the key."
  ([id-infos] (id-infos->version-series true id-infos))
  ([group-unversioned-ids? id-infos]
   (when (seq id-infos)
     (let [result (lciu/mapfonv (partial sort-by :id) (group-by #(id->version-series (if group-unversioned-ids? nil (:id %)) (:id %)) id-infos))]
       (when-not (empty? result)
         result)))))

(defn- default-fn
  "Returns a function for finding the default value for the given series-id.
  The returned function assumes it's executed against a sorted sequence of
  values (versions, ids, etc.)."
  [series-id]
  (case series-id
    "GPL"  first
    "LGPL" first
    "AGPL" first
    last))        ; Most version series default to latest version

;####TODO: FIND A BETTER NAME!!!!
(defn version-series
  "Splits `ids` into two sequences and returns them as a tuple of:

  1. A sequence of version sequence maps (may be nil)
  2. A sequence of ids that are not members of a version series (may be nil)

  Each version sequence map has all of these keys:

  * `:id` - an identifier (`String`) for the series (not an SPDX identifier)
  * `:versions` - a sequence of versions (`String`s) in the series, from oldest
    to newest
  * `:ids` - sequence of ids (`String`s) in the series, from oldest to newest
  * `:id-template` - a template identifier for the series, with `${version}`
    where the version was
  * `:name-template` - a template name for the series, with `${version}` where
    the version was"
  [ids]
  (when (seq ids)
    (let [groups          (group-by id->version-series ids)
          unversioned-ids (get groups nil)
          groups          (dissoc groups nil)]
      [(seq
         (map
           #(let [series-id       (key %)
                  default         (default-fn series-id)
                  series-ids      (sort (val %))
                  series-versions (distinct (map (fn [id] (get (rencg/re-matches-ncg re-version-series id) "version")) series-ids))
                  series-names    (distinct (filter identity (map (fn [id] (:name (si/id->info id))) series-ids)))
                  default-id      (default series-ids)
                  default-version (default series-versions)
                  default-name    (default series-names)
;                  default-version (default-version series-id series-versions)
                  id-template     (-> default-id
                                      (s/replace re-version-in-id (s/re-quote-replacement "$VER"))
                                      (s/replace re-oool-in-id    (s/re-quote-replacement "$OOOL")))
                  name-template   (-> default-name
                                      (s/replace (re/join re-version-label (lcir/version-number->re default-version true)) (s/re-quote-replacement "$VER"))
                                      (s/replace re-oool-in-name                                                           (s/re-quote-replacement "$OOOL")))]
              {:id              series-id
               :versions        series-versions
;####TODO: REMOVE THIS IF UNUSED
;               :default-version default-version
               :default-id      default-id
               :ids             series-ids
               :id-template     id-template
               :name-template   name-template})
         groups))
       unversioned-ids])))


;;####TODO: CONSIDER MOVING THIS TO lice-comb.impl.regexes
;(defn version-series-id-template->re
;  [id-template]
;  (when-not (s/blank? id-template)
;    (-> [#"(?<!\w)" (s/trim id-template) #"(?!\w)"]
;        ; Version series placeholders
;        (lciu/replace-in-coll #"\s*\$OOOL" "")
;        (lciu/replace-in-coll #"\s*\$VER"  )
;        ; Special cases for some double and/or weird version components
;        (lciu/replace-in-coll #"9.11-to-9.20"                         #"0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20")
;        ; Special cases for certain licenses
;        (lciu/replace-in-coll #"(?i)(?<!\w)MIT(?!\w)"                 #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))")
;        (lciu/replace-in-coll #"(?i)(?<!\w)X11(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)ISC(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"    #"(?<!zlib/[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
;        (lciu/replace-in-coll #"(?i)(?<!\w)SGI-B(?!\w)"               #"SGI(?:[\s\-–—]+B)?")
;        (lciu/replace-in-coll #"(?i)\-(?<versionNumber>\d+\.\d+(?:\.\d+)*)(?:(?<only>-only)|(?<orLater>\+|-or-later))?(?=(-|\z))"
;                              (partial re-version-replacement "versionNumberId" "onlyId" "orLaterId"))
;        ; Character equivalents
;        (lciu/replace-in-coll #"[\s\-]+"                              #"[\s\-–—]+")  ; Note: hyphen, en-dash, em-dash
;        ; Cleanup and combine into a single pattern
;        (->> (filter #(or (not (string? %)) (not (s/blank? %))))   ; Remove empty strings
;             (lciu/mapcat-str #(vector (re/esc %)))
;             (apply (partial re/flags-grp "iuU"))))))
