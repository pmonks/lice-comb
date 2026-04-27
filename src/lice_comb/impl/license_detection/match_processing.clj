;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.match-processing
  "Regex match processing logic.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                 :as s]
            [wreck.api                      :as re]
            [spdx.identifiers               :as si]
            [lice-comb.impl.spdx            :as lcis]
            [lice-comb.impl.expression-info :as ei]
            [lice-comb.impl.version-number  :as vernum]
            [lice-comb.impl.version-series  :as verser]
            [lice-comb.impl.license-regexes :as licre]))

(def ^:private re-placeholder-ver  (re-pattern (re/esc verser/placeholder-ver)))
(def ^:private re-placeholder-oool (re-pattern (re/esc verser/placeholder-oool)))


;####TODO: COME UP WITH A BETTER NAME?
(defn get-rencgs
  "Retrieve a value ncgs in `m` (a Map, as produced by rencg), trying each of
  the provided `names` in turn until a non-blank value is found.  Returns
  `default` when no non-blank value is found (and which defaults to `nil` if not
  provided). Trims and lower-cases the value, and normalises multiple whitespace
  to a single space."
  ([m names] (get-rencgs m names nil))
  ([m names default]
    (loop [[f & r] names]
      (if f
        (let [value (get m f)]
          (if (s/blank? value)
            (recur r)
            (-> value
                (s/trim)
                (s/lower-case)
                (s/replace #"\s+" " "))))
        default))))

(defn- or-later-with-explanation
  "Returns a tuple of `or-later?` (a `boolean`) and `confidence-explanation` (a
  `set`, possibly `nil`) found in match `m`."
  [m]
  (case [(licre/match->or-later? m) (licre/match->only? m)]
    [false false] [false]
    [false true]  [false]
    [true  false] [true]
    [true  true]  [true #{:contradictory-version-suffixes}]))

(defn- ids-from-versions-special-cases
  "Because SPDX has a few bonkers identifiers... 🙄"
  [id-format version]
  (case id-format
    "W3C" (when (some #{version} ["20021231" "2002-12-31"]) "W3C")
    (throw (ex-info "Internal logic error: unhandled special case" {:id-format id-format :version version}))))

(defn- ids-from-versions
  "Returns a sequence of valid and canonicalised SPDX identifiers (or
  expressions) from the given versions.  Versions that don't result in a valid
  identifier will not appear in the result.  Returns `nil` if none of the
  versions result in a valid SPDX identifier."
  [id-formats or-later? versions]
  (when (and (seq id-formats) (seq versions))
    (let [ids (seq
                (map #(lcis/canonicalise-id % or-later?)
                     (distinct (filter si/listed? (for [id-format id-formats
                                                        version   versions]
                                                    (if (re-find re-placeholder-ver id-format)
                                                      (s/replace id-format re-placeholder-ver version)  ;####TODO: NEED TO REPLACE TRAILING OOOL AS WELL (test case: GFDL/invariants etc.)!!!
                                                      (ids-from-versions-special-cases id-format version)))))))]  ;####TODO: NEED TO REPLACE TRAILING OOOL AS WELL (test case: GFDL/invariants etc.)!!!
      ids)))

(defn- best-id-from-match
  "Returns a tuple made up of:

  1. the 'best' SPDX identifier from match `m`, a regex match for
     `version-series`
  2. a set of confidence explanations (which may be `nil`)"
  [version-series m]
  (if-let [versions (licre/match->versions m)]
    ; The match includes a version number, so try and determine which identifier in the series
    (let [[or-later? confidence-explanations] (or-later-with-explanation m)
          canonicalised-versions              (seq (distinct (map (partial vernum/canonicalise (:version-type version-series)) versions)))
          confidence-explanations             (some-> (if (> (count canonicalised-versions) 1)
                                                        (conj confidence-explanations :inconsistent-versions)
                                                        confidence-explanations)
                                                      seq
                                                      set)
          ; First try substituting the literal version number text(s) from the match and seeing if that gives us any valid ids
          ids                                 (ids-from-versions (:id-formats version-series) or-later? versions)]
      (if (pos? (count ids))
        [(verser/best-id (:series-id version-series) ids) (when confidence-explanations confidence-explanations)]
        ; Then try substituting the canonical representations of the version number(s) from the match and seeing if that gives us any valid ids
        (let [ids (ids-from-versions (:id-formats version-series) or-later? canonicalised-versions)]
          (if (pos? (count ids))
            [(verser/best-id (:series-id version-series) ids) (set (conj confidence-explanations :version-near-match))]
            ; Finally, try some year4 / date specific variations
            (case (:version-type version-series)
              ; For year4, try adding "19" to the start of versions that only had 2 digits
              :year4 (let [expanded-versions (map #(str "19" (subs % 2)) (filter #(and (= 4 (count %)) (s/starts-with? % "00")) canonicalised-versions))]
                       (when-let [ids (ids-from-versions (:id-formats version-series) or-later? expanded-versions)]
                         [(verser/best-id (:series-id version-series) ids) (set (conj confidence-explanations :version-near-match))]))
              ; For date, try without separators
              :date  (let [de-hyphenated-versions (map #(s/replace % "-" "") canonicalised-versions)]
                       (if-let [ids (ids-from-versions (:id-formats version-series) or-later? de-hyphenated-versions)]
                         [(verser/best-id (:series-id version-series) ids) (set (conj confidence-explanations :version-near-match))]
                         [(:default-id version-series) (set (conj confidence-explanations :invalid-version))]))
              ; No faffing about with the matched version numbers resulted in any valid SPDX identifiers, so give up and return the default identifier in the version series
              [(:default-id version-series) (set (conj confidence-explanations :invalid-version))])))))
    ; No version number was found in the match
    [(:default-id version-series) #{:missing-version}]))

(defn- matched-text->strategy
  "Determines the strategy from matched text."
  ([ids-checked matched-text] (matched-text->strategy ids-checked matched-text nil))
  ([ids-checked ^String matched-text regex-type]
   (let [ids   (seq (filter identity ids-checked))
         names (seq (filter identity (map #(:name (si/id->info %)) ids)))]
     (cond
       (some #{(s/lower-case matched-text)} (map s/lower-case ids))   :spdx-listed-identifier
       (some #{matched-text}                names)                    :spdx-listed-name-exact-match
       (some #{(s/lower-case matched-text)} (map s/lower-case names)) :spdx-listed-name-case-insensitive-match
       :else                                                          (case regex-type
                                                                        :id-regex   :spdx-listed-identifier-near-match
                                                                        :name-regex :spdx-listed-name-near-match
                                                                        :regex-match)))))

(defn- expression-info
  "Constructs a valid expression-info map from the given match information."
  [ids-checked ^String detected-id regex-type ^String matched-text confidence-explanations]
   (let [strategy   (matched-text->strategy ids-checked matched-text regex-type)
         match-type (if (= strategy :spdx-listed-identifier) :declared :concluded)]
     (ei/expression-info detected-id strategy match-type matched-text confidence-explanations)))

(defn unversioned-match->expression-info
  "Returns an expression info map for the unversioned (non-version-series)
  match `m`, which matched `id` via `regex-type`."
  [^String id regex-type m]
  (expression-info [id] id regex-type (:match m) nil))

(defn versioned-match->expression-info
  "Returns an expression info map based on the 'best' identifier in match m,
  assumed to be within `version-series`.  Returns `nil` only if `m` is `nil`,
  otherwise _always_ returns a result."
  [version-series regex-type m]
  (let [[detected-id confidence-explanations] (best-id-from-match version-series m)]
    (expression-info (:ids version-series) detected-id regex-type (:match m) confidence-explanations)))
