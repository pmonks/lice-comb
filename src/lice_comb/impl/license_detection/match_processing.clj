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
  (:require [clojure.string                      :as s]
            [wreck.api                           :as re]
            [spdx.identifiers                    :as si]
            [lice-comb.impl.spdx                 :as lcis]
            [lice-comb.impl.regex.version-number :as vernum]
            [lice-comb.impl.version-series       :as verser]
            [lice-comb.impl.regex.licenses       :as rel]))

(def ^:private re-placeholder-ver  (re-pattern (re/esc verser/placeholder-ver)))
(def ^:private re-placeholder-oool (re-pattern (re/esc verser/placeholder-oool)))

(defn- or-later-with-explanation
  "Returns a tuple of `or-later?` (a `boolean`) and `confidence-explanation` (a
  `set`, possibly `nil`) found in match `m`."
  [m]
  (case [(rel/match->or-later? m) (rel/match->only? m)]
    [false false] [false]
    [false true]  [false]
    [true  false] [true]
    [true  true]  [true #{:contradictory-version-suffixes}]))

(defn- ids-from-versions
  "Returns a sequence "
  [id-formats or-later? versions]
  (when (and (seq id-formats) (seq versions))
    (seq
      (map #(lcis/canonicalise-id % or-later?)
           (distinct (filter si/listed? (for [version   versions
                                              id-format id-formats]
                                          (when (re-find re-placeholder-ver id-format)  ; Ignore id formats that happen to not have a version number (e.g. "W3C")
                                            (s/replace id-format re-placeholder-ver version)))))))))  ;####TODO: NEED TO REPLACE TRAILING OOOL AS WELL (test case: GFDL/invariants etc.)!!!

(defn- best-id-from-match
  "Returns a tuple made up of:

  1. the 'best' SPDX identifier from match `m`, a regex match for
     `version-series`
  2. a set of confidence explanations (which may be `nil`)"
  [version-series m]
  (if-let [versions (rel/match->versions m)]
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
            [(:default-id version-series)                     (set (conj confidence-explanations :invalid-version))]))))
    [(:default-id version-series) #{:missing-version}]))

(defn- strategy-from-matched-text
  "Determines the strategy from matched text."
  [ids names ^String matched-text regex-type]
  (let [ids   (filter identity ids)
        names (filter identity names)]
    (cond
      (some #{(s/lower-case matched-text)} (map s/lower-case ids))   :spdx-listed-identifier
      (some #{matched-text}                names)                    :spdx-listed-name-exact-match
      (some #{(s/lower-case matched-text)} (map s/lower-case names)) :spdx-listed-name-case-insensitive-match
      :else                                                          (case regex-type
                                                                       :id-regex   :spdx-listed-identifier-near-match
                                                                       :name-regex :spdx-listed-name-near-match))))

(defn- matching-type
  "Determines the matching type (`:declared` or `:concluded`) from a `strategy`
  keyword."
  [strategy]
  (if (= strategy :spdx-listed-identifier)
    :declared
    :concluded))

; Map of confidence explanations to their associated confidence scores
(def ^:private confidence-explanation->confidence {
  :missing-version                :low     ; Version is missing completely
  :invalid-version                :low     ; Version is invalid (e.g. "Apache 2.1")
  :version-near-match             :medium  ; Version is a near match (e.g. "Apache 2", "Apache 2.0.0")
  :inconsistent-versions          :medium  ; Multiple versions found, but they don't match (e.g. "Apache License 1.1 (Apache-2.0)")
  :extraneous-version-component   :high    ; Extraneous version component (e.g. "Apache 2.0.0")
  :contradictory-version-suffixes :medium  ; Contradictory version suffix (e.g. "GPL 2.0 only+")
  :multiple-ids-found             :low})   ; Multiple different identifiers were found  ;####TODO: CONFIRM THAT THIS ONE MAKES SENSE

(defn- matching-confidence
  "Determines the overall confidence level from a sequence of
  `confidence-explanations`. Returns `:high` if there are no explanations."
  [confidence-explanations]
  (if (seq confidence-explanations)
    (let [confidences (set (map confidence-explanation->confidence confidence-explanations))]
      ; Internal logic check
      (if (contains? confidences nil)
        (throw (ex-info "Internal logic error: a confidence explanation is missing a confidence level" {:confidence-explanations confidence-explanations}))
        (first (sort-by {:low 0 :medium 1 :high 2} confidences))))
    :high))  ; Default to :high when confidence-explanations is empty

(defn unversioned-match->expression-info
  "Returns an expression info map for the unversioned (non-version-series)
  match `m`."
  [^String id regex-type m]
  (when-let [matched-text (:match m)]
    (let [{nm :name}             (si/id->info id)
          canonical-id           (lcis/canonicalise-id id)
          {canonical-name :name} (si/id->info canonical-id)  ; Note: may be null (e.g. canonical-id is an SPDX expression due to canonicalisation)
          strategy               (strategy-from-matched-text [id canonical-id] [nm canonical-name] matched-text regex-type)
          matching-type          (matching-type strategy)
          confidence             (when (= :concluded matching-type) :high)]  ; Unversioned matches always have high confidence, since there's nothing "variable" in the match to take into account
      (merge {:id       canonical-id
              :strategy strategy
              :source   (list matched-text)
              :type     matching-type}
             (when confidence {:confidence confidence})))))

(defn versioned-match->expression-info
  "Returns an expression info map based on the 'best' identifier in match m,
  assumed to be within `version-series`.  Returns `nil` only if `m` is `nil`,
  otherwise _always_ returns a result."
  [version-series regex-type m]
  (when m
    (let [matched-text                 (:match m)
          [id confidence-explanations] (best-id-from-match version-series m)
          strategy                     (strategy-from-matched-text (:ids version-series) (:names version-series) matched-text regex-type)
          matching-type                (matching-type strategy)
          confidence                   (when (= :concluded matching-type) (matching-confidence confidence-explanations))]
      (merge {:id       id
              :strategy strategy
              :source   (list matched-text)
              :type     matching-type}
             (when (= matching-type :concluded)
               (merge {:confidence confidence}
                      (when confidence-explanations {:confidence-explanations confidence-explanations})))))))
