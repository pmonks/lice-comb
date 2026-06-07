;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.expression-info
  "lice-comb expression-info and expressions-info map helper functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string      :as s]
            [spdx.expressions    :as sexp]
            [lice-comb.impl.spdx :as lcis]))

(defn prepend-source
  "Prepends the given source s (a String) onto the :source sequence of all
  expression-info sub-maps in m (an expressions-info map)."
  [s m]
  (if (or (s/blank? s) (empty? m))
    m
    (into {} (map #(if (sequential? (val %))
                     (let [id            (key %)
                           metadata-list (val %)]
                       (hash-map id (map (fn [x] (assoc x :source (let [old-source (seq (:source x))
                                                                        new-source (if (not= s (first old-source))  ; Only add s if it isn't already there
                                                                                     (conj old-source s)
                                                                                     old-source)]
                                                                    new-source)))
                                         metadata-list)))
                     %)
                  m))))

(defn merge-maps
  "Merges any number of expressions-info maps, by concatenating and de-duping
  values for the same key (expression). Returns a single map that may contain
  multiple map entries."
  [& maps]
  (let [maps (filter identity maps)]
    (when-not (empty? maps)
      (let [grouped-maps (group-by first (mapcat identity maps))]
        (into {} (map #(vec [% (seq (distinct (mapcat second (get grouped-maps %))))])
                      (keys grouped-maps)))))))

(defn join-maps-with-operator
  "Joins `eim`, an expressions-info map with multiple entries into an
  expressions-info map with a single entry that is an SPDX expression joining
  all of the entries via SPDX operator `op` (either `:and` or `:or`)."
  [op eim]
  (when (and op eim)
    (if (<= (count eim) 1)
      eim
      (let [new-expr (sexp/canonicalise (s/join (str " " (s/upper-case (name op)) " ") (keys eim)))
            new-ei   (apply concat (vals eim))]
        {new-expr new-ei}))))

(def ^:private confidence-sort {
  :low    0
  :medium 1
  :high   2})

;####TODO: DOES THIS HAVE TO BE PUBLIC?
(defn sort-confidences
  "Sorts a sequence of confidences from low to high."
  [cs]
  (when cs
    (sort-by confidence-sort cs)))

;####TODO: DOES THIS HAVE TO BE PUBLIC?
(defn lowest-confidence
  "Returns the lowest confidence in a sequence of confidences."
  [cs]
  (when cs
    (first (sort-confidences cs))))

;####TODO: DOES THIS HAVE TO BE PUBLIC?
(defn highest-confidence
  "Returns the highest confidence in a sequence of confidences."
  [cs]
  (when cs
    (last (sort-confidences cs))))

;####TODO: DOES THIS HAVE TO BE PUBLIC?
(defn calculate-confidence-for-expression
  "Calculate the confidence for an expression, as the lowest confidence in the
  expression-infos for the identifiers that make up the expression."
  [expression-infos]
  (if-let [confidence (lowest-confidence (filter identity (map :confidence expression-infos)))]
    confidence
    :high))   ; For when none of the components have a confidence (i.e. they're all :type :declared)

; Map of confidence explanations to their associated confidence scores
(def ^:private confidence-explanation->confidence {
  ; General confidence explanations
  :missing-version                :low     ; Version is completely missing
  :invalid-version                :low     ; Version is invalid (e.g. "Apache 2.1")
  :version-near-match             :medium  ; Version is a near match (e.g. "Apache 2", "Apache 2.0.0")
  :inconsistent-versions          :medium  ; Multiple valid versions found, but they don't match (e.g. "Apache License 1.1 (Apache-2.0)")
  ; BSD-specific confidence explanations
  :missing-bsd-clause-count       :low     ; BSD clause count is missing (e.g. "BSD")
  :invalid-bsd-clause-count       :low     ; BSD clause count is invalid (e.g. "BSD 99 Clause")
  :inconsistent-bsd-clause-counts :medium  ; BSD clause counts are inconsistent (e.g. "2 Clause BSD 4 Clause")
  :invalid-bsd-combination        :medium  ; Invalid combination of BSD clause counts and suffixes (e.g. "BSD 2 Clause No Nuclear License")
  ; CC-BY-specific confidence explanations
  :invalid-cc-suffix              :low
  ; GNU-specific confidence explanations
  :missing-version-suffix         :medium  ; GNU family suffix is missing (e.g. "GPL 2.0")
})

(def ^:private strategies->match-type
  {"SPDX expression"   :declared
   "SPDX identifier"   :declared
   "SPDX LicenseRef"   :declared
   "SPDX AdditionRef"  :declared
   "SPDX special form" :declared})

(defn expression-info
  "Returns a fully populated and validated expression-info map, canonicalising
  `id` as appropriate (including into an SPDX expression, in some cases).

  Throws if any argument is invalid."
  ([^String id-or-expression ^String matched-text ^String strategy] (expression-info id-or-expression matched-text strategy nil))
  ([^String id-or-expression ^String matched-text ^String strategy confidence-explanations]
   (let [canonical-id-or-expression
                    (if-let [ci (lcis/canonicalise-id-or-expression id-or-expression)]
                      ci
                      (throw (ex-info "Internal logic error: an invalid identifier or expression was produced" {:invalid-id id-or-expression :matched-text matched-text})))
         src        (if (s/blank? matched-text)
                      (throw (ex-info "Internal logic error: matched text was blank" {:id-or-expression id-or-expression :matched-text matched-text}))
                      (list (s/trim matched-text)))
         match-type (get strategies->match-type strategy :concluded)
         confidence (when (= :concluded match-type)
                      (if (seq confidence-explanations)
                        (let [confidences (set (map confidence-explanation->confidence confidence-explanations))]
                          (if (contains? confidences nil)
                            (throw (ex-info "Internal logic error: a confidence explanation is missing a confidence level" {:confidence-explanations confidence-explanations}))
                            (lowest-confidence confidences)))
                        :high))]  ; Default to :high when confidence-explanations is empty
     (merge {:id       canonical-id-or-expression
             :strategy strategy
             :type     match-type
             :source   src}
             (when confidence
               {:confidence confidence})
             (when (seq confidence-explanations)
               {:confidence-explanations (set confidence-explanations)})))))
