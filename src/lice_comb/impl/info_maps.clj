;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.info-maps
  "lice-comb information map (expressions, fragments) helper functions.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string      :as s]
            [spdx.expressions    :as sexp]
            [lice-comb.impl.spdx :as lcis]))

(defn prepend-source-to-fim
  "Prepends the given source `s` (a String) onto the `:source` sequence of a
  fragment information map (`fim`)."
  [^CharSequence s ^java.util.Map {:keys [source] :as fim}]
  (when fim
    (if (s/blank? s)
      fim
      (assoc fim :source (into [(s/trim s)] source)))))  ;####TODO: do we need to check if `s` is already at the front of `source`?  The old code did this, but I'm not sure it's needed...

(defn prepend-source-to-fims-within-em
  "Prepends the given source `s` (a String) onto the `:source` sequences of all
  fragment info maps within `em` (an expressions map)."
  [^CharSequence s ^java.util.Map em]
  (when em
    (if (s/blank? s)
      em
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
                    em)))))

(defn merge-expressions-maps
  "Merges any number of expressions maps, by concatenating and de-duping values
  for the same key (expression). Returns a single map that may contain multiple
  map entries."
  [& ems]
  (let [ems (filter identity ems)]
    (when-not (empty? ems)
      (let [grouped-ems (group-by first (mapcat identity ems))]
        (into {} (map #(vec [% (seq (distinct (mapcat second (get grouped-ems %))))])
                      (keys grouped-ems)))))))

(defn combine-expressions-map-with-operator
  "When expressions map `em` contains more than one expression, combines them
  all together with operation `op` (either `:and` or `:or`), and concatenates
  all of their fragment info maps.  Returns `em` unchanged if it only contains a
  single expression.  Returns `nil` if `em` or `op` are `nil`."
  [^java.util.Map em ^clojure.lang.Keyword op]
  (when (and em op)
    (if (<= (count em) 1)
      em
      (let [new-expr (sexp/canonicalise (s/join (str " " (s/upper-case (name op)) " ") (keys em)))
            new-fims (apply concat (vals em))]
        {new-expr new-fims}))))

(def ^:private confidence-sort {
  :low    0
  :medium 1
  :high   2})

(defn- sort-confidences
  "Sorts a sequence of confidences from low to high."
  [^clojure.lang.Sequential cs]
  (when cs
    (sort-by confidence-sort cs)))

(defn- lowest-confidence
  "Returns the lowest confidence in a sequence of confidences."
  [^clojure.lang.Sequential cs]
  (when cs
    (first (sort-confidences cs))))

(defn- highest-confidence
  "Returns the highest confidence in a sequence of confidences."
  [^clojure.lang.Sequential cs]
  (when cs
    (last (sort-confidences cs))))

(defn- calculate-confidence-for-expression
  "Calculate the confidence for an expression, as the lowest confidence in the
  expression-infos for the identifiers that make up the expression."
  [^clojure.lang.Sequential fims]
  (if-let [confidence (lowest-confidence (filter identity (map :confidence fims)))]
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
  :inconsistent-bsd-clause-counts :low     ; BSD clause counts are inconsistent (e.g. "2 Clause BSD 4 Clause")
  :invalid-bsd-combination        :low     ; Invalid combination of BSD clause counts and suffixes (e.g. "BSD 2 Clause No Nuclear License")
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

(defn fragment-info
  "Returns a fully populated and validated fragment-info map, canonicalising
  `fragment` (if possible).

  Note that `src` can be a `String` or a sequence of `String`s.

  Throws if any argument is invalid."
  ([^CharSequence fragment src ^CharSequence strategy] (fragment-info fragment src strategy nil))
  ([^CharSequence fragment src ^CharSequence strategy ^clojure.lang.Sequential confidence-explanations]
   (let [canonical-fragment
                    (if-let [ci (lcis/canonicalise-spdx-expression-fragment fragment)]
                      ci
                      (throw (ex-info "Internal logic error: invalid fragment" {:fragment fragment :source src})))
         src        (cond
                      (string? src)
                        (if (s/blank? src)
                          (throw (ex-info "Internal logic error: source was a blank string" {:fragment fragment :source src}))
                          (list (s/trim src)))
                      (sequential? src)
                        (let [src (filter (complement s/blank?) src)]
                          (if (empty? src)
                            (throw (ex-info "Internal logic error: source was an empty sequence" {:fragment fragment :source src}))
                            (map s/trim src)))
                      :else
                        (throw (ex-info "Internal logic error: source was an unsupported type" {:fragment fragment :source src :source-type (type src)})))
         match-type (get strategies->match-type strategy :concluded)
         confidence (when (= :concluded match-type)
                      (if (seq confidence-explanations)
                        (let [confidences (set (map confidence-explanation->confidence confidence-explanations))]
                          (if (contains? confidences nil)
                            (throw (ex-info "Internal logic error: a confidence explanation is missing a confidence level" {:confidence-explanations confidence-explanations}))
                            (lowest-confidence confidences)))
                        :high))]  ; Default to :high when confidence-explanations is empty
     (merge {:fragment   canonical-fragment
             :strategy   strategy
             :match-type match-type
             :source     src}
            (when confidence
              {:confidence confidence})
            (when (seq confidence-explanations)
              {:confidence-explanations (set confidence-explanations)})))))
