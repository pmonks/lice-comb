;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.listed-licenses
  "License detection functionality, based on automatic processing of the SPDX
  license and exception lists.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.spdx                               :as spdx]
            [lice-comb.impl.version-series                     :as verser]
            [lice-comb.impl.regexes.license                    :as relic]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]
            [lice-comb.impl.license-detection.bsd              :as bsd]
            [lice-comb.impl.license-detection.cc               :as cc]
            [lice-comb.impl.license-detection.gnu              :as gnu]
            [lice-comb.impl.license-detection.wtf              :as wtf]))

; Default license and exception ids i.e. those that don't need special case support
(def ids-d
  (delay
    (apply disj @spdx/ids-d
                (concat @bsd/ids-d @cc/ids-d @gnu/ids-d @wtf/ids-d))))

;####TODO: COME UP WITH A BETTER NAME!!!!
(defmulti ^:private build-pairs-from-regexes
  "Returns a sequence of regex/function pairs for a given item, either an SPDX
  identifier (`String`) or a version series (a map)."
  {:arglists '([id-or-version-series regex-map])}
  (fn [id-or-version-series _] (class id-or-version-series)))

(defmethod ^:private build-pairs-from-regexes java.lang.String
  [^String id regex-map]
  (map (fn [regex-map-entry]
         (let [regex-type (key regex-map-entry)
               regex      (val regex-map-entry)]
           [regex (partial mp/unversioned-match->fragment-info id regex-type)]))
       regex-map))

(defmethod ^:private build-pairs-from-regexes java.util.Map
  [version-series regex-map]
  (map (fn [regex-map-entry]
         (let [regex-type (key regex-map-entry)
               regex      (val regex-map-entry)]
           [regex (partial mp/versioned-match->fragment-info version-series regex-type)]))
       regex-map))

;####TODO: COME UP WITH A BETTER NAME!!!!
(defn- build-regex-fn-pairs
  [id-or-version-series]
  (when-let [regexes (relic/regexes id-or-version-series)]
    (mapcat (partial build-pairs-from-regexes id-or-version-series) regexes)))

(defn- build-regex-fn-pairs-for-ids
  "Builds a sequence of regex/fn pairs for every id in `ids`, in such an order
  that maximises the chances of correct matches."  ; ####TODO: OR SORT THE REGEXES, AFTER THEY'VE BEEN GENERATED?  THIS WILL "MIX UP" REGEXES FOR THE SAME ID HOWEVER - POSSIBLY A PROBLEM?
  [ids]
  (let [;####TODO: NEED TO SPLIT IDS INTO DEPRECATED AND NON-DEPRECATED, THEN SORT!!!!
        {raw-version-series :version-series
         unversioned-ids    :unversioned-ids} (verser/version-series ids)
        version-series (vals raw-version-series)
        all-items      (concat unversioned-ids version-series)
        re-fn-pairs    (mapcat build-regex-fn-pairs all-items)]
    (reverse (sort-by #(count (re/str' (first %))) re-fn-pairs))))  ; Sort from longest regex to shortest

; Pairs of regex/fn based on listed SPDX license and exception names and ids
(def ^:private pairs-d (delay (build-regex-fn-pairs-for-ids @ids-d)))

(defn detect
  "Detects any default identifiers found inside the `String`s in `coll` and
  replaces them with a fragment info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse-with-pairs @pairs-d coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
;####TODO: SORT THIS OUT
;  (si/init!)
;  (lcis/init!)
  @ids-d
  @pairs-d
  nil)
