;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.default
  "Default license detection functionality, based on automatic processing of the
  SPDX license and exception lists.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [wreck.api                                         :as re]
            [spdx.identifiers                                  :as si]
            [lice-comb.impl.version-series                     :as verser]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.license-regexes                    :as licre]
            [lice-comb.impl.license-detection.match-processing :as mp]))

;####TODO: REMOVE ONCE UNNEEDED (duplicated in lice-comb.impl.license-detection.utils)
(def ^:private re-placeholder-ver (re-pattern (re/esc verser/placeholder-ver)))

; Default license and exception ids i.e. those that don't need special case support
(def ids-d
  (delay
    (remove #(s/ends-with? % "+") (si/ids))
;####TODO: IMPLEMENT ME!
;    (apply disj (si/ids)
;                (concat @bsd/ids-d @cc/ids-d @cddl/ids-d @cpe/ids-d @cursed/ids-d
;                        @custom/ids-d @epl/ids-d @gnu/ids-d @gnuexc/ids-d
;                        @hippocratic/ids-d @mpl/ids-d @refs/ids-d @wtf/ids-d))
))



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
           [regex (partial mp/unversioned-match->expression-info id regex-type)]))
       regex-map))

(defmethod ^:private build-pairs-from-regexes java.util.Map
  [version-series regex-map]
  (map (fn [regex-map-entry]
         (let [regex-type (key regex-map-entry)
               regex      (val regex-map-entry)]
           [regex (partial mp/versioned-match->expression-info version-series regex-type)]))
       regex-map))

;####TODO: COME UP WITH A BETTER NAME!!!!
(defn- build-regex-fn-pairs
  [id-or-version-series]
  (when-let [regexes (licre/regexes id-or-version-series)]
    (mapcat (partial build-pairs-from-regexes id-or-version-series) regexes)))

(defn- build-regex-fn-pairs-for-ids
  "Builds a sequence of regex/fn pairs for every id in `ids`, sorted by longest
  name to shortest name."  ; ####TODO: OR SORT THE REGEXES, AFTER THEY'VE BEEN GENERATED?  THIS WILL "MIX UP" REGEXES FOR THE SAME ID HOWEVER - POSSIBLY A PROBLEM?
  [ids]
  (let [;####TODO: NEED TO SPLIT IDS INTO DEPRECATED AND NON-DEPRECATED, THEN SORT BY POPULARITY, THEN BY LENGTH!!!!
        {raw-version-series :version-series
         unversioned-ids    :unversioned-ids} (verser/version-series ids)
        version-series (vals raw-version-series)
        all-items      (sort-by (fn [x]    ;####TODO: OR SORT THE REGEXES, AFTER THEY'VE BEEN GENERATED?  THIS WILL "MIX UP" REGEXES FOR THE SAME ID HOWEVER - POSSIBLY A PROBLEM?
                                  (* -1  ; Sort in reverse order (longest to shortest)
                                     (if (string? x)
                                       (count (:name (si/id->info x)))
                                       (apply max (map #(count %) (:name-formats x))))))  ; Only consider the longest name in each version series
                                (concat unversioned-ids version-series))]
    (mapcat build-regex-fn-pairs all-items)))

; Pairs of regex/fn based on listed SPDX license and exception names and ids
(def ^:private pairs-d (delay (build-regex-fn-pairs-for-ids @ids-d)))

(defn detect
  "Detects any default identifiers found inside the `String`s in `coll` and
  replaces them with an expression-info map in that location. Returns other
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
