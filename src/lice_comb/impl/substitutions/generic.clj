;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.generic
  "Helper functionality related to substituting matches for any generic license
  or exceptions that are not handled explicitly.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                              :as s]
            [clojure.set                                 :as set]
            [rencg.api                                   :as rencg]
            [wreck.api                                   :as re]
            [spdx.identifiers                            :as si]
            [lice-comb.impl.spdx                         :as lcis]
            [lice-comb.impl.regexes                      :as lcir]
            [lice-comb.impl.version-series               :as lcivs]
            [lice-comb.impl.substitutions.bsd            :as bsd]
            [lice-comb.impl.substitutions.cc             :as cc]
            [lice-comb.impl.substitutions.cddl           :as cddl]
            [lice-comb.impl.substitutions.cpe            :as cpe]
            [lice-comb.impl.substitutions.cursed         :as cursed]
            [lice-comb.impl.substitutions.custom         :as custom]
            [lice-comb.impl.substitutions.epl            :as epl]
            [lice-comb.impl.substitutions.gnu            :as gnu]
            [lice-comb.impl.substitutions.gnu-exceptions :as gnuexc]
            [lice-comb.impl.substitutions.hippocratic    :as hippocratic]
            [lice-comb.impl.substitutions.mpl            :as mpl]
            [lice-comb.impl.substitutions.refs           :as refs]
            [lice-comb.impl.substitutions.wtf            :as wtf]
            [lice-comb.impl.substitutions.utils          :as lcisu]))

; Ids of all of the generic licenses i.e. those without special-cased support
; Note: includes both license AND exception identifiers
(def ids-d
  (delay
    (apply disj (si/ids)
                (concat @bsd/ids-d @cc/ids-d @cddl/ids-d @cpe/ids-d @cursed/ids-d
                        @custom/ids-d @epl/ids-d @gnu/ids-d @gnuexc/ids-d
                        @hippocratic/ids-d @mpl/ids-d @refs/ids-d @wtf/ids-d))))

(defn- sorter
  "A comparator for sorting a mix of version series maps and (unversioned) ids
  (Strings) by type descending, then name length descending, then id."
  [x y]
  (let [x-id         (if (map? x) (:default-id x) x)
        y-id         (if (map? y) (:default-id y) y)
        x-info       (si/id->info x-id)
        y-info       (si/id->info y-id)
        type-compare (compare (:type y-info) (:type x-info))]  ; Descending order (:license before :exception)
    (if-not (zero? type-compare)
      type-compare
      (let [name-length-compare (compare (count (:name y-info)) (count (:name x-info)))]  ; Descending order (longer names first)
        (if-not (zero? name-length-compare)
          name-length-compare
          (compare x-id y-id))))))


;####TODO: MAKE THESE PRIVATE
(def ids-and-version-series-d
  (delay
    (let [id-infos                              (map si/id->info @ids-d)
          non-deprecated-ids                    (map :id (filter (complement :deprecated?) id-infos))
          deprecated-ids                        (map :id (filter :deprecated? id-infos))
          non-deprecated-version-series-and-ids (let [[vs ids] (lcivs/version-series non-deprecated-ids)] (concat vs ids))
          deprecated-version-series-and-ids     (let [[vs ids] (lcivs/version-series deprecated-ids)] (concat vs ids))]
      (concat
        (sort sorter non-deprecated-version-series-and-ids)
        (sort sorter deprecated-version-series-and-ids)))))

; Latest non-deprecated version of ids that have multiple versions (i.e. are in a version series)
(def ^:private latest-version-ids-d (delay
                                      (let [ids-to-consider (remove #(or (:deprecated? (lcis/id->info %))  ; Ignore deprecated ids for versionless matching
                                                                         (s/starts-with? % "LZMA-SDK-")    ; Version-less matching not feasible
                                                                         (s/starts-with? % "OPL-")         ; License name is too short & generic for matching
                                                                         (s/starts-with? % "OSL-"))        ; License name is too short & generic for matching
                                                                    @ids-d)
                                            version-series  (dissoc (lcivs/ids->version-series ids-to-consider) nil)]  ; Remove all "no version series" ids
                                        (lcis/sort-ids (map last (vals version-series))))))

(def ^:private re-name-or-id-and-version (re/flags-grp "iuU" #"(?<nameOrId>.+?)" (re/opt-grp lcir/fre-ows (lcir/re-version))))

(defn- id->deversioned-name
  "Converts `id` into its name, with any version components removed."
  [id]
  (when-let [n (:name (lcis/id->info id))]
    (get (rencg/re-matches-ncg re-name-or-id-and-version n) "nameOrId")))

(defn- id->deversioned-name-regex
  "Gets the name of the license with identifier `id`, removed the version from
  it, then turns that into a name regex."
  [id]
  (when-let [n (id->deversioned-name id)]
    (lcir/name->regex n)))

(defn- id->deversioned-id
  "Returns `id` with any version components removed."
  [id]
  (when id
    (get (rencg/re-matches-ncg re-name-or-id-and-version id) "nameOrId")))

(defn- id->deversioned-id-regex
  "Gets the name of the license with identifier `id`, removed the version from
  it, then turns that into an id regex."
  [id]
  (when-let [id (id->deversioned-id id)]
    (lcir/id->regex id)))

; Pairs of regex/fn based on listed SPDX license and exception names and ids
(def ^:private pairs-d (delay (concat
  ; Default regex matching based on ids
  (lcisu/spdx-match-pairs @ids-d)

  ; Regex matching based on names minus versions
  (map #(vec [(id->deversioned-name-regex %) (lcisu/simple-regex-match-ei-fn % :low #{:missing-version})]) @latest-version-ids-d)

  ; Regex matching based on ids minus versions
  (map #(vec [(id->deversioned-id-regex %) (lcisu/simple-regex-match-ei-fn % :low #{:missing-version})]) @latest-version-ids-d))))

(defn sub
  "Substitutes any licenses found in the `String`s in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (si/init!)
  (lcis/init!)
  @ids-d
  @latest-version-ids-d
  @ids-and-version-series-d
  @pairs-d
  nil)

