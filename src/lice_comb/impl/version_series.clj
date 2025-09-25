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
    without notice."
  (:require [clojure.string       :as s]
            [rencg.api            :as rencg]
            [lice-comb.impl.utils :as lciu]))

(def ^:private re-version-series #"(?<prefix>.*?)-(?<version>\d{8}|\d{4}|\d{2}|\d+\.\d+(?:\.\d+)*\w?)(?:\+|(?:-(?<suffix>.*)))?")

;####TODO: consider whether having a configurable `not-versioned` value is helpful
(defn id->version-series
  "Returns the 'version-series' of `id` (an SPDX license or exception
  identifier), or `not-versioned` (default: `nil`) if it doesn't belong to a
  version series."
  ([^String id] (id->version-series nil id))
  ([not-versioned ^String id]
   (when-not (s/blank? id)
     (cond
       ; Special cases
       (s/starts-with? id "LZMA-SDK-") not-versioned   ; Has a version range in one of the ids, which is a pain to process in the regex
       (s/starts-with? id "BSD")       not-versioned   ; e.g. to properly handle BSD-3-Clause-No-Nuclear-License-2014
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

;####TODO: REMOVE ME ONCE TESTED!!!
;                                          (get (rencg/re-matches-ncg re-version-series id) "versionSeries" not-versioned)))))

(defn version-series-member?
  "Is `id` a member of a version series?"
  [^String id]
  (boolean (id->version-series id)))

;####TODO: consider whether having configurable grouping behaviour for non-version-series ids is helpful
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
