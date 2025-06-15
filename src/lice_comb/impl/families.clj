;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.families
  "Functionality related to 'families' of licenses. 'Families' are groups of
  licenses that share the same base identifier, with one or more versions.

  Notes:

  * this is _not_ an official SPDX concept
  * this namespace is not part of the public API of lice-comb and may change
    without notice."
  (:require [clojure.string       :as s]
            [rencg.api            :as rencg]
            [lice-comb.impl.utils :as lciu]))

(def ^:private re-family #"(?<family>.*)-(?<version>\d{8}|\d{4}|\d{2}|\d+\.\d+(?:\.\d+)*\w?(?:\+|(?:-.*))?)")

(defn id->family
  "Returns the 'family' of `id` (an SPDX license or exception identifier), or
  `no-family-value` (default: `nil`) if it doesn't belong to a family."
  ([^String id] (id->family nil id))
  ([no-family-value ^String id]
   (when-not (s/blank? id)
     (cond
       ; Special cases
       (s/starts-with? id "LZMA-SDK-") "LZMA-SDK"   ; Has a version range in one of the ids, which is a pain to process in the regex
       ; Default regex based family identification
       :else                           (get (rencg/re-matches-ncg re-family id) "family" no-family-value)))))

(defn family-member?
  "Is `id` a member of a family?"
  [^String id]
  (boolean (id->family id)))

(defn ids->families
  "Turns `ids` into a map of 'families', where each key is a family name
  (`String`) as defined by [[id->family]], and each value is a sequence of
  identifiers in that family, in ascending order (oldest version first).

  Ids that don't belong to a family are included in the result, with
  `group-non-family-ids?` (default: `true`) controlling whether they're grouped
  under the key `:none`, or stored separately with themselves as the key."
  ([ids] (ids->families true ids))
  ([group-non-family-ids? ids]
   (when (seq ids)
     (let [result (lciu/mapfonv sort (group-by #(id->family (if group-non-family-ids? :none %) %) ids))]
       (when-not (empty? result)
         result)))))

(defn id-infos->families
  "Turns `id-infos` (a sequence of id-info maps) into a map of 'families', where
  each key is a family name (`String`) and each value is a sequence of id-info
  maps in that family, in ascending order (oldest version first)."
  ([id-infos] (id-infos->families true id-infos))
  ([group-non-family-ids? id-infos]
  (when (seq id-infos)
    (let [result (lciu/mapfonv (partial sort-by :id) (group-by #(id->family (if group-non-family-ids? :none (:id %)) (:id %)) id-infos))]
      (when-not (empty? result)
        result)))))
