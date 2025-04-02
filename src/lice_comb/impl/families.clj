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
  "Functionality related to 'families' of licenses. These are groups of licenses
  that have same base identifier, with one or more versions.
  Notes:

  * this is _not_ an official SPDX concept
  * this namespace is not part of the public API of lice-comb and may change
    without notice."
  (:require [clojure.string       :as s]
            [rencg.api            :as rencg]
            [lice-comb.impl.utils :as lciu]))

(defn id->family
  "Returns the 'family' of `id` (and SPDX license or exception identifier), or
  `nil` if it doesn't belong to a family."
  [id]
  (when-not (s/blank? id)
    (get (rencg/re-matches-ncg #"(?<family>.*)-(?<version>\d+(?:\.\d+)*\w?)" id) "family")))

(defn ids->families
  "Turns `ids` into a map of 'families', where each key is a family name
  (`String`) and each value is a sequence of identifiers in that family, in
  ascending order (oldest version first).  `ids` that aren't members of a family
  will not appear in the result."
  [ids]
  (lciu/mapfonv sort (dissoc (group-by id->family ids) nil)))
