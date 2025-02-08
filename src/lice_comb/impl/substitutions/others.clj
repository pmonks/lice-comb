;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.others
  "Helper functionality related to substituting matches for any other licenses
  or exceptions not otherwise handled explicitly.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.set                              :as set]
            [lice-comb.impl.spdx                      :as lcis]
            [lice-comb.impl.substitutions.bsd         :as bsd]
            [lice-comb.impl.substitutions.cc          :as cc]
            [lice-comb.impl.substitutions.cddl        :as cddl]
            [lice-comb.impl.substitutions.cpe         :as cpe]
            [lice-comb.impl.substitutions.epl         :as epl]
            [lice-comb.impl.substitutions.gnu         :as gnu]
            [lice-comb.impl.substitutions.hippocratic :as hippocratic]
            [lice-comb.impl.substitutions.mpl         :as mpl]
            [lice-comb.impl.substitutions.wtf         :as wtf]
            [lice-comb.impl.substitutions.utils       :as lcisu]))

; Ids of all of the "other" licenses i.e. those without special-cased support
; Note: includes both license AND exception identifiers
(def ids-d (delay (apply disj (set/union @lcis/license-ids-d @lcis/exception-ids-d)
                              (concat @bsd/ids-d @cc/ids-d @cddl/ids-d @cpe/ids-d
                                      @epl/ids-d @gnu/ids-d @hippocratic/ids-d
                                      @mpl/ids-d @wtf/ids-d))))

; Pairs of regex/fn based on SPDX license and exception names and ids
(def ^:private pairs-d (delay (lcisu/spdx-match-pairs @ids-d)))

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
  (lcis/init!)
  @ids-d
  @pairs-d
  nil)

