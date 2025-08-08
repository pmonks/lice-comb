;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.gnu-exceptions
  "Helper functionality related to substituting matches for GNU-esque license
  exceptions.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.set                        :as set]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.substitutions.cpe   :as cpe]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; All GNU-esque exception ids, including deprecated ones
(def ids-d (delay (lcis/sort-ids (filter lcisu/gnu-family? (set/difference (set @lcis/exception-ids-d) (set @cpe/ids-d))))))

; Pairs of regex/fn based on listed SPDX exception names and ids
(def ^:private pairs-d (delay (concat
  ; Default regex matching based on ids
  (lcisu/spdx-match-pairs @ids-d))))

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
