;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.spdx-matching
  "SPDX matching guidelines based matching, against long-form texts.

  Notes:

  * this namespace does NOT follow the same pattern as other namespaces in this
    folder.  Specifically, it does NOT support detection _within_ a name (i.e.
    detection during parsing).  This is for the (obvious) reason that the SPDX
    matching guidelines strictly define a different algorithm.
  * this namespace is not part of the public API of lice-comb and may change
    without notice."
  (:require [clojure.set                      :as set]
            [spdx.matching                    :as sm]
            [lice-comb.impl.spdx              :as spdx]
            [lice-comb.impl.parsing.info-maps :as info]))

(def ^:private num-cpus (.availableProcessors (Runtime/getRuntime)))

(defn text->fragment-infos
  [^String s]
  ; clj-spdx's *-within-text APIs are *expensive* but support batching, so we check batches of ids in parallel (CPU bound, so virtual threads aren't appropriate here)
  (let [license-id-batches   (partition num-cpus @spdx/license-ids-d)
        exception-id-batches (partition num-cpus @spdx/exception-ids-d)
        license-ids-found    (apply set/union (pmap #(sm/licenses-within-text   s %) license-id-batches))
        exception-ids-found  (apply set/union (pmap #(sm/exceptions-within-text s %) exception-id-batches))
        expressions-found    (if (and (= 1 (count license-ids-found))
                                      (= 1 (count exception-ids-found)))
                               #{(str (first license-ids-found) " WITH " (first exception-ids-found))}
                               (set/union license-ids-found exception-ids-found))]
    (when expressions-found
      (map #(info/fragment-info % "<content>" "SPDX matching guidelines") expressions-found))))
