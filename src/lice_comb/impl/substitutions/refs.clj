;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.refs
  "Helper functionality related to substituting matches for SPDX LicenseRefs and
  AdditionRefs.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [spdx.regexes                       :as sre]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; NOTE: we can't use the clj-spdx regexes for listed identifiers, since they
; don't capture enough text (e.g. when presented with "GNU LGPL-3.0 or later"
; those regexes will only capture "LGPL-3.0")

; This is redundant here, but we include it for consistency with other substutition namespaces
(def ids-d (delay '()))

(defn- match->ei
  "Turns a match from the clj-spdx regex into an expression info map."
  [strategy m]
  (when-let [ref (:match m)]
    {:id         ref
     :type       :declared
     :strategy   strategy
     :source     ref}))

(defn sub
  "Substitutes any SPDX license or addition refs found in the `String`s in
  `coll` with an expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res [[(sre/license-ref-re)  (partial match->ei :spdx-license-ref)]
                  [(sre/addition-ref-re) (partial match->ei :spdx-addition-ref)]]
                 coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (sre/init!)
  @ids-d
  nil)
