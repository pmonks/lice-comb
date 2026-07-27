;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.spdx-refs
  "Helper functionality related to substituting matches for SPDX LicenseRefs and
  AdditionRefs.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [spdx.regexes                                      :as sre]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(defn- ref-match->ei
  "Turns a match from the ref regex into an expression info map."
  [strategy m]
  (mp/match->fragment-info (:match m) strategy m))


(defn detect
  "Detects any SPDX license or addition refs found in the `String`s in
  `coll` with a fragment info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll
              (sre/license-ref-re)  (partial ref-match->ei "SPDX LicenseRef")
              (sre/addition-ref-re) (partial ref-match->ei "SPDX AdditionRef")))
