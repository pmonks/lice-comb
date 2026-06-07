;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.spdx-special-forms
  "Helper functionality related to substituting matches for SPDX special forms
  (NONE and NOASSERTION).

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [spdx.regexes                                      :as sre]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(defn- special-form-match->ei
  "Turns a match from the special form regex into an expression info map."
  [m]
  (mp/match->expression-info (:match m) "SPDX special form" m))

(defn detect
  "Detects any SPDX license or addition refs found in the `String`s in
  `coll` with an expression-info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll
              (sre/special-form-re) special-form-match->ei))
