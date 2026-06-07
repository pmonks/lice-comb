;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.mx4j
  "MX4J license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.version-expression                 :as verexp]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; MX4J license is Apache 1.1 - see https://wiki.spdx.org/view/Legal_Team/License_List/Licenses_Under_Consideration#Processed_License_Requests

(def ^:private re-mx4j
  (re/fgrp "i"
           ref/nwb
           (re/opt-grp "The" ref/ows)
           "MX4J"
           (re/opt-grp ref/mws ref/public)
           (re/opt-grp ref/mws ref/license)
           (re/opt-grp (verexp/expression-regex ["1.0"]))
           ref/nwa))

(defn detect
  "Detects any proprietary/commercial identifiers found inside the `String`s in
  `coll` and replaces them with an expression-info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re-mx4j (partial mp/match->expression-info "Apache-1.1" "MX4J regex")))
