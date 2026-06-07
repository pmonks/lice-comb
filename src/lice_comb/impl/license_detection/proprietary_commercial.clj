;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.proprietary-commercial
  "Proprietary/commercial license detection, producing a custom lice-comb
  LicenseRef.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.spdx                               :as lcis]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(def ^:private re-proprietary-commercial
  (re/fgrp "i"
           ref/nwb
           (re/alt-grp (re/join ref/proprietary (re/opt-grp (re/oom ref/ws+hyphens+slashes) "Commercial"))
                       "Commercial"
                       (re/join (re/opt-grp "Copyright" ref/mws (re/n2m 0 20 ".")) "All" ref/mws "Rights" ref/mws "Reserved")
                       "Private")
           (re/opt-grp ref/mws ref/license)
           ref/nwa))

(defn detect
  "Detects any proprietary/commercial values found inside the `String`s in
  `coll` and replaces them with an expression-info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re-proprietary-commercial (partial mp/match->expression-info (lcis/proprietary-commercial) "Proprietary/commercial regex")))
