;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.bouncy-castle
  "Bouncy Castle license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; Bouncy Castle License is MIT - see https://github.com/spdx/license-list-XML/issues/910

(def ^:private re-bouncy-castle
  (re/fgrp "i"
           ref/nwb
           (re/opt-grp "The" ref/ows)
           "Bouncy" ref/ows "Castle"
           (re/opt-grp ref/mws ref/license)
           ref/nwa))

(defn detect
  "Detects any Bouncy Castle license declarations found inside the `String`s in
  `coll` and replaces them with an expression-info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re-bouncy-castle (partial mp/match->expression-info "MIT" "Bouncy Castle regex")))
