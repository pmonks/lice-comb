;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.expat
  "Expat license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; The "expat" license is MIT - see https://en.wikipedia.org/wiki/MIT_License#Ambiguity_and_variants for some history

; Public for ease of testing
(def re (re/fgrp "i"
                 ref/nwb
                 (re/opt-grp "The" ref/ows)
                 "Expat"
                 (re/opt-grp (re/zom ref/ws+slashes) "MIT")
                 (re/opt-grp ref/mws ref/public)
                 (re/opt-grp ref/mws ref/license)
                 ref/nwa))

(defn detect
  "Detects any Expat identifiers found inside the `String`s in `coll` and
  replaces them with a fragment info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse coll re (partial mp/match->fragment-info "MIT" "Expat regex")))
