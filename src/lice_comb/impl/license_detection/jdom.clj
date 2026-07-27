;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.jdom
  "JDOM license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; JDOM License is Plexus - see https://lists.spdx.org/g/Spdx-legal/topic/22080544#msg997

(def ^:private re-jdom
  (re/fgrp "i"
           ref/nwb
           "Similar" ref/mws
           "to" ref/mws
           (re/opt-grp "the" ref/mws)
           "Apache" ref/mws
           (re/opt-grp ref/license ref/mws)
           (re/opt-grp "but" ref/mws)
           ref/withs ref/mws
           (re/opt-grp "the" ref/mws)
           ref/acknowledgement ref/mws
           "clause" ref/mws
           "removed"
           ref/nwa))

(defn detect
  "Detects any JDOM license declarations found inside the `String`s in `coll`
  and replaces them with a fragment info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse coll re-jdom (partial mp/match->fragment-info "Plexus" "JDOM regex")))
