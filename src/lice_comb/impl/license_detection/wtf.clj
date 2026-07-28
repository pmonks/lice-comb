;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.wtf
  "Helper functionality related to substituting matches for the WTFPL license.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(def ids-d (delay ["WTFPL"]))

(def re-wtf (re/fgrp "i"
              (re/alt-grp
                (re/join (re/opt-grp ref/ows "WTFPL" ref/ows)
                         (re/opt-grp "The" ref/mws)
                         "Do" ref/mws
                         (re/alt-grp "WTF" (re/join "What" ref/mws "The" ref/mws #"[f*][us*][c*][k*]")) ref/mws   ; Note: don't need to escape * inside character classes
                         (re/alt-grp "You" "U") ref/mws
                         "Want" ref/mws
                         (re/opt (re/alt-grp "To" "2")))
                "WTFPL")
              (re/opt-grp ref/mws ref/public)
              (re/opt-grp ref/mws ref/license)
              (re/opt-grp ref/ows ref/version-label ref/ows #"[\d\.]+")   ; We don't care about capturing any version numbers included in a WTFPL value, as it has no versions
              #"(?!\w)"))

(defn detect
  "Detects any WTFPL identifiers found inside the `String`s in `coll` and
  replaces them with a fragment info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse coll re-wtf (partial mp/match->fragment-info "WTFPL" "WTFPL regex")))
