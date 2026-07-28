;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.public-domain
  "Public domain license detection, producing a custom lice-comb LicenseRef.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.spdx                               :as spdx]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(def ^:private re-public-domain
  (let [re-cc0 (re/alt-grp (re/join #"Creative" ref/ows #"Commons") (re/join "CC" (re/n2m 0 4 ref/ws+hyphens+slashes) (re/n2m 0 4 "0")))]  ; Make sure this regex _doesn't_ match if preceeded or followed by "CC0"
    (re/fgrp "i"
             ref/nwb
             (re/-lb re-cc0 ref/bounded-ows)  ; Old versions of the JVM don't support unbounded look behind groups
             ref/public (re/zom ref/ws+hyphens+slashes) "Domain"
             (re/opt-grp ref/mws ref/license)
             (re/-la ref/bounded-ows (re/opt-grp #"per" ref/bounded-ows) re-cc0)  ; Or unbounded look ahead groups
             ref/nwa)))

(defn detect
  "Detects any proprietary/commercial identifiers found inside the `String`s in
  `coll` and replaces them with a fragment info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re-public-domain (partial mp/match->fragment-info (spdx/public-domain) "Public domain regex")))
