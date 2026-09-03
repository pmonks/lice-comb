;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.freebsd-netbsd
  "FreeBSD / NetBSD license detection.  Some variations of these are also
  detected by lice-comb.impl.license-detection.bsd

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [wreck.api                                         :as re]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; Public for ease of testing
(def re (re/fgrp "i"
                 ref/nwb
                 (re/opt-grp "The" ref/ows)
                 (re/ncg "flavour" (re/alt-grp "Free" "Net")) ref/ows "BSD"
                 (re/opt-grp ref/mws ref/public)
                 (re/opt-grp ref/mws ref/license)
                 ref/nwa))

(def ^:private flavour->id {
  "free" "BSD-2-Clause-FreeBSD"   ; We use deprecated identifiers here, and let clj-spdx canonicalise them
  "net"  "BSD-2-Clause-NetBSD"})

(defn- freebsd-netbsd-match->ei
  "Turns a match from the ref regex into an expression info map."
  [m]
  (let [flavour (s/lower-case (s/trim (get m "flavour")))
        id      (get flavour->id flavour)]
    (mp/match->fragment-info id "FreeBSD/NetBSD regex" m)))

(defn detect
  "Detects any FreeBSD or NetBSD identifiers found inside the `String`s in
  `coll` and replaces them with a fragment info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re freebsd-netbsd-match->ei))
