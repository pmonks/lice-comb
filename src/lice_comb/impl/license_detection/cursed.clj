;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.cursed
  "'Cursed' license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                         :as re]
            [lice-comb.impl.regexes.fragments  :as ref]
            [lice-comb.impl.parsing.faux-parse :as faux]
            [lice-comb.impl.parsing.info-maps  :as info]))

; Map of name values seen in the wild that are too ambiguous / cursed to support any reasonable form of automated parsing
(def ^:private cursed-names {
  ; Seen in https://repo.maven.apache.org/maven2/com/sun/mail/all/1.4.7/all-1.4.7.pom and other javax.mail/javax.mail-api artifacts
  "GPLv2+CE" (list (info/fragment-info "GPL-2.0-only" ["GPLv2+CE" "GPLv2"] "Manual verification")
                   :with
                   (info/fragment-info "Classpath-exception-2.0" ["GPLv2+CE" "CE"] "Manual verification"))})

;####TODO: REMOVE ONCE TESTED!!!!
;             '({:id "GPL-2.0-only"            :type :concluded :confidence :high :strategy "Manual verification" :source ("GPLv2+CE" "GPLv2")}
;               :with
;               {:id "Classpath-exception-2.0" :type :concluded :confidence :high :strategy "Manual verification" :source ("GPLv2+CE" "CE")})})

(def ^:private pairs
  (map #(vector (re/fgrp "i" ref/nwb (re/esc (key %)) ref/nwa)
                (val %))
       cursed-names))

(defn detect
  "Detects any cursed expressions found inside the `String`s in `coll` and
  replaces them with a fragment info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse-with-pairs pairs coll))
