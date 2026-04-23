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
  "Helper functionality related to substituting matches for certain cursed
  license names.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                      :as re]
            [lice-comb.impl.regex-fragments :as ref]
            [lice-comb.impl.faux-parse      :as faux]))

; Map of name values seen in the wild that are too ambiguous / cursed to support any reasonable form of automated parsing
(def ^:private cursed-names {
  ; Seen in https://repo.maven.apache.org/maven2/com/sun/mail/all/1.4.7/all-1.4.7.pom and other javax.mail/javax.mail-api artifacts
  "GPLv2+CE" '({:id "GPL-2.0-only"            :type :concluded :confidence :high :strategy :manual-verification :source ("GPLv2+CE" "GPLv2")}
               :with
               {:id "Classpath-exception-2.0" :type :concluded :confidence :high :strategy :manual-verification :source ("GPLv2+CE" "CE")})})

(defn- cursed-re
  "Builds a regex for the given cursed name `s`."
  [s]
  (when s
    (re/fgrp "i" ref/nwb (re/esc s) ref/nwa)))

(def ^:private pairs
  (map #(vector (cursed-re (key %)) (val %)) cursed-names))

(defn detect
  "Detects any cursed expressions found inside the `String`s in `coll` and
  replaces them with an expression-info map in that location. Returns other
  elements unchanged."
  [coll]
  (faux/parse-with-pairs pairs coll))
