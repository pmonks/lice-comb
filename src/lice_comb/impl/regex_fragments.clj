;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.regex-fragments
  "Regex fragments. Note: this namespace is not part of the public API of
  lice-comb and may change without notice."
  (:require [wreck.api :as re]))

; GUIDING PRINCIPLES FOR THE STATIC REGEX FRAGMENTS IN THIS NAMESPACE:
;
; 1. None of the regexes use capturing groups, named or otherwise - it's up to
;    callers to add them, if needed.
;
; 2. None of these regexes (except for the whitespace ones, naturally) capture
;    whitespace at the beginning or end, or check for word boundaries there, or
;    similar boundary conditions. Again it's up to callers to add that, if
;    needed.
;
; These principles exist to make these fragments easier to reason about and
; compose into larger regexes, *especially* regexes that might include these
; fragments repeatedly (where ncgs in particular become problematic due to
; limitations in the JVM's regex engine).

(def ^:private raw-hyphens #"\p{Pd}")
(def ^:private raw-ws      (re/join #"\p{IsWhitespace}" raw-hyphens (re/esc "_,.()")))  ; Character classes etc. that we consider to be whitespace
(def ^:private raw-slashes (re-pattern (re/esc "\\/")))
(def ^:private raw-quotes  (re/join (re/esc "\"'`") #"\p{Pi}\p{Pf}"))

(def ws                    (re/chcl raw-ws))
(def ows                   (re/zom ws))
(def mws                   (re/oom ws))

(def hyphens               (re/chcl raw-hyphens))
(def slashes               (re/chcl raw-slashes))

(def qots                  (re/chcl raw-quotes))

(def ws+hyphens            (re/chcl raw-ws raw-hyphens))
(def ws+slashes            (re/chcl raw-ws raw-slashes))
(def hyphens+slashes       (re/chcl raw-hyphens raw-slashes))
(def ws+hyphens+slashes    (re/chcl raw-ws raw-hyphens raw-slashes))

(def nwb                   (re/-lb #"\w"))  ; nwb = no word before
(def nwa                   (re/-la #"\w"))  ; nwa = no word after

(def date                  (re/fgrp "i"  ;####TODO: NOT SURE THIS SHOULD BE HERE
                                    ; Day (optional)
                                    (re/opt-grp #"\d\d?" ows #"(?:st|nd|rd|th)?" ows)
                                    ; Textual month (mandatory)
                                    (re/alt-grp #"Jan(?:uary)?" #"Feb(?:ruary)?" #"Mar(?:ch)?" #"Apr(?:il)?" #"May" #"June?" #"July?" #"Aug(?:ust)?" #"Sep(?:t(?:ember)?)?" #"Oct(?:ober)?" #"Nov(?:ember)?" #"Dec(?:ember)?")
                                    ; Numeric year (mandatory - either 2 or 4 digits)
                                    ows #"\d\d(?:\d\d)?" ows))

