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
;    similar boundary conditions. It's up to callers to add that, if needed.
;
; 3. None of these regexes match case INsensitively, despite that usually being
;    required.  It's up to callers to wrap them in an appropriate flag group.
;
; These principles exist to make these fragments easier to reason about and
; compose into larger regexes, *especially* regexes that might include these
; fragments repeatedly (where ncgs in particular become problematic due to
; limitations in the JVM's regex engine).

(def ^:private raw-hyphens #"\p{Pd}")
(def raw-ws      (re/join #"\p{IsWhitespace}" raw-hyphens (re/esc "_,.()")))  ; Character classes etc. that we consider to be whitespace
(def ^:private raw-slashes (re-pattern (re/esc "\\/")))
(def ^:private raw-quotes  (re/join (re/esc "\"'`") #"\p{Pi}\p{Pf}"))  ; Note: " ' ` not considered initial (\p{Pi}) or final (\p{Pf}) punctuation in Unicode

; Whitespace and punctuation character classes

(def ws                 (re/chcl raw-ws))
(def ows                (re/zom ws))
(def mws                (re/oom ws))
(def bounded-ows        (re/n2m 0 4 ws))  ; Useful in look aheads / behinds on older JVM versions that don't support unbounded expressions there
(def bounded-mws        (re/n2m 1 4 ws))  ; Ditto

(def hyphens            (re/chcl raw-hyphens))
(def slashes            (re/chcl raw-slashes))

(def qots               (re/chcl raw-quotes))
(def oqots              (re/zom qots))
(def mqots              (re/oom qots))
(def single-qots        (re/chcl (re/esc "'`‛’‘‚❜❛＇")))  ; Note: these are all different Unicode characters, despite appearances

(def ws+hyphens         (re/chcl raw-ws raw-hyphens))
(def ws+slashes         (re/chcl raw-ws raw-slashes))
(def hyphens+slashes    (re/chcl raw-hyphens raw-slashes))
(def ws+hyphens+slashes (re/chcl raw-ws raw-hyphens raw-slashes))

; No words before/after

(def nwb (re/-lb #"\w"))
(def nwa (re/-la #"\w"))

; Dates (note: approximate - will match some invalid dates such as "99nd May 2025")

(def date (re/grp ; Day (optional)
                  (re/opt-grp #"\d\d?" ows #"(?:st|nd|rd|th)?" mws)
                  ; Textual month (mandatory)
                  (re/alt-grp #"Jan(?:uary)?" #"Feb(?:ruary)?" #"Mar(?:ch)?" #"Apr(?:il)?" #"May" #"June?" #"July?" #"Aug(?:ust)?" #"Sep(?:t(?:ember)?)?" #"Oct(?:ober)?" #"Nov(?:ember)?" #"Dec(?:ember)?")
                  mws
                  ; Numeric year (mandatory - either 2 or 4 digits)
                  #"\d\d(?:\d\d)?"))

; Number variations

(def decimal (re/join #"\d+" (re/opt-grp #"\.\d+")))
(def semver  (re/join #"\d+" (re/zom-grp #"\.\d+")))

; Common word variations

(def ands            (re/alt-grp "and" "&"))
(def withs           (re/alt-grp "with" "w/"))
(def acknowledgement #"acknowledge?ment")
(def license         #"licen?[cs]e")   ; Note: the optional missing `n` is a known misspelling in a POM license name: https://repo.clojars.org/net/unit8/excelebration/excelebration/0.2.0/excelebration-0.2.0.pom
(def public          #"pub?lic")       ; Note: the optional missing `b` is a known misspelling in a POM license name: https://repo.clojars.org/org/immutant/immutant-common/1.1.4/immutant-common-1.1.4.pom (there are others too)
(def proprietary     #"propriet[aoe]ry")
(def general         #"genere?al")     ; Note: "genereal" is a known misspelling in a POM license name: https://repo.clojars.org/clj-file-zip/clj-file-zip/0.1.0/clj-file-zip-0.1.0.pom
(def software        (re/alt-grp #"Software" #"SW"))
(def version-label   (re/join "v" (re/opt-grp "er" (re/opt-grp "sion" (re/opt "s")))))

; Country variants

(def au (re/alt-grp "Australia" "AU"))
(def at (re/alt-grp "Austria" "AT"))
(def gb (re/alt-grp "Great Britain" "United Kingdom" (re/join #"Eng(?:land)?" mws ands mws #"Wales") "GB" "UK"))  ; Note: while geopolitically inaccurate, this tends to match how these terms are used in practice
(def fr (re/alt-grp "France" "FR"))
(def de (re/alt-grp "Germany" "Deutsche" "DE"))
(def jp (re/alt-grp "Japan" "JP"))
(def nl (re/alt-grp "Netherlands" "NL"))
(def us (re/alt-grp (re/join "United" mws "States" (re/opt-grp (re/opt-grp mws "of") mws "America")) #"USA?"))
