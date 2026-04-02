;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.regex.version-expression
  "Version expression regex related functionality. A version expression is
  comprised of:
  * a version label (e.g. 'v', 'ver', 'version', etc.)
  * a version number (e.g. '2.0', '1.3a', '1.1.3', '86', '2015', '19980720',
    etc.)
  * a suffix (e.g. 'only', 'or later', '+', etc.)

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                      :as s]
            [wreck.api                           :as re]
            [lice-comb.impl.regex.fragments      :as ref]
            [lice-comb.impl.regex.version-number :as ver]))

(def re-version-label (re/fgrp "i" "v" (re/opt-grp "er" (re/opt-grp "sion" (re/opt "s")))))

(def ncg-version-number "VersionNumber")
(def ncg-or-later       "OrLater")
(def ncg-only           "Only")

; These are private, because `suffix-regex` should be used instead, in order to get the correct semantics across all combinations of only/or-later
(def ^:private re-or-later    (let [re-at-your-option (re/join #"\(?" "at" ref/mws "your" ref/mws (re/alt-grp "discretion" "option") #"\)?")]
                                (re/alt #"\+"
                                        (re/join ref/mws
                                                 "or"
                                                 ref/mws
                                                 (re/opt-grp re-at-your-option ref/mws)  ; "At your discretion" can appear before "or later"...
                                                 (re/opt-grp #"a(?:ny)?" ref/mws)
                                                 (re/alt-grp #"lat[eo]r" "newer")
                                                 (re/opt-grp ref/mws #"versions?")
                                                 (re/opt-grp ref/mws re-at-your-option)  ; ...or after it
                                                 #"\)?"))))
(def ^:private re-only        (re/join #"\(?" ref/ows #"only" ref/ows #"\)?"))
(def ^:private re-oool-no-cgs (re/fgrp "i"
                                       (re/alt re-or-later)
                                               (re/join ref/mws re-only)))

(defn suffix-regex
  "Returns a regex fragment that will match various combinations of a version
  'or later or only' suffix, made up of either:

   1. An or-later suffix (+, or later, or at your discretion any later version,
      etc.)
   2. An only suffix (only, etc.)

  If ncg-prefix is provided and is not blank, each of these elements will be
  placed into an NCG whose name will be the value of `ncg-prefix` prefixed to
  one of these values:

  * `OrLater` - containing the matched 'or later' text (item 1 above)
  * `Only` - containing the matched 'only' text (item 2 above)"
  ([] (suffix-regex nil))
  ([^String ncg-prefix]
   (if (s/blank? ncg-prefix)
     re-oool-no-cgs
     (re/fgrp "i"
              (re/alt
                (re/ncg (str ncg-prefix ncg-or-later) re-or-later)
                (re/ncg (str ncg-prefix ncg-only)     ref/mws re-only))))))

(defn expression-regex
  "Returns a regex fragment that will match various combinations of an entire
  version expression, made up of:

  1. Optionally a version label (v, ver, version, etc.)
  2. A version number of the pattern defined by `version-numbers`
  3. Optionally, either:
     a. An only suffix (+, only, etc.)
     b. An or-later suffix (or later, or at your discretion any larer version,
        etc.)

  If ncg-prefix is provided and is not blank, items 2 and 3 on this list will be
  placed into an NCG whose name will be the value of `ncg-prefix` prefixed to
  one of these values:

  * `Version` - containing the matched version number (item #2 above)
  * `Only` - containing the matched 'only' text (item 3a above)
  * `OrLater` - containing the matched 'or later' text (item 3b above)

  Notes:

  * Does not provide any pre- or post- matching logic, so care should be taken
    to ensure that the fragment is wrapped in additional fragments to do that,
    if needed (e.g. whitespace)."
  ([version-numbers] (expression-regex nil version-numbers))
  ([ ^String ncg-prefix version-numbers]
   (when (seq version-numbers)
     (let [version-number-regex (re/ncg (when-not (s/blank? ncg-prefix) (str ncg-prefix ncg-version-number)) (ver/range-regex version-numbers))
           suffix-regex         (suffix-regex ncg-prefix)]
       (re/join (re/opt-grp re-version-label ref/ows)
                version-number-regex
                (re/opt-grp suffix-regex))))))
