;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.regexes.version-number
  "Version number regex related functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                :as s]
            [wreck.api                     :as re]
            [lice-comb.impl.utils          :as u]
            [lice-comb.impl.version-number :as vernum]
            [lice-comb.impl.3rd-party      :as third-party]))

; Public because they're used during license detection
(def re-separators-date   (re/chcl (re/esc "-–—_")))
(def re-separators-semver (re/chcl (re/esc "-–—_,.")))

(defn exact-regex
  "Returns a regex fragment that will match variations on the exact
  `version-number` (a `String`). `strict-separators?` (a `boolean`, default
  `false`) controls whether separator matching is strict (`.` only for `:semver`,
  `-` only for `:date`) or a wider variety of possibilities.

  This regex fragment is primarily intended to be used to find version numbers
  inside the official names of SPDX listed licenses.  For finding potential
  version numbers as part of a human-authored license name in some random input
  text, [[range-regex]] is a better choice.

  Notes:

  * Does not provide any pre- or post- matching logic, so care should be taken
    to ensure that the fragment is wrapped in additional fragments to do that,
    if needed.
  * Does not make use of any kind of capture groups - it is expected that
    callers will wrap the returned fragment in such a thing if needed."
  ([^String version-number] (exact-regex version-number false))
  ([^String version-number strict-separators?]
   (when-let [{components :components version-type :type suffix :suffix} (vernum/parse version-number)]
     (re/join (case version-type
                :year2  (re/join #"0*(?:19)?" (vernum/canonicalise-components version-type components nil))  ; We only check for 19xx, as no SPDX listed license since 2000 has had a 2 digit year
                :year4  (re/join #"0*"        (vernum/canonicalise-components version-type components nil))
                :date   (let [separator  (re/opt (if strict-separators? (re/esc "-") re-separators-date))]
                          (re/join #"0*" (first components) separator #"0*" (second components) separator #"0*" (u/third components)))
                :semver (let [components (concat [(first components)] (seq (third-party/rdrop-while zero? (rest components))))  ; Drop any trailing ".0" components (we handle them below)
                              separator  (if strict-separators? (re/esc ".") re-separators-semver)]
                          (re/join (s/join separator (map #(str "0*" %) components))               ; Allow any number of 0s at the start of each component
                                   (when (= version-type :semver) (re/zom-grp separator "0+")))))  ; Allow any number of ".0" to appear at the end of a semver version
              (when suffix (re/fgrp "i" suffix))))))  ; Match suffix (if there is one) case-insensitively

(defn range-regex
  "Returns a regex fragment that will match any possible value for the given
  `version-numbers` (a sequence of `String`s), **including values that aren't in
  the sequence but have the same pattern**. `strict-separators?` (a `boolean`,
  default `false`) controls whether separator matching is strict (`.` only for
  `:semver`, `-` only for `:date`) or not.

  Notes:

  * Does not provide any pre- or post- matching logic, so care should be taken
    to ensure that the fragment is wrapped in additional fragments to do that,
    if needed.
  * Does not make use of any kind of capture groups - it is expected that
    callers will wrap the returned fragment in such a thing if needed."
  ([version-numbers] (range-regex version-numbers false))
  ([version-numbers strict-separators?]
   (when (seq (filter (complement s/blank?) version-numbers))
     (let [{version-type :version-type suffix? :version-suffix?} (vernum/metadata version-numbers)]
       (re/join (case version-type
                  :year2  #"0*(?:19)?\d{2}"
                  :year4  (if (some (partial re-matches #"\A19\d\d\p{Alpha}?") version-numbers)
                            ; version-numbers includes some dates from the 20th century, so accept 2 digit variations (e.g. "86")
                            #"0*(?:[1-9]\d{3}|\d{2})"
                            ; version-numbers are all outside the 20th century, so only accept 4 digits
                            #"0*[1-9]\d{3}")  ; Note: we ignore dates before the year 1000
                  :date   (let [separator  (if strict-separators? (re/esc ".") (re/chcl (re/esc "-–—_")))]
                            (re/join #"0*[1-9]\d{3}" (re/opt-grp separator #"0*") #"\d{2}" (re/opt-grp separator #"0*") #"\d{2}"))  ; Note: we ignore dates before the year 1000
                  :semver (let [separator (if strict-separators? (re/esc ".") (re/chcl (re/esc "-–—_,.")))]
                            (re/join #"\d+" (re/zom-grp separator #"\d+"))))
                (when suffix? #"\p{Alpha}?"))))))
