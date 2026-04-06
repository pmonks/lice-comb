;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;



;####TODO: CONSIDER MOVING UP A NAMESPACE, AS IT DOES MORE THAN JUST REGEXES NOW??

(ns lice-comb.impl.version-number
  "Version number regex related functionality. Note: this namespace is not part
  of the public API of lice-comb and may change without notice."
  (:require [clojure.string           :as s]
            [wreck.api                :as re]
            [rencg.api                :as ncg]
            [lice-comb.impl.utils     :as lciu]
            [lice-comb.impl.3rd-party :as lci3]))

(def ^:private re-version-number (re/join #"0*"
                                          (re/alt-grp (re/ncg "year2"  (re/exn 2 #"\d"))                                                                ; year2
                                                      (re/ncg "year4"  (re/exn 4 #"\d"))                                                                ; year4
                                                      (re/ncg "date"   (re/ncg "dateYear"  (re/exn 4 #"\d")) #"\-?0*"                                   ; date
                                                                       (re/ncg "dateMonth" (re/exn 2 #"\d")) #"\-?0*"
                                                                       (re/ncg "dateDay"   (re/exn 2 #"\d")))
                                                      (re/ncg "semver" (re/ncg "semverFirst" #"\d+") (re/opt-ncg "semverRest" (re/zom-grp #"\.\d+"))))  ; semver (must come last)
                                          (re/opt-ncg "suffix" #"\p{Alpha}")))

; Public because they're used during id detection
(def re-separators-date   (re/chcl (re/esc "-–—_")))
(def re-separators-semver (re/chcl (re/esc "-–—_,.")))

(defn- prefix-zero-pad
  "Pre-pads `x` (something that can be turned into a String) with 0s, to create
  a String that is at least `n` characters long."
  [^long n x]
  (when (and n x)
    (let [s         (str x)
          pad-count (- n (count s))]
      (if (pos? pad-count)
        (str (s/join (repeat pad-count "0")) s)
        s))))

(defn- third
  "Third element in `coll`, or `nil` if it doesn't have one."
  [coll]
  (nth coll 2))

(defn- parse
  "Parses `version-number` (a `String`), returning either `nil` if
  `version-number` isn't recognised as a version number (optionally of the
  specified `version-type`), or a map with these keys:

  * `:components` - a sequence of the numeric components in the version number,
                    as ints. Always has at least one element.
  * `:type`       - a keyword describing the type of the version number; one of:
                    `:semver`, `:year2`, `:year4`, `:date`.
  * `:suffix`     - a `String` containing a single letter, if `version-number`
                    had a single letter suffix (e.g. `\"c\"` for `1.3c`).

  Supports these formats for `version-number`:

  * Semver-esque e.g. `1`, `1.0`, `1.2.3`, `1.2.3.4`, `0002`, etc.
  * Year / date e.g. `86`, `2006`, `20150513`, `2015-05-13`, etc.
  * Any of the above with a single letter suffix e.g. `1.3c`

  Notes:

  * Any leading zeros are stripped from each element in `components`"
  ([^String version-number] (parse nil version-number))
  ([version-type ^String version-number]
   (when-not (s/blank? version-number)
     (when-let [m (ncg/re-matches re-version-number (s/trim version-number))]
       (merge (when-let [suffix (get m "suffix")] {:suffix suffix})
              (cond
                (or (= version-type :year2)
                    (and (nil? version-type) (contains? m "year2")))
                  (let [year (get m "year2" (get m "year4"))]  ; Handle the case where version-type is provided but version-number has 4 digits
                    {:type       :year2
                     :components (list (lciu/parse-lng year))})

                (or (= version-type :year4)
                    (and (nil? version-type) (contains? m "year4")))
                  (let [year (get m "year4" (get m "year2"))]  ; Handle the case where version-type is provided but version-number has 2 digits
                    {:type       :year4
                     :components (list (lciu/parse-lng year))})

                (or (= version-type :date)
                    (and (nil? version-type) (contains? m "date")))
                  {:type       :date
                   :components (list (lciu/parse-lng (get m "dateYear")) (lciu/parse-lng (get m "dateMonth")) (lciu/parse-lng (get m "dateDay")))}

                (or (= version-type :semver)
                    (and (nil? version-type) (contains? m "semver")))
                  (let [version-first (lciu/parse-lng (get m "semverFirst"))
                        version-rest  (let [vr (get m "semverRest")]
                                         (when-not (s/blank? vr)
                                           (subs vr 1)))  ; Strip leading . character
                         components    (concat [version-first]
                                               (when version-rest (seq (map lciu/parse-lng (s/split version-rest #"\.")))))]
                     {:type       :semver
                      :components components})))))))

(defn- canonicalise-components
  "Canonicalises a parsed version number."
  [version-type components ^String suffix]
  (str (case version-type
         :semver (let [r (s/join "." (lci3/rdrop-while zero? (rest components)))]
                   (str (first components) "." (if (s/blank? r) "0" r)))
         :year2  (let [year (str (first components))
                       year (if (and (= 4 (count year)) (s/starts-with? year "19"))  ; For year2 with 4 digits starting with "19", remove the "19"
                              (subs year 2)
                              year)]
                   (prefix-zero-pad 2 year))
         :year4  (prefix-zero-pad 4 (str (first components)))
         :date   (str (prefix-zero-pad 4 (first components)) "-" (prefix-zero-pad 2 (second components)) "-" (prefix-zero-pad 2 (third components))))
       (when-not (s/blank? suffix) suffix)))

(defn canonicalise
  "Canonicalises `version-number` (a `String`), returning either `nil` if
  `version-number` isn't recognised as a version number, or the canonical form
  of the version number (a `String`).  Auto-detects the type of the version
  number when `version-type` is not provided."
  ([^String version-number] (canonicalise nil version-number))
  ([version-type ^String version-number]
   (when-let [{components   :components
               suffix       :suffix
               version-type :type} (parse version-type version-number)]
     (canonicalise-components version-type components suffix))))

(defn canonical?
  "Is `version-number` (a `String`) in canonical form?  Returns `nil` when
  `version-number` is `nil`."
  [^String version-number]
  (when version-number
    (= version-number (canonicalise version-number))))

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
   (when-let [{components :components version-type :type suffix :suffix} (parse version-number)]
     (re/join (case version-type
                :year2  (re/join #"0*(?:19)?" (canonicalise-components version-type components nil))  ; We only check for 19xx, as no SPDX listed license since 2000 has had a 2 digit year
                :year4  (re/join #"0*"        (canonicalise-components version-type components nil))
                :date   (let [separator  (re/opt (if strict-separators? (re/esc "-") re-separators-date))]
                          (re/join #"0*" (first components) separator #"0*" (second components) separator #"0*" (third components)))
                :semver (let [components (concat [(first components)] (seq (lci3/rdrop-while zero? (rest components))))  ; Drop any trailing ".0" components (we handle them below)
                              separator  (if strict-separators? (re/esc ".") re-separators-semver)]
                          (re/join (s/join separator (map #(str "0*" %) components))               ; Allow any number of 0s at the start of each component
                                   (when (= version-type :semver) (re/zom-grp separator "0+")))))  ; Allow any number of ".0" to appear at the end of a semver version
              (when suffix (re/fgrp "i" suffix))))))  ; Match suffix (if there is one) case-insensitively

(defn metadata
  "Metadata about `version-numbers` (a sequence), represented as a map that may
  contain these keys:

  * `:type` (keyword) - the 'type' of the version number; one of: `:semver`,
    `:year2`, `:year4` or `:date`
  * `:suffix?` (boolean) - whether the version number can include a single
    letter suffix"
  [version-numbers]
  (when-let [version-numbers (seq (remove s/blank? version-numbers))]
    (let [cvers         (seq (map parse version-numbers))
          suffix?       (boolean (some :suffix cvers))
          version-types (seq (distinct (filter identity (map :type cvers))))
          version-type  (if (= 1 (count version-types))
                          (first version-types)
                          (let [msg (if (pos? (count version-types))
                                      (str "Version numbers have multiple types: " (s/join ", " version-types))
                                      (str "Version numbers have no type"))]
                            (throw (ex-info msg {:versions version-numbers}))))]
      {:version-type    version-type
       :version-suffix? suffix? })))

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
     (let [{version-type :version-type suffix? :version-suffix?} (metadata version-numbers)]
       (re/join (case version-type
                  :year2  #"0*(?:19)?\d{2}"
                  :year4  (if (some (partial re-matches #"\A19\d\d") version-numbers)
                            ; version-numbers includes some dates from the 20th century, so accept 2 digit variations (e.g. "86")
                            #"0*(?:\d{2}|[1-9]\d{3})"
                            ; version-numbers is only outside the 20th century, so only accept 4 digits
                            #"0*[1-9]\d{3}")  ; Note: we ignore dates before the year 1000
                  :date   (let [separator  (if strict-separators? (re/esc ".") (re/chcl (re/esc "-–—_")))]
                            (re/join #"0*[1-9]\d{3}" (re/opt-grp separator #"0*") #"\d{2}" (re/opt-grp separator #"0*") #"\d{2}"))  ; Note: we ignore dates before the year 1000
                  :semver (let [separator (if strict-separators? (re/esc ".") (re/chcl (re/esc "-–—_,.")))]
                            (re/join #"\d+" (re/zom-grp separator #"\d+"))))
                (when suffix? #"\p{Alpha}?"))))))
