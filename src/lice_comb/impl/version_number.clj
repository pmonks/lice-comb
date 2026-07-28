;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.version-number
  "Version number related functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string           :as s]
            [wreck.api                :as re]
            [rencg.api                :as ncg]
            [lice-comb.impl.utils     :as u]
            [lice-comb.impl.3rd-party :as third-party]))

(def ^:private re-version-number (re/join (re/alt-grp (re/join #"0?" (re/ncg "year2"  (re/exn 2 #"\d")))                                                                  ; year2
                                                      (re/join #"0*" (re/ncg "year4"  (re/exn 4 #"\d")))                                                                  ; year4
                                                      (re/join #"0*" (re/ncg "date"   (re/ncg "dateYear"  (re/exn 4 #"\d")) #"[\-_]?0*"                                   ; date
                                                                                      (re/ncg "dateMonth" (re/exn 2 #"\d")) #"[\-_]?0*"
                                                                                      (re/ncg "dateDay"   (re/exn 2 #"\d"))))
                                                      (re/join #"0*" (re/ncg "semver" (re/ncg "semverFirst" #"\d+") (re/opt-ncg "semverRest" (re/zom-grp #"[\._]\d+")))))  ; semver (must come last)
                                          (re/opt-ncg "suffix" #"\p{Alpha}")))

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

(defn parse
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
                     :components (list (u/parse-lng year))})

                (or (= version-type :year4)
                    (and (nil? version-type) (contains? m "year4")))
                  (let [year (get m "year4" (get m "year2"))]  ; Handle the case where version-type is provided but version-number has 2 digits
                    {:type       :year4
                     :components (list (u/parse-lng year))})

                (or (= version-type :date)
                    (and (nil? version-type) (contains? m "date")))
                  {:type       :date
                   :components (list (u/parse-lng (get m "dateYear")) (u/parse-lng (get m "dateMonth")) (u/parse-lng (get m "dateDay")))}

                (or (= version-type :semver)
                    (and (nil? version-type) (contains? m "semver")))
                  (let [version-first (u/parse-lng (get m "semverFirst"))
                        version-rest  (let [vr (get m "semverRest")]
                                         (when-not (s/blank? vr)
                                           (subs vr 1)))  ; Strip leading separator character
                         components    (concat [version-first]
                                               (when version-rest (seq (map u/parse-lng (s/split version-rest #"[\._]")))))]
                     {:type       :semver
                      :components components})))))))

(defn canonicalise-components
  "Canonicalises a parsed version number."
  [version-type components ^String suffix]
  (str (case version-type
         :semver (let [r (s/join "." (third-party/rdrop-while zero? (rest components)))]
                   (str (first components) "." (if (s/blank? r) "0" r)))
         :year2  (let [year (str (first components))
                       year (if (and (= 4 (count year)) (s/starts-with? year "19"))  ; For year2 with 4 digits starting with "19", remove the "19"
                              (subs year 2)
                              year)]
                   (prefix-zero-pad 2 year))
         :year4  (prefix-zero-pad 4 (str (first components)))
         :date   (str (prefix-zero-pad 4 (first components)) "-" (prefix-zero-pad 2 (second components)) "-" (prefix-zero-pad 2 (u/third components))))
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
