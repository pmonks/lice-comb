;
; Copyright © 2024 Peter Monks
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; SPDX-License-Identifier: Apache-2.0
;

(ns lice-comb.impl.license-regexes-test
  (:require [clojure.test                   :refer [deftest testing is use-fixtures]]
            [clojure.string                 :as s]
            [clojure.set                    :as set]
            [spdx.identifiers               :as si]
            [lice-comb.test-boilerplate     :refer [fixture ids-d all-version-series-d]]
            [lice-comb.impl.license-regexes :refer [regexes]]))

(use-fixtures :once fixture)

(defn- at-least-one-regex-matches?
  [regex-type regexes ^String s]
  (loop [[f & r] regexes]
    (if f
      (if-let [re (regex-type f)]
        (if (re-matches re s)
          true
          (recur r))
        false)  ; Didn't have the associated regex-type
      false)))  ; Exhausted all regexes without a match

; Ids we explicitly ignore, since they're deprecated duplicates
; Note: as currently constructed, this only supports single pairs of ids - if a given id duplicates multiple other ids, a different structure will be necessary
(def id-pairs-to-ignore (let [id-pairs {"SMLNJ" "StandardML-NJ"}]
                          (into id-pairs (set/map-invert id-pairs))))

(deftest regexes-tests
  (testing "Nil, blank, etc."
    (is (nil? (regexes nil)))
    (is (nil? (regexes "")))
    (is (nil? (regexes " ")))
    (is (nil? (regexes {}))))
  (let [{version-series  :version-series
         unversioned-ids :unversioned-ids} @all-version-series-d]
    (testing "Regexes of unversioned identifiers"
      (run! (fn [id]
              (let [res       (regexes id)
                    name      (:name (si/id->info id))
                    other-ids (apply disj @ids-d (filter identity (vector id (get id-pairs-to-ignore id))))]  ; Note: ids-d includes _every_ identifier, whether it's in a version series or not
                ; Check that the id and name is matched (and also both together), and also case insensitively
                (is (true?  (at-least-one-regex-matches? :id-regex   res id))                                    (str "Id regex for id "   id " did not match itself"))
                (is (true?  (at-least-one-regex-matches? :name-regex res name))                                  (str "Name regex for id " id " did not match its own name"))
                (is (true?  (at-least-one-regex-matches? :name-regex res (str name " (" id ")")))                (str "Name regex for id " id " did not match its own name with the id tacked on"))
                (is (true?  (at-least-one-regex-matches? :id-regex   res (s/lower-case id)))                     (str "Id regex for id "   id " did not match itself (lower cased)"))
                (is (true?  (at-least-one-regex-matches? :name-regex res (s/lower-case name)))                   (str "Name regex for id " id " did not match its own name (lower cased)"))
                (is (true?  (at-least-one-regex-matches? :name-regex res (s/lower-case (str name " (" id ")")))) (str "Name regex for id " id " did not match its own name with the id tacked on (lower cased)"))
                ; Check that the id and name, with text appended, is _not_ matched
                (is (false? (at-least-one-regex-matches? :id-regex   res (str "prefix" id)))   (str "Id regex for id "  id " incorrectly matched when it had a prefix"))
                (is (false? (at-least-one-regex-matches? :id-regex   res (str id "suffix")))   (str "Id regex for id "  id " incorrectly matched when it had a suffix"))
                (is (false? (at-least-one-regex-matches? :name-regex res (str "prefix" name))) (str "Name regex for id " id " incorrectly matched when the name had a prefix"))
                (is (false? (at-least-one-regex-matches? :name-regex res (str name "suffix"))) (str "Name regex for id " id " incorrectly matched when the name had a suffix"))
                ; Check that the ids and names of all other ids are _not_ matched
                (run! (fn [other-id]
                        (let [other-name (:name (si/id->info other-id))]
                          (is (false? (at-least-one-regex-matches? :id-regex   res other-id))   (str "Id regex for id "   id " incorrectly matched id "         other-id))
                          (is (false? (at-least-one-regex-matches? :name-regex res other-name)) (str "Name regex for id " id " incorrectly matched name of id " other-id))))
                      other-ids)))
            unversioned-ids))
    (testing "Regexes of version series'"
      (run! (fn [version-series]
              (let [series-id (:series-id version-series)
                    res       (regexes version-series)
                    ids       (:ids version-series)]
                (run! (fn [id]
                        (let [name (:name (si/id->info id))]
                          ; Check that every id and name is matched (and also both together), and also case insensitively
                          (is (true?  (at-least-one-regex-matches? :id-regex   res id))                                    (str "Id regex(es) for version series "   series-id " did not match id "            id))
                          (is (true?  (at-least-one-regex-matches? :name-regex res name))                                  (str "Name regex(es) for version series " series-id " did not match name of id "    id))
                          (is (true?  (at-least-one-regex-matches? :name-regex res (str name " (" id ")")))                (str "Name regex(es) for version series " series-id " did not match name+id of id " id))
                          (is (true?  (at-least-one-regex-matches? :id-regex   res (s/lower-case id)))                     (str "Id regex(es) for version series "   series-id " did not match lower cased id "            (s/lower-case id)))
                          (is (true?  (at-least-one-regex-matches? :name-regex res (s/lower-case name)))                   (str "Name regex(es) for version series " series-id " did not match lower cased name of id "    id))
                          (is (true?  (at-least-one-regex-matches? :name-regex res (s/lower-case (str name " (" id ")")))) (str "Name regex(es) for version series " series-id " did not match lower cased name+id of id " id))
                          ; Check that every id and name, with text appended, is _not_ matched
                          (is (false? (at-least-one-regex-matches? :id-regex   res (str "prefix" id)))   (str "Id regex(es) for version series "   series-id " incorrectly matched id " id " when it had a prefix"))
                          (is (false? (at-least-one-regex-matches? :id-regex   res (str id "suffix")))   (str "Id regex(es) for version series "   series-id " incorrectly matched id " id " when it had a suffix"))
                          (is (false? (at-least-one-regex-matches? :name-regex res (str "prefix" name))) (str "Name regex(es) for version series " series-id " incorrectly matched the name of id " id " when the name had a prefix"))
                          (is (false? (at-least-one-regex-matches? :name-regex res (str name "suffix"))) (str "Name regex(es) for version series " series-id " incorrectly matched the name of id " id " when the name had a suffix"))))
                      ids)))
            (vals version-series)))
    (testing "Handpicked variations on ids and names"
      ; Unversioned ids
      (let [mit-regexes (regexes "MIT")]
        (is (true? (at-least-one-regex-matches? :id-regex   mit-regexes "MIT")))
        (is (true? (at-least-one-regex-matches? :name-regex mit-regexes "MIT License")))
        (is (true? (at-least-one-regex-matches? :name-regex mit-regexes "MIT Licence"))))
      (let [mit-regexes (regexes "NTP-0")]
        (is (true? (at-least-one-regex-matches? :id-regex   mit-regexes "NTP-0")))
        (is (true? (at-least-one-regex-matches? :id-regex   mit-regexes "NTP-00")))
        (is (true? (at-least-one-regex-matches? :id-regex   mit-regexes "NTP 00")))
        (is (true? (at-least-one-regex-matches? :id-regex   mit-regexes "NTP_00"))))
      ; Version series'
      (let [apache-regexes (regexes (get version-series "Apache"))]
        (is (true? (at-least-one-regex-matches? :id-regex  apache-regexes "Apache 1.1")))
        (is (true? (at-least-one-regex-matches? :id-regex  apache-regexes "Apache 1.1.0")))
        (is (true? (at-least-one-regex-matches? :id-regex  apache-regexes "Apache 02.00.00.00.00.00")))
        )
      )
    ;####TODO: ADD MORE - SEE BELOW FOR EXAMPLES!!!!!
))







(comment
  (let [regex-apache-11 (id->regex "Apache-1.1")
        regex-apache-20 (id->regex "Apache-2.0")
        regex-mit       (id->regex "MIT")
        regex-lzma-920  (id->regex "LZMA-SDK-9.11-to-9.20")]
    (testing "Id variations - matches"
      (is (true? (boolean (re-matches regex-apache-11 "Apache 1.1"))))             ; Licence with 2 c's
      (is (true? (boolean (re-matches regex-apache-11 "Apache 1.1+"))))            ; "+" suffix
      (is (true? (boolean (re-matches regex-apache-11 "Apache 1.1 or later"))))    ; "or later" suffix
      (is (true? (boolean (re-matches regex-apache-11 "Apache 1.1 only"))))        ; "only" suffix
      (is (true? (boolean (re-matches regex-apache-11 "Apache 1.1.0"))))           ; Extra .0 component
      (is (true? (boolean (re-matches regex-apache-20 "Apache 2"))))               ; Whole number version
      (is (true? (boolean (re-matches regex-apache-20 "Apache-2"))))               ; Hyphens instead of spaces
      (is (true? (boolean (re-matches regex-apache-20 "Apache 02"))))              ; Leading zeroes
      (is (true? (boolean (re-matches regex-apache-20 "Apache 002.000"))))         ; Excess zeroes
      (is (true? (boolean (re-matches regex-apache-11 "Apache 001.001"))))         ; Excess zeroes
      (is (true? (boolean (re-matches regex-apache-20 "Apache 2.0.0.0.0.0"))))     ; Excess .0 components
      (is (true? (boolean (re-matches regex-apache-20 "Apache v2.0"))))            ; Version prefix added
      (is (true? (boolean (re-matches regex-apache-20 "Apache ver 2.0"))))         ; Version prefix added
      (is (true? (boolean (re-matches regex-apache-20 "Apache version 2.0"))))     ; Version prefix added
      (is (true? (boolean (re-matches regex-lzma-920  "LZMA-SDK-9.11-to-9.20"))))  ; Double version number
      (is (true? (boolean (re-matches regex-lzma-920  "LZMA-SDK-9.11-9.20")))))    ; Removed "to"
    (testing "Id variations - non-matches"
      (is (nil? (re-matches regex-apache-11 "Apache 1.0")))       ; Wrong minor version
      (is (nil? (re-matches regex-apache-11 "Apache 1.10")))      ; Wrong minor version
      (is (nil? (re-matches regex-apache-20 "Apache 3.0")))       ; Wrong major version
      (is (nil? (re-matches regex-apache-20 "Apache 2.1")))       ; Wrong minor version
      (is (nil? (re-matches regex-mit       "X11/MIT")))          ; X11 MIT variant (as a prefix)
      (is (nil? (re-matches regex-mit       "MIT/ISC")))))        ; ISC MIT variant (as a suffix)
  (testing "All id regexes match at least their associated id"
    ; We use the full set of ids here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-matches (id->regex %) %))) (str "Regex doesn't match license id '" % "'"))   @lic-ids-d)
    (run! #(is (not (nil? (re-matches (id->regex %) %))) (str "Regex doesn't match exception id '" % "'")) @exc-ids-d))
  (testing "All id regexes find their associated id"
    ; We use the full set of ids here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-find (id->regex %) (str "prefix " % " suffix")))) (str "Regex doesn't find license id '" % "'"))   @lic-ids-d)
    (run! #(is (not (nil? (re-find (id->regex %) (str "prefix " % " suffix")))) (str "Regex doesn't find exception id '" % "'")) @exc-ids-d))
  ; Note: this test is combinatorial (results in ~540,000 assertions!), but completes in < 10s
  (testing "All id regexes do NOT match any other ids"
    (let [all-ids (into @non-deprecated-lic-ids-d @exc-ids-d)]   ; We use the non-deprecated license ids here, as some non-deprecated GPL family license id regexes also match deprecated GPL family license ids
      (run! #(let [re          (id->regex %)
                   ids-to-test (disj all-ids %)]
               (run! (fn [id-to-test] (is (nil? (re-matches re id-to-test)) (str "Regex for id '" % "' erroneously matches id '" id-to-test "'"))) ids-to-test))
            all-ids)))
  ; Note: we don't test to ensure that a regex doesn't find other ids, as some of them will (e.g. 'GPL-2.0' will find 'GPL-2.0-or-later')

(deftest name->regex-tests
  (testing "Nil, blank, etc."
    (is (nil? (name->regex nil)))
    (is (nil? (name->regex "")))
    (is (nil? (name->regex " ")))
    (is (nil? (name->regex "\r\n  \n \t"))))
  (let [regex-apache-11  (name->regex "Apache License 1.1")
        regex-apache-20  (name->regex "Apache License 2.0")
        regex-lppl-13c   (name->regex "LaTeX Project Public License v1.3c")
        regex-liliq-r-11 (name->regex "Licence Libre du Québec – Réciprocité version 1.1")   ; Note: that's an en-dash, _not_ a hyphen
        regex-oldap-20   (name->regex "Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)")
        regex-oldap-221  (name->regex "Open LDAP Public License v2.2.1")
        regex-lzma-920   (name->regex "LZMA SDK License (versions 9.11 to 9.20)")
        regex-lzma-922   (name->regex "LZMA SDK License (versions 9.22 and beyond)")
        regex-pddl-10    (name->regex "Open Data Commons Public Domain Dedication & License 1.0")
        regex-mit        (name->regex "The MIT License")]
    (testing "Name variations - matches"
      (is (not (nil? (re-matches regex-apache-11  "Apache Licence 1.1"))))                                          ; Licence with 2 c's
      (is (not (nil? (re-matches regex-apache-11  "Apache Licence 1.1+"))))                                         ; "+" suffix
      (is (not (nil? (re-matches regex-apache-11  "Apache Licence 1.1 or later"))))                                 ; "or later" suffix
      (is (not (nil? (re-matches regex-apache-11  "Apache Licence 1.1 only"))))                                     ; "only" suffix
      (is (not (nil? (re-matches regex-apache-11  "Apache Licence 1.1.0"))))                                        ; Extra .0 component
      (is (not (nil? (re-matches regex-apache-20  "Apache-License-2.0"))))                                          ; Hyphens instead of spaces
      (is (not (nil? (re-matches regex-apache-20  "Apache License 2"))))                                            ; Whole number version
      (is (not (nil? (re-matches regex-apache-20  "Apache License 02"))))                                           ; Leading zeroes
      (is (not (nil? (re-matches regex-apache-20  "Apache License 002.000"))))                                      ; Excess zeroes
      (is (not (nil? (re-matches regex-apache-11  "Apache License 001.001"))))                                      ; Excess zeroes
      (is (not (nil? (re-matches regex-apache-20  "Apache License 2.0.0.0.0.0"))))                                  ; Excess .0 components
      (is (not (nil? (re-matches regex-apache-20  "Apache 2.0"))))                                                  ; Optional "License"
      (is (not (nil? (re-matches regex-apache-20  "Apache License v2.0"))))                                         ; Version prefix added
      (is (not (nil? (re-matches regex-apache-20  "Apache License ver 2.0"))))                                      ; Version prefix added
      (is (not (nil? (re-matches regex-apache-20  "Apache License version 2.0"))))                                  ; Version prefix added
      (is (not (nil? (re-matches regex-lppl-13c   "LaTeX Project Public License 1.3c"))))                           ; Version prefix removed
      (is (not (nil? (re-matches regex-lppl-13c   "LaTeX Project Public License v1.3C"))))                          ; Version suffix capitalised
      (is (not (nil? (re-matches regex-lppl-13c   "LaTeX Project Public License 01.03.0c"))))                       ; Spurious zeroes
      (is (not (nil? (re-matches regex-liliq-r-11 "Licence Libre du Quebec – Reciprocite version 1.1"))))           ; Accent removed
      (is (not (nil? (re-matches regex-liliq-r-11 "Licence Libre du Québec Réciprocité version 1.1"))))             ; en-dash removed
      (is (not (nil? (re-matches regex-liliq-r-11 "Licence Libre du Québec - Réciprocité version 1.1"))))           ; en-dash replaced with hyphen
      (is (not (nil? (re-matches regex-liliq-r-11 "Licence Libre du Québec — Réciprocité version 1.1"))))           ; en-dash replaced with em-dash
      (is (not (nil? (re-matches regex-oldap-20   "Open LDAP Public License 2.0 (or possibly 2.0A and 2.0B)"))))    ; Interior version number; version prefix removed
      (is (not (nil? (re-matches regex-oldap-221  "Open LDAP Public License v2.2.1.0.0.0.0"))))                     ; Excess .0 components
      (is (not (nil? (re-matches regex-lzma-920   "LZMA SDK License (versions 9.11-9.20)"))))                       ; Hyphen instead of "to"
      (is (not (nil? (re-matches regex-lzma-922   "LZMA SDK License (versions 9.22 & beyond)"))))                   ; and replaced with &
      (is (not (nil? (re-matches regex-pddl-10    "Open Data Commons Public Domain Dedication and License 1.0"))))  ; & replaced with and
      (is (not (nil? (re-matches regex-mit        "MIT License"))))                                                 ; The removed
      (is (not (nil? (re-matches regex-mit        "MIT")))))                                                        ; The and License removed
    (testing "Name variations - non-matches"
      (is (nil? (re-matches regex-apache-11 "Apache License 1.0")))                   ; Wrong minor version
      (is (nil? (re-matches regex-apache-11 "Apache License 1.10")))                  ; Wrong minor version
      (is (nil? (re-matches regex-apache-20 "Apache License 3.0")))                   ; Wrong major version
      (is (nil? (re-matches regex-apache-20 "Apache License 2.1")))                   ; Wrong minor version
      (is (nil? (re-matches regex-mit       "X11/MIT")))                              ; X11 MIT variant (as a prefix)
      (is (nil? (re-matches regex-mit       "MIT/ISC")))                              ; ISC MIT variant (as a suffix)
      (is (nil? (re-matches regex-lppl-13c  "LaTeX Project Public License v1.3a")))   ; Wrong letter after the version
      (is (nil? (re-matches regex-lppl-13c  "LaTeX Project Public License v1.3cd")))  ; Too many letters after the version
      (is (nil? (re-matches regex-oldap-221 "Open LDAP Public License v2.2.0")))))    ; Wrong patchlevel version
  (testing "All name regexes match at least their associated name"
    ; We use the full license lists here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-matches (name->regex (:name %)) (:name %)))) (str "Regex doesn't match license name '"   (:name %) "'")) @license-list-d)
    (run! #(is (not (nil? (re-matches (name->regex (:name %)) (:name %)))) (str "Regex doesn't match exception name '" (:name %) "'")) @exception-list-d))
  (testing "All name regexes find at least their associated name"
    ; We use the full license lists here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-find (name->regex (:name %)) (str "prefix " (:name %) " suffix")))) (str "Regex doesn't find license name '"   (:name %) "'")) @license-list-d)
    (run! #(is (not (nil? (re-find (name->regex (:name %)) (str "prefix " (:name %) " suffix")))) (str "Regex doesn't find exception name '" (:name %) "'")) @exception-list-d))
  ; Note: this test is combinatorial (results in ~540,000 assertions!), but completes in < 10s
  (testing "All name regexes do NOT match other names"
    (let [all-names (set (concat (map :name @non-deprecated-license-list-d)   ; We use the non-deprecated license list here, as some non-deprecated GPL family license name regexes also match deprecated GPL family license names
                                 (map :name @exception-list-d)))]
      (run! #(let [re            (name->regex %)
                   names-to-test (disj all-names %)]
               (run! (fn [name-to-test] (is (nil? (re-matches re name-to-test)) (str "Regex for name '" % "' erroneously matches name '" name-to-test "'"))) names-to-test))
            all-names))))
  ; Note: we don't test to ensure that a regex doesn't `find` other names, as some of them will (e.g. 'BSD with attribution' will find 'BSD with Attribution and HPND disclaimer')
)
