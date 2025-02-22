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

(ns lice-comb.impl.regexes-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate :refer [fixture lic-ids-d exc-ids-d license-list-d exception-list-d non-deprecated-license-list-d non-deprecated-lic-ids-d]]
            [lice-comb.impl.regexes     :refer [id->regex name->regex]]))

(use-fixtures :once fixture)

(deftest id->regex-tests
  (testing "Nil, blank, etc."
    (is (nil? (id->regex nil)))
    (is (nil? (id->regex "")))
    (is (nil? (id->regex " ")))
    (is (nil? (id->regex "\r\n  \n \t"))))
  (let [regex-apache-11 (id->regex "Apache-1.1")
        regex-apache-20 (id->regex "Apache-2.0")
        regex-mit       (id->regex "MIT")
        regex-lzma-920  (id->regex "LZMA-SDK-9.11-to-9.20")]
    (testing "Id variations - matches"
      (is (not (nil? (re-matches regex-apache-11 "Apache 1.1"))))             ; Licence with 2 c's
      (is (not (nil? (re-matches regex-apache-11 "Apache 1.1+"))))            ; "+" suffix
      (is (not (nil? (re-matches regex-apache-11 "Apache 1.1 or later"))))    ; "or later" suffix
      (is (not (nil? (re-matches regex-apache-11 "Apache 1.1 only"))))        ; "only" suffix
      (is (not (nil? (re-matches regex-apache-11 "Apache 1.1.0"))))           ; Extra .0 component
      (is (not (nil? (re-matches regex-apache-20 "Apache 2"))))               ; Whole number version
      (is (not (nil? (re-matches regex-apache-20 "Apache-2"))))               ; Hyphens instead of spaces
      (is (not (nil? (re-matches regex-apache-20 "Apache 02"))))              ; Leading zeroes
      (is (not (nil? (re-matches regex-apache-20 "Apache 002.000"))))         ; Excess zeroes
      (is (not (nil? (re-matches regex-apache-11 "Apache 001.001"))))         ; Excess zeroes
      (is (not (nil? (re-matches regex-apache-20 "Apache 2.0.0.0.0.0"))))     ; Excess .0 components
      (is (not (nil? (re-matches regex-apache-20 "Apache v2.0"))))            ; Version prefix added
      (is (not (nil? (re-matches regex-apache-20 "Apache ver 2.0"))))         ; Version prefix added
      (is (not (nil? (re-matches regex-apache-20 "Apache version 2.0"))))     ; Version prefix added
      (is (not (nil? (re-matches regex-lzma-920  "LZMA-SDK-9.11-to-9.20"))))  ; Double version number
      (is (not (nil? (re-matches regex-lzma-920  "LZMA-SDK-9.11-9.20")))))    ; Removed "to"
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
            all-ids))))
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
