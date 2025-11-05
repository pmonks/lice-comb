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
            [wreck.api                  :as re]
            [rencg.api                  :as rencg]
            [lice-comb.test-boilerplate :refer [fixture lic-ids-d exc-ids-d license-list-d exception-list-d non-deprecated-license-list-d non-deprecated-lic-ids-d]]
            [lice-comb.impl.regexes     :refer [version-number->re re-version-and-suffix id->regex name->regex]]))

(use-fixtures :once fixture)

(defn- re-version-and-suffix*
  "As for re-version-and-suffix, but wraps it in flags \"iuU\" (required for it
  to function as expected as an independent regex)."
  [version-number version-number-ncg-name
   only?          only-ncg-name
   or-later?      or-later-ncg-name]
  (when-let [re (re-version-and-suffix version-number version-number-ncg-name only? only-ncg-name or-later? or-later-ncg-name)]
    (re/flags-grp "iuU" re)))

(deftest version-number->re-tests
  (testing "Nil, blank, etc."
    (is (nil? (version-number->re nil)))
    (is (nil? (version-number->re "")))
    (is (nil? (version-number->re "  "))))
  (testing "Invalid version values"
    (is (nil? (version-number->re "foo")))
    (is (nil? (version-number->re ".")))
    (is (nil? (version-number->re ".2")))
    (is (nil? (version-number->re "-2"))))
  (testing "Valid version numbers - semver (non-strict)"
    (is (re/=' #"0*0(?:[\-–—_,\.]0+)*"                           (version-number->re "00")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (version-number->re "2")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (version-number->re "02")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (version-number->re "2.0")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (version-number->re "2.0.0")))
    (is (re/=' #"0*2[\-–—_,\.]0*1(?:[\-–—_,\.]0+)*"              (version-number->re "2.1")))
    (is (re/=' #"0*2[\-–—_,\.]0*0[\-–—_,\.]0*1(?:[\-–—_,\.]0+)*" (version-number->re "2.0.1")))
    (is (re/=' #"0*99[\-–—_,\.]0*100(?:[\-–—_,\.]0+)*"           (version-number->re "000000099.00000100"))))
  (testing "Valid version numbers - semver (strict)"
    (is (re/=' #"0*0(?:\.0+)*"           (version-number->re "00"    true)))
    (is (re/=' #"0*2(?:\.0+)*"           (version-number->re "2"     true)))
    (is (re/=' #"0*2(?:\.0+)*"           (version-number->re "02"    true)))
    (is (re/=' #"0*2(?:\.0+)*"           (version-number->re "2.0"   true)))
    (is (re/=' #"0*2(?:\.0+)*"           (version-number->re "2.0.0" true)))
    (is (re/=' #"0*2\.0*1(?:\.0+)*"      (version-number->re "2.1"   true)))
    (is (re/=' #"0*2\.0*0\.0*1(?:\.0+)*" (version-number->re "2.0.1" true)))
    (is (re/=' #"0*99\.0*100(?:\.0+)*"   (version-number->re "000000099.00000100" true))))
  (testing "Valid version numbers - 2 digit year"
    (is (re/=' #"0*86" (version-number->re "86")))
    (is (re/=' #"0*86" (version-number->re "86" true)))
    (is (re/=' #"0*89" (version-number->re "0000089"))))
  (testing "Valid version numbers - 4 digit year"
    (is (re/=' #"0*1986" (version-number->re "1986")))
    (is (re/=' #"0*1986" (version-number->re "1986" true)))
    (is (re/=' #"0*2006" (version-number->re "000002006"))))
  (testing "Valid version numbers - 8 digit year"
    (is (re/=' #"0*19980720" (version-number->re "19980720")))
    (is (re/=' #"0*19980720" (version-number->re "19980720" true)))
    (is (re/=' #"0*20150513" (version-number->re "0020150513"))))
  (testing "Valid version numbers - suffixes")
; 1.3a
  )

(deftest re-version-and-suffix-tests
  (testing "Nil, blank, etc. - note that these all result in generic 'any version number' regexes"
    (is (not (nil? (re-version-and-suffix nil nil false nil false nil))))
    (is (not (nil? (re-version-and-suffix "" nil false nil false nil))))
    (is (not (nil? (re-version-and-suffix " " nil false nil false nil))))
    (is (not (nil? (re-version-and-suffix "\r\n  \n \t" nil false nil false nil)))))
  (testing "Any version matching regexes"
    (let [any-version-re (re-version-and-suffix* nil nil false nil false nil)]
      (is (true?  (boolean (re-matches any-version-re "0"))))
      (is (true?  (boolean (re-matches any-version-re "1"))))
      (is (true?  (boolean (re-matches any-version-re "009999999"))))
      (is (true?  (boolean (re-matches any-version-re "0.1"))))
      (is (true?  (boolean (re-matches any-version-re "1.0.0"))))
      (is (true?  (boolean (re-matches any-version-re "1-0-0"))))
      (is (true?  (boolean (re-matches any-version-re "1_0_0"))))
      (is (true?  (boolean (re-matches any-version-re "009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "v009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "v 009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "v-009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "v_009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "ver009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "ver 009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "ver-009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "ver_009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "version009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "version 009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "version-009999999.009999999.009999999"))))
      (is (true?  (boolean (re-matches any-version-re "version_009999999.009999999.009999999"))))))
  (testing "Specific version number regexes"
    (let [v0-re (re-version-and-suffix* "0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v0-re "0"))))
      (is (true?  (boolean (re-matches v0-re "00"))))
      (is (true?  (boolean (re-matches v0-re "0000000000000000000"))))
      (is (true?  (boolean (re-matches v0-re "0.0"))))
      (is (true?  (boolean (re-matches v0-re "00.00"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0000"))))
      (is (false? (boolean (re-matches v0-re "1"))))
      (is (false? (boolean (re-matches v0-re "1.0"))))
      (is (false? (boolean (re-matches v0-re "0.1"))))
      (is (false? (boolean (re-matches v0-re "00000000001"))))
      (is (false? (boolean (re-matches v0-re "0.00000000001")))))
    (let [v0-re (re-version-and-suffix* "0.0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v0-re "0"))))
      (is (true?  (boolean (re-matches v0-re "00"))))
      (is (true?  (boolean (re-matches v0-re "0000000000000000000"))))
      (is (true?  (boolean (re-matches v0-re "0.0"))))
      (is (true?  (boolean (re-matches v0-re "00.00"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0000"))))
      (is (false? (boolean (re-matches v0-re "1"))))
      (is (false? (boolean (re-matches v0-re "1.0"))))
      (is (false? (boolean (re-matches v0-re "0.1"))))
      (is (false? (boolean (re-matches v0-re "00000000001"))))
      (is (false? (boolean (re-matches v0-re "0.00000000001")))))
    (let [v0-re (re-version-and-suffix* "0.0.0.0.0.0.0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v0-re "0"))))
      (is (true?  (boolean (re-matches v0-re "00"))))
      (is (true?  (boolean (re-matches v0-re "0000000000000000000"))))
      (is (true?  (boolean (re-matches v0-re "0.0"))))
      (is (true?  (boolean (re-matches v0-re "00.00"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0"))))
      (is (true?  (boolean (re-matches v0-re "0.0.0000"))))
      (is (false? (boolean (re-matches v0-re "1"))))
      (is (false? (boolean (re-matches v0-re "1.0"))))
      (is (false? (boolean (re-matches v0-re "0.1"))))
      (is (false? (boolean (re-matches v0-re "00000000001"))))
      (is (false? (boolean (re-matches v0-re "0.00000000001")))))
    (let [v1-re (re-version-and-suffix* "1" nil false nil false nil)]
      (is (true?  (boolean (re-matches v1-re "1"))))
      (is (true?  (boolean (re-matches v1-re "01"))))
      (is (true?  (boolean (re-matches v1-re "0000000000000000001"))))
      (is (true?  (boolean (re-matches v1-re "1.0"))))
      (is (true?  (boolean (re-matches v1-re "01.00"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000"))))
      (is (false? (boolean (re-matches v1-re "0"))))
      (is (false? (boolean (re-matches v1-re "0.1"))))
      (is (false? (boolean (re-matches v1-re "0.0.1"))))
      (is (false? (boolean (re-matches v1-re "10"))))
      (is (false? (boolean (re-matches v1-re "0.00000000001")))))
    (let [v1-re (re-version-and-suffix* "1.0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v1-re "1"))))
      (is (true?  (boolean (re-matches v1-re "01"))))
      (is (true?  (boolean (re-matches v1-re "0000000000000000001"))))
      (is (true?  (boolean (re-matches v1-re "1.0"))))
      (is (true?  (boolean (re-matches v1-re "01.00"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000"))))
      (is (false? (boolean (re-matches v1-re "0"))))
      (is (false? (boolean (re-matches v1-re "0.1"))))
      (is (false? (boolean (re-matches v1-re "0.0.1"))))
      (is (false? (boolean (re-matches v1-re "10"))))
      (is (false? (boolean (re-matches v1-re "0.00000000001")))))
    (let [v11-re (re-version-and-suffix* "1.1" nil false nil false nil)]
      (is (true?  (boolean (re-matches v11-re "1.1"))))
      (is (true?  (boolean (re-matches v11-re "01.1"))))
      (is (true?  (boolean (re-matches v11-re "0000000000000000001.01"))))
      (is (true?  (boolean (re-matches v11-re "1.1.0"))))
      (is (true?  (boolean (re-matches v11-re "01.01.00.00.00"))))
      (is (true?  (boolean (re-matches v11-re "1.1.0000"))))
      (is (false? (boolean (re-matches v11-re "1"))))
      (is (false? (boolean (re-matches v11-re "1.0"))))
      (is (false? (boolean (re-matches v11-re "1.10"))))
      (is (false? (boolean (re-matches v11-re "1.0.1"))))
      (is (false? (boolean (re-matches v11-re "10"))))
      (is (false? (boolean (re-matches v11-re "0.00000000001")))))
    (let [v321-re (re-version-and-suffix* "3.2.1" nil false nil false nil)]
      (is (true?  (boolean (re-matches v321-re "3.2.1"))))
      (is (true?  (boolean (re-matches v321-re "03.02.01"))))
      (is (true?  (boolean (re-matches v321-re "0000000000000000003.0000002.0000001"))))
      (is (true?  (boolean (re-matches v321-re "3.2.1.0"))))
      (is (true?  (boolean (re-matches v321-re "03.02.01.00.00.00"))))
      (is (true?  (boolean (re-matches v321-re "3.2.00001"))))
      (is (false? (boolean (re-matches v321-re "3"))))
      (is (false? (boolean (re-matches v321-re "3.2"))))
      (is (false? (boolean (re-matches v321-re "3.2.10"))))
      (is (false? (boolean (re-matches v321-re "3.1.2"))))
      (is (false? (boolean (re-matches v321-re "30.2.1"))))
      (is (false? (boolean (re-matches v321-re "3.2.000000000010")))))
    (let [v321-re (re-version-and-suffix* "03.02.01.0.0.0.0.0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v321-re "3.2.1"))))
      (is (true?  (boolean (re-matches v321-re "03.02.01"))))
      (is (true?  (boolean (re-matches v321-re "0000000000000000003.0000002.0000001"))))
      (is (true?  (boolean (re-matches v321-re "3.2.1.0"))))
      (is (true?  (boolean (re-matches v321-re "03.02.01.00.00.00"))))
      (is (true?  (boolean (re-matches v321-re "3.2.00001"))))
      (is (false? (boolean (re-matches v321-re "3"))))
      (is (false? (boolean (re-matches v321-re "3.2"))))
      (is (false? (boolean (re-matches v321-re "3.2.10"))))
      (is (false? (boolean (re-matches v321-re "3.1.2"))))
      (is (false? (boolean (re-matches v321-re "30.2.1"))))
      (is (false? (boolean (re-matches v321-re "3.2.000000000010"))))))
  (testing "Version numbers with alternative decimals"
    (let [v21-re (re-version-and-suffix* "2.1.0" nil false nil false nil)]
      (is (true? (boolean (re-matches v21-re "2_1"))))
      (is (true? (boolean (re-matches v21-re "2-1"))))
      (is (true? (boolean (re-matches v21-re "2,1"))))))
  (testing "Version labels"
    (let [v2-re (re-version-and-suffix* "2.0" nil false nil false nil)]
      (is (true?  (boolean (re-matches v2-re "v2"))))
      (is (true?  (boolean (re-matches v2-re "V2"))))
      (is (true?  (boolean (re-matches v2-re "v2.0"))))
      (is (true?  (boolean (re-matches v2-re "v 2"))))
      (is (true?  (boolean (re-matches v2-re "v 2.0"))))
      (is (true?  (boolean (re-matches v2-re "ver2"))))
      (is (true?  (boolean (re-matches v2-re "VER2"))))
      (is (true?  (boolean (re-matches v2-re "ver2.0"))))
      (is (true?  (boolean (re-matches v2-re "ver 2"))))
      (is (true?  (boolean (re-matches v2-re "ver 2.0"))))
      (is (true?  (boolean (re-matches v2-re "version2"))))
      (is (true?  (boolean (re-matches v2-re "version2.0"))))
      (is (true?  (boolean (re-matches v2-re "version 2"))))
      (is (true?  (boolean (re-matches v2-re "VERSION 2"))))
      (is (true?  (boolean (re-matches v2-re "version 2.0"))))
      (is (true?  (boolean (re-matches v2-re "vErSiON 2.0"))))
      (is (true?  (boolean (re-matches v2-re "versions 2"))))
      (is (false? (boolean (re-matches v2-re "vers 2"))))))
  (testing "Version number NCG"
    (let [v2-re (re-version-and-suffix* "2.0" "versionNumber" false nil false nil)]
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2") "versionNumber")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02") "versionNumber")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0") "versionNumber")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02.00") "versionNumber")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0.0") "versionNumber")))))
  (testing "only suffix"
    (let [v1-re (re-version-and-suffix* "1.0" nil true nil false nil)]
      (is (true?  (boolean (re-matches v1-re "1 only"))))
      (is (true?  (boolean (re-matches v1-re "01only"))))
      (is (true?  (boolean (re-matches v1-re "0000000000000000001       only"))))
      (is (true?  (boolean (re-matches v1-re "1.0-only"))))
      (is (true?  (boolean (re-matches v1-re "01.00_only"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0, only"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 only"))))
      (is (false? (boolean (re-matches v1-re "1"))))
      (is (false? (boolean (re-matches v1-re "01"))))
      (is (false? (boolean (re-matches v1-re "0000000000000000001"))))
      (is (false? (boolean (re-matches v1-re "1.0"))))
      (is (false? (boolean (re-matches v1-re "01.00"))))
      (is (false? (boolean (re-matches v1-re "1.0.0"))))
      (is (false? (boolean (re-matches v1-re "1.0.0000 or later"))))))
  (testing "only suffix NCG"
    (let [v2-re (re-version-and-suffix* "2.0" nil true "only" false nil)]
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2 only") "only")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02only") "only")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "0000000000000000002       only") "only")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0-only") "only")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02.00_only") "only")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0.0 only") "only")))))
(testing "or later suffix"
    (let [v1-re (re-version-and-suffix* "1.0" nil false nil true nil)]
      (is (true?  (boolean (re-matches v1-re "1 or later"))))
      (is (true?  (boolean (re-matches v1-re "1+"))))
      (is (true?  (boolean (re-matches v1-re "01or later"))))
      (is (true?  (boolean (re-matches v1-re "01 +"))))
      (is (true?  (boolean (re-matches v1-re "0000000000000000001       or              later"))))
      (is (true?  (boolean (re-matches v1-re "1.0-or-later"))))
      (is (true?  (boolean (re-matches v1-re "01.00_or_later"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0, or later"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or,later"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or any later version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or a later version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or a later ver"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 (or any later version)"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or any lator version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or newer version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or at your discretion a newer version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or, at your option, a later version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 or (at your option) any newer version"))))
      (is (true?  (boolean (re-matches v1-re "1.0.0000 (or at your discretion any lator versions)"))))
      (is (false? (boolean (re-matches v1-re "1"))))
      (is (false? (boolean (re-matches v1-re "01"))))
      (is (false? (boolean (re-matches v1-re "0000000000000000001"))))
      (is (false? (boolean (re-matches v1-re "1.0"))))
      (is (false? (boolean (re-matches v1-re "01.00"))))
      (is (false? (boolean (re-matches v1-re "1.0.0"))))
      (is (false? (boolean (re-matches v1-re "1.0.0000 only"))))))
  (testing "or later-suffix NCG"
    (let [v2-re (re-version-and-suffix* "2.0" nil false nil true "orLater")]
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2 or later") "orLater")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02or later") "orLater")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "0000000000000000002       or                     later") "orLater")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0-or-later") "orLater")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "02.00_or_later") "orLater")))
      (is (true? (contains? (rencg/re-matches-ncg v2-re "2.0.0 or,later") "orLater")))))
  (testing "only and/or or-later suffixes"
    (let [v3-re (re-version-and-suffix* "3.0" nil true "only" true "orLater")]
      ; No suffix
      (is (true?  (boolean (re-matches v3-re "3"))))
      (is (true?  (boolean (re-matches v3-re "03"))))
      (is (true?  (boolean (re-matches v3-re "0000000000000000003"))))
      (is (true?  (boolean (re-matches v3-re "3.0"))))
      (is (true?  (boolean (re-matches v3-re "03.00"))))
      (is (true?  (boolean (re-matches v3-re "3.0.0"))))
      (is (false? (boolean (re-matches v3-re "3 only or later"))))
      ; Only suffix
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3 only") "only")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03only") "only")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "0000000000000000003       only") "only")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0-only") "only")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03.00_only") "only")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0.0 only") "only")))
      ; Or later suffix
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03or later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "0000000000000000003       or                     later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0-or-later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03.00_or_later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0.0 or,later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3 or later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03or later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "0000000000000000003       or                     later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0-or-later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "03.00_or_later") "orLater")))
      (is (true?  (contains? (rencg/re-matches-ncg v3-re "3.0.0 or,later") "orLater")))))
  (testing "The whole enchilada"
    (let [v21-re (re-version-and-suffix* "2.1" "versionNumber" true "only" true "orLater")]
      (let [m1 (rencg/re-matches-ncg v21-re "version 2.1 only")]
        (is (true?  (contains? m1 "versionNumber")))
        (is (true?  (contains? m1 "only")))
        (is (false? (contains? m1 "orLater"))))
      (let [m2 (rencg/re-matches-ncg v21-re "version 2.1+")]
        (is (true?  (contains? m2 "versionNumber")))
        (is (false? (contains? m2 "only")))
        (is (true?  (contains? m2 "orLater"))))
      (is (nil? (rencg/re-matches-ncg v21-re "version 2.1 only or later"))))))

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


