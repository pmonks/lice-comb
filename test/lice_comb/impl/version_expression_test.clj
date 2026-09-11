;
; Copyright © 2026 Peter Monks
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

(ns lice-comb.impl.version-expression-test
  (:require [clojure.test                              :refer [deftest testing is use-fixtures]]
            [wreck.api                                 :as re]
            [rencg.api                                 :as ncg]
            [lice-comb.test-boilerplate                :refer [fixture]]
            [lice-comb.impl.regexes.version-expression :refer [expression-regex]]))

(use-fixtures :once fixture)

(defn- expression-regex*
  "As for expression-regex, but wraps it in flag \"i\" (required for it to
  function as expected as an independent regex)."
  [ncg-prefix version-numbers]
  (when-let [re (expression-regex ncg-prefix version-numbers)]
    (re/flags-grp "i" re)))

(deftest expression-regex-tests
  (testing "Nil, blank, etc."
    (is (nil?      (expression-regex nil nil)))
    (is (nil?      (expression-regex "" nil)))
    (is (nil?      (expression-regex " " nil)))
    (is (nil?      (expression-regex "\r\n  \n \t" nil)))
    (is (nil?      (expression-regex nil [])))
    (is (nil?      (expression-regex "" []))))
  (testing "Simple non-nil results"
    (is (re/regex? (expression-regex nil   ["0"])))
    (is (re/regex? (expression-regex nil   ["1.0"])))
    (is (re/regex? (expression-regex nil   ["1.0" "2.0"])))
    (is (re/regex? (expression-regex nil   ["1.0" "2.0a"])))
    (is (re/regex? (expression-regex nil   ["2.0.1"])))
    (is (re/regex? (expression-regex nil   ["0.3.0"])))
    (is (re/regex? (expression-regex nil   ["1.03"])))
    (is (re/regex? (expression-regex nil   ["1.3a"])))
    (is (re/regex? (expression-regex nil   ["0.51"])))
    (is (re/regex? (expression-regex nil   ["86"])))
    (is (re/regex? (expression-regex nil   ["1989"])))
    (is (re/regex? (expression-regex nil   ["20150513"])))
    (is (re/regex? (expression-regex nil   ["2002-12-31"])))
    (is (re/regex? (expression-regex "foo" ["1.0"]))))
  (testing "Semver regexes"
    (let [semver-re (expression-regex* nil ["1.0" "1.1" "2.0"])]   ; e.g. Apache-x.y
      (is (true?  (boolean (re-matches semver-re "0"))))
      (is (true?  (boolean (re-matches semver-re "00"))))
      (is (true?  (boolean (re-matches semver-re "0000000000000000000"))))
      (is (true?  (boolean (re-matches semver-re "0.0"))))
      (is (true?  (boolean (re-matches semver-re "00.00"))))
      (is (true?  (boolean (re-matches semver-re "0.0.0"))))
      (is (true?  (boolean (re-matches semver-re "0.0.0000"))))
      (is (false? (boolean (re-matches semver-re "1"))))
      (is (false? (boolean (re-matches semver-re "1.0"))))
      (is (false? (boolean (re-matches semver-re "0.1"))))
      (is (false? (boolean (re-matches semver-re "00000000001"))))
      (is (false? (boolean (re-matches semver-re "0.00000000001"))))))
  (testing "Semver with suffix regexes"
    (let [semver-re (expression-regex* nil ["1.0" "1.1" "1.2" "1.3a" "1.3c"])]  ; e.g. LPPL-x.ys
      (is (true?  (boolean (re-matches semver-re "0"))))
      (is (true?  (boolean (re-matches semver-re "00"))))
      (is (true?  (boolean (re-matches semver-re "0000000000000000000"))))
      (is (true?  (boolean (re-matches semver-re "0.0"))))
      (is (true?  (boolean (re-matches semver-re "00.00"))))
      (is (true?  (boolean (re-matches semver-re "0.0.0"))))
      (is (true?  (boolean (re-matches semver-re "0.0.0000"))))
      (is (false? (boolean (re-matches semver-re "1"))))
      (is (false? (boolean (re-matches semver-re "1.0"))))
      (is (false? (boolean (re-matches semver-re "0.1"))))
      (is (false? (boolean (re-matches semver-re "00000000001"))))
      (is (false? (boolean (re-matches semver-re "0.00000000001"))))))
  (testing "Version numbers with alternative decimals"
    (let [v21-re (expression-regex* nil ["2.1.0"])]
      (is (true? (boolean (re-matches v21-re "2_1"))))
      (is (true? (boolean (re-matches v21-re "2-1"))))
      (is (true? (boolean (re-matches v21-re "2,1"))))))
  (testing "Version labels"
    (let [v2-re (expression-regex* nil ["2.0"])]
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
    (let [v2-re (expression-regex* "" ["2.0"])]
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2") "VersionNumber")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02") "VersionNumber")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0") "VersionNumber")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02.00") "VersionNumber")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0.0") "VersionNumber")))))
  (testing "only suffix"
    (let [v1-re (expression-regex* nil ["1.0"])]
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
    (let [v2-re (expression-regex* "" ["2.0"])]
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2 only") "Only")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02only") "Only")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "0000000000000000002       only") "Only")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0-only") "Only")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02.00_only") "Only")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0.0 only") "Only")))))
(testing "or later suffix"
    (let [v1-re (expression-regex* nil ["1.0"])]
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
    (let [v2-re (expression-regex* "" ["2.0"])]
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2 or later") "OrLater")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02or later") "OrLater")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "0000000000000000002       or                     later") "OrLater")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0-or-later") "OrLater")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "02.00_or_later") "OrLater")))
      (is (true? (contains? (ncg/re-matches-ncg v2-re "2.0.0 or,later") "OrLater")))))
  (testing "only and/or or-later suffixes"
    (let [v3-re (expression-regex* "" ["3.0"])]
      ; No suffix
      (is (true?  (boolean (re-matches v3-re "3"))))
      (is (true?  (boolean (re-matches v3-re "03"))))
      (is (true?  (boolean (re-matches v3-re "0000000000000000003"))))
      (is (true?  (boolean (re-matches v3-re "3.0"))))
      (is (true?  (boolean (re-matches v3-re "03.00"))))
      (is (true?  (boolean (re-matches v3-re "3.0.0"))))
      (is (false? (boolean (re-matches v3-re "3 only or later"))))
      ; Only suffix
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3 only") "Only")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03only") "Only")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "0000000000000000003       only") "Only")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0-only") "Only")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03.00_only") "Only")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0.0 only") "Only")))
      ; Or later suffix
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03or later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "0000000000000000003       or                     later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0-or-later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03.00_or_later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0.0 or,later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3 or later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03or later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "0000000000000000003       or                     later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0-or-later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "03.00_or_later") "OrLater")))
      (is (true?  (contains? (ncg/re-matches-ncg v3-re "3.0.0 or,later") "OrLater")))))
  (testing "The whole enchilada"
    (let [v21-re (expression-regex* "" ["2.1"])]
      (let [m1 (ncg/re-matches-ncg v21-re "version 2.1 only")]
        (is (true?  (contains? m1 "VersionNumber")))
        (is (true?  (contains? m1 "Only")))
        (is (false? (contains? m1 "OrLater"))))
      (let [m2 (ncg/re-matches-ncg v21-re "version 2.1+")]
        (is (true?  (contains? m2 "VersionNumber")))
        (is (false? (contains? m2 "Only")))
        (is (true?  (contains? m2 "OrLater"))))
      (is (nil? (ncg/re-matches-ncg v21-re "version 2.1 only or later"))))))

