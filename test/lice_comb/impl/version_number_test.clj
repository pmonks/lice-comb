;
; Copyright © 2025 Peter Monks
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

(ns lice-comb.impl.version-number-test
  (:require [clojure.test                  :refer [deftest testing is use-fixtures]]
            [clojure.string                :as s]
            [wreck.api                     :as re]
            [lice-comb.test-boilerplate    :refer [fixture]]
            [lice-comb.impl.version-number :refer [canonicalise metadata exact-regex range-regex]]))

(use-fixtures :once fixture)

(deftest canonicalise-tests
  (testing "Nil, blank, not a valid version"
    (is (nil? (canonicalise nil)))
    (is (nil? (canonicalise "")))
    (is (nil? (canonicalise " ")))
    (is (nil? (canonicalise "\r\n  \n \t")))
    (is (nil? (canonicalise "foo")))
    (is (nil? (canonicalise ".")))
    (is (nil? (canonicalise ".2")))
    (is (nil? (canonicalise "-2")))
    (is (nil? (canonicalise "LZMA-SDK-9.22")))
    (is (nil? (canonicalise "LZMA-SDK-9.11-to-9.20"))))
  (testing "semver - no change"
    (is (= "1.0"         (canonicalise "1.0")))
    (is (= "1.1"         (canonicalise "1.1")))
    (is (= "1.0.1"       (canonicalise "1.0.1")))
    (is (= "1.1.1"       (canonicalise "1.1.1")))
    (is (= "1.1.1a"      (canonicalise "1.1.1a"))))
  (testing "semver - canonicalised"
    (is (= "1.0"         (canonicalise "1")))
    (is (= "1.0"         (canonicalise "01")))
    (is (= "1.0"         (canonicalise "1.0.0")))
    (is (= "1.1"         (canonicalise "01.1")))
    (is (= "1.1"         (canonicalise "01.01")))
    (is (= "1.1"         (canonicalise "1.1.0")))
    (is (= "1.0.1"       (canonicalise "000000001.0000000.000000001")))
    (is (= "1.0.1x"      (canonicalise "000000001.0000000.000000001x")))
    (is (= "2.1"         (canonicalise "2_1"))))  ; e.g. from things like lgpl_v2_1
  (testing "year2 - no change"
    (is (= "89"          (canonicalise "89")))
    (is (= "96"          (canonicalise "96")))
    (is (= "96b"         (canonicalise "96b"))))
  (testing "year2 - canonicalised"
    (is (= "89"          (canonicalise "089")))
    (is (= "96"          (canonicalise "096")))
    (is (= "96g"         (canonicalise "096g"))))
  (testing "year4 - no change"
    (is (= "1989"        (canonicalise "1989")))
    (is (= "2006"        (canonicalise "2006")))
    (is (= "2006f"       (canonicalise "2006f"))))
  (testing "year4 - canonicalised"
    (is (= "1989"        (canonicalise "01989")))
    (is (= "2006"        (canonicalise "002006")))
    (is (= "2006g"       (canonicalise "002006g"))))
  (testing "date - no change"
    (is (= "1998-07-20"  (canonicalise "1998-07-20")))
    (is (= "2015-05-13"  (canonicalise "2015-05-13")))
    (is (= "2015-05-13y" (canonicalise "2015-05-13y"))))
  (testing "date - canonicalised"
    (is (= "1998-07-20"  (canonicalise "19980720")))
    (is (= "2017-10-02"  (canonicalise "20171002")))
    (is (= "2017-10-02"  (canonicalise "2017_10_02")))
    (is (= "2015-05-13"  (canonicalise "0020150513")))
    (is (= "2015-05-13"  (canonicalise "002015-0005-0013")))
    (is (= "2015-05-13k" (canonicalise "0020150513k")))))

(deftest metadata-tests
  (testing "Nil, blank, not a valid version"
    (is (nil? (metadata nil)))
    (is (nil? (metadata [])))
    (is (nil? (metadata [""])))
    (is (nil? (metadata [" " "\t\n"]))))
  ;####TODO: IMPLEMENT ME!!!!
  )

(deftest exact-regex-tests
  (testing "Nil, blank, not a valid version"
    (is (nil? (exact-regex nil)))
    (is (nil? (exact-regex "")))
    (is (nil? (exact-regex " ")))
    (is (nil? (exact-regex "\r\n  \n \t")))
    (is (nil? (exact-regex "foo")))
    (is (nil? (exact-regex ".")))
    (is (nil? (exact-regex ".2")))
    (is (nil? (exact-regex "-2")))
    (is (nil? (exact-regex "LZMA-SDK-9.22")))
    (is (nil? (exact-regex "LZMA-SDK-9.11-to-9.20")))
    (is (nil? (exact-regex "2.0ab"))))  ; Can only have a 1 letter suffix
  (testing "Valid version numbers"
    (is (re/regex? (exact-regex "0")))
    (is (re/regex? (exact-regex "1")))
    (is (re/regex? (exact-regex "1.0")))
    (is (re/regex? (exact-regex "1.1")))
    (is (re/regex? (exact-regex "1.1.0.1")))
    (is (re/regex? (exact-regex "0.0.0.0.0.1")))
  (testing "Valid version numbers - semver (non-strict)"
    (is (re/=' #"0*0(?:[\-–—_,\.]0+)*"                           (exact-regex "00")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (exact-regex "2")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (exact-regex "02")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (exact-regex "2.0")))
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*"                           (exact-regex "2.0.0")))
    (is (re/=' #"0*2[\-–—_,\.]0*1(?:[\-–—_,\.]0+)*"              (exact-regex "2.1")))
    (is (re/=' #"0*2[\-–—_,\.]0*0[\-–—_,\.]0*1(?:[\-–—_,\.]0+)*" (exact-regex "2.0.1")))
    (is (re/=' #"0*99[\-–—_,\.]0*100(?:[\-–—_,\.]0+)*"           (exact-regex "000000099.00000100"))))
  (testing "Valid version numbers - semver (strict)"
    (is (re/=' #"0*0(?:\.0+)*"           (exact-regex "00"    true)))
    (is (re/=' #"0*2(?:\.0+)*"           (exact-regex "2"     true)))
    (is (re/=' #"0*2(?:\.0+)*"           (exact-regex "02"    true)))
    (is (re/=' #"0*2(?:\.0+)*"           (exact-regex "2.0"   true)))
    (is (re/=' #"0*2(?:\.0+)*"           (exact-regex "2.0.0" true)))
    (is (re/=' #"0*2\.0*1(?:\.0+)*"      (exact-regex "2.1"   true)))
    (is (re/=' #"0*2\.0*0\.0*1(?:\.0+)*" (exact-regex "2.0.1" true)))
    (is (re/=' #"0*99\.0*100(?:\.0+)*"   (exact-regex "000000099.00000100" true))))
  (testing "Valid version numbers - 2 digit year"
    (is (re/=' #"0*(?:19)?86" (exact-regex "86")))
    (is (re/=' #"0*(?:19)?86" (exact-regex "86" true)))  ; Strict separators flag has no impact on 2 digit years
    (is (re/=' #"0*(?:19)?89" (exact-regex "0000089"))))
  (testing "Valid version numbers - 4 digit year"
    (is (re/=' #"0*1986" (exact-regex "1986")))
    (is (re/=' #"0*1986" (exact-regex "1986" true)))  ; Strict separators flag has no impact on 4 digit years
    (is (re/=' #"0*2006" (exact-regex "000002006"))))
  (testing "Valid version numbers - 8 digit year"
    (is (re/=' #"0*1998[\-–—_]?0*7[\-–—_]?0*20"  (exact-regex "19980720")))
    (is (re/=' #"0*1998\-?0*7\-?0*20"            (exact-regex "19980720" true)))  ; Strict separators _do_ have an impact on dates
    (is (re/=' #"0*2015[\-–—_]?0*5[\-–—_]?0*13"  (exact-regex "0020150513"))))
  (testing "Valid version numbers - suffixes"
    (is (re/=' #"0*2(?:[\-–—_,\.]0+)*(?i:a)"              (exact-regex "2a")))
    (is (re/=' #"0*1[\-–—_,\.]0*3(?:[\-–—_,\.]0+)*(?i:c)" (exact-regex "1.3c")))
    (is (re/=' #"0*1\.0*3(?:\.0+)*(?i:c)"                 (exact-regex "1.3c" true)))
    (is (re/=' #"0*(?:19)?86(?i:b)"                       (exact-regex "86b")))                ; This case doesn't exist in the SPDX license list as of v3.28, but we test it anyway
    (is (re/=' #"0*2006(?i:x)"                            (exact-regex "2006x")))              ; This case doesn't exist in the SPDX license list as of v3.28, but we test it anyway
    (is (re/=' #"0*2015[\-–—_]?0*5[\-–—_]?0*13(?i:q)"     (exact-regex "0020150513q")))        ; This case doesn't exist in the SPDX license list as of v3.28, but we test it anyway
    (is (re/=' #"0*2020\-?0*10\-?0*11(?i:v)"              (exact-regex "20201011v" true))))))  ; This case doesn't exist in the SPDX license list as of v3.28, but we test it anyway

(defn range-regex-matches?
  "Turns the given `version-numbers` into a regex, then confirms that it matches
  every version in `version-numbers` is matched by it."
  [version-numbers]
  (boolean
    (when (seq (filter (complement s/blank?) version-numbers))
      (let [regex (range-regex version-numbers)]
        (every? identity (map (partial re-matches regex) version-numbers))))))

(deftest range-regex-tests
  (testing "Nil, blank, not a valid version"
    (is (nil? (range-regex nil)))
    (is (nil? (range-regex [])))
    (is (nil? (range-regex [""])))
    (is (nil? (range-regex [" " "\t\n"]))))
  (testing "Invalid version number patterns"
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["ab"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["-" "." "%"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["2.0ab"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["2.0" "89"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["2.0" "1984"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["2.0" "1984-01-01"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["84" "1984"])))
    (is (thrown? clojure.lang.ExceptionInfo (range-regex ["1984" "19890101"]))))
  (testing "Valid version ranges produce regexes"
    (is (re/regex? (range-regex ["1"])))
    (is (re/regex? (range-regex ["1.0"])))
    (is (re/regex? (range-regex ["1.0" "2.0"])))
    (is (re/regex? (range-regex ["1.0" "2.0" "2.1c"])))
    (is (re/regex? (range-regex ["389"])))
    (is (re/regex? (range-regex ["12345"])))
    (is (re/regex? (range-regex ["2024-05-05"])))
    (is (re/regex? (range-regex ["86" "89"])))
    (is (re/regex? (range-regex ["1986" "1989"])))
    (is (re/regex? (range-regex ["19980720" "20150513"]))))
  (testing "Valid version ranges match produced regexes"
    (is (range-regex-matches? ["1"]))
    (is (range-regex-matches? ["1.0"]))
    (is (range-regex-matches? ["1.0" "2.0"]))
    (is (range-regex-matches? ["1.0" "2.0" "2.1c"]))
    (is (range-regex-matches? ["86" "89"]))
    (is (range-regex-matches? ["1986" "1989" "2007"]))
    (is (range-regex-matches? ["19980720" "20150513"])))
  ; Semver
  (let [regex (range-regex ["1.0" "2.1"])]
    (testing "Semver range regexes match other values of the same pattern"
      (is (re-matches regex "1"))
      (is (re-matches regex "1.0"))
      (is (re-matches regex "1.1"))
      (is (re-matches regex "1.2"))
      (is (re-matches regex "2.0"))
      (is (re-matches regex "3.1.1"))
      (is (re-matches regex "2_1"))
      (is (re-matches regex "000002.000002"))
      (is (re-matches regex "99"))
      (is (re-matches regex "1999"))
      (is (re-matches regex "19990909"))
      (is (re-matches regex "9999.9999")))
    (testing "Semver range regexes do NOT match unrelated values"
      (is (not (re-matches regex "a1.0")))
      (is (not (re-matches regex "1.0a")))
      (is (not (re-matches regex "1.0+")))
      (is (not (re-matches regex "ab")))))
  ; Semver + suffix
  (let [regex (range-regex ["1.0" "1.5c"])]
    (testing "Semver+suffix range regexes match other values of the same pattern"
      (is (re-matches regex "1"))
      (is (re-matches regex "1a"))
      (is (re-matches regex "1.2"))
      (is (re-matches regex "1.2c"))
      (is (re-matches regex "3.1.1"))
      (is (re-matches regex "3.1.1m"))
      (is (re-matches regex "000002.000002"))
      (is (re-matches regex "000002.000002x"))
      (is (re-matches regex "99"))
      (is (re-matches regex "99n"))
      (is (re-matches regex "1999"))
      (is (re-matches regex "1999r"))
      (is (re-matches regex "19990909"))
      (is (re-matches regex "19990909p"))
      (is (re-matches regex "9999.9999"))
      (is (re-matches regex "9999.9999X")))
    (testing "Semver+suffix range regexes do NOT match unrelated values"
      (is (not (re-matches regex "a1.0")))
      (is (not (re-matches regex "1.0ab")))
      (is (not (re-matches regex "1.0+")))
      (is (not (re-matches regex "ab")))))
  ; 2 digit year
  (let [regex (range-regex ["86" "89"])]
    (testing "2 digit year range regexes match other values of the same pattern"
      (is (re-matches regex "92"))
      (is (re-matches regex "1992"))
      (is (re-matches regex "00092"))
      (is (re-matches regex "01992")))
    (testing "2 digit year range regexes do NOT match unrelated values"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "2004")))  ; This regex only accepts 4 digit years of the form 19xx
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101"))))
  ; 2 digit year + suffix
  (let [regex (range-regex ["86" "89a"])]
    (testing "2 digit year+suffix range regexes match other values of the same pattern"
      (is (re-matches regex "92"))
      (is (re-matches regex "92a"))
      (is (re-matches regex "1992"))
      (is (re-matches regex "1992v"))
      (is (re-matches regex "00092"))
      (is (re-matches regex "00092j"))
      (is (re-matches regex "01992"))
      (is (re-matches regex "01992X")))
    (testing "2 digit year+suffix range regexes do NOT match unrelated values"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "a1")))
      (is (not (re-matches regex "1a")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "1.0ab")))
      (is (not (re-matches regex "99ab")))
      (is (not (re-matches regex "2004a")))  ; This regex only accepts 4 digit years of the form 19xx
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101")))))
  ; 4 digit year
  (let [regex (range-regex ["1997" "2005"])]
    (testing "4 digit year range regexes match other values of the same pattern - 20th century"
      (is (re-matches regex "1992"))
      (is (re-matches regex "01992"))
      (is (re-matches regex "92"))
      (is (re-matches regex "00092")))
    (testing "4 digit year range regexes do NOT match unrelated values - 20th century"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101")))))
  ; 4 digit year, none in the 20th century
  (let [regex (range-regex ["2001" "2006"])]
    (testing "4 digit year range regexes match other values of the same pattern - no 20th century"
      (is (re-matches regex "1992"))
      (is (re-matches regex "01992")))
    (testing "4 digit year range regexes do NOT match unrelated values - 20th century"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "92")))
      (is (not (re-matches regex "00092")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101")))))
  ; 4 digit year + suffix
  (let [regex (range-regex ["1997v" "2005"])]
    (testing "4 digit year+suffix range regexes match other values of the same pattern - 20th century"
      (is (re-matches regex "1992"))
      (is (re-matches regex "1992a"))
      (is (re-matches regex "92"))
      (is (re-matches regex "92a"))
      (is (re-matches regex "00092y"))
      (is (re-matches regex "99a"))
      (is (re-matches regex "01992X")))
    (testing "4 digit year+suffix range regexes do NOT match unrelated values - 20th century"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101")))))
  ; 4 digit year + suffix, none in the 20th century
  (let [regex (range-regex ["2001a" "2001b"])]
    (testing "4 digit year+suffix range regexes match other values of the same pattern - no 20th century"
      (is (re-matches regex "1992"))
      (is (re-matches regex "1992a"))
      (is (re-matches regex "01992X")))
    (testing "4 digit year+suffix range regexes do NOT match unrelated values - no 20th century"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "92")))
      (is (not (re-matches regex "92a")))
      (is (not (re-matches regex "00092y")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101")))))
  ; 8 digit date
  (let [regex (range-regex ["19980720" "20150513"])]
    (testing "8 digit date range regexes match other values of the same pattern"
      (is (re-matches regex "19920101"))
      (is (re-matches regex "029990909"))
      (is (re-matches regex "18741231"))
      (is (re-matches regex "0000002999-00009-00009"))
      (is (re-matches regex "2005-01-01")))
    (testing "8 digit date range regexes do NOT match unrelated values"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "92")))
      (is (not (re-matches regex "00092")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "12345")))))
  ; 8 digit date + suffix
  (let [regex (range-regex ["19980720" "20150513a"])]
    (testing "8 digit date+suffix range regexes match other values of the same pattern"
      (is (re-matches regex "19920101"))
      (is (re-matches regex "029990909x"))
      (is (re-matches regex "18741231c"))
      (is (re-matches regex "0000002999-00009-00009z"))
      (is (re-matches regex "2005-01-01X")))
    (testing "8 digit date+suffix range regexes do NOT match unrelated values"
      (is (not (re-matches regex "1")))
      (is (not (re-matches regex "1.0")))
      (is (not (re-matches regex "92")))
      (is (not (re-matches regex "00092")))
      (is (not (re-matches regex "99a")))
      (is (not (re-matches regex "12345")))
      (is (not (re-matches regex "20050101ab")))
      (is (not (re-matches regex "1770-04-19ab")))))))
