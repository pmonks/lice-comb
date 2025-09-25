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

(ns lice-comb.impl.version-series-test
  (:require [clojure.test                  :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate    :refer [fixture lic-ids-d exc-ids-d]]
            [lice-comb.impl.version-series :refer [version-series-member? id->version-series ids->version-series]]))

(use-fixtures :once fixture)

(deftest version-series-member?-tests
  (testing "Nil, blank"
    (is (false? (version-series-member? nil)))
    (is (false? (version-series-member? "")))
    (is (false? (version-series-member? " ")))
    (is (false? (version-series-member? "\r\n  \n \t"))))
  (testing "Ids that are not in a version series"
    (is (false? (version-series-member? "BSD-2-Clause")))
    (is (false? (version-series-member? "BSD-3-Clause-No-Nuclear-License-2014")))
    (is (false? (version-series-member? "MIT")))
    (is (false? (version-series-member? "MIT-0")))
    (is (false? (version-series-member? "MIT-Modern-Variant")))
    (is (false? (version-series-member? "NIST-PD-fallback")))
    (is (false? (version-series-member? "LZMA-SDK-9.22")))
    (is (false? (version-series-member? "LZMA-SDK-9.11-to-9.20"))))
  (testing "Ids that are in a version series"
    (is (true? (version-series-member? "Apache-2.0")))
    (is (true? (version-series-member? "GPL-2.0")))
    (is (true? (version-series-member? "GPL-2.0-only")))
    (is (true? (version-series-member? "LGPL-3.0-or-later")))
    (is (true? (version-series-member? "LGPL-3.0+")))
    (is (true? (version-series-member? "GPL-2.0-with-classpath-exception")))
    (is (true? (version-series-member? "LPL-1.02")))
    (is (true? (version-series-member? "LPPL-1.3c")))
    (is (true? (version-series-member? "SHL-0.51")))
    (is (true? (version-series-member? "copyleft-next-0.3.1")))
    (is (true? (version-series-member? "Spencer-94")))
    (is (true? (version-series-member? "Sun-PPP-2000")))
    (is (true? (version-series-member? "QPL-1.0-INRIA-2004")))))

(deftest id->version-series-tests
  (testing "Nil, blank"
    (is (nil? (id->version-series nil)))
    (is (nil? (id->version-series "")))
    (is (nil? (id->version-series " ")))
    (is (nil? (id->version-series "\r\n  \n \t"))))
  (testing "Ids that are not in a version series"
    (is (nil? (id->version-series "MIT")))
    (is (nil? (id->version-series "MIT-0")))
    (is (nil? (id->version-series "App-s2p")))
    (is (nil? (id->version-series "0BSD")))
    (is (nil? (id->version-series "BSD-3-Clause")))
    (is (nil? (id->version-series "BSD-2-Clause-NetBSD")))
    (is (nil? (id->version-series "X11")))
    (is (nil? (id->version-series "NTP-0")))
    (is (nil? (id->version-series "Bootloader-exception"))))
  (testing "Ids in a version series - simple cases"
    (is (= "Apache"                   (id->version-series "Apache-1.0")))
    (is (= "Apache"                   (id->version-series "Apache-1.1")))
    (is (= "Apache"                   (id->version-series "Apache-2.0")))
    (is (= "GPL"                      (id->version-series "GPL-1.0")))
    (is (= "GPL"                      (id->version-series "GPL-2.0")))
    (is (= "GPL"                      (id->version-series "GPL-3.0")))
    (is (= "CC-BY-SA"                 (id->version-series "CC-BY-SA-3.0")))
    (is (= "Xdebug"                   (id->version-series "Xdebug-1.03")))
    (is (= "Spencer"                  (id->version-series "Spencer-86")))
    (is (= "Spencer"                  (id->version-series "Spencer-94")))
    (is (= "Spencer"                  (id->version-series "Spencer-99")))
    (is (= "HP"                       (id->version-series "HP-1986")))
    (is (= "HP"                       (id->version-series "HP-1989")))
    (is (= "W3C"                      (id->version-series "W3C-19980720")))
    (is (= "W3C"                      (id->version-series "W3C-20150513")))
    (is (= "Classpath-exception"      (id->version-series "Classpath-exception-2.0")))
    (is (= "Bison-exception"          (id->version-series "Bison-exception-1.24")))
    (is (= "Bison-exception"          (id->version-series "Bison-exception-2.2")))
    (is (= "PS-or-PDF-font-exception" (id->version-series "PS-or-PDF-font-exception-20170817"))))
  (testing "Ids in a version series - suffixes"
    (is (= "GPL"                (id->version-series "GPL-3.0-only")))
    (is (= "GPL"                (id->version-series "GPL-1.0-or-later")))
    (is (= "GPL"                (id->version-series "GPL-1.0+")))
    (is (= "GFDL/invariants"    (id->version-series "GFDL-1.3-invariants-only")))
    (is (= "GFDL/no-invariants" (id->version-series "GFDL-1.3-no-invariants-or-later")))
    (is (= "CC-BY"              (id->version-series "CC-BY-1.0")))
    (is (= "CC-BY/AU"           (id->version-series "CC-BY-2.5-AU")))
    (is (= "CC-BY"              (id->version-series "CC-BY-4.0")))
    (is (= "CC-BY-SA/UK"        (id->version-series "CC-BY-SA-2.0-UK")))
    (is (= "CC-BY-SA/JP"        (id->version-series "CC-BY-SA-2.1-JP")))
    (is (= "CC-BY-SA/IGO"       (id->version-series "CC-BY-SA-3.0-IGO")))
    (is (= "Artistic"           (id->version-series "Artistic-1.0")))
    (is (= "Artistic/cl8"       (id->version-series "Artistic-1.0-cl8")))
    (is (= "Artistic/Perl"      (id->version-series "Artistic-1.0-Perl")))
    (is (= "LPPL"               (id->version-series "LPPL-1.0")))
    (is (= "LPPL"               (id->version-series "LPPL-1.1")))
    (is (= "LPPL"               (id->version-series "LPPL-1.2")))
    (is (= "LPPL"               (id->version-series "LPPL-1.3a")))
    (is (= "LPPL"               (id->version-series "LPPL-1.3c")))
    (is (= "XFree86"            (id->version-series "XFree86-1.1")))
    (is (= "GCC-exception"      (id->version-series "GCC-exception-2.0")))
    (is (= "GCC-exception/note" (id->version-series "GCC-exception-2.0-note")))
    (is (= "GCC-exception"      (id->version-series "GCC-exception-3.1"))))
  (testing "Ids in a version series - weird cases"
    (is (= "CECILL"         (id->version-series "CECILL-1.0")))
    (is (= "CECILL"         (id->version-series "CECILL-2.1")))
    (is (nil?               (id->version-series "CECILL-B")))
    (is (nil?               (id->version-series "LZMA-SDK-9.11-to-9.20")))
    (is (nil?               (id->version-series "LZMA-SDK-9.22")))
    (is (= "QPL/INRIA-2004" (id->version-series "QPL-1.0-INRIA-2004")))))

(deftest ids->version-series-tests
  (testing "Nil, empty, etc."
    (is (nil? (ids->version-series nil)))
    (is (nil? (ids->version-series []))))
  (testing "Short lists of ids"
    (is (= ["Apache" "GPL" nil] (keys (ids->version-series ["Apache-1.0" "GPL-2.0" "Apache-2.0" "CECILL-B" "GPL-3.0-or-later"])))))
  (testing "Long lists of ids"
    ; We just cherrypick a few version series' here (checking all of them would be difficult as the SPDX license list grows)
    (let [license-families (set (keys (ids->version-series @lic-ids-d)))]
      (is (contains? license-families "Apache"))
      (is (contains? license-families "AGPL"))
      (is (contains? license-families "GPL"))
      (is (contains? license-families "LGPL"))
      (is (contains? license-families "CC-BY"))
      (is (contains? license-families "CC-BY-SA"))
      (is (contains? license-families "CC-BY-NC"))
      (is (contains? license-families "CC-BY-ND"))
      (is (contains? license-families "CC-BY-NC-ND")))
    (let [exception-families (set (keys (ids->version-series @exc-ids-d)))]
      (is (contains? exception-families "Classpath-exception"))  ; 1 version
      (is (contains? exception-families "Autoconf-exception"))   ; 2 versions
      (is (contains? exception-families "GStreamer-exception"))  ; 2 versions, but versions are years rather than semver
      (is (contains? exception-families "GCC-exception")))))     ; 3 versions, including one with a suffix
