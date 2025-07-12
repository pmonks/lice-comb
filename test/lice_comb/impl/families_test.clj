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

(ns lice-comb.impl.families-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate :refer [fixture lic-ids-d exc-ids-d]]
            [lice-comb.impl.families    :refer [family-member? id->family ids->families]]))

(use-fixtures :once fixture)

(deftest family-member?-tests
  (testing "Nil, blank"
    (is (false? (family-member? nil)))
    (is (false? (family-member? "")))
    (is (false? (family-member? " ")))
    (is (false? (family-member? "\r\n  \n \t"))))
  (testing "Ids that are not in a family"
    (is (false? (family-member? "BSD-2-Clause")))
    (is (false? (family-member? "MIT")))
    (is (false? (family-member? "MIT-0")))
    (is (false? (family-member? "MIT-Modern-Variant")))
    (is (false? (family-member? "NIST-PD-fallback"))))
  (testing "Ids that are in a family"
    (is (true? (family-member? "Apache-2.0")))
    (is (true? (family-member? "GPL-2.0")))
    (is (true? (family-member? "GPL-2.0-only")))
    (is (true? (family-member? "LGPL-3.0-or-later")))
    (is (true? (family-member? "LGPL-3.0+")))
    (is (true? (family-member? "GPL-2.0-with-classpath-exception")))
    (is (true? (family-member? "LPL-1.02")))
    (is (true? (family-member? "LPPL-1.3c")))
    (is (true? (family-member? "SHL-0.51")))
    (is (true? (family-member? "copyleft-next-0.3.1")))
    (is (true? (family-member? "LZMA-SDK-9.22")))
    (is (true? (family-member? "LZMA-SDK-9.11-to-9.20")))
    (is (true? (family-member? "Spencer-94")))
    (is (true? (family-member? "Sun-PPP-2000")))
    (is (true? (family-member? "QPL-1.0-INRIA-2004")))
  ))

(deftest id->family-tests
  (testing "Nil, blank"
    (is (nil? (id->family nil)))
    (is (nil? (id->family "")))
    (is (nil? (id->family " ")))
    (is (nil? (id->family "\r\n  \n \t"))))
  (testing "Ids that are not in a family"
    (is (nil? (id->family "MIT")))
    (is (nil? (id->family "App-s2p")))
    (is (nil? (id->family "BSD-3-Clause")))
    (is (nil? (id->family "X11")))
    (is (nil? (id->family "Bootloader-exception"))))
  (testing "Ids in a family - simple cases"
    (is (= "Apache"                   (id->family "Apache-1.0")))
    (is (= "Apache"                   (id->family "Apache-1.1")))
    (is (= "Apache"                   (id->family "Apache-2.0")))
    (is (= "GPL"                      (id->family "GPL-1.0")))
    (is (= "GPL"                      (id->family "GPL-2.0")))
    (is (= "GPL"                      (id->family "GPL-3.0")))
    (is (= "CC-BY-SA"                 (id->family "CC-BY-SA-3.0")))
    (is (= "Xdebug"                   (id->family "Xdebug-1.03")))
    (is (= "Spencer"                  (id->family "Spencer-86")))
    (is (= "Spencer"                  (id->family "Spencer-94")))
    (is (= "Spencer"                  (id->family "Spencer-99")))
    (is (= "HP"                       (id->family "HP-1986")))
    (is (= "HP"                       (id->family "HP-1989")))
    (is (= "W3C"                      (id->family "W3C-19980720")))
    (is (= "W3C"                      (id->family "W3C-20150513")))
    (is (= "Classpath-exception"      (id->family "Classpath-exception-2.0")))
    (is (= "Bison-exception"          (id->family "Bison-exception-1.24")))
    (is (= "Bison-exception"          (id->family "Bison-exception-2.2")))
    (is (= "PS-or-PDF-font-exception" (id->family "PS-or-PDF-font-exception-20170817"))))
  (testing "Ids in a family - suffixes"
    (is (= "GPL"           (id->family "GPL-3.0-only")))
    (is (= "GPL"           (id->family "GPL-1.0-or-later")))
    (is (= "GPL"           (id->family "GPL-1.0+")))
    (is (= "GFDL"          (id->family "GFDL-1.3-no-invariants-or-later")))
    (is (= "CC-BY"         (id->family "CC-BY-1.0")))
    (is (= "CC-BY"         (id->family "CC-BY-2.5-AU")))
    (is (= "CC-BY"         (id->family "CC-BY-4.0")))
    (is (= "CC-BY-SA"      (id->family "CC-BY-SA-2.0-UK")))
    (is (= "CC-BY-SA"      (id->family "CC-BY-SA-2.1-JP")))
    (is (= "CC-BY-SA"      (id->family "CC-BY-SA-3.0-IGO")))
    (is (= "Artistic"      (id->family "Artistic-1.0")))
    (is (= "Artistic"      (id->family "Artistic-1.0-cl8")))
    (is (= "Artistic"      (id->family "Artistic-1.0-Perl")))
    (is (= "LPPL"          (id->family "LPPL-1.0")))
    (is (= "LPPL"          (id->family "LPPL-1.1")))
    (is (= "LPPL"          (id->family "LPPL-1.2")))
    (is (= "LPPL"          (id->family "LPPL-1.3a")))
    (is (= "LPPL"          (id->family "LPPL-1.3c")))
    (is (= "GCC-exception" (id->family "GCC-exception-2.0")))
    (is (= "GCC-exception" (id->family "GCC-exception-2.0-note")))
    (is (= "GCC-exception" (id->family "GCC-exception-3.1"))))
  (testing "Ids in a family - weird cases"
    (is (= "CECILL"   (id->family "CECILL-1.0")))
    (is (= "CECILL"   (id->family "CECILL-2.1")))
    (is (nil?         (id->family "CECILL-B")))
    (is (= "LZMA-SDK" (id->family "LZMA-SDK-9.11-to-9.20")))
    (is (= "LZMA-SDK" (id->family "LZMA-SDK-9.22")))))

(deftest ids->families-tests
  (testing "Nil, empty, etc."
    (is (nil? (ids->families nil)))
    (is (nil? (ids->families []))))
  (testing "Short lists of ids"
    (is (= ["Apache" "GPL" nil] (keys (ids->families ["Apache-1.0" "GPL-2.0" "Apache-2.0" "CECILL-B" "GPL-3.0-or-later"])))))
  (testing "Long lists of ids"
    ; We just cherrypick a few families here (checking all of them would be difficult as the SPDX license list grows)
    (let [license-families (set (keys (ids->families @lic-ids-d)))]
      (is (contains? license-families "Apache"))
      (is (contains? license-families "AGPL"))
      (is (contains? license-families "GPL"))
      (is (contains? license-families "LGPL"))
      (is (contains? license-families "CC-BY"))
      (is (contains? license-families "CC-BY-SA"))
      (is (contains? license-families "CC-BY-NC"))
      (is (contains? license-families "CC-BY-ND"))
      (is (contains? license-families "CC-BY-NC-ND")))
    (let [exception-families (set (keys (ids->families @exc-ids-d)))]
      (is (contains? exception-families "Classpath-exception"))  ; 1 version
      (is (contains? exception-families "Autoconf-exception"))   ; 2 versions
      (is (contains? exception-families "GStreamer-exception"))  ; 2 versions, but versions are years rather than semver
      (is (contains? exception-families "GCC-exception")))))     ; 3 versions, including one with a suffix
