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
            [clojure.set                   :as set]
            [lice-comb.test-boilerplate    :refer [fixture all-version-series-d]]
            [lice-comb.impl.version-series :refer [id->version-series irregular-ids irregular-names mixed-type-version-series version-series]]))

(use-fixtures :once fixture)


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
    (is (nil? (id->version-series "389-exception")))
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
    (is (= "LPPL"                     (id->version-series "LPPL-1.0")))
    (is (= "LPPL"                     (id->version-series "LPPL-1.1")))
    (is (= "LPPL"                     (id->version-series "LPPL-1.2")))
    (is (= "LPPL"                     (id->version-series "LPPL-1.3a")))
    (is (= "LPPL"                     (id->version-series "LPPL-1.3c")))
    (is (= "XFree86"                  (id->version-series "XFree86-1.1")))
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
    (is (= "FSL/ALv2"           (id->version-series "FSL-1.1-ALv2")))
    (is (= "FSL/MIT"            (id->version-series "FSL-1.1-MIT")))
    (is (= "GCC-exception"      (id->version-series "GCC-exception-2.0")))
    (is (= "GCC-exception/note" (id->version-series "GCC-exception-2.0-note")))
    (is (= "GCC-exception"      (id->version-series "GCC-exception-3.1"))))
  (testing "Ids in a version series - weird/special cases"
    (is (= "Affero"         (id->version-series "AGPL-1.0")))
    (is (= "AGPL"           (id->version-series "AGPL-3.0")))
    (is (= "CECILL"         (id->version-series "CECILL-1.0")))
    (is (= "CECILL"         (id->version-series "CECILL-2.1")))
    (is (nil?               (id->version-series "CECILL-B")))
    (is (= "W3C"            (id->version-series "W3C-19980720")))
    (is (= "W3C"            (id->version-series "W3C")))
    (is (= "W3C"            (id->version-series "W3C-20150513")))
    (is (= "LZMA-SDK"       (id->version-series "LZMA-SDK-9.11-to-9.20")))
    (is (= "LZMA-SDK"       (id->version-series "LZMA-SDK-9.22")))
    (is (= "QPL/INRIA-2004" (id->version-series "QPL-1.0-INRIA-2004")))
    (is (nil?               (id->version-series "QPL-1.0-INRIA-2004-exception")))  ; The version number relates to the parent license, not the exception
    (is (= "libpng"         (id->version-series "Libpng")))
    (is (= "SHL"            (id->version-series "SHL-0.5")))    ; License id
    (is (= "SHL"            (id->version-series "SHL-0.51")))   ;    "
    (is (= "SHL"            (id->version-series "SHL-2.0")))    ; Exception id
    (is (= "SHL"            (id->version-series "SHL-2.1")))    ;    "
    (is (nil?               (id->version-series "LGPL-3.0-linking-exception")))       ; The version number relates to the parent license, not the exception
    (is (nil?               (id->version-series "GPL-3.0-389-ds-base-exception")))))  ;    "

(deftest irregular-and-mixed-tests
  (testing "Version series with irregular ids haven't changed as new SPDX license lists are released"
    (is (= #{"libpng" "W3C" "LZMA-SDK"}
        (set (keys (irregular-ids))))))
  (testing "Version series with irregular names haven't changed as new SPDX license lists are released"
    (is (= #{"CC-BY-NC-ND" "CC-BY-SA" "libpng" "CC-BY-NC" "W3C" "CC-BY-ND" "LZMA-SDK" "CC-BY-NC-SA" "CC-BY" "LGPL" "OLDAP"}
           (set (keys (irregular-names))))))
  (testing "Version series with mixed identifier types haven't changed as new SPDX license lists are released"
    (is (= #{"SHL"} (set (keys (mixed-type-version-series)))))))

(deftest version-series-tests
  (testing "Nil, empty, etc."
    (is (nil? (version-series nil)))
    (is (nil? (version-series []))))
  (testing "Short sequences of cherry-picked ids"
    (is (= {:unversioned-ids #{"MIT"}} (version-series ["MIT"])))
    (is (= {:version-series  {"Apache" {:series-id       "Apache"
                                        :default-id      "Apache-2.0"
                                        :version-type    :semver
                                        :version-suffix? false
                                        :versions        ["2.0"]
                                        :ids             ["Apache-2.0"]
                                        :names           ["Apache License 2.0"]
                                        :id-formats      ["Apache-${VER}"]
                                        :name-formats    ["Apache License ${VER}"]}}}
           (version-series ["Apache-2.0"])))
    (is (= {:unversioned-ids #{"MIT" "BSD-2-Clause"}
            :version-series  {"Apache" {:series-id       "Apache"
                                        :default-id      "Apache-2.0"
                                        :version-type    :semver
                                        :version-suffix? false
                                        :versions        ["2.0"]
                                        :ids             ["Apache-2.0"]
                                        :names           ["Apache License 2.0"]
                                        :id-formats      ["Apache-${VER}"]
                                        :name-formats    ["Apache License ${VER}"]}}}
           (version-series ["Apache-2.0" "BSD-2-Clause" "MIT"])))
    (is (= {:version-series  {"Apache" {:series-id       "Apache"
                                        :default-id      "Apache-2.0"
                                        :version-type    :semver
                                        :version-suffix? false
                                        :versions        ["2.0"]
                                        :ids             ["Apache-2.0"]
                                        :names           ["Apache License 2.0"]
                                        :id-formats      ["Apache-${VER}"]
                                        :name-formats    ["Apache License ${VER}"]}}}
           (version-series ["Apache-2.0"])))
    (is (= {:version-series  {"OSL" {:series-id       "OSL"
                                     :default-id      "OSL-3.0"
                                     :version-type    :semver
                                     :version-suffix? false
                                     :versions        ["1.0" "1.1" "2.0" "2.1" "3.0"]
                                     :ids             ["OSL-1.0" "OSL-1.1" "OSL-2.0" "OSL-2.1" "OSL-3.0"]
                                     :names           ["Open Software License 1.0" "Open Software License 1.1" "Open Software License 2.0" "Open Software License 2.1" "Open Software License 3.0"]
                                     :id-formats      ["OSL-${VER}"]
                                     :name-formats    ["Open Software License ${VER}"]}}}
           (version-series ["OSL-1.0" "OSL-1.1" "OSL-2.0" "OSL-2.1" "OSL-3.0"]))))
  (let [{version-series  :version-series
         unversioned-ids :unversioned-ids} @all-version-series-d]
    (testing "Handpicked unversioned ids that could be confused for being in a version series"
      (is (contains? unversioned-ids "BSD-2-Clause"))
      (is (contains? unversioned-ids "NTP-0"))
      (is (contains? unversioned-ids "mpich2"))
      (is (contains? unversioned-ids "BSD-4.3RENO"))
      (is (contains? unversioned-ids "BSD-4.3TAHOE"))
      (is (contains? unversioned-ids "389-exception")))
    (testing "Version series - semver2"
      (is (= {:series-id       "Hippocratic"
              :default-id      "Hippocratic-2.1"
              :version-type    :semver
              :version-suffix? false
              :versions        ["2.1"]
              :ids             ["Hippocratic-2.1"]
              :names           ["Hippocratic License 2.1"]
              :id-formats      ["Hippocratic-${VER}"]
              :name-formats    ["Hippocratic License ${VER}"]}
             (get version-series "Hippocratic")))
      (is (= {:series-id       "Apache"
              :default-id      "Apache-2.0"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0" "1.1" "2.0"]
              :ids             ["Apache-1.0" "Apache-1.1" "Apache-2.0"]
              :names           ["Apache License 1.0" "Apache License 1.1" "Apache License 2.0"]
              :id-formats      ["Apache-${VER}"]
              :name-formats    ["Apache License ${VER}"]}
             (get version-series "Apache"))))
    (testing "Version series - semver3"
      (is (= {:series-id       "Python"
              :default-id      "Python-2.0.1"
              :version-type    :semver
              :version-suffix? false
              :versions        ["2.0" "2.0.1"]
              :ids             ["Python-2.0" "Python-2.0.1"]
              :names           ["Python License 2.0" "Python License 2.0.1"]
              :id-formats      ["Python-${VER}"]
              :name-formats    ["Python License ${VER}"]}
             (get version-series "Python")))
      (is (= {:series-id       "copyleft-next"
              :default-id      "copyleft-next-0.3.1"
              :version-type    :semver
              :version-suffix? false
              :versions        ["0.3.0" "0.3.1"]
              :ids             ["copyleft-next-0.3.0" "copyleft-next-0.3.1"]
              :names           ["copyleft-next 0.3.0" "copyleft-next 0.3.1"]
              :id-formats      ["copyleft-next-${VER}"]
              :name-formats    ["copyleft-next ${VER}"]}
             (get version-series "copyleft-next"))))
    (testing "Version series - semver + suffix"
      (is (= {:series-id       "gSOAP"
              :default-id      "gSOAP-1.3b"
              :version-type    :semver
              :version-suffix? true
              :versions        ["1.3b"]
              :ids             ["gSOAP-1.3b"]
              :names           ["gSOAP Public License v1.3b"]
              :id-formats      ["gSOAP-${VER}"]
              :name-formats    ["gSOAP Public License ${VER}"]}
             (get version-series "gSOAP"))))
    (testing "Version series - 2 digit year"
      (is (= {:series-id       "Spencer"
              :default-id      "Spencer-99"
              :version-type    :year2
              :version-suffix? false
              :versions        ["86" "94" "99"]
              :ids             ["Spencer-86" "Spencer-94" "Spencer-99"]
              :names           ["Spencer License 86" "Spencer License 94" "Spencer License 99"]
              :id-formats      ["Spencer-${VER}"]
              :name-formats    ["Spencer License ${VER}"]}
             (get version-series "Spencer"))))
    (testing "Version series - 2 digit year + suffix")  ; DOESN'T EXIST AS OF SPDX LICENSE LIST v3.27.0
    (testing "Version series - 4 digit year"
      (is (= {:series-id       "Unicode-DFS"
              :default-id      "Unicode-DFS-2016"
              :version-type    :year4
              :version-suffix? false
              :versions        ["2015" "2016"]
              :ids             ["Unicode-DFS-2015" "Unicode-DFS-2016"]
              :names           ["Unicode License Agreement - Data Files and Software (2015)" "Unicode License Agreement - Data Files and Software (2016)"]
              :id-formats      ["Unicode-DFS-${VER}"]
              :name-formats    ["Unicode License Agreement - Data Files and Software (${VER})"]}
             (get version-series "Unicode-DFS"))))
    (testing "Version series - 4 digit year + suffix")  ; DOESN'T EXIST AS OF SPDX LICENSE LIST v3.27.0
    (testing "Version series - full date"
      (is (= {:series-id       "W3C"
              :default-id      "W3C-20150513"
              :version-type    :date
              :version-suffix? false
              :versions        ["19980720" "20150513"]
              :ids             ["W3C-19980720" "W3C" "W3C-20150513"]
              :names           ["W3C Software Notice and License (1998-07-20)" "W3C Software Notice and License (2002-12-31)" "W3C Software Notice and Document License (2015-05-13)"]
              :id-formats      ["W3C-${VER}" "W3C"]
              :name-formats    ["W3C Software Notice and License (${VER})" "W3C Software Notice and Document License (${VER})"]}
             (get version-series "W3C"))))
    (testing "Version series - full date + suffix")  ; DOESN'T EXIST AS OF SPDX LICENSE LIST v3.27.0
    (testing "Irregular version series - these have multiple ids and/or names"
      (let [irregular-version-series-ids (set/union (set (keys (irregular-ids))) (set (keys (irregular-names))))
            irregular-version-series     (map (partial get version-series) irregular-version-series-ids)]
        (run! #(is (let [num-id-formats   (count (:id-formats %))
                         num-name-formats (count (:name-formats %))]
                     (or (>= num-id-formats 2)
                         (>= num-name-formats 2)))
                   (str "Irregular version series " % " is not irregular!"))
              irregular-version-series)))
    (testing "Corner cases (for various different reasons)"
      ; LZMA-SDK has both irregular ids and names
      (is (= {:series-id       "LZMA-SDK"
              :default-id      "LZMA-SDK-9.22"
              :version-type    :semver
              :version-suffix? false
              :versions        ["9.11" "9.22"]
              :ids             ["LZMA-SDK-9.11-to-9.20" "LZMA-SDK-9.22"]
              :names           ["LZMA SDK License (versions 9.11 to 9.20)" "LZMA SDK License (versions 9.22 and beyond)"]
              :id-formats      ["LZMA-SDK-${VER}-to-9.20" "LZMA-SDK-${VER}"]
              :name-formats    ["LZMA SDK License (${VER} to 9.20)" "LZMA SDK License (${VER} and beyond)"]}
             (get version-series "LZMA-SDK")))
      ; libpng has both irregular ids and names
      (is (= {:series-id       "libpng"
              :default-id      "libpng-2.0"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.6.35" "2.0"]
              :ids             ["Libpng" "libpng-1.6.35" "libpng-2.0"]
              :names           ["libpng License" "PNG Reference Library License v1 (for libpng 0.5 through 1.6.35)" "PNG Reference Library version 2"]
              :id-formats      ["Libpng" "libpng-${VER}"]
              :name-formats    ["libpng License" "PNG Reference Library License v1 (for libpng 0.5 through ${VER})" "PNG Reference Library ${VER}"]}
             (get version-series "libpng")))
      ; FSL/ALv2 has a version number in the middle of the id and the name
      (is (= {:series-id       "FSL/ALv2"
              :default-id      "FSL-1.1-ALv2"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.1"]
              :ids             ["FSL-1.1-ALv2"]
              :names           ["Functional Source License, Version 1.1, ALv2 Future License"]
              :id-formats      ["FSL-${VER}-ALv2"]
              :name-formats    ["Functional Source License ${VER}, ALv2 Future License"]}
             (get version-series "FSL/ALv2")))
      ; FSL/MIT has a version number in the middle of the id and the name
      (is (= {:series-id       "FSL/MIT"
              :default-id      "FSL-1.1-MIT"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.1"]
              :ids             ["FSL-1.1-MIT"]
              :names           ["Functional Source License, Version 1.1, MIT Future License"]
              :id-formats      ["FSL-${VER}-MIT"]
              :name-formats    ["Functional Source License ${VER}, MIT Future License"]}
             (get version-series "FSL/MIT")))
      ; Sun-PPP has a 4 digit year version number in parentheses in the name
      (is (= {:series-id       "Sun-PPP"
              :default-id      "Sun-PPP-2000"
              :version-type    :year4
              :version-suffix? false
              :versions        ["2000"]
              :ids             ["Sun-PPP-2000"]
              :names           ["Sun PPP License (2000)"]
              :id-formats      ["Sun-PPP-${VER}"]
              :name-formats    ["Sun PPP License (${VER})"]}
             (get version-series "Sun-PPP")))
      ; OLDAP his irregular names
      (is (= {:series-id       "OLDAP"
              :default-id      "OLDAP-2.8"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.1" "1.2" "1.3" "1.4" "2.0" "2.0.1" "2.1" "2.2" "2.2.1" "2.2.2" "2.3" "2.4" "2.5" "2.6" "2.7" "2.8"]
              :ids             ["OLDAP-1.1" "OLDAP-1.2" "OLDAP-1.3" "OLDAP-1.4" "OLDAP-2.0" "OLDAP-2.0.1" "OLDAP-2.1" "OLDAP-2.2" "OLDAP-2.2.1" "OLDAP-2.2.2" "OLDAP-2.3" "OLDAP-2.4" "OLDAP-2.5" "OLDAP-2.6" "OLDAP-2.7" "OLDAP-2.8"]
              :names           ["Open LDAP Public License v1.1" "Open LDAP Public License v1.2" "Open LDAP Public License v1.3" "Open LDAP Public License v1.4" "Open LDAP Public License v2.0 (or possibly 2.0A and 2.0B)" "Open LDAP Public License v2.0.1" "Open LDAP Public License v2.1" "Open LDAP Public License v2.2" "Open LDAP Public License v2.2.1" "Open LDAP Public License 2.2.2" "Open LDAP Public License v2.3" "Open LDAP Public License v2.4" "Open LDAP Public License v2.5" "Open LDAP Public License v2.6" "Open LDAP Public License v2.7" "Open LDAP Public License v2.8"]
              :id-formats      ["OLDAP-${VER}"]
              :name-formats    ["Open LDAP Public License ${VER}" "Open LDAP Public License ${VER} (or possibly 2.0A and 2.0B)"]}
             (get version-series "OLDAP")))
      ; GPL has a complex set of identifiers, and an unusual default identifier
      (is (= {:series-id       "GPL"
              :default-id      "GPL-1.0-or-later"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0" "2.0" "3.0"]
              :ids             ["GPL-1.0-or-later" "GPL-1.0+" "GPL-1.0" "GPL-1.0-only" "GPL-2.0-or-later" "GPL-2.0+" "GPL-2.0" "GPL-2.0-only" "GPL-3.0-or-later" "GPL-3.0+" "GPL-3.0" "GPL-3.0-only"]
              :names           ["GNU General Public License v1.0 or later" "GNU General Public License v1.0 or later" "GNU General Public License v1.0 only" "GNU General Public License v1.0 only" "GNU General Public License v2.0 or later" "GNU General Public License v2.0 or later" "GNU General Public License v2.0 only" "GNU General Public License v2.0 only" "GNU General Public License v3.0 or later" "GNU General Public License v3.0 or later" "GNU General Public License v3.0 only" "GNU General Public License v3.0 only"]
              :id-formats      ["GPL-${VER}"]
              :name-formats    ["GNU General Public License ${VER}"]}
             (get version-series "GPL")))
      ; SHL has both license and exception identifiers
      (is (= {:series-id       "SHL"
              :default-id      "SHL-2.1"
              :version-type    :semver
              :version-suffix? false
              :versions        ["0.5" "0.51" "2.0" "2.1"]
              :ids             ["SHL-0.5" "SHL-0.51" "SHL-2.0" "SHL-2.1"]
              :names           ["Solderpad Hardware License v0.5" "Solderpad Hardware License, Version 0.51" "Solderpad Hardware License v2.0" "Solderpad Hardware License v2.1"]
              :id-formats      ["SHL-${VER}"]
              :name-formats    ["Solderpad Hardware License ${VER}"]}
             (get version-series "SHL")))
      ; SSPL has an unusually formatted version in the name
      (is (= {:series-id       "SSPL"
              :default-id      "SSPL-1.0"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0"]
              :ids             ["SSPL-1.0"]
              :names           ["Server Side Public License, v 1"]
              :id-formats      ["SSPL-${VER}"]
              :name-formats    ["Server Side Public License ${VER}"]}
             (get version-series "SSPL")))
      ; TORQUE has a separate version number in the name, unrelated to the license's version
      (is (= {:series-id       "TORQUE"
              :default-id      "TORQUE-1.1"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.1"]
              :ids             ["TORQUE-1.1"]
              :names           ["TORQUE v2.5+ Software License v1.1"]
              :id-formats      ["TORQUE-${VER}"]
              :name-formats    ["TORQUE v2.5+ Software License ${VER}"]}
             (get version-series "TORQUE")))
      ; Risk of this exception being included in the GPL version series
      (is (= {:series-id       "GPL-CC"
              :default-id      "GPL-CC-1.0"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0"]
              :ids             ["GPL-CC-1.0"]
              :names           ["GPL Cooperation Commitment 1.0"]
              :id-formats      ["GPL-CC-${VER}"]
              :name-formats    ["GPL Cooperation Commitment ${VER}"]}
             (get version-series "GPL-CC")))
      (is (nil? (get version-series "Linux-syscall-note")))   ;####TODO: What's special about this one?
      ; Risk of this exception being included in the LGPL version series
      (is (nil? (get version-series "LLGPL")))
      ; Unusually formatted version number
      (is (= {:series-id       "PHP"
              :default-id      "PHP-3.01"
              :version-type    :semver
              :version-suffix? false
              :versions        ["3.0" "3.01"]
              :ids             ["PHP-3.0" "PHP-3.01"]
              :names           ["PHP License v3.0" "PHP License v3.01"]
              :id-formats      ["PHP-${VER}"]
              :name-formats    ["PHP License ${VER}"]}
             (get version-series "PHP")))
      ; Identifier with 2 version-like numeric values in it
      (is (= {:series-id       "QPL/INRIA-2004"
              :default-id      "QPL-1.0-INRIA-2004"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0"]
              :ids             ["QPL-1.0-INRIA-2004"]
              :names           ["Q Public License 1.0 - INRIA 2004 variant"]
              :id-formats      ["QPL-${VER}-INRIA-2004"]
              :name-formats    ["Q Public License ${VER} - INRIA 2004 variant"]}
             (get version-series "QPL/INRIA-2004")))
      ; Special cased "AGPL"
      (is (= {:series-id       "Affero"
              :default-id      "AGPL-1.0-only"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0"]
              :ids             ["AGPL-1.0-or-later" "AGPL-1.0" "AGPL-1.0-only"]
              :names           ["Affero General Public License v1.0 or later" "Affero General Public License v1.0" "Affero General Public License v1.0 only"]
              :id-formats      ["AGPL-${VER}"]
              :name-formats    ["Affero General Public License ${VER}"]}
             (get version-series "Affero")))
      ; Vanilla AGPL
      (is (= {:series-id       "AGPL"
              :default-id      "AGPL-3.0-or-later"
              :version-type    :semver
              :version-suffix? false
              :versions        ["3.0"]
              :ids             ["AGPL-3.0-or-later" "AGPL-3.0" "AGPL-3.0-only"]
              :names           ["GNU Affero General Public License v3.0 or later" "GNU Affero General Public License v3.0" "GNU Affero General Public License v3.0 only"]
              :id-formats      ["AGPL-${VER}"]
              :name-formats    ["GNU Affero General Public License ${VER}"]}
             (get version-series "AGPL")))
      ; Identifier with 2 version-like numeric values in it
      (is (= {:series-id       "CC0"
              :default-id      "CC0-1.0"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.0"]
              :ids             ["CC0-1.0"]
              :names           ["Creative Commons Zero v1.0 Universal"]
              :id-formats      ["CC0-${VER}"]
              :name-formats    ["Creative Commons Zero ${VER} Universal"]}
             (get version-series "CC0")))
      ; Identifiers with only or or-later suffix in a non-standard location
      (is (= {:series-id       "GFDL/invariants"
              :default-id      "GFDL-1.1-invariants-or-later"
              :version-type    :semver
              :version-suffix? false
              :versions        ["1.1" "1.2" "1.3"]
              :ids             ["GFDL-1.1-invariants-or-later" "GFDL-1.1-invariants-only" "GFDL-1.2-invariants-or-later" "GFDL-1.2-invariants-only" "GFDL-1.3-invariants-or-later" "GFDL-1.3-invariants-only"]
              :names           ["GNU Free Documentation License v1.1 or later - invariants" "GNU Free Documentation License v1.1 only - invariants" "GNU Free Documentation License v1.2 or later - invariants" "GNU Free Documentation License v1.2 only - invariants" "GNU Free Documentation License v1.3 or later - invariants" "GNU Free Documentation License v1.3 only - invariants"]
              :id-formats      ["GFDL-${VER}-invariants-${OOOL}"]
              :name-formats    ["GNU Free Documentation License ${VER} - invariants"]}
             (get version-series "GFDL/invariants"))))))
