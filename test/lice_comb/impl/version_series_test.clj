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
            [clojure.pprint                :as pp]
            [lice-comb.test-boilerplate    :refer [fixture lic-ids-d exc-ids-d]]
            [lice-comb.impl.version-series :refer [version-series-member? id->version-series ids->version-series version-series]]))

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
    (is (= "GPL"                      (id->version-series "GPL-3.0-only")))
    (is (= "GPL"                      (id->version-series "GPL-1.0-or-later")))
    (is (= "GPL"                      (id->version-series "GPL-1.0+")))
    (is (= "GFDL/invariants"          (id->version-series "GFDL-1.3-invariants-only")))
    (is (= "GFDL/no-invariants"       (id->version-series "GFDL-1.3-no-invariants-or-later")))
    (is (= "CC-BY"                    (id->version-series "CC-BY-1.0")))
    (is (= "CC-BY/AU"                 (id->version-series "CC-BY-2.5-AU")))
    (is (= "CC-BY"                    (id->version-series "CC-BY-4.0")))
    (is (= "CC-BY-SA/UK"              (id->version-series "CC-BY-SA-2.0-UK")))
    (is (= "CC-BY-SA/JP"              (id->version-series "CC-BY-SA-2.1-JP")))
    (is (= "CC-BY-SA/IGO"             (id->version-series "CC-BY-SA-3.0-IGO")))
    (is (= "Artistic"                 (id->version-series "Artistic-1.0")))
    (is (= "Artistic/cl8"             (id->version-series "Artistic-1.0-cl8")))
    (is (= "Artistic/Perl"            (id->version-series "Artistic-1.0-Perl")))
    (is (= "GCC-exception"            (id->version-series "GCC-exception-2.0")))
    (is (= "GCC-exception/note"       (id->version-series "GCC-exception-2.0-note")))
    (is (= "GCC-exception"            (id->version-series "GCC-exception-3.1"))))
  (testing "Ids in a version series - weird cases"
    (is (= "CECILL"         (id->version-series "CECILL-1.0")))
    (is (= "CECILL"         (id->version-series "CECILL-2.1")))
    (is (nil?               (id->version-series "CECILL-B")))
    (is (nil?               (id->version-series "LZMA-SDK-9.11-to-9.20")))
    (is (nil?               (id->version-series "LZMA-SDK-9.22")))
    (is (= "QPL/INRIA-2004" (id->version-series "QPL-1.0-INRIA-2004")))
    (is (= "QPL/INRIA-2004-exception" (id->version-series "QPL-1.0-INRIA-2004-exception")))))

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

(defn assert-singleton-vs
  "Asserts that the given version-series fn return contains a single version
  series map, and then returns it."
  [vs]
  ; Tuple is as expected
  (is (= 2  (count vs)))
  (is (nil? (second vs)))
  ; Version series sequence is a singleton and that singleton is a map
  (is (= 1 (count (first vs))))
  (let [result (first (first vs))]
    (is (map? result))
    result))

(deftest version-series-tests
  (testing "Nil, empty, etc."
    (is (nil? (version-series nil)))
    (is (nil? (version-series []))))
  (testing "Basic checking - handpicked subsets of ids"  ; Should probably learn spec and use it for this tbqh...
    (let [vs (version-series ["MIT"])]
      (is (= 2 (count vs)))
      (is (nil? (first vs)))
      (is (= ["MIT"] (second vs))))
    (let [vs (version-series ["MIT-0"])]
      (is (= 2 (count vs)))
      (is (nil? (first vs)))
      (is (= ["MIT-0"] (second vs))))
    (let [vs (version-series (shuffle ["0BSD" "BSD-4.3RENO" "BSD-4.3TAHOE"]))]
      (is (= 2 (count vs)))
      (is (nil? (first vs)))
      (is (= #{"0BSD" "BSD-4.3RENO" "BSD-4.3TAHOE"} (set (second vs)))))
    (let [vs (version-series (shuffle ["Apache-1.0" "Apache-1.1" "Apache-2.0"]))]
      (is (= 2 (count vs)))
      (is (not (nil? (first vs))))
      (is (nil? (second vs)))))
  (testing "Basic checking - all ids"
    (let [all-ids (shuffle (concat @lic-ids-d @exc-ids-d))  ; We shuffle here, since input ordering shouldn't change the result
          vs      (version-series all-ids)]
      (is (= 2 (count vs)))
      (is (sequential? (first vs)))
      (is (sequential? (second vs)))
      (run! #(is (map? %))    (first vs))
      (run! #(is (string? %)) (second vs))
      (run! #(is (= #{:id :versions :default-id :ids :id-template :name-template} (set (keys %)))) (first vs))))
  (testing "Detailed return values"
    (let [vsm (assert-singleton-vs (version-series (shuffle ["Apache-1.0" "Apache-1.1" "Apache-2.0"])))]
      (is (= {:id            "Apache"
              :versions      ["1.0" "1.1" "2.0"]
              :default-id    "Apache-2.0"
              :ids           ["Apache-1.0" "Apache-1.1" "Apache-2.0"]
              :id-template   "Apache-$VER"
              :name-template "Apache License $VER"}
             vsm)))
    ; Has "v" version number prefix in name
    (let [vsm (assert-singleton-vs (version-series (shuffle ["BitTorrent-1.0" "BitTorrent-1.1"])))]
      (is (= {:id             "BitTorrent"
               :versions      ["1.0" "1.1"]
               :default-id    "BitTorrent-1.1"
               :ids           ["BitTorrent-1.0" "BitTorrent-1.1"]
               :id-template   "BitTorrent-$VER"
               :name-template "BitTorrent Open Source License $VER"}
             vsm)))
    ; Has "Version " version number prefix in name
    (let [vsm (assert-singleton-vs (version-series (shuffle ["DL-DE-BY-2.0"])))]
      (is (= {:id             "DL-DE-BY"
               :versions      ["2.0"]
               :default-id    "DL-DE-BY-2.0"
               :ids           ["DL-DE-BY-2.0"]
               :id-template   "DL-DE-BY-$VER"
               :name-template "Data licence Germany – attribution – $VER"}
             vsm)))
    ; Has a different version number representation in the name than in the id
    (let [vsm (assert-singleton-vs (version-series (shuffle ["CERN-OHL-P-2.0"])))]
      (is (= {:id             "CERN-OHL-P"
               :versions      ["2.0"]
               :default-id    "CERN-OHL-P-2.0"
               :ids           ["CERN-OHL-P-2.0"]
               :id-template   "CERN-OHL-P-$VER"
               :name-template "CERN Open Hardware Licence $VER - Permissive"}
             vsm)))
    ; Has "Version " version number prefix in name, and version number in names have fewer components than in id
    (let [vsm (assert-singleton-vs (version-series (shuffle ["MulanPSL-1.0" "MulanPSL-2.0"])))]
      (is (= {:id             "MulanPSL"
               :versions      ["1.0" "2.0"]
               :default-id    "MulanPSL-2.0"
               :ids           ["MulanPSL-1.0" "MulanPSL-2.0"]
               :id-template   "MulanPSL-$VER"
               :name-template "Mulan Permissive Software License, $VER"}
             vsm)))
    ; Full 3 component SemVer
    (let [vsm (assert-singleton-vs (version-series (shuffle ["copyleft-next-0.3.0" "copyleft-next-0.3.1"])))]
      (is (= {:id             "copyleft-next"
               :versions      ["0.3.0" "0.3.1"]
               :default-id    "copyleft-next-0.3.1"
               :ids           ["copyleft-next-0.3.0" "copyleft-next-0.3.1"]
               :id-template   "copyleft-next-$VER"
               :name-template "copyleft-next $VER"}
             vsm)))
    ; Has single letter suffix in (some) version numbers
    (let [vsm (assert-singleton-vs (version-series (shuffle ["LPPL-1.0" "LPPL-1.1" "LPPL-1.2" "LPPL-1.3a" "LPPL-1.3c"])))]
      (is (= {:id             "LPPL"
               :versions      ["1.0" "1.1" "1.2" "1.3a" "1.3c"]
               :default-id    "LPPL-1.3c"
               :ids           ["LPPL-1.0" "LPPL-1.1" "LPPL-1.2" "LPPL-1.3a" "LPPL-1.3c"]
               :id-template   "LPPL-$VER"
               :name-template "LaTeX Project Public License $VER"}
             vsm)))
;###TODO!!!!
; Close version numbers
; ["SHL-0.5" "SHL-0.51"]

; 2 digit year
; ["Spencer-86" "Spencer-94" "Spencer-99"]

; 4 digit year
; ["HP-1986" "HP-1989"]                      ; year in name not in parens
; ["Unicode-DFS-2015" "Unicode-DFS-2016"]    ; year in name in parens

; 8 digit full date
; ["W3C-19980720" "W3C-20150513"]

; Mix of version formats
; ["libpng-1.6.35" "libpng-2.0"]

;####TODO: OTHER NASTY CORNER CASES
; ["XFree86-1.1"]               ; Number as part of version series id
; ["QPL-1.0-INRIA-2004"]        ; Double version numbers of difference types in id and name
; ["libpng-1.6.35"]             ; Multiple version numbers in name
; ["SAX-PD" "SAX-PD-2.0"]       ; A logical series, but first version is missing version number
; ["Xdebug-1.03"]               ; Space in name between version label and version number
; ["NBPL-1.0"]                  ; Version number in name has fewer components than id
; ["SSPL-1.0"]                  ; Space in name between version label and version number, and version number in name has fewer components than id
; ["OGL-Canada-2.0"]            ; id has version number (semver), name does not
; ["Adobe-2006"]                ; id has version number (year4), name does not
; ["SNIA"]                      ; Name has version number, id does not
; ["PHP-3.0" "PHP-3.01"]        ; Leading zero in one of the version numbers
; ["OLDAP-2.0"]                 ; Multiple version numbers in name


    ; Complicated suffix variations, and or/or-later suffix separated from version number
;####TODO: ADD OTHER EXAMPLES OF THIS, SUCH AS:
; ["Unicode-3.0" "Unicode-DFS-2015" "Unicode-DFS-2016" "Unicode-TOU"]
; ["FSL-1.1-ALv2" "FSL-1.1-MIT"]
    (let [vs (version-series (shuffle ["GFDL-1.1"
                                       "GFDL-1.1-invariants-only"
                                       "GFDL-1.1-invariants-or-later"
                                       "GFDL-1.1-no-invariants-only"
                                       "GFDL-1.1-no-invariants-or-later"
                                       "GFDL-1.1-only"
                                       "GFDL-1.1-or-later"
                                       "GFDL-1.2"
                                       "GFDL-1.2-invariants-only"
                                       "GFDL-1.2-invariants-or-later"
                                       "GFDL-1.2-no-invariants-only"
                                       "GFDL-1.2-no-invariants-or-later"
                                       "GFDL-1.2-only"
                                       "GFDL-1.2-or-later"
                                       "GFDL-1.3"
                                       "GFDL-1.3-invariants-only"
                                       "GFDL-1.3-invariants-or-later"
                                       "GFDL-1.3-no-invariants-only"
                                       "GFDL-1.3-no-invariants-or-later"
                                       "GFDL-1.3-only"
                                       "GFDL-1.3-or-later"]))]
      (is (= 2 (count vs)))
      (is (nil? (second vs)))
      (is (= 3 (count (first vs))))  ; GFDL, GFDL/invariants, GFDL/no-invariants
    ;####TODO: implement test
;(println "⭐️⭐️⭐️ vs:") (pp/pprint vs)
    ))
  (testing "Id templates"
    (is (= "Apache-$VER"      (:id-template (assert-singleton-vs (version-series (shuffle ["Apache-1.0" "Apache-1.1" "Apache-2.0"]))))))
    (is (= "Spencer-$VER"     (:id-template (assert-singleton-vs (version-series (shuffle ["Spencer-86" "Spencer-94" "Spencer-99"]))))))
    (is (= "Unicode-DFS-$VER" (:id-template (assert-singleton-vs (version-series (shuffle ["Unicode-DFS-2015" "Unicode-DFS-2016"]))))))
    (is (= "GPL-$VER-$OOOL"   (:id-template (assert-singleton-vs (version-series (shuffle ["GPL-1.0-only" "GPL-1.0-or-later" "GPL-2.0+" "GPL-3.0"]))))))
  )
  (testing "Name templates"
    (is (= "Apache License $VER"                                        (:name-template (assert-singleton-vs (version-series (shuffle ["Apache-1.0" "Apache-1.1" "Apache-2.0"]))))))
    (is (= "Spencer License $VER"                                       (:name-template (assert-singleton-vs (version-series (shuffle ["Spencer-86" "Spencer-94" "Spencer-99"]))))))
    (is (= "Unicode License Agreement - Data Files and Software ($VER)" (:name-template (assert-singleton-vs (version-series (shuffle ["Unicode-DFS-2015" "Unicode-DFS-2016"]))))))
  )
)


