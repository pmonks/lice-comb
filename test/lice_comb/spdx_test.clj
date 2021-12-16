;
; Copyright © 2021 Peter Monks
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

(ns lice-comb.spdx-test
  (:require [clojure.test    :refer [deftest testing is]]
            [clojure.java.io :as io]
            [lice-comb.spdx  :refer [name->ids uri->id text->ids]]))

; Note: these tests should be extended indefinitely, as it exercises the guts of the matching algorithm
(deftest name->ids-tests
  (testing "Nil, empty or blank names"
    (is (nil?                                   (name->ids nil)))
    (is (nil?                                   (name->ids "")))
    (is (nil?                                   (name->ids "       ")))
    (is (nil?                                   (name->ids "\n")))
    (is (nil?                                   (name->ids "\t"))))
  (testing "Names that are SPDX license ids"
    (is (= #{"Apache-2.0"}                       (name->ids "Apache-2.0")))
    (is (= #{"Apache-2.0"}                       (name->ids "    Apache-2.0        ")))   ; Test whitespace
    (is (= #{"GPL-2.0"}                          (name->ids "GPL-2.0")))
    (is (= #{"GPL-2.0-with-classpath-exception"} (name->ids "GPL-2.0-with-classpath-exception")))
    (is (= #{"AGPL-3.0"}                         (name->ids "AGPL-3.0")))
    (is (= #{"AGPL-3.0-only"}                    (name->ids "AGPL-3.0-only")))
    (is (= #{"CC-BY-SA-4.0"}                     (name->ids "CC-BY-SA-4.0"))))
  (testing "Names that appear verbatim in the SPDX license list"
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License, Version 2.0")))
    (is (= #{"Apache-2.0"}                       (name->ids "               Apache Software License, Version 2.0             ")))   ; Test whitespace
    (is (= #{"AGPL-3.0"}                         (name->ids "GNU Affero General Public License v3.0")))
    (is (= #{"AGPL-3.0-only"}                    (name->ids "GNU Affero General Public License v3.0 only")))
    (is (= #{"CC-BY-SA-4.0"}                     (name->ids "Creative Commons Attribution Share Alike 4.0 International")))
    (is (= #{"GPL-2.0-with-classpath-exception"} (name->ids "GNU General Public License v2.0 w/Classpath exception")))
    (is (= #{"Apache-1.0"}                       (name->ids "Apache Software License"))))
  (testing "Names that appear in licensey things, but aren't in the SPDX license list verbatim"
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License v2.0")))
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License 2.0")))
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License Version 2.0")))
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License v2")))
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License 2")))
    (is (= #{"Apache-2.0"}                       (name->ids "Apache Software License Version 2")))
    (is (= #{"LGPL-2.0"}                         (name->ids "GNU Library General Public License")))
    (is (= #{"LGPL-2.1"}                         (name->ids "GNU Lesser General Public License")))
    (is (= #{"CDDL-1.0"}                         (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE (CDDL) Version 1.0")))
    (is (= #{"MIT"}                              (name->ids "The MIT License")))
    (is (= #{"MIT"}                              (name->ids "MIT License")))
    (is (= #{"MPL-2.0"}                          (name->ids "Mozilla Public License Version 2.0")))
    (is (= #{"CC-BY-3.0"}                        (name->ids "Creative Commons Legal Code Attribution 3.0 Unported")))
    (is (= #{"CC-BY-3.0"}                        (name->ids "Attribution 3.0 Unported")))
    (is (= #{"CC-BY-4.0"}                        (name->ids "Attribution 4.0 International"))))
  (testing "Names that appear in licensey things, but are ambiguous"
    (is (nil?                                    (name->ids "BSD"))))
  (testing "Names that appear in licensey things, but aren't in the SPDX license list, and don't have identified SPDX identifiers"
    (is (= #{"NON-SPDX-JDOM"}                    (name->ids "Similar to Apache License but with the acknowledgment clause removed")))
    (is (= #{"NON-SPDX-Public-Domain"}           (name->ids "Public Domain")))
    (is (= #{"NON-SPDX-Public-Domain"}           (name->ids "Public domain")))))

(deftest uri->id-tests
  (testing "Nil, empty or blank uri"
    (is (nil?                                 (uri->id nil)))
    (is (nil?                                 (uri->id "")))
    (is (nil?                                 (uri->id "       ")))
    (is (nil?                                 (uri->id "\n")))
    (is (nil?                                 (uri->id "\t"))))
  (testing "URIs that appear verbatim in the SPDX license list"
    (is (= "Apache-2.0"                       (uri->id "https://www.apache.org/licenses/LICENSE-2.0")))
    (is (= "Apache-2.0"                       (uri->id "               https://www.apache.org/licenses/LICENSE-2.0             ")))   ; Test whitespace
    (is (= "AGPL-3.0"                         (uri->id "https://www.gnu.org/licenses/agpl.txt")))
    (is (= "CC-BY-SA-4.0"                     (uri->id "https://creativecommons.org/licenses/by-sa/4.0/legalcode")))
    (is (= "GPL-2.0-with-classpath-exception" (uri->id "https://www.gnu.org/software/classpath/license.html"))))
  (testing "URIs that appear in licensey things, but aren't in the SPDX license list"
    (is (= "Apache-2.0"                       (uri->id "http://www.apache.org/licenses/LICENSE-2.0")))
    (is (= "Apache-2.0"                       (uri->id "https://www.apache.org/licenses/LICENSE-2.0.txt")))))

(defn- string-text->ids
  [s]
  (with-open [is (io/input-stream (.getBytes s "UTF-8"))]
    (text->ids is)))

(deftest text->ids-tests
  (testing "Nil, empty or blank text"
    (is (nil?                                  (text->ids nil)))
    (is (thrown? java.io.FileNotFoundException (text->ids "")))
    (is (thrown? java.io.FileNotFoundException (text->ids "       ")))
    (is (thrown? java.io.FileNotFoundException (text->ids "\n")))
    (is (thrown? java.io.FileNotFoundException (text->ids "\t"))))
  (testing "Text"
    (is (= #{"Apache-2.0"}   (string-text->ids "Apache License\nVersion 2.0, January 2004")))
    (is (= #{"Apache-2.0"}   (string-text->ids "               Apache License\n               Version 2.0, January 2004             ")))
    (is (= #{"AGPL-3.0"}     (string-text->ids "GNU AFFERO GENERAL PUBLIC LICENSE\nVersion 3, 19 November 2007")))
    (is (= #{"CC-BY-SA-4.0"} (string-text->ids "Creative Commons Attribution-ShareAlike\n4.0 International Public License")))))
