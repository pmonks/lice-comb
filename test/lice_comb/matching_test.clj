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

(ns lice-comb.matching-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate :refer [fixture]]
            [lice-comb.matching         :refer [unlisted? name->unlisted text->ids name->ids uri->ids]]
            [spdx.licenses              :as sl]
            [spdx.exceptions            :as se]))

(use-fixtures :once fixture)

(defn unlisted-only?
  "Does the given set of ids contain only a single unlisted license?"
  [ids]
  (and (= 1 (count ids))
       (unlisted? (first ids))))

(deftest unlisted?-tests
  (testing "Nil, empty or blank ids"
    (is (nil?   (unlisted? nil)))
    (is (false? (unlisted? "")))
    (is (false? (unlisted? "       ")))
    (is (false? (unlisted? "\n")))
    (is (false? (unlisted? "\t"))))
  (testing "Unlisted ids"
    (is (true?  (unlisted? (name->unlisted "foo")))))
  (testing "Listed ids"
    (is (true?  (every? false? (map unlisted? (sl/ids)))))
    (is (true?  (every? false? (map unlisted? (se/ids)))))))

; Note: these tests should be extended indefinitely, as it exercises the most-utilised part of the library (matching license names found in POMs)
(deftest name->ids-tests
  (testing "Nil, empty or blank names"
    (is (nil?                                      (name->ids nil)))
    (is (nil?                                      (name->ids "")))
    (is (nil?                                      (name->ids "       ")))
    (is (nil?                                      (name->ids "\n")))
    (is (nil?                                      (name->ids "\t"))))
  (testing "Names that are SPDX license ids"
    (is (= #{"AGPL-3.0-only"}                      (name->ids "AGPL-3.0")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "AGPL-3.0-only")))
    (is (= #{"Apache-2.0"}                         (name->ids "    Apache-2.0        ")))   ; Test whitespace
    (is (= #{"Apache-2.0"}                         (name->ids "Apache-2.0")))
    (is (= #{"CC-BY-SA-4.0"}                       (name->ids "CC-BY-SA-4.0")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GPL-2.0")))
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GPL-2.0-with-classpath-exception"))))
  (testing "Names that are SPDX expressions"
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GPL-2.0 WITH Classpath-exception-2.0")))
    (is (= #{"Apache-2.0" "GPL-3.0-only"}          (name->ids "Apache-2.0 OR GPL-3.0")))
    (is (= #{"EPL-2.0" "GPL-2.0-or-later" "Classpath-exception-2.0" "MIT" "BSD-3-Clause" "Apache-2.0"}
                                                   (name->ids "EPL-2.0 OR (GPL-2.0+ WITH Classpath-exception-2.0) OR MIT OR (BSD-3-Clause AND Apache-2.0)"))))
(comment  ; ####TODO: RE-ENABLE ME!!!!
  (testing "Names, with an emphasis on those seen in POMs on Maven Central"
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License (AGPL) version 3.0")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License v3.0")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License v3.0 only")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache Software License")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache License 1")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache License 1.0")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache License Version 1.0")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache License, Version 1.0")))
    (is (= #{"Apache-1.0"}                         (name->ids "Apache Software License - Version 1.0")))
    (is (= #{"Apache-1.1"}                         (name->ids "Apache License 1.1")))
    (is (= #{"Apache-1.1"}                         (name->ids "Apache License Version 1.1")))
    (is (= #{"Apache-1.1"}                         (name->ids "Apache License, Version 1.1")))
    (is (= #{"Apache-1.1"}                         (name->ids "Apache Software License - Version 1.1")))
    (is (= #{"Apache-1.1"}                         (name->ids "The MX4J License, version 1.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "               Apache Software License, Version 2.0             ")))   ; Test whitespace
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License - Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License Version 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License v2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License v2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache v2")))
    (is (= #{"Apache-2.0"}                         (name->ids "The Apache Software License, Version 2.0")))
    (is (= #{"MIT"}                                (name->ids "Bouncy Castle Licence")))  ; Note spelling of "licence"
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-Clause BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "The BSD 3-Clause License (BSD3)")))
    (is (= #{"BSD-3-Clause-Attribution"}           (name->ids "BSD 3-Clause Attribution")))
    (is (= #{"CC-BY-3.0"}                          (name->ids "Attribution 3.0 Unported")))
    (is (= #{"CC-BY-3.0"}                          (name->ids "Creative Commons Legal Code Attribution 3.0 Unported")))
    (is (= #{"CC-BY-4.0"}                          (name->ids "Attribution 4.0 International")))
    (is (= #{"CC-BY-SA-4.0"}                       (name->ids "Creative Commons Attribution Share Alike 4.0 International")))
    (is (= #{"CDDL-1.0"}                           (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE Version 1")))
    (is (= #{"CDDL-1.0"}                           (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE Version 1.0")))
    (is (= #{"CDDL-1.0"}                           (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE (CDDL) Version 1.0")))
    (is (= #{"CDDL-1.1"}                           (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE Version 1.1")))
    (is (= #{"CDDL-1.1"}                           (name->ids "COMMON DEVELOPMENT AND DISTRIBUTION LICENSE (CDDL) Version 1.1")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License (EPL)")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License - v 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License, Version 1.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License 2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License version 2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU General Public License, version 2")))
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GNU General Public License, version 2 (GPL2), with the classpath exception")))
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GNU General Public License, version 2 with the GNU Classpath Exception")))
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GNU General Public License v2.0 w/Classpath exception")))
    (is (= #{"JSON"}                               (name->ids "JSON License")))
    (is (= #{"LGPL-2.0-only"}                      (name->ids "GNU Library General Public License")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Public License (LGPL)")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Public License")))
    (is (= #{"MIT"}                                (name->ids "MIT License")))
    (is (= #{"MIT"}                                (name->ids "MIT license")))     ; Test capitalisation
    (is (= #{"MIT"}                                (name->ids "The MIT License")))
    (is (= #{"MPL-1.0"}                            (name->ids "Mozilla Public License")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License Version 2.0")))
    (is (= #{"Plexus"}                             (name->ids "Similar to Apache License but with the acknowledgment clause removed"))))   ; JDOM - see https://lists.linuxfoundation.org/pipermail/spdx-legal/2014-December/001280.html
  (testing "Names that appear in licensey things, but are ambiguous"
    (is (nil?                                      (name->ids "BSD"))))
  (testing "Names that appear in licensey things, but aren't in the SPDX license list"
    (is (= #{"LicenseRef-lice-comb-PUBLIC-DOMAIN"} (name->ids "Public Domain")))
    (is (= #{"LicenseRef-lice-comb-PUBLIC-DOMAIN"} (name->ids "Public domain"))))
)
  (testing "Distinct license names that appear in POMs on Clojars"   ; synced from Clojars 2023-07-13
;####TODO: SORT ALL OF THESE!!!!
    (is (= #{"AFL-3.0"}                            (name->ids "Academic Free License 3.0")))
(comment  ;####TODO: UNCOMMENT THIS!!!!
    (is (= #{"AGPL-3.0-only"}                      (name->ids "AGPL v3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "AGPLv3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "Affero GNU Public License v3")))  ; Listed license missing version - we assume the latest
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU AFFERO GENERAL PUBLIC LICENSE Version 3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU AFFERO GENERAL PUBLIC LICENSE, Version 3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU AGPLv3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License 3.0 (AGPL-3.0)")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License Version 3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License Version 3; Other commercial licenses available.")))  ; ####TODO: THINK MORE ABOUT THIS ONE!!!
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License v3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License v3.0")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License, Version 3")))
    (is (= #{"AGPL-3.0-only"}                      (name->ids "GNU Affero General Public License, version 3")))
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "AGPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "Affero General Public License v3 or later (at your option)")))
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "Affero General Public License version 3 or lator")))
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "Affero General Public License")))
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "Affero General Public License,")))  ; Listed license missing version - we assume the latest
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "GNU AGPL-V3 or later")))
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "GNU Affero General Public Licence")))  ; Listed license missing version - we assume the latest
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "GNU Affero General Public License (AGPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"AGPL-3.0-or-later"}                  (name->ids "GNU Affero General Public License")))  ; Listed license missing version - we assume the latest
)
    (is (= #{"Apache-2.0" "EPL-2.0"}               (name->ids "Double licensed under the Eclipse Public License (the same as Clojure) or the Apache Public License 2.0.")))  ; Listed license missing version - we assume the latest
    (is (= #{"Apache-2.0" "LLVM-exception"}        (name->ids "Apache 2.0 with LLVM Exception")))
    (is (= #{"Apache-2.0"}                         (name->ids " Apache License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "APACHE LICENSE, VERSION 2.0 (CURRENT)")))
    (is (= #{"Apache-2.0"}                         (name->ids "APACHE LICENSE, VERSION 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "APACHE")))  ; Listed license missing version - we assume the latest
    (is (= #{"Apache-2.0"}                         (name->ids "ASL 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "ASL")))  ; Listed license missing version - we assume the latest
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2 Public License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2, see LICENSE")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2.0 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Licence 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Licence")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Licence, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License - Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License - v 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License - v2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License V2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License V2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License Version 2.0, January 2004")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License v 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License v2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License v2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, Version 2.0.")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, version 2.")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache License, version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Public License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Public License v2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Public License")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Public License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Public License, version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License - v 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Software Licesne")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Sofware Licencse 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Sofware License 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache V2 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache V2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache license version 2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache license, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache v2 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache v2")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache v2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache")))  ; Listed license missing clause info
    (is (= #{"Apache-2.0"}                         (name->ids "Apache, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache-2.0 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache-2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "Apache2 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "The Apache 2 License")))
    (is (= #{"Apache-2.0"}                         (name->ids "The Apache License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "The Apache Software License, Version 2.0")))
    (is (= #{"Apache-2.0"}                         (name->ids "apache")))  ; Listed license missing version - we assume the latest
    (is (= #{"Apache-2.0"}                         (name->ids "apache-2.0")))
    (is (= #{"Artistic-2.0" "GPL-3.0-only"}        (name->ids "Artistic License/GPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"Artistic-2.0"}                       (name->ids "Artistic License")))  ; Listed license missing version - we assume the latest
    (is (= #{"Artistic-2.0"}                       (name->ids "Artistic-2.0")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "2-Clause BSD License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "2-Clause BSD")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD (2 Clause)")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD (2-Clause)")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD (Type 2) Public License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2 Clause")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2 clause license")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-Clause Licence")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-Clause License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-Clause \"Simplified\" License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-Clause license")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-Clause")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD 2-clause \"Simplified\" License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD C2")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "BSD-2-Clause")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "New BSD 2-clause license")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "Simplified BSD License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "Simplified BSD license")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "The BSD 2-Clause License")))
    (is (= #{"BSD-2-Clause"}                       (name->ids "Two clause BSD license")))
    (is (= #{"BSD-3-Clause" "MIT"}                 (name->ids "New-BSD / MIT")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-Clause BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-Clause BSD")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-clause BSD licence (Revised BSD licence), also included in the jar file")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-clause BSD license")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "3-clause license (New BSD License or Modified BSD License)")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "Aduna BSD license")))  ; Listed license missing clause info, but the license text shows BSD-3-Clause
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3 Clause")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause 'New' or 'Revised' License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause \"New\" or \"Revised\" License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause license")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-Clause")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-clause License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-clause license")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD 3-clause")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD New, Version 3.0")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD-3")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "BSD-3-Clause")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "Modified BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "New BSD License or Modified BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "New BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "New BSD license")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "Revised BSD")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "The 3-Clause BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "The BSD 3-Clause License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "The New BSD License")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "The New BSD license")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "Three Clause BSD-like License")))
    (is (unlisted-only?                            (name->ids "https://github.com/jaycfields/jry/blob/master/README.md#license")))  ; We don't support full text matching in Markdown yet
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/clafka/blob/master/LICENSE")))                           ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/faraday-atom/blob/master/LICENSE")))                     ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/graphite-filter/blob/master/LICENSE")))                  ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/instrumented-ring-jetty-adapter/blob/master/LICENSE")))  ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/mr-clojure/blob/master/LICENSE")))                       ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/mr-edda/blob/master/LICENSE")))                          ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/multi-atom/blob/master/LICENSE")))                       ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/party/blob/master/LICENSE")))                            ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/mixradio/radix/blob/master/LICENSE")))                            ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/riverford/datagrep/blob/master/LICENSE")))                        ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/riverford/durable-ref/blob/master/LICENSE")))                     ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
;    (is (= #{"BSD-3-Clause"}                       (name->ids "https://github.com/smsharman/sxm-clojure-ms/blob/master/LICENSE")))                  ; Failing due to https://github.com/spdx/Spdx-Java-Library/issues/182
    (is (= #{"BSD-3-Clause"}                       (name->ids "https://opensource.org/licenses/BSD-3-Clause")))
    (is (= #{"BSD-3-Clause"}                       (name->ids "new BSD License")))
    (is (= #{"BSD-4-Clause"}                       (name->ids "BSD License")))  ; Listed license missing clause info - we assume original (4 clause)
    (is (= #{"BSD-4-Clause"}                       (name->ids "BSD Standard License")))  ; Listed license missing clause info - we assume original (4 clause)
    (is (= #{"BSD-4-Clause"}                       (name->ids "BSD license")))  ; Listed license missing clause info - we assume original (4 clause)
    (is (= #{"BSD-4-Clause"}                       (name->ids "BSD")))  ; Listed license missing clause info - we assume original (4 clause)
    (is (= #{"BSD-4-Clause"}                       (name->ids "BSD-style")))  ; Listed license missing clause info - we assume original (4 clause)
    (is (= #{"BSD-4-Clause"}                       (name->ids "The BSD License")))
    (is (= #{"BSL-1.0"}                            (name->ids "Boost Software License - Version 1.0")))
    (is (= #{"Beerware"}                           (name->ids "Beerware 42")))
    (is (= #{"Beerware"}                           (name->ids "THE BEER-WARE LICENSE")))
    (is (= #{"CC-BY-2.5"}                          (name->ids "Creative Commons Attribution 2.5 License")))
    (is (= #{"CC-BY-3.0"}                          (name->ids "Creative Commons 3.0")))
    (is (= #{"CC-BY-SA-3.0"}                       (name->ids "Creative Commons Attribution-ShareAlike 3.0 US (CC-SA) license")))  ; Note: the US suffix here is meaningless, as there is no CC-BY-SA-3.0-US license id
    (is (= #{"CC-BY-SA-3.0"}                       (name->ids "Creative Commons Attribution-ShareAlike 3.0 US (CC-SA)")))  ; Note: the US suffix here is meaningless, as there is no CC-BY-SA-3.0-US license id
    (is (= #{"CC-BY-SA-3.0"}                       (name->ids "Creative Commons Attribution-ShareAlike 3.0 Unported License")))
    (is (= #{"CC-BY-SA-3.0"}                       (name->ids "Creative Commons Attribution-ShareAlike 3.0 Unported")))
    (is (= #{"CC-BY-SA-3.0"}                       (name->ids "Creative Commons Attribution-ShareAlike 3.0")))
    (is (= #{"CC-BY-4.0"}                          (name->ids "CC Attribution 4.0 International with exception for binary distribution")))
    (is (= #{"CC-BY-4.0"}                          (name->ids "CC-BY-4.0")))
    (is (= #{"CC-BY-4.0"}                          (name->ids "Creative Commons Attribution License")))  ; Listed license missing version - we assume the latest
    (is (= #{"CC-BY-NC-3.0"}                       (name->ids "Creative Commons Attribution-NonCommercial 3.0")))
    (is (= #{"CC-BY-NC-4.0"}                       (name->ids "CC BY-NC")))  ; Listed license missing version - we assume the latest
    (is (= #{"CC-BY-NC-ND-3.0"}                    (name->ids "Attribution-NonCommercial-NoDerivs 3.0 Unported")))
    (is (= #{"CC-BY-SA-4.0"}                       (name->ids "CC BY-SA 4.0")))
    (is (= #{"CC0-1.0"}                            (name->ids "Public domain (CC0)")))
    (is (= #{"CC0-1.0"}                            (name->ids "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication")))
    (is (= #{"CC0-1.0"}                            (name->ids "CC0 1.0 Universal")))
    (is (= #{"CC0-1.0"}                            (name->ids "CC0")))
    (is (= #{"CDDL-1.1"}                           (name->ids "Common Development and Distribution License (CDDL)")))  ; Listed license missing clause info
    (is (= #{"CDDL-1.1"}                           (name->ids "Common Development and Distribution License")))  ; Listed license missing clause info
    (is (= #{"CECILL-2.1"}                         (name->ids "CeCILL License")))  ; Listed license missing version - we assume the latest
    (is (= #{"CPL-1.0"}                            (name->ids "Common Public License - v 1.0")))
    (is (= #{"CPL-1.0"}                            (name->ids "Common Public License Version 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "EPL 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "EPL-1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "EPL-v1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License (EPL) - v 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License - Version 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License - v 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License - v1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License 1.0 (EPL-1.0)")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License v 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License v1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License version 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public License, version 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "Eclipse Public Licese - v 1.0")))
    (is (= #{"EPL-1.0"}                            (name->ids "https://github.com/cmiles74/uio/blob/master/LICENSE")))
    (is (= #{"EPL-2.0" "GPL-2.0-or-later" "Classpath-exception-2.0"} (name->ids "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0")))
;    (is (= #{"EPL-2.0" "GPL-2.0-or-later" "Classpath-exception-2.0"} (name->ids "Eclipse Public License 2.0 OR GNU GPL v2+ with Classpath exception")))  ; ####TODO: THINK MORE ABOUT THIS ONE!!!
;    (is (= #{"EPL-2.0" "GPL-2.0-or-later" "Classpath-exception-2.0"} (name->ids "EPL-2.0 OR GPL-2.0-or-later WITH Classpath Exception")))  ; Listed exception missing version - we assume the latest
    (is (= #{"EPL-2.0" "GPL-2.0-or-later"}         (name->ids "EPL-2.0 OR GPL-2.0-or-later")))
    (is (= #{"EPL-2.0" "GPL-3.0-or-later" "Classpath-exception-2.0"} (name->ids "EPL-2.0 OR GPL-3.0-or-later WITH Classpath-exception-2.0")))
    (is (= #{"EPL-2.0" "GPL-3.0-or-later"}         (name->ids "EPL-2.0 OR GPL-3.0-or-later")))
;    (is (= #{"EPL-2.0" "LGPL-3.0-or-later"}        (name->ids "Dual: EPL and LGPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0" "MIT"}                      (name->ids "Eclipse Public MIT")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Copyright (C) 2013 Mathieu Gauthron. Distributed under the Eclipse Public License.")))
    (is (= #{"EPL-2.0"}                            (name->ids "Copyright (C) 2014 Mathieu Gauthron. Distributed under the Eclipse Public License.")))
    (is (= #{"EPL-2.0"}                            (name->ids "Distributed under the Eclipse Public License, the same as Clojure.")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "ECLIPSE PUBLIC LICENSE")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "EPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "EPL-2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "EPLv2")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse License")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public Licence")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License (EPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License - v 2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License 2")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License 2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License 2.0,")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License v2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License version 2")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License version 2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License, v. 2.0")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Public License, v2")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse Pulic License")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse public license, the same as Clojure")))
    (is (= #{"EPL-2.0"}                            (name->ids "Eclipse")))  ; Listed license missing version - we assume the latest
    (is (= #{"EPL-2.0"}                            (name->ids "Some Eclipse Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"EUPL-1.1"}                           (name->ids "European Union Public Licence (EUPL v.1.1)")))
    (is (= #{"EUPL-1.1"}                           (name->ids "The European Union Public License, Version 1.1")))
    (is (= #{"EUPL-1.2"}                           (name->ids "European Union Public Licence v. 1.2")))
    (is (= #{"EUPL-1.2"}                           (name->ids "European Union Public License 1.2 or later")))
    (is (= #{"EUPL-1.2"}                           (name->ids "European Union Public License")))  ; Listed license missing version - we assume the latest
(comment  ;####TODO: UNCOMMENT THIS!!!!
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GNU General Public License, Version 2, with the Classpath Exception")))
    (is (= #{"GPL-2.0-only" "Classpath-exception-2.0"} (name->ids "GPLv2 with Classpath exception")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU GENERAL PUBLIC LICENSE Version 2, June 1991")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU General Public License 2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU General Public License, version 2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU Public License v2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU Public License, Version 2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU Public License, Version 2.0")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GNU Public License, v2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GPL v2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GPL-2.0")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "GPLv2")))
    (is (= #{"GPL-2.0-only"}                       (name->ids "The GNU General Public License, Version 2")))
    (is (= #{"GPL-2.0-or-later" "Classpath-exception-2.0"} (name->ids "GPL-2.0-or-later WITH Classpath-exception-2.0")))
    (is (= #{"GPL-2.0-or-later"}                   (name->ids "GNU GPL V2+")))
    (is (= #{"GPL-2.0-or-later"}                   (name->ids "GPL 2.0+")))
    (is (= #{"GPL-2.0-or-later"}                   (name->ids "GPL v2+ or Swiss Ephemeris")))  ; ####TODO: THINK MORE ABOUT THIS
    (is (= #{"GPL-3.0-only"}                       (name->ids " GNU GENERAL PUBLIC LICENSE Version 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL v 3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL v. 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL v3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL v3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU GPL, version 3, 29 June 2007")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License V3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License Version 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License v3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License v3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License, Version 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License, version 3 (GPLv3)")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU General Public License, version 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU Public License V. 3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU Public License V3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNU public licence V3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GNUv3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL 3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL V3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL v3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL version 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL-3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL-3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL-3.0-only")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPL3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "GPLv3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "General Public License 3")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "General Public License v3.0")))
    (is (= #{"GPL-3.0-only"}                       (name->ids "The GNU General Public License v3.0")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU GENERAL PUBLIC LICENSE")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU GPL v3+")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU GPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU GPLv3+")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU General Public License (GPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU General Public License v3.0 or later")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU General Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU General Public License, Version 3 (or later)")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU General Public License,version 2.0 or (at your option) any later version")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GNU")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GPL V3+")))
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "GPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"GPL-3.0-or-later"}                   (name->ids "The GNU General Public License")))  ; Listed license missing version - we assume the latest
)
    (is (= #{"Hippocratic-2.1"}                    (name->ids "Hippocratic License")))
    (is (= #{"ISC" "Classpath-exception-2.0"}      (name->ids "ISC WITH Classpath-exception-2.0")))
    (is (= #{"ISC"}                                (name->ids "ISC Licence")))
    (is (= #{"ISC"}                                (name->ids "ISC License")))
    (is (= #{"ISC"}                                (name->ids "ISC")))
    (is (= #{"ISC"}                                (name->ids "MIT/ISC License")))
    (is (= #{"ISC"}                                (name->ids "MIT/ISC")))
(comment  ;####TODO: UNCOMMENT THIS!!!!
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU LESSER GENERAL PUBLIC LICENSE - Version 2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU LESSER GENERAL PUBLIC LICENSE Version 2.1, February 1999")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU LGPL v2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Public License 2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Public License v2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Public License, Version 2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Lesser General Pulic License v2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Library or Lesser General Public License (LGPL) 2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "GNU Library or Lesser General Public License (LGPL) V2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "LGPL 2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "LGPL-2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "LGPL-2.1-only")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "LGPLv2.1")))
    (is (= #{"LGPL-2.1-only"}                      (name->ids "lgpl_v2_1")))
    (is (= #{"LGPL-2.1-or-later"}                  (name->ids "GNU Lesser General Public License, version 2.1 or newer")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU General Lesser Public License (LGPL) version 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LESSER GENERAL PUBLIC LICENSE")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LESSER GENERAL PUBLIC LICENSE, Version 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LGPL 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LGPL v3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LGPL version 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LGPL-3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU LGPLv3 ")))  ; Note trailing space
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser GPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public Licence 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public Licence")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License (LGPL) Version 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License (LGPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License - v 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License - v 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License - v3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License v3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License version 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License version 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser General Public License, Version 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser Genereal Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Lesser Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "GNU Library or Lesser General Public License (LGPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "Gnu Lesser Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "L GPL 3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL 3.0 (GNU Lesser General Public License)")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL 3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL Open Source license")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL v3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL-3.0")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPL-3.0-only")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "LGPLv3")))
    (is (= #{"LGPL-3.0-only"}                      (name->ids "Lesser GPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "Lesser General Public License (LGPL)")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-only"}                      (name->ids "Lesser General Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "GNU Lesser General Public License, Version 3 or later")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "GNU Lesser General Public License, v. 3 or later")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "GNU Lesser General Public License, version 3 or later")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "GNU Lesser General Public License, version 3.0 or (at your option) any later version")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "LGPL-3.0-or-later")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "LGPLv3+")))
    (is (= #{"LGPL-3.0-or-later"}                  (name->ids "Licensed under GNU Lesser General Public License Version 3 or later (the ")))  ; Note trailing space
)
    (is (= #{"Libpng"}                             (name->ids "zlib/libpng License")))
    (is (= #{"LicenseRef-lice-comb-PUBLIC-DOMAIN"} (name->ids "Public Domain")))
    (is (= #{"MIT" "Apache-2.0" "BSD-3-Clause"}    (name->ids "MIT/Apache-2.0/BSD-3-Clause")))
    (is (= #{"MIT"}                                (name->ids " MIT License")))
    (is (= #{"MIT"}                                (name->ids "Distributed under an MIT-style license (see LICENSE for details).")))
    (is (= #{"MIT"}                                (name->ids "Dual MIT & Proprietary")))  ; ####TODO: THINK MORE ABOUT THIS ONE!!!
    (is (= #{"MIT"}                                (name->ids "Expat (MIT) license")))
    (is (= #{"MIT"}                                (name->ids "MIT LICENSE")))
    (is (= #{"MIT"}                                (name->ids "MIT Licence")))
    (is (= #{"MIT"}                                (name->ids "MIT Licens")))
    (is (= #{"MIT"}                                (name->ids "MIT License (MIT)")))
    (is (= #{"MIT"}                                (name->ids "MIT License")))
    (is (= #{"MIT"}                                (name->ids "MIT Public License")))
    (is (= #{"MIT"}                                (name->ids "MIT license")))
    (is (= #{"MIT"}                                (name->ids "MIT public License")))
    (is (= #{"MIT"}                                (name->ids "MIT public license")))
    (is (= #{"MIT"}                                (name->ids "MIT")))
    (is (= #{"MIT"}                                (name->ids "MIT-style license (see LICENSE for details).")))
    (is (= #{"MIT"}                                (name->ids "THE MIT LICENSE")))
    (is (= #{"MIT"}                                (name->ids "The MIT Licence")))
    (is (= #{"MIT"}                                (name->ids "The MIT License (MIT) ")))  ; Note trailing space
    (is (= #{"MIT"}                                (name->ids "The MIT License (MIT) | Open Source Initiative")))
    (is (= #{"MIT"}                                (name->ids "The MIT License (MIT)")))
    (is (= #{"MIT"}                                (name->ids "The MIT License")))
    (is (= #{"MIT"}                                (name->ids "The MIT License.")))
;####TODO: UNCOMMENT ONCE URL DETECTION AND RESOLUTION IS IMPLEMENTED!!!!
;    (is (= #{"MIT"}                                (name->ids "http://opensource.org/licenses/MIT")))
;    (is (= #{"MIT"}                                (name->ids "https://github.com/clanhr/clanhr-service/blob/master/LICENSE")))
    (is (= #{"MPL-1.0"}                            (name->ids "Mozilla Public License Version 1.0")))
    (is (= #{"MPL-1.1"}                            (name->ids "Mozilla Public License Version 1.1")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL 2")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL v2")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL")))  ; Listed license missing version - we assume the latest
    (is (= #{"MPL-2.0"}                            (name->ids "MPL-2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL-v2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "MPL2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public Licence 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License (Version 2.0)")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License Version 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License v2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License v2.0+")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License version 2")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License version 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License")))  ; Listed license missing version - we assume the latest
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License, v. 2.0")))
    (is (= #{"MPL-2.0"}                            (name->ids "Mozilla Public License, version 2.0")))
    (is (= #{"NASA-1.3"}                           (name->ids "NASA OPEN SOURCE AGREEMENT VERSION 1.3")))
    (is (= #{"NASA-1.3"}                           (name->ids "NASA Open Source Agreement, Version 1.3")))
    (is (= #{"NCSA"}                               (name->ids "University of Illinois/NCSA Open Source License")))
    (is (= #{"Ruby"}                               (name->ids "Ruby License")))
    (is (= #{"SGI-B-2.0"}                          (name->ids "SGI")))  ; Listed license missing version - we assume the latest
    (is (= #{"SMPPL"}                              (name->ids "SMPPL")))
    (is (= #{"Unlicense"}                          (name->ids "The UnLicense")))
    (is (= #{"Unlicense"}                          (name->ids "The Unlicence")))
    (is (= #{"Unlicense"}                          (name->ids "The Unlicense")))
    (is (= #{"Unlicense"}                          (name->ids "UnLicense")))
    (is (= #{"Unlicense"}                          (name->ids "Unlicense License")))
    (is (= #{"Unlicense"}                          (name->ids "Unlicense")))
    (is (= #{"Unlicense"}                          (name->ids "unlicense")))
    (is (= #{"W3C"}                                (name->ids "W3C Software license")))
    (is (= #{"WTFPL"}                              (name->ids "DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE")))
    (is (= #{"WTFPL"}                              (name->ids "DO-WTF-U-WANT-2")))
    (is (= #{"WTFPL"}                              (name->ids "Do What The Fuck You Want To Public License")))
    (is (= #{"WTFPL"}                              (name->ids "Do What The Fuck You Want To Public License, Version 2")))
    (is (= #{"WTFPL"}                              (name->ids "WTFPL v2")))
    (is (= #{"WTFPL"}                              (name->ids "WTFPL – Do What the Fuck You Want to Public License")))
    (is (= #{"WTFPL"}                              (name->ids "WTFPL")))
    (is (= #{"X11"}                                (name->ids "MIT X11 License")))
    (is (= #{"X11"}                                (name->ids "MIT/X11")))
    (is (= #{"Zlib"}                               (name->ids "Zlib License")))
    (is (= #{"Zlib"}                               (name->ids "zlib License")))
    (is (= #{"Zlib"}                               (name->ids "zlib license")))
    (is (unlisted-only?                            (name->ids "${license.id}")))
;####TODO: UNCOMMENT ME!!!!
;    (is (unlisted-only?                            (name->ids "<script lang=\"javascript\">alert('hi');</script>EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0")))
    (is (unlisted-only?                            (name->ids "A Clojure library for Google Cloud Pub/Sub.")))
    (is (unlisted-only?                            (name->ids "APGL")))  ; Probable typo
    (is (unlisted-only?                            (name->ids "All Rights Reserved")))
    (is (unlisted-only?                            (name->ids "All rights reserved")))
    (is (unlisted-only?                            (name->ids "Amazon Software License")))
    (is (unlisted-only?                            (name->ids "BankersBox License")))
    (is (unlisted-only?                            (name->ids "Bespoke")))
    (is (unlisted-only?                            (name->ids "Bloomberg Open API")))
    (is (unlisted-only?                            (name->ids "Bostock")))
    (is (unlisted-only?                            (name->ids "Built In Project License")))
    (is (unlisted-only?                            (name->ids "CRAPL License")))
    (is (unlisted-only?                            (name->ids "Contact JMonkeyEngine forums for license details")))
    (is (unlisted-only?                            (name->ids "Copyright & all rights reserved Lean Pixel")))
    (is (unlisted-only?                            (name->ids "Copyright (C) 2015 by Glowbox LLC")))
    (is (unlisted-only?                            (name->ids "Copyright (c) 2011 Drew Colthorp")))
    (is (unlisted-only?                            (name->ids "Copyright (c) 2017, Lingchao Xin")))
    (is (unlisted-only?                            (name->ids "Copyright 2013 The Fresh Diet. All rights reserved.")))
    (is (unlisted-only?                            (name->ids "Copyright 2016, klaraHealth, Inc.")))
    (is (unlisted-only?                            (name->ids "Copyright 2017 All Rights Reserved")))
    (is (unlisted-only?                            (name->ids "Copyright 2017 Zensight")))
    (is (unlisted-only?                            (name->ids "Copyright 4A Volcano. 2015.")))
    (is (unlisted-only?                            (name->ids "Copyright Ona Systems Inc.")))
    (is (unlisted-only?                            (name->ids "Copyright meissa GmbH")))
    (is (unlisted-only?                            (name->ids "Copyright © SparX 2014")))
    (is (unlisted-only?                            (name->ids "Copyright")))
    (is (unlisted-only?                            (name->ids "Custom")))
    (is (unlisted-only?                            (name->ids "Cydeas Public License")))
    (is (unlisted-only?                            (name->ids "Don't steal my stuff")))
    (is (unlisted-only?                            (name->ids "Dropbox ToS")))
    (is (unlisted-only?                            (name->ids "FIXME: choose")))
    (is (unlisted-only?                            (name->ids "Firebase ToS")))
    (is (= #{"BSD-2-Clause-FreeBSD"}               (name->ids "FreeBSD License")))
    (is (unlisted-only?                            (name->ids "GG Public License")))
    (is (unlisted-only?                            (name->ids "Google Maps ToS")))
    (is (unlisted-only?                            (name->ids "GraphiQL license")))
    (is (unlisted-only?                            (name->ids "Hackthorn Innovation Ltd")))
    (is (unlisted-only?                            (name->ids "Hackthorn Innovation copyright")))
    (is (unlisted-only?                            (name->ids "Heap ToS")))
    (is (unlisted-only?                            (name->ids "Interel")))
    (is (unlisted-only?                            (name->ids "JLGL Backend")))
    (is (unlisted-only?                            (name->ids "Jedis License")))
    (is (unlisted-only?                            (name->ids "Jiegao Owned")))
    (is (unlisted-only?                            (name->ids "LICENSE")))
    (is (unlisted-only?                            (name->ids "Libre Uso MX")))
    (is (unlisted-only?                            (name->ids "License of respective package")))
    (is (unlisted-only?                            (name->ids "License")))
    (is (unlisted-only?                            (name->ids "Like Clojure.")))
    (is (unlisted-only?                            (name->ids "Mixed")))
    (is (unlisted-only?                            (name->ids "Multiple")))
    (is (unlisted-only?                            (name->ids "Not fit for public use so formally proprietary software - this is not open-source")))
    (is (unlisted-only?                            (name->ids "OTN License Agreement")))
    (is (unlisted-only?                            (name->ids "Open Source Community License - Type C version 1.0")))
    (is (unlisted-only?                            (name->ids "Other License")))
    (is (unlisted-only?                            (name->ids "Private License")))
    (is (unlisted-only?                            (name->ids "Private")))
    (is (unlisted-only?                            (name->ids "Proprietary License")))
    (is (unlisted-only?                            (name->ids "Proprietary")))
    (is (unlisted-only?                            (name->ids "Proprietory. Copyright Jayaraj Poroor. All Rights Reserved.")))
    (is (unlisted-only?                            (name->ids "Provisdom")))
    (is (unlisted-only?                            (name->ids "Research License 1.0")))
    (is (unlisted-only?                            (name->ids "Restricted Distribution.")))
    (is (unlisted-only?                            (name->ids "SYNNEX China Owned")))
    (is (unlisted-only?                            (name->ids "See the LICENSE file")))
    (is (unlisted-only?                            (name->ids "Shen License")))
    (is (unlisted-only?                            (name->ids "Slick2D License")))
    (is (unlisted-only?                            (name->ids "Stripe ToS")))
    (is (unlisted-only?                            (name->ids "TODO")))
    (is (unlisted-only?                            (name->ids "TODO: Choose a license")))
    (is (unlisted-only?                            (name->ids "The I Haven't Got Around To This Yet License")))
    (is (unlisted-only?                            (name->ids "To ill!")))
    (is (unlisted-only?                            (name->ids "Tulos Commercial License")))
    (is (unlisted-only?                            (name->ids "UNLICENSED")))
    (is (unlisted-only?                            (name->ids "University of Buffalo Public License")))
    (is (unlisted-only?                            (name->ids "Unknown")))
    (is (unlisted-only?                            (name->ids "VNETLPL - Limited Public License")))
    (is (unlisted-only?                            (name->ids "VNet PL")))
    (is (unlisted-only?                            (name->ids "Various")))
    (is (unlisted-only?                            (name->ids "Vimeo License")))
    (is (unlisted-only?                            (name->ids "WIP")))
    (is (unlisted-only?                            (name->ids "Wildbit Proprietary License")))
    (is (unlisted-only?                            (name->ids "YouTube ToS")))
    (is (unlisted-only?                            (name->ids "avi license")))
    (is (unlisted-only?                            (name->ids "esl-sdk-external-signer-verification")))
    (is (unlisted-only?                            (name->ids "jank license")))
    (is (unlisted-only?                            (name->ids "name")))
    (is (unlisted-only?                            (name->ids "none")))
    (is (unlisted-only?                            (name->ids "proprietary")))
    (is (unlisted-only?                            (name->ids "state-node license")))
    (is (unlisted-only?                            (name->ids "trove")))
    (is (unlisted-only?                            (name->ids "url")))
    (is (unlisted-only?                            (name->ids "wisdragon")))
    (is (unlisted-only?                            (name->ids "wiseloong")))))

(deftest uri->ids-tests
  (testing "Nil, empty or blank uri"
    (is (nil?                                                (uri->ids nil)))
    (is (nil?                                                (uri->ids "")))
    (is (nil?                                                (uri->ids "       ")))
    (is (nil?                                                (uri->ids "\n")))
    (is (nil?                                                (uri->ids "\t"))))
  (testing "URIs that appear verbatim in the SPDX license or exception lists"
    (is (= #{"Apache-2.0"}                                   (uri->ids "http://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "               http://www.apache.org/licenses/LICENSE-2.0.html             ")))   ; Test whitespace
    (is (= #{"AGPL-3.0-or-later" "AGPL-3.0-only" "AGPL-3.0"} (uri->ids "https://www.gnu.org/licenses/agpl.txt")))
    (is (= #{"CC-BY-SA-4.0"}                                 (uri->ids "https://creativecommons.org/licenses/by-sa/4.0/legalcode")))
    (is (= #{"Classpath-exception-2.0"}                      (uri->ids "https://www.gnu.org/software/classpath/license.html"))))
  (testing "URI variations that should be handled identically"
    (is (= #{"Apache-2.0"}                                   (uri->ids "https://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "http://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "https://www.apache.org/licenses/LICENSE-2.0.txt")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "http://apache.org/licenses/LICENSE-2.0.pdf"))))
  (testing "URIs that appear in licensey things, but aren't in the SPDX license list as shown"
    (is (= #{"Apache-2.0"}                                   (uri->ids "http://www.apache.org/licenses/LICENSE-2.0")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "https://www.apache.org/licenses/LICENSE-2.0.txt"))))
  (testing "URIs that aren't in the SPDX license list, but do match via retrieval and full text matching"
    (is (= #{"Apache-2.0"}                                   (uri->ids "https://raw.githubusercontent.com/pmonks/lice-comb/main/LICENSE")))
    (is (= #{"Apache-2.0"}                                   (uri->ids "https://github.com/pmonks/lice-comb/blob/main/LICENSE")))))   ; ####TODO: Not sure about this one
