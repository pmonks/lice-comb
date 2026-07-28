;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.info-maps-test
  (:require [clojure.test                     :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate       :refer [fixture]]
            [lice-comb.impl.parsing.info-maps :refer [prepend-source-to-fims-within-em merge-expressions-maps]]))

(use-fixtures :once fixture)

(def em1 {
  "Apache-2.0" '({:fragment "Apache-2.0" :type :concluded :confidence :medium :strategy "Regex matching"  :source ("Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")})
  "MIT"        '({:fragment "MIT"        :type :concluded :confidence :high   :strategy "SPDX identifier" :source ("Apache Software Licence v2.0 / MIT" "MIT")})})

(def em2 {
  "Apache-2.0 AND BSD-4-Clause" '({:fragment "Apache-2.0"   :type :concluded :confidence :low :strategy "Regex matching" :source ("Apache style license & BSD" "Apache style license")}
                                  {:fragment "BSD-4-Clause" :type :concluded :confidence :low :strategy "Regex matching" :source ("Apache style license & BSD" "BSD")})})

(def em3 {
  "Apache-2.0"       '({:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache style license")}
                       {:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "SPDX identifier near match" :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "apache-2.0")}
                       {:fragment "Apache-2.0"       :type :declared                      :strategy "SPDX identifier"            :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache-2.0")})
  "GPL-3.0-or-later" '({:fragment "GPL-3.0-or-later" :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "GNU General Public License 3.0 or later")})})

(def ems (list em1 em2 em3))

(deftest prepend-source-to-fims-within-em-tests
  (testing "nil/empty/blank"
    (is (nil? (prepend-source-to-fims-within-em nil nil)))
    (is (= {} (prepend-source-to-fims-within-em nil {})))
    (is (nil? (prepend-source-to-fims-within-em "" nil)))
    (is (= {} (prepend-source-to-fims-within-em "" {}))))
  (testing "non-nil map that isn't lice-comb specific"
    (is (= {:a "a"} (prepend-source-to-fims-within-em "foo" {:a "a"}))))
  (testing "simple prepending"
    (is (= {"Apache-2.0" '({:fragment "Apache-2.0" :type :concluded :confidence :medium :strategy "Regex matching"  :source ("pom.xml" "Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")})
            "MIT"        '({:fragment "MIT"        :type :concluded :confidence :high   :strategy "SPDX identifier" :source ("pom.xml" "Apache Software Licence v2.0 / MIT" "MIT")})}
           (prepend-source-to-fims-within-em "pom.xml" em1)))
    (is (= {"Apache-2.0 AND BSD-4-Clause" '({:fragment "Apache-2.0"   :type :concluded :confidence :low :strategy "Regex matching" :source ("pom.xml" "Apache style license & BSD" "Apache style license")}
                                            {:fragment "BSD-4-Clause" :type :concluded :confidence :low :strategy "Regex matching" :source ("pom.xml" "Apache style license & BSD" "BSD")})}
           (prepend-source-to-fims-within-em "pom.xml" em2)))
    (is (= {"Apache-2.0"       '({:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("pom.xml" "GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache style license")}
                                 {:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "SPDX identifier near match" :source ("pom.xml" "GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "apache-2.0")}
                                 {:fragment "Apache-2.0"       :type :declared                      :strategy "SPDX identifier"            :source ("pom.xml" "GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache-2.0")})
            "GPL-3.0-or-later" '({:fragment "GPL-3.0-or-later" :type :concluded :confidence :low    :strategy "Regex matching"             :source ("pom.xml" "GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "GNU General Public License 3.0 or later")})}
           (prepend-source-to-fims-within-em "pom.xml" em3))))
  (testing "repeated prepending"
    (is (= {"Apache-2.0" '({:fragment "Apache-2.0" :type :concluded :confidence :medium :strategy "Regex matching"  :source ("library.jar" "pom.xml" "Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")})
            "MIT"        '({:fragment "MIT"        :type :concluded :confidence :high   :strategy "SPDX identifier" :source ("library.jar" "pom.xml" "Apache Software Licence v2.0 / MIT" "MIT")})}
           (prepend-source-to-fims-within-em "library.jar" (prepend-source-to-fims-within-em "pom.xml" em1))))))

(deftest merge-expressions-maps-tests
  (testing "nil/empty"
    (is (nil? (merge-expressions-maps)))
    (is (nil? (merge-expressions-maps nil))))
  (testing "identity"
    (is (= em1 (merge-expressions-maps em1))))
  (testing "merges"
    (is (= {"Apache-2.0"                  '({:fragment "Apache-2.0"   :type :concluded :confidence :medium :strategy "Regex matching"  :source ("Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")})
            "MIT"                         '({:fragment "MIT"          :type :concluded :confidence :high   :strategy "SPDX identifier" :source ("Apache Software Licence v2.0 / MIT" "MIT")})
            "Apache-2.0 AND BSD-4-Clause" '({:fragment "Apache-2.0"   :type :concluded :confidence :low    :strategy "Regex matching"  :source ("Apache style license & BSD" "Apache style license")}
                                            {:fragment "BSD-4-Clause" :type :concluded :confidence :low    :strategy "Regex matching"  :source ("Apache style license & BSD" "BSD")})}
           (merge-expressions-maps em1 em2)))
    (is (= {"Apache-2.0"       '({:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "Regex matching"             :source ("Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")}
                                 {:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache style license")}
                                 {:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "SPDX identifier near match" :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "apache-2.0")}
                                 {:fragment "Apache-2.0"       :type :declared                      :strategy "SPDX identifier"            :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache-2.0")})
            "MIT"              '({:fragment "MIT"              :type :concluded :confidence :high   :strategy "SPDX identifier"            :source ("Apache Software Licence v2.0 / MIT" "MIT")})
            "GPL-3.0-or-later" '({:fragment "GPL-3.0-or-later" :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "GNU General Public License 3.0 or later")})}
           (merge-expressions-maps em1 em3)))
    (is (= {"Apache-2.0 AND BSD-4-Clause" '({:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("Apache style license & BSD" "Apache style license")}
                                            {:fragment "BSD-4-Clause"     :type :concluded :confidence :low    :strategy "Regex matching"             :source ("Apache style license & BSD" "BSD")})
            "Apache-2.0"                  '({:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache style license")}
                                            {:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "SPDX identifier near match" :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "apache-2.0")}
                                            {:fragment "Apache-2.0"       :type :declared                      :strategy "SPDX identifier"            :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache-2.0")})
            "GPL-3.0-or-later"            '({:fragment "GPL-3.0-or-later" :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "GNU General Public License 3.0 or later")})}
           (merge-expressions-maps em2 em3)))
    (is (= {"Apache-2.0"                  '({:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "Regex matching"             :source ("Apache Software Licence v2.0 / MIT" "Apache Software Licence v2.0")}
                                            {:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache style license")}
                                            {:fragment "Apache-2.0"       :type :concluded :confidence :medium :strategy "SPDX identifier near match" :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "apache-2.0")}
                                            {:fragment "Apache-2.0"       :type :declared                      :strategy "SPDX identifier"            :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "Apache-2.0")})
            "MIT"                         '({:fragment "MIT"              :type :concluded :confidence :high   :strategy "SPDX identifier"            :source ("Apache Software Licence v2.0 / MIT" "MIT")})
            "Apache-2.0 AND BSD-4-Clause" '({:fragment "Apache-2.0"       :type :concluded :confidence :low    :strategy "Regex matching"             :source ("Apache style license & BSD" "Apache style license")}
                                            {:fragment "BSD-4-Clause"     :type :concluded :confidence :low    :strategy "Regex matching"             :source ("Apache style license & BSD" "BSD")})
            "GPL-3.0-or-later"            '({:fragment "GPL-3.0-or-later" :type :concluded :confidence :low    :strategy "Regex matching"             :source ("GNU General Public License 3.0 or later / apache-2.0 / Apache-2.0 / Apache style license" "GNU General Public License 3.0 or later")})}
           (apply merge-expressions-maps ems)))))
