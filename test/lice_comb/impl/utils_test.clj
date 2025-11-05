;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.utils-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [clojure.java.io            :as io]
            [lice-comb.test-boilerplate :refer [fixture test-data-path]]
            [lice-comb.impl.utils       :refer [canonicalise-version-number simplify-uri filepath filename html->text]]))

(use-fixtures :once fixture)

(deftest canonicalise-version-number-tests
  (testing "Nil, blank, etc."
    (is (nil? (canonicalise-version-number nil)))
    (is (nil? (canonicalise-version-number "")))
    (is (nil? (canonicalise-version-number "  "))))
  (testing "Invalid version numbers"
    (is (nil? (canonicalise-version-number "foo")))
    (is (nil? (canonicalise-version-number ".")))
    (is (nil? (canonicalise-version-number ".2")))
    (is (nil? (canonicalise-version-number "2.")))
    (is (nil? (canonicalise-version-number "-2"))))
  (testing "Valid version numbers - semver"
    (is (= {:components [0]    :type :semver}              (canonicalise-version-number "0")))
    (is (= {:components [2]    :type :semver}              (canonicalise-version-number "2")))
    (is (= {:components [2]    :type :semver}              (canonicalise-version-number "02")))
    (is (= {:components [2]    :type :semver}              (canonicalise-version-number "2.0")))
    (is (= {:components [2]    :type :semver}              (canonicalise-version-number "2.0.0")))
    (is (= {:components [2]    :type :semver}              (canonicalise-version-number "00000002.000000000000.00000000000")))
    (is (= {:components [2 1]  :type :semver}              (canonicalise-version-number "2.1")))
    (is (= {:components [2 1]  :type :semver}              (canonicalise-version-number "2.01")))
    (is (= {:components [2 1]  :type :semver}              (canonicalise-version-number "02.01.00")))
    (is (= {:components [86]   :type :semver}              (canonicalise-version-number "86.0")))
    (is (= {:components [0 99] :type :semver}              (canonicalise-version-number "0.99")))
    (is (= {:components [1 2 3 4 5 6 7 8 9] :type :semver} (canonicalise-version-number "1.02.003.0004.00005.000006.0000007.00000008.000000009"))))
  (testing "Valid version numbers - years / dates"
    (is (= {:components [86]       :type :year2} (canonicalise-version-number "86")))
    (is (= {:components [86]       :type :year2} (canonicalise-version-number "0086")))
    (is (= {:components [2006]     :type :year4} (canonicalise-version-number "2006")))
    (is (= {:components [2006]     :type :year4} (canonicalise-version-number "02006")))
    (is (= {:components [19980720] :type :date}  (canonicalise-version-number "19980720")))
    (is (= {:components [19980720] :type :date}  (canonicalise-version-number "0000000000019980720")))
    (is (= {:components [20150513] :type :date}  (canonicalise-version-number "20150513"))))
  (testing "Valid version numbers - suffix"
    (is (= {:components [0]        :type :semver :suffix "a"} (canonicalise-version-number "0a")))
    (is (= {:components [1]        :type :semver :suffix "c"} (canonicalise-version-number "1.0c")))
    (is (= {:components [1]        :type :semver :suffix "X"} (canonicalise-version-number "1.0.0.0X")))
    (is (= {:components [99 3]     :type :semver :suffix "a"} (canonicalise-version-number "0099.03a")))
    (is (= {:components [89]       :type :year2  :suffix "a"} (canonicalise-version-number "89a")))
    (is (= {:components [1986]     :type :year4  :suffix "D"} (canonicalise-version-number "1986D")))
    (is (= {:components [1986]     :type :year4  :suffix "D"} (canonicalise-version-number "001986D")))
    (is (= {:components [19980720] :type :date   :suffix "g"} (canonicalise-version-number "19980720g")))))

(def simplified-apache2-uri "http://apache.org/license/license-2.0")
(def local-mpl2-html        (str test-data-path "/MPL-2.0/LICENSE.html"))

(deftest simplify-uri-tests
  (testing "Nil, empty or blank values"
    (is (nil? (simplify-uri nil)))
    (is (nil? (simplify-uri "")))
    (is (nil? (simplify-uri "       ")))
    (is (nil? (simplify-uri "\n")))
    (is (nil? (simplify-uri "\t"))))
  (testing "Values that are not uris"
    (is (= "foo"    (simplify-uri "FOO")))
    (is (= "foo"    (simplify-uri "foo")))
    (is (= "foobar" (simplify-uri "   FoObAr    "))))
  (testing "Values that are non-http(s) uris"
    (is (= "ftp://user@host/foo/bar.txt" (simplify-uri "ftp://user@host/foo/bar.txt")))
    (is (= "ftp://user@host/foo/bar.txt" (simplify-uri "FTP://USER@HOST/FOO/BAR.TXT")))
    (is (= "mailto:someone@example.com?subject=this%20is%20the%20subject&cc=someone_else@example.com&body=this%20is%20the%20body"
           (simplify-uri "mailto:someone@example.com?subject=This%20is%20the%20subject&cc=someone_else@example.com&body=This%20is%20the%20body"))))
  (testing "Valid uris that don't get simplified"
    (is (= simplified-apache2-uri                                   (simplify-uri simplified-apache2-uri)))
    (is (= "http://creativecommons.org/license/by-sa/4.0/legalcode" (simplify-uri "http://creativecommons.org/licenses/by-sa/4.0/legalcode"))))
  (testing "Valid uris that get simplified"
    (is (= simplified-apache2-uri                                           (simplify-uri "http://www.apache.org/licenses/LICENSE-2.0")))
    (is (= simplified-apache2-uri                                           (simplify-uri "https://www.apache.org/licenses/LICENSE-2.0")))
    (is (= simplified-apache2-uri                                           (simplify-uri "http://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= simplified-apache2-uri                                           (simplify-uri "https://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= simplified-apache2-uri                                           (simplify-uri "http://www.apache.org/licenses/LICENSE-2.0.html")))
    (is (= simplified-apache2-uri                                           (simplify-uri "https://www.apache.org/licenses/LICENSE-2.0.txt")))
    (is (= simplified-apache2-uri                                           (simplify-uri "https://www.apache.org/licenses/license-2.0.txt")))
    (is (= simplified-apache2-uri                                           (simplify-uri "https://www.apache.org/licenses/license-2.0.md")))
    (is (= simplified-apache2-uri                                           (simplify-uri "http://apache.org/licenses/LICENSE-2.0.pdf")))
    (is (= simplified-apache2-uri                                           (simplify-uri "               http://www.apache.org/licenses/LICENSE-2.0.html             ")))
    (is (= "http://gnu.org/license/agpl"                                    (simplify-uri "https://www.gnu.org/licenses/agpl.txt")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/license/MIT")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/license/MIT/")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/license/mit/")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/license/MIT.TXT")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/licence/MIT")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/licenses/MIT")))
    (is (= "http://opensource.org/license/mit"                              (simplify-uri "https://opensource.org/licences/MIT")))
    (is (= "http://gnu.org/software/classpath/license"                      (simplify-uri "https://www.gnu.org/software/classpath/license.html")))
    (is (= "http://raw.githubusercontent.com/pmonks/lice-comb/main/license" (simplify-uri "https://raw.githubusercontent.com/pmonks/lice-comb/main/LICENSE")))
    (is (= "http://github.com/pmonks/lice-comb/blob/main/license"           (simplify-uri "https://github.com/pmonks/lice-comb/blob/main/LICENSE")))))

(deftest filepath-tests
  (testing "Nil, empty or blank values"
    (is (nil? (filepath nil)))
    (is (= "" (filepath "")))
    (is (= "" (filepath "       ")))
    (is (= "" (filepath "\n")))
    (is (= "" (filepath "\t"))))
  (testing "Files"
    (is (= "/file.txt"                                               (filepath (io/file "/file.txt"))))
    (is (= "/some/path/or/other/file.txt"                           (filepath (io/file "/some/path/or/other/file.txt")))))
  (testing "Strings"
    (is (= "file.txt"                                               (filepath "file.txt")))
    (is (= "/some/path/or/other/file.txt"                           (filepath "/some/path/or/other/file.txt")))
    (is (= "https://www.google.com/"                                (filepath "https://www.google.com/")))
    (is (= "https://www.google.com/"                                (filepath "       https://www.google.com/       ")))
    (is (= "https://github.com/pmonks/lice-comb/blob/main/deps.edn" (filepath "https://github.com/pmonks/lice-comb/blob/main/deps.edn"))))
  (testing "ZipEntries"
    (is (= "file.txt"                                               (filepath (java.util.zip.ZipEntry. "file.txt"))))
    (is (= "/some/path/or/other/file.txt"                           (filepath (java.util.zip.ZipEntry. "/some/path/or/other/file.txt")))))
  (testing "URLs"
    (is (= "https://www.google.com/"                                (filepath (io/as-url "https://www.google.com/"))))
    (is (= "https://github.com/pmonks/lice-comb/blob/main/deps.edn" (filepath (io/as-url "https://github.com/pmonks/lice-comb/blob/main/deps.edn")))))
  (testing "URIs"
    (is (= "https://www.google.com/"                                (filepath (java.net.URI. "https://www.google.com/"))))
    (is (= "https://github.com/pmonks/lice-comb/blob/main/deps.edn" (filepath (java.net.URI. "https://github.com/pmonks/lice-comb/blob/main/deps.edn")))))
  (testing "InputStream"
    (is (thrown? clojure.lang.ExceptionInfo                         (filepath (io/input-stream "deps.edn"))))))

(deftest filename-tests
  (testing "Nil, empty or blank values"
    (is (nil? (filename nil)))
    (is (= "" (filename "")))
    (is (= "" (filename "       ")))
    (is (= "" (filename "\n")))
    (is (= "" (filename "\t"))))
  (testing "Files"
    (is (= "file.txt" (filename (io/file "file.txt"))))
    (is (= "file.txt" (filename (io/file "/some/path/or/other/file.txt")))))
  (testing "Strings"
    (is (= "file.txt" (filename "file.txt")))
    (is (= "file.txt" (filename "/some/path/or/other/file.txt")))
    (is (= ""         (filename "https://www.google.com")))
    (is (= ""         (filename "https://www.google.com/")))
    (is (= "deps.edn" (filename "https://github.com/pmonks/lice-comb/blob/main/deps.edn"))))
  (testing "ZipEntries"
    (is (= "file.txt" (filename (java.util.zip.ZipEntry. "file.txt"))))
    (is (= "file.txt" (filename (java.util.zip.ZipEntry. "/some/path/or/other/file.txt")))))
  (testing "URLs"
    (is (= ""         (filename (io/as-url "https://www.google.com/"))))
    (is (= "deps.edn" (filename (io/as-url "https://github.com/pmonks/lice-comb/blob/main/deps.edn")))))
  (testing "URIs"
    (is (= ""         (filename (java.net.URI. "https://www.google.com/"))))
    (is (= "deps.edn" (filename (java.net.URI. "https://github.com/pmonks/lice-comb/blob/main/deps.edn")))))
  (testing "InputStream"
    (is (thrown? clojure.lang.ExceptionInfo (filename (io/input-stream "deps.edn"))))))

(deftest html->text-tests
  (testing "Nil, empty or blank values"
    (is (nil? (html->text nil)))
    (is (= "" (html->text "")))
    (is (= "" (filename "       ")))
    (is (= "" (filename "\n")))
    (is (= "" (filename "\t"))))
  (testing "Simple HTML"
    (is (= "Hello, world!" (html->text "Hello, world!")))
    (is (= "Hello, world!" (html->text "<html><body><p>Hello, world!</p></body></html>")))
    (is (= "Hello, world!" (html->text "<html><body><h1>Hello, world!</h1></body></html>")))
    (is (= "Hello, world!" (html->text "<html><head><title>Hello, world!</title></head></html>")))
    (is (= ""              (html->text "<html><body><p class=\"Hello, world!\"></p></body></html>"))))
  (testing "Real world HTML"
    (is (= "Mozilla Public License, version 2.0" (subs (html->text (slurp local-mpl2-html)) 0 35)))))
