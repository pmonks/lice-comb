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

(ns lice-comb.files-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [clojure.java.io            :as io]
            [lice-comb.test-boilerplate :refer [fixture valid=]]
            [lice-comb.files            :refer [probable-license-file? probable-license-files file->expressions dir->expressions zip->expressions]]))

(use-fixtures :once fixture)

(def test-data-path "./test/lice_comb/data")

(deftest probable-license-file?-tests
  (testing "Nil, empty or blank names"
    (is (= false (probable-license-file? nil)))
    (is (= false (probable-license-file? "")))
    (is (= false (probable-license-file? "       ")))
    (is (= false (probable-license-file? "\n")))
    (is (= false (probable-license-file? "\t"))))
  (testing "Filenames that are probable license files"
    (is (= true  (probable-license-file? "pom.xml")))
    (is (= true  (probable-license-file? "POM.XML")))
    (is (= true  (probable-license-file? "asf-cat-1.0.12.pom")))
    (is (= true  (probable-license-file? "license")))
    (is (= true  (probable-license-file? "LICENSE")))
    (is (= true  (probable-license-file? "license.txt")))
    (is (= true  (probable-license-file? "LICENSE.TXT")))
    (is (= true  (probable-license-file? "copying")))
    (is (= true  (probable-license-file? "COPYING"))))
  (testing "Filenames that are not probable license files"
    (is (= false (probable-license-file? "NOTICES")))
    (is (= false (probable-license-file? "notices")))
    (is (= false (probable-license-file? "licenses")))
    (is (= false (probable-license-file? "LICENSES")))
    (is (= false (probable-license-file? "deps.edn")))
    (is (= false (probable-license-file? "pm.xml"))))
  (testing "Filenames including paths"
    (is (= true  (probable-license-file? "/path/to/a/project/containing/a/pom.xml")))
    (is (= false (probable-license-file? "/a/different/path/to/some/NOTICES")))))

(deftest probable-license-files-tests
  (testing "Nil, empty, or blank directory"
    (is (nil?                                  (probable-license-files nil)))
    (is (thrown? java.io.FileNotFoundException (probable-license-files "")))
    (is (thrown? java.io.FileNotFoundException (probable-license-files "       ")))
    (is (thrown? java.io.FileNotFoundException (probable-license-files "\n")))
    (is (thrown? java.io.FileNotFoundException (probable-license-files "\t"))))
  (testing "Not a directory"
    (is (thrown? java.nio.file.NotDirectoryException (probable-license-files "deps.edn"))))
  (testing "A real directory"
      (is (= #{(io/file (str test-data-path "/asf-cat-1.0.12.pom"))
               (io/file (str test-data-path "/with-parent.pom"))
               (io/file (str test-data-path "/no-xml-ns.pom"))
               (io/file (str test-data-path "/simple.pom"))
               (io/file (str test-data-path "/CC-BY-4.0/LICENSE"))
               (io/file (str test-data-path "/MPL-2.0/LICENSE"))}
             (probable-license-files test-data-path)))))

(deftest file->expressions-tests
  (testing "Nil, empty, or blank filename"
    (is (nil?                                  (file->expressions  nil)))
    (is (thrown? java.io.FileNotFoundException (file->expressions  "")))
    (is (thrown? java.io.FileNotFoundException (file->expressions  "       ")))
    (is (thrown? java.io.FileNotFoundException (file->expressions  "\n")))
    (is (thrown? java.io.FileNotFoundException (file->expressions  "\t"))))
  (testing "Non-existent files"
    (is (thrown? java.io.FileNotFoundException (file->expressions  "this_file_does_not_exist"))))
  (testing "Files on disk"
;    (is (= #{"CC-BY-4.0"} (file->expressions  (str test-data-path "/CC-BY-4.0/LICENSE"))))  ; Failing due to https://github.com/spdx/license-list-XML/issues/1960
    (is (valid= #{"MPL-2.0"}   (file->expressions  (str test-data-path "/MPL-2.0/LICENSE")))))
  (testing "URLs"
    (is (valid= #{"Apache-2.0"} (file->expressions  "https://www.apache.org/licenses/LICENSE-2.0.txt")))
    (is (valid= #{"Apache-2.0"} (file->expressions  (io/as-url "https://www.apache.org/licenses/LICENSE-2.0.txt")))))
  (testing "InputStreams"
    (is (thrown? clojure.lang.ExceptionInfo (with-open [is (io/input-stream "https://www.apache.org/licenses/LICENSE-2.0.txt")] (file->expressions  is))))
    (is (valid= #{"Apache-2.0"} (with-open [is (io/input-stream "https://www.apache.org/licenses/LICENSE-2.0.txt")]                  (file->expressions  is "LICENSE_2.0.txt")))))
  (testing "POM files"
    (is (valid= #{"Apache-2.0"}   (file->expressions  (str test-data-path "/simple.pom"))))
    (is (valid= #{"BSD-3-Clause"} (file->expressions  (str test-data-path "/no-xml-ns.pom"))))
    (is (valid= #{"Apache-2.0"}   (file->expressions  (str test-data-path "/asf-cat-1.0.12.pom"))))
    (is (valid= #{"Apache-2.0"}   (file->expressions  (str test-data-path "/with-parent.pom"))))))

(deftest dir->expressions-tests
  (testing "Nil, empty, or blank directory name"
    (is (nil?                                  (dir->expressions  nil)))
    (is (thrown? java.io.FileNotFoundException (dir->expressions  "")))
    (is (thrown? java.io.FileNotFoundException (dir->expressions  "       ")))
    (is (thrown? java.io.FileNotFoundException (dir->expressions  "\n")))
    (is (thrown? java.io.FileNotFoundException (dir->expressions  "\t"))))
  (testing "Non-existent or invalid directory"
    (is (thrown? java.io.FileNotFoundException       (dir->expressions  "this_directory_does_not_exist")))
    (is (thrown? java.nio.file.NotDirectoryException (dir->expressions  "deps.edn"))))
  (testing "Valid directory"
;    (is (valid= #{"Apache-2.0" "BSD-3-Clause" "MPL-2.0" "CC-BY-4.0"} (dir->expressions  ".")))  ; Failing due to https://github.com/spdx/license-list-XML/issues/1960
))

(deftest zip->expressions-tests
  (testing "Nil, empty, or blank zip file name"
    (is (nil?                                      (zip->expressions nil)))
    (is (thrown? java.io.FileNotFoundException     (zip->expressions "")))            ; Note the hodgepodge of different thrown exception types here - java.util.zip is a mess!
    (is (thrown? java.nio.file.NoSuchFileException (zip->expressions "       ")))
    (is (thrown? java.nio.file.NoSuchFileException (zip->expressions "\n")))
    (is (thrown? java.nio.file.NoSuchFileException (zip->expressions "\t"))))
  (testing "Non-existent zip file"
    (is (thrown? java.nio.file.NoSuchFileException (zip->expressions "this_zip_file_does_not_exist"))))
  (testing "Invalid zip file"
    (is (thrown? java.util.zip.ZipException (zip->expressions (str test-data-path "/bad.zip")))))
  (testing "Valid zip file"
    (is (valid= #{"Apache-2.0"} (zip->expressions (str test-data-path "/good.zip"))))))

