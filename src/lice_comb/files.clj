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

(ns lice-comb.files
  "Functionality related to finding and determining license information from
  files and directories."
  (:require [clojure.string       :as s]
            [clojure.set          :as set]
            [clojure.java.io      :as io]
            [lice-comb.matching   :as lcmtch]
            [lice-comb.maven      :as lcmvn]
            [lice-comb.impl.utils :as lcu]))

(def ^:private probable-license-filenames #{"pom.xml" "license" "license.txt" "copying" "unlicense"})   ;TODO: consider "license.md" and #".+\.spdx" (see https://github.com/spdx/spdx-maven-plugin for why the latter is important)...

(defn probable-license-file?
  "Returns true if the given file-like thing (String, File, ZipEntry) is a
  probable license file, false otherwise."
  [f]
  (and (not (nil? f))
       (let [fname (s/lower-case (lcu/filename f))]
         (and (not (s/blank? fname))
              (or (contains? probable-license-filenames fname)
                  (s/ends-with? fname ".pom"))))))

(defn probable-license-files
  "Returns all probable license files in the given directory, recursively, as a
  set of java.io.File objects. dir may be a String or a java.io.File, both of
  which must refer to a directory."
  [dir]
  (when dir
    (let [dir (io/file dir)]
      (if (.exists dir)    ; Note: we have to do this, because file-seq does weird things when handed a file that doesn't exist
        (if (.isDirectory dir)
          (lcu/nset (filter #(and (.isFile ^java.io.File %) (probable-license-file? %)) (file-seq (io/file dir))))
          (throw (java.nio.file.NotDirectoryException. (str dir))))
        (throw (java.io.FileNotFoundException. (str dir)))))))

(defn file->expressions
  "Attempts to determine the SPDX license expression(s) (a set) from the given
  file (an InputStream or something that can have an io/input-stream opened on
  it). If an InputStream is provided, the associated filename should also be
  provided as the second parameter (it is unnecessary in other cases).

  The result has metadata attached that describes how the identifiers in the
  expression(s) were determined."
  ([f] (file->expressions f (lcu/filename f)))
  ([f fname]
   (when (and f fname)
     (let [fname (s/lower-case fname)]
       (cond (= fname "pom.xml")         (lcmvn/pom->expressions f)
             (s/ends-with? fname ".pom") (lcmvn/pom->expressions f)
             :else                       (lcmtch/text->ids (io/input-stream f)))))))   ; Default is to assume it's a plain text file containing license text(s)

(defn dir->expressions
  "Attempt to detect the SPDX license expression(s) (a set) in a directory. dir
  may be a String or a java.io.File, both of which must refer to a
  readable directory.

  The result has metadata attached that describes how the identifiers in the
  expression(s) were determined."
  [dir]
  (when dir
;####TODO: MERGE METADATA MAPS AND EMBELLISH :source!!!!
    (lcu/nset (mapcat file->expressions (probable-license-files dir)))))

(defn zip->expressions
  "Attempt to detect the SPDX license expression(s) in a ZIP file. zip may be a
  String or a java.io.File, both of which must refer to a ZIP-format compressed
  file.

  The result has metadata attached that describes how the identifiers in the
  expression(s) were determined."
  [zip]
  (when zip
    (let [zip-file (io/file zip)]
      (java.util.zip.ZipFile. zip-file)  ; This no-op forces validation of the zip file - ZipInputStream does not reliably perform validation
      (with-open [zip-is (java.util.zip.ZipInputStream. (io/input-stream zip-file))]
        (loop [result #{}
               entry  (.getNextEntry zip-is)]
          (if entry
            (if (probable-license-file? entry)
;####TODO: MERGE METADATA MAPS AND EMBELLISH :source!!!!
              (recur (set/union result (file->expressions zip-is (lcu/filename entry))) (.getNextEntry zip-is))
              (recur result                                                             (.getNextEntry zip-is)))
            (doall (some-> (seq result) set))))))))  ; De-lazy the result before we exit the with-open scope
