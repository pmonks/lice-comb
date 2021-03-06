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

(ns lice-comb.maven
  "Maven related functionality, mostly related to POMs."
  (:require [clojure.string        :as s]
            [clojure.java.io       :as io]
            [clojure.data.xml      :as xml]
            [clojure.tools.logging :as log]
            [xml-in.core           :as xi]
            [clojure.java.shell    :as sh]
            [lice-comb.spdx        :as spdx]
            [lice-comb.utils       :as u]))

(def ^:private local-maven-repo
  (try
    (let [sh-result (sh/sh "mvn" "help:evaluate" "-Dexpression=settings.localRepository" "-q" "-DforceStdout")]
      (if (= 0 (:exit sh-result))
        (s/trim (:out sh-result))
        (str (System/getProperty "user.home") "/.m2/repository")))
    (catch java.io.IOException _
      (str (System/getProperty "user.home") "/.m2/repository"))))

(def ^:private remote-maven-repos #{"https://repo1.maven.org/maven2" "https://repo.clojars.org"})

(defn- uri-resolves?
  "Does the given URI resolve (i.e. does the resource it points to exist)?"
  [^java.net.URI uri]
  (and uri
       (let [http (doto ^java.net.HttpURLConnection (.openConnection (.toURL uri))
                        (.setRequestMethod "HEAD"))]
         (= 200 (.getResponseCode http)))))

(defn pom-uri-for-gav
  "Attempts to locate the POM for the given GAV, which is a URI that may point to a file in the local Maven repository or a remote Maven repository (e.g. on Maven Central or Clojars)."
  ([{:keys [group-id artifact-id version]}] (pom-uri-for-gav group-id artifact-id version))
  ([group-id artifact-id version]
   (when (and (not (s/blank? group-id))
              (not (s/blank? artifact-id))
              (not (s/blank? version)))
     (let [gav-path  (str (s/replace group-id "." "/") "/" artifact-id "/" version "/" artifact-id "-" version ".pom")
           local-pom (io/file (str local-maven-repo "/" gav-path))]
       (if (and (.exists local-pom)
                (.isFile local-pom))
         (.toURI local-pom)
         (first (filter uri-resolves? (map #(java.net.URI. (str % "/" gav-path)) remote-maven-repos))))))))

(defn- licenses-from-pair
  "Attempts to determine the license(s) (a set) from a POM license name/URL pair."
  [{:keys [name url]}]
  (if-let [license (spdx/uri->id url)]
    #{license}
    (if-let [licenses (spdx/name->ids name)]
      licenses
      (when name #{(str "NON-SPDX-Unknown (" name ")")}))))   ; Last resort - return the license name

(xml/alias-uri 'pom "http://maven.apache.org/POM/4.0.0")

(defmulti pom->ids
  "Attempt to detect the license(s) reported in a pom.xml file. pom may be a java.io.InputStream, or anything that can be opened by clojure.java.io/input-stream."
  {:arglists '([pom])}
  type)

(defmethod pom->ids java.io.InputStream
  [pom-is]
  (let [pom-xml        (xml/parse pom-is)
        licenses       (seq (xi/find-all pom-xml [::pom/project ::pom/licenses ::pom/license]))
        licenses-no-ns (seq (xi/find-all pom-xml [:project      :licenses      :license]))]        ; Note: a few rare pom.xml files are missing the xmlns declation (e.g. software.amazon.ion/ion-java) - this case catches those
    (if (or licenses licenses-no-ns)
      ; Licenses block exists - process it
      (let [name-uri-pairs (u/nset (concat (u/map-pad #(hash-map :name (u/strim %1) :url (u/strim %2)) (xi/find-all licenses       [::pom/name]) (xi/find-all licenses       [::pom/url]))
                                           (u/map-pad #(hash-map :name (u/strim %1) :url (u/strim %2)) (xi/find-all licenses-no-ns [:name])      (xi/find-all licenses-no-ns [:url]))))]
        (u/nset (mapcat licenses-from-pair name-uri-pairs)))
      ; License block doesn't exist, so attempt to lookup the parent pom and get it from there
      (let [parent       (seq (xi/find-first pom-xml [::pom/project ::pom/parent]))
            parent-no-ns (seq (xi/find-first pom-xml [:project      :parent]))
            parent-gav   (merge {}
                                (when parent       {:group-id    (u/strim (first (xi/find-first parent       [::pom/groupId])))
                                                    :artifact-id (u/strim (first (xi/find-first parent       [::pom/artifactId])))
                                                    :version     (u/strim (first (xi/find-first parent       [::pom/version])))})
                                (when parent-no-ns {:group-id    (u/strim (first (xi/find-first parent-no-ns [:groupId])))
                                                    :artifact-id (u/strim (first (xi/find-first parent-no-ns [:artifactId])))
                                                    :version     (u/strim (first (xi/find-first parent-no-ns [:version])))}))]
        (when-not (empty? parent-gav)
          (pom->ids (pom-uri-for-gav parent-gav)))))))   ; Note: naive (stack consuming) recursion

(defmethod pom->ids :default
  [pom]
  (when pom
    (with-open [pom-is (io/input-stream pom)]
      (if-let [pom-licenses (pom->ids pom-is)]
        pom-licenses
        (log/info (str "'" pom "'") "contains no license information")))))
