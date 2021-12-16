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

(ns lice-comb.deps-test
  (:require [clojure.test    :refer [deftest testing is]]
            [lice-comb.deps  :refer [dep->ids deps-licenses]]))

(deftest dep->ids-tests
  (testing "Nil deps"
    (is (nil? (dep->ids nil))))
  (testing "Valid deps - single license"
    (is (= #{"Apache-2.0"} (dep->ids ['com.github.pmonks/asf-cat {:deps/manifest :mvn :mvn/version "1.0.12"}])))
    (is (= #{"EPL-1.0"}    (dep->ids ['org.clojure/clojure       {:deps/manifest :mvn :mvn/version "1.10.3"}])))
    (is (nil?              (dep->ids ['sci.impl/reflector        {:deps/manifest :mvn :mvn/version "0.0.1"}]))))  ; No licenses in deployed artifacts
  (testing "Valid deps - multi license"
    (is (= #{"GPL-2.0-with-classpath-exception" "MIT"} (dep->ids ['org.checkerframework/checker-compat-qual {:deps/manifest :mvn :mvn/version "2.5.5"}])))))

(deftest deps-licenses-test
  (testing "Nil and empty deps"
    (is (nil? (deps-licenses nil)))
    (is (= {} (deps-licenses {}))))
  (testing "Single deps"
    (is (= {'org.clojure/clojure {:deps/manifest :mvn :mvn/version "1.10.3" :lice-comb/licenses #{"EPL-1.0"}}}
           (deps-licenses {'org.clojure/clojure {:deps/manifest :mvn :mvn/version "1.10.3"}})))
    ; TODO: Test both :mvn and :deps deps
    )
  (testing "Multiple deps"
    (comment
    (is (= {} (deps-licenses {'org.clojure/clojure                                       {:deps/manifest :mvn :mvn/version "1.10.3"}
                              'org.clojure/spec.alpha                                    {:deps/manifest :mvn :mvn/version "0.2.194"}
                              'org.clojure/core.specs.alpha                              {:deps/manifest :mvn :mvn/version "0.2.56"}
                              'org.clojure/data.xml                                      {:deps/manifest :mvn :mvn/version "0.2.0-alpha6"}
                              'org.clojure/data.codec                                    {:deps/manifest :mvn :mvn/version "0.1.0"}
                              'cheshire/cheshire                                         {:deps/manifest :mvn :mvn/version "5.10.1"}
                              'com.fasterxml.jackson.core/jackson-core                   {:deps/manifest :mvn :mvn/version "2.12.4"}
                              'com.fasterxml.jackson.dataformat/jackson-dataformat-smile {:deps/manifest :mvn :mvn/version "2.12.4"}
                              'com.fasterxml.jackson.core/jackson-databind               {:deps/manifest :mvn :mvn/version "2.12.4"}
                              'com.fasterxml.jackson.core/jackson-annotations            {:deps/manifest :mvn :mvn/version "2.12.4"}
                              'com.fasterxml.jackson.dataformat/jackson-dataformat-cbor  {:deps/manifest :mvn :mvn/version "2.12.4"}
                              'tigris/tigris                                             {:deps/manifest :mvn :mvn/version "0.1.2"}
                              'clj-xml-validation/clj-xml-validation                     {:deps/manifest :mvn :mvn/version "1.0.2"}
                              'camel-snake-kebab/camel-snake-kebab                       {:deps/manifest :mvn :mvn/version "0.4.2"}
                              'tolitius/xml-in                                           {:deps/manifest :mvn :mvn/version "0.1.1"}})))
    )
    ; TODO: Test :deps deps
    ; TODO: Test a mixture of :mvn and :deps deps
    ))

