;
; Copyright © 2024 Peter Monks
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

(ns lice-comb.impl.spdx-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [lice-comb.test-boilerplate :refer [fixture lic-ids-d exc-ids-d license-list-d exception-list-d]]
            [lice-comb.impl.spdx        :refer [init! id->regex name->regex]]))

(use-fixtures :once fixture)

(deftest init!-tests
  (testing "Nil response"
    (is (nil? (init!)))))

(deftest id->regex-tests
  (testing "All id regexes match at least their associated id"
    ; We use the full set of ids here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-matches (id->regex %) %))) (str "Regex doesn't match id for license id " %))   @lic-ids-d)
    (run! #(is (not (nil? (re-matches (id->regex %) %))) (str "Regex doesn't match id for exception id " %)) @exc-ids-d))
  ; Note: this test is combinatorial (results in ~540,000 assertions!), but completes in < 10s
  (testing "All id regexes do NOT match any other ids"
    (let [all-ids (into @lic-ids-d @exc-ids-d)]
      (run! #(let [re          (id->regex %)
                   ids-to-test (disj all-ids %)]
               (run! (fn [id-to-test] (is (nil? (re-matches re id-to-test)) (str "Regex for id " % " erroneously matches id " id-to-test))) ids-to-test))
            all-ids))))

(deftest name->regex-tests
  (testing "All name regexes match at least their associated name"
    ; We use the full license lists here, rather than the ones lice-comb uses for detection, since the real world may contain anything
    (run! #(is (not (nil? (re-matches (name->regex (:name %)) (:name %)))) (str "Regex doesn't match name for license name "   (:name %))) @license-list-d)
    (run! #(is (not (nil? (re-matches (name->regex (:name %)) (:name %)))) (str "Regex doesn't match name for exception name " (:name %))) @exception-list-d))
  ; Note: this test is combinatorial (results in ~540,000 assertions!), but completes in < 10s
  (testing "All name regexes do NOT match other names"
    (let [all-names (set (concat (map :name @license-list-d)
                                 (map :name @exception-list-d)))]
      (run! #(let [re            (name->regex %)
                   names-to-test (disj all-names %)]
               (run! (fn [name-to-test] (is (nil? (re-matches re name-to-test)) (str "Regex for name " % " erroneously matches name " name-to-test))) names-to-test))
            all-names))))
