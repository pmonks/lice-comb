;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.faux-parse-test
  (:require [clojure.test               :refer [deftest testing is use-fixtures]]
            [clojure.string             :as s]
            [lice-comb.test-boilerplate :refer [fixture]]
            [lice-comb.impl.faux-parse  :refer [replace-in-strings]]))

(use-fixtures :once fixture)

(deftest replace-in-strings-tests
  (testing "Nil values"
    (is (nil? (replace-in-strings nil nil    nil)))
    (is (nil? (replace-in-strings nil #"foo" nil)))
    (is (nil? (replace-in-strings nil nil    identity)))
    (is (nil? (replace-in-strings nil #"foo" identity))))
  (testing "Non-replacements"
    (is (= []                               (replace-in-strings []                               nil     nil)))
    (is (= []                               (replace-in-strings []                               #"foo"  nil)))
    (is (= ["foo"]                          (replace-in-strings ["foo"]                          nil     nil)))
    (is (= ["foo" "bar"]                    (replace-in-strings ["foo" "bar"]                    nil     nil)))
    (is (= ["foo" "bar"]                    (replace-in-strings ["foo" "bar"]                    #"blah" nil)))
    (is (= ["foo" "bar" :blah]              (replace-in-strings ["foo" "bar" :blah]              #"blah" nil)))
    (is (= ['(:foo) #{:bar} {:blah "blah"}] (replace-in-strings ['(:foo) #{:bar} {:blah "blah"}] #"blah" nil))))
  (testing "Simple replacements"
    (is (= ["FOO"]              (replace-in-strings ["foo"]            #"foo"      "FOO")))
    (is (= ["FOO"]              (replace-in-strings ["foo"]            #"foo"      #(s/upper-case (:match %)))))
    (is (= ["FOO"]              (replace-in-strings ["fOo"]            #"(?i:foo)" #(s/upper-case (:match %)))))
    (is (= ["foo" "BAR" "blah"] (replace-in-strings ["foobarblah"]     #"(?i:bar)" #(s/upper-case (:match %)))))
    (is (= ["FOO" "FOO" "FOO"]  (replace-in-strings ["foofoofoo"]      #"(?i:foo)" #(s/upper-case (:match %)))))
    (is (= ["FOO" :foo "FOO"]   (replace-in-strings ["foo" :foo "foo"] #"(?i:foo)" #(s/upper-case (:match %))))))
  (testing "Non-string replacements"
    (is (= [:foo]                     (replace-in-strings ["foo"]        #"(?i:foo)" #(keyword (:match %)))))
    (is (= ["foo" :bar "blah"]        (replace-in-strings ["foobarblah"] #"(?i:bar)" #(keyword (:match %))))))
  (testing "1:N replacements, with concatenation (lists)"
    (is (= ["FOO"]                     (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (s/upper-case match))))))
    (is (= [:foo]                      (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (keyword match))))))
    (is (= [{:foo "foo"}]              (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (hash-map (keyword match) match))))))
    (is (= ["FOO" "OOF"]               (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (s/upper-case match) (s/upper-case (s/reverse match)))))))
    (is (= [:foo :oof]                 (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (keyword match) (keyword (s/reverse match)))))))
    (is (= [{:foo "foo"} {:oof "oof"}] (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (list (hash-map (keyword match) match) (hash-map (keyword (s/reverse match)) (s/reverse match)))))))
    (is (= ["foo" :bar :BAR "blah"]    (replace-in-strings ["foobarblah"] #"(?i:bar)" #(let [match (:match %)] (list (keyword match) (keyword (s/upper-case match))))))))
  (testing "1:N replacements, without concatenation (only lists get flattened)"
    (is (= [["FOO" "OOF"]]            (replace-in-strings ["foo"]        #"(?i:foo)" #(let [match (:match %)] (vector (s/upper-case match) (s/upper-case (s/reverse match)))))))
    (is (= ["foo" [:bar :BAR] "blah"] (replace-in-strings ["foobarblah"] #"(?i:bar)" #(let [match (:match %)] (vector (keyword match) (keyword (s/upper-case match)))))))))
