;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitution-test
  (:require [clojure.test                             :refer [deftest testing is use-fixtures]]
            [clojure.set                              :as set]
            [rencg.api                                :as rencg]
            [lice-comb.impl.utils                     :as lcu]
            [lice-comb.test-boilerplate               :refer [fixture load-edn-resource]]
            [lice-comb.impl.parsing-utils             :refer [done-parsing?]]
            [lice-comb.impl.substitutions.bsd         :as bsd]
            [lice-comb.impl.substitutions.cc          :as cc]
            [lice-comb.impl.substitutions.cddl        :as cddl]
            [lice-comb.impl.substitutions.cpe         :as cpe]
            [lice-comb.impl.substitutions.epl         :as epl]
            [lice-comb.impl.substitutions.gnu         :as gnu]
            [lice-comb.impl.substitutions.hippocratic :as hippocratic]
            [lice-comb.impl.substitutions.others      :as others]
            [lice-comb.impl.substitutions.mpl         :as mpl]
            [lice-comb.impl.substitutions.wtf         :as wtf]))

(use-fixtures :once fixture)

(def bsd-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/bsd.edn")))

(def cddl-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/cddl.edn")))

(def cpe-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/cpe.edn")))

(def agpl-names-d        (delay (load-edn-resource "lice_comb/data/name_lists/agpl.edn")))
(def lgpl-names-d        (delay (load-edn-resource "lice_comb/data/name_lists/lgpl.edn")))
(def gpl-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/gpl.edn")))

(def hippocratic-names-d (delay (load-edn-resource "lice_comb/data/name_lists/hippocratic.edn")))

(def mpl-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/mpl.edn")))

(def wtf-names-d         (delay (load-edn-resource "lice_comb/data/name_lists/wtf.edn")))

(deftest bsd-sub-tests
  (testing "Nil, empty"
    (is (nil? (bsd/sub nil)))
    (is (nil? (bsd/sub []))))
  (testing "BSD substitutions"
    (run! #(is (done-parsing? (bsd/sub [%])) (str "Failed to substitute \"" % "\"")) @bsd-names-d)))

(deftest cddl-sub-tests
  (testing "Nil, empty"
    (is (nil? (cddl/sub nil)))
    (is (nil? (cddl/sub []))))
  (testing "CDDL substitutions"
    (run! #(is (done-parsing? (cddl/sub [%])) (str "Failed to substitute \"" % "\"")) @cddl-names-d)))

(deftest cpe-sub-tests
  (testing "Nil, empty"
    (is (nil? (cpe/sub nil)))
    (is (nil? (cpe/sub []))))
  (testing "Classpath-exception substitutions"
    (run! #(is (done-parsing? (cpe/sub [%])) (str "Failed to substitute \"" % "\"")) @cpe-names-d)))

(deftest gnu-sub-tests
  (testing "Nil, empty"
    (is (nil? (gnu/sub nil)))
    (is (nil? (gnu/sub []))))
  (testing "AGPL substitutions"
    (run! #(is (done-parsing? (gnu/sub [%])) (str "Failed to substitute \"" % "\"")) @agpl-names-d))
  (testing "LGPL substitutions"
    (run! #(is (done-parsing? (gnu/sub [%])) (str "Failed to substitute \"" % "\"")) @lgpl-names-d))
  (testing "GPL substitutions"
    (run! #(is (done-parsing? (gnu/sub [%])) (str "Failed to substitute \"" % "\"")) @gpl-names-d)))

(deftest hippocratic-sub-tests
  (testing "Nil, empty"
    (is (nil? (hippocratic/sub nil)))
    (is (nil? (hippocratic/sub []))))
  (testing "Hippocratic substitutions"
    (run! #(is (done-parsing? (hippocratic/sub [%])) (str "Failed to substitute \"" % "\"")) @hippocratic-names-d)))

(deftest mpl-sub-tests
  (testing "Nil, empty"
    (is (nil? (mpl/sub nil)))
    (is (nil? (mpl/sub []))))
  (testing "MPL substitutions"
    (run! #(is (done-parsing? (mpl/sub [%])) (str "Failed to substitute \"" % "\"")) @mpl-names-d)))

(deftest wtf-sub-tests
  (testing "Nil, empty"
    (is (nil? (wtf/sub nil)))
    (is (nil? (wtf/sub []))))
  (testing "WTFPL substitutions"
    (run! #(is (done-parsing? (wtf/sub [%])) (str "Failed to substitute \"" % "\"")) @wtf-names-d)))

