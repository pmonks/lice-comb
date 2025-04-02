;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.others
  "Helper functionality related to substituting matches for any other licenses
  or exceptions not otherwise handled explicitly.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                           :as s]
            [clojure.set                              :as set]
            [rencg.api                                :as rencg]
            [wreck.api                                :as re]
            [lice-comb.impl.spdx                      :as lcis]
            [lice-comb.impl.regexes                   :as lcir]
            [lice-comb.impl.families                  :as lcif]
            [lice-comb.impl.substitutions.bsd         :as bsd]
            [lice-comb.impl.substitutions.cc          :as cc]
            [lice-comb.impl.substitutions.cddl        :as cddl]
            [lice-comb.impl.substitutions.cpe         :as cpe]
            [lice-comb.impl.substitutions.epl         :as epl]
            [lice-comb.impl.substitutions.gnu         :as gnu]
            [lice-comb.impl.substitutions.hippocratic :as hippocratic]
            [lice-comb.impl.substitutions.mpl         :as mpl]
            [lice-comb.impl.substitutions.wtf         :as wtf]
            [lice-comb.impl.substitutions.utils       :as lcisu]))

; Ids of all of the "other" licenses i.e. those without special-cased support
; Note: includes both license AND exception identifiers
(def ids-d (delay (apply disj (set/union @lcis/license-ids-d @lcis/exception-ids-d)
                              (concat @bsd/ids-d @cc/ids-d @cddl/ids-d @cpe/ids-d
                                      @epl/ids-d @gnu/ids-d @hippocratic/ids-d
                                      @mpl/ids-d @wtf/ids-d))))

; License families that we'll do "deversioned" matching on
(def ^:private families-d (delay (lcif/ids->families (remove #(s/starts-with? % "LZMA-SDK") @ids-d))))

(def ^:private re-name-and-version (re/join #"(?iuU)(?<name>.+?)" (re/opt-grp lcir/fre-version)))

(defn- id->deversioned-name
  "Converts `id` into its name, with any version components removed."
  [id]
  (when-let [n (:name (lcis/id->info id))]
    (get (rencg/re-matches-ncg re-name-and-version n) "name")))

(defn- id->deversioned-name-regex
  "Gets the name of the license with identifier `id`, removed the version from
  it, then turns that into a name regex."
  [id]
  (when-let [n (id->deversioned-name id)]
    (lcir/name->regex n)))

; Pairs of regex/fn based on listed SPDX license and exception names and ids
(def ^:private pairs-d (delay (concat
  ; Default regex matching based on ids
  (lcisu/spdx-match-pairs @ids-d)

  ; Regex matching based on names minus versions
  (map #(vec [(id->deversioned-name-regex %) (lcisu/simple-regex-match-ei-fn % :low #{:missing-version})])
       (map #(last (val %))
            (remove #(or (s/starts-with? (key %) "OPL")   ; License name is too short & generic for matching
                         (s/starts-with? (key %) "OSL"))  ; License name is too short & generic for matching
                    @families-d)))

  ; Regex matching based on ids minus versions
  (map #(vec [(lcir/id->regex (key %)) (lcisu/simple-regex-match-ei-fn (last (val %)) :low #{:missing-version})]) @families-d))))

(defn sub
  "Substitutes any licenses found in the `String`s in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  @ids-d
  @families-d
  @pairs-d
  nil)

