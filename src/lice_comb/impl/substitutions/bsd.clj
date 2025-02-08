;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.bsd
  "Helper functionality related to substituting matches for the BSD family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [clojure.set                        :as set]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.utils               :as lciu]
            [lice-comb.impl.parsing-utils       :as lcipu]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (map :id (filter #(s/starts-with? (:id %) "BSD-") @lcis/full-license-list-d)))))

(def ^:private pairs-d (delay (concat
  [
  ;####TODO: IMPLEMENT ME!!!!
  ]
  (lcisu/spdx-match-pairs @ids-d))))

(defn sub
  "Substitutes any BSD licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it."
  []
  (lcis/init!)
  @ids-d
  @pairs-d
  nil)








(comment

(defn- bsd-id-constructor
  "Construct an expression-info map from `m`, a map returned from a rencg regex
  match/find."
  [m]
  (let [clause-count1             (let [s (lcipu/get-rencgs m ["clauseCount"])
                                        n (lciu/digit-name-to-number s)]
                                    (if n
                                      n
                                      s))
        clause-count2             (let [s (lcipu/get-rencgs m ["clausecount2"])
                                        n (lciu/digit-name-to-number s)]
                                    (if n
                                      n
                                      s))
        preferred-clause-count    (case [(number? clause-count1) (number? clause-count2)]
                                    [true true]   clause-count1
                                    [true false]  clause-count1
                                    [false true]  clause-count2
                                    (if (contains? #{"simplified" "new" "revised" "modified" "aduna"} clause-count1)
                                      clause-count1
                                      clause-count2))
        [clause-count confidence confidence-explanations]
                                  (case preferred-clause-count
                                    (2 "simplified")                       ["2" :high]
                                    (3 "new" "revised" "modified" "aduna") ["3" :high]
                                    (4 "original")                         ["4" :high]
                                    [4 :low #{:missing-clause-count}])  ; Note: we default to 4 clause, since it was the original form of the BSD license
        suffix                    (case (lcipu/get-rencgs m ["suffix" "clausecount2"])  ; Note: when the clause count is missing, the suffix can end up being captured by the clausecount2 capturing group
                                    "patent"                                              "Patent"
                                    "views"                                               "Views"
                                    "attribution"                                         "Attribution"
                                    "clear"                                               "Clear"
                                    "lbnl"                                                "LBNL"
                                    "hp"                                                  "HP"
                                    "sun"                                                 "Sun"
                                    "flex"                                                "flex"
                                    "freebsd"                                             "FreeBSD"
                                    "netbsd"                                              "NetBSD"
                                    "modification"                                        "Modification"
                                    ("no military license" "no military licence")         "No-Military-License"
                                    ("no nuclear license 2014" "no nuclear licence 2014") "No-Nuclear-License-2014"
                                    ("no nuclear license" "no nuclear licence")           "No-Nuclear-License"
                                    "no nuclear warranty"                                 "No-Nuclear-Warranty"
                                    "open mpi"                                            "Open-MPI"
                                    "shortened"                                           "Shortened"
                                    "uc"                                                  "UC"
                                    "darwin"                                              "Darwin"
                                    "acpica"                                              "acpica"
                                    nil)
        base-id                   (str "BSD-" clause-count "-Clause")
        id-with-suffix            (str base-id "-" suffix)]
    (if suffix
      (if (contains? @lcis/license-ids-d id-with-suffix)  ; Not all suffixes are valid with all BSD clause counts, so check that it's valid before returning it
        [id-with-suffix confidence confidence-explanations]
        [(lcipu/assert-listed-id base-id) :low (set/union #{:invalid-suffix} confidence-explanations)])  ; We got a suffix but it wasn't valid, which lowers our confidence
      [(lcipu/assert-listed-id base-id) confidence confidence-explanations])))                       ; We didn't get a suffix






(defn- regex-match->ei
  "Construct an expression-info map from `m`, a map returned from a rencg regex
  match/find."
  [m]
  (let [version-present?   (boolean (lcipu/get-rencgs m ["versionNumber"] false))
        version            (lcipu/get-rencgs m ["versionNumber"] "2.0")
        version            (s/replace version #"\p{Punct}+" ".")
        [confidence confidence-explanations]
                           (if version-present?
                             (if (s/includes? version ".")
                               [:high]
                               [:medium #{:partial-version}])
                             [:low #{:missing-version}])
        version            (if (s/includes? version ".")
                             version
                             (str version ".0"))
        suffix             (when (contains? m "orLater") "+")
        id                 (str "MPL-" version suffix)]
    (merge {:id         (lcipu/assert-listed-id id)
            :type       :concluded
            :confidence confidence
            :strategy   :regex-matching
            :source     (list (:match m))}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))

(def ^:private re-sub-pairs (delay (concat
  (map vector (map lcis/id->name->regex @ids-d) (repeat regex-match->ei))
  (map vector (map lcis/id->regex       @ids-d) (repeat regex-match->ei))
  [[#"(MPL|Mozilla[\s\-–—]+([\s\-–—]+Public)?([\s\-–—]+Licen?[cs]e)?)" regex-match->ei]])))  ; Match version-less license name last

(defn sub
  "Substitutes any BSD licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcipu/sub-res @re-sub-pairs coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  (lcipu/init!)
  @ids-d
  @re-sub-pairs
  nil)
)