6;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.gnu
  "Helper functionality related to substituting matches for the GNU family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [clojure.set                        :as set]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; Unlike the other substitution namespaces, for the GNU family we use a "word salad" strategy for matching.  Basically
; this involves matching any words that are known to appear in a GNU family license *in any order*.  So even nonsensical
; values that don't appear in the wild will match, such as "Open Source License License GNU v3 Licensed Under or later"
;
; For this reason, the `sub`stitution function in this namespace should be called *last, after all other substitutions
; have already been performed*.
;
; We do this because of the sheer number of variations of GNU family license names in the wild.

(defn- base-id
  "Returns the 'base id' of a GNU family id.  This is the id with version
  suffixes removed (`+`, `-or-later`, or `-only`)."
  [id]
  (when id
    (-> (s/trim id)
        (s/replace #"\+\z"        "")
        (s/replace #"-or-later\z" "")
        (s/replace #"-only\z"     ""))))

(defn- suffix
  "Returns a keyword representing the suffix of `id`; one of:
  * `:only` - `-only` suffix
  * `:or-later` - `+` or `-or-later` suffix

  If no suffix is found, returns `nil`."
  [id]
  (when id
    (cond
      (s/ends-with? id "-only")                                :only
      (or (s/ends-with? id "+") (s/ends-with? id "-or-later")) :or-later)))

(defn- gnu-id-comparator
  "Compares GNU family ids, by putting the `-only` variants first.  This order
  is important as there are certain license name values that both the -only and
  -or-later regex will match, but the -only id is the more correct choice."
  [id1 id2]
  (if (= id1 id2)
    0
    (if (= (base-id id1) (base-id id2))
      (case [(suffix id1) (suffix id2)]  ; Note: don't need all possibilities as the if statement takes care of some of them
        [nil       :only]     1
        [nil       :or-later] 1
        [:only     nil]       -1
        [:or-later nil]       -1
        [:only     :or-later] -1
        [:or-later :only]     1
        [:or-later :or-later] 0)  ; This can occur due to +/-or-later being equivalent
      (compare id1 id2))))

; All GNU family license ids, including deprecated ones - we report that we match all GPL ids so that they're removed from lice-comb.impl.substitutions.others matching
(def ids-d (delay (set (filter #(or (lcisu/agpl-license? %) (lcisu/lgpl-license? %) (lcisu/gpl-license? %)) (map :id @lcis/full-license-list-d)))))

; Undeprecated GNU family license ids (these ones are used for matching)
(def ^:private agpl-license-ids-d (delay (sort gnu-id-comparator (map :id (filter #(and (lcisu/agpl-license? (:id %)) (not (:deprecated? %))) @lcis/full-license-list-d)))))
(def ^:private lgpl-license-ids-d (delay (sort gnu-id-comparator (map :id (filter #(and (lcisu/lgpl-license? (:id %)) (not (:deprecated? %))) @lcis/full-license-list-d)))))
(def ^:private gpl-license-ids-d  (delay (sort gnu-id-comparator (map :id (filter #(and (lcisu/gpl-license?  (:id %)) (not (:deprecated? %))) @lcis/full-license-list-d)))))

(defn- match->ei
  "Construct an expression-info map from `m`, a map returned from a rencg regex
  match/find match."
  [variant m]
  (let [match              (s/trim (:match m))
        version-present?   (boolean (lcisu/get-rencgs m ["versionNumber"] false))
        default-version    (if (= variant "LGPL") "2.0" "1.0")  ; Note: on the advice of the SPDX technical team, default to earliest version when version not present
        version            (lcisu/get-rencgs m ["versionNumber"] default-version)
        version            (s/replace version #"[\s\p{Punct}]+" ".")   ; Turn other version number point separators into . (undercore appears in at least one license name, for example)
        [confidence confidence-explanations]
                           (if version-present?
                             (if (s/includes? version ".")
                               [:high]
                               [:medium #{:partial-version}])
                             [:low #{:missing-version}])
        version            (if (s/includes? version ".")
                             version
                             (str version ".0"))
        [suffix confidence-explanations]
                           (cond (contains? m "orLater") ["or-later" confidence-explanations]
                                 (contains? m "only")    ["only"     confidence-explanations]
                                 :else                   [(if version-present? "only" "or-later")  ; Note: on the advice of SPDX technical team, default to "or later" variant if version or suffix not present
                                                          (set/union #{:missing-version-suffix} confidence-explanations)])
        id                 (str variant "-" version  "-" suffix)
        [id confidence confidence-explanations]
                           (if (lcisu/listed-id? id)
                             [id confidence confidence-explanations]
                             [(str variant "-" default-version "-or-later")  ; Note: on the advice of SPDX technical team, default to "or later" variant if version not valid
                              :low
                              (set/union #{:invalid-version-for-variant} confidence-explanations)])]
    (merge {:id         (lcisu/assert-listed-id id)
            :type       :concluded
            :confidence confidence
            :strategy   :regex-matching
            :source     (list match)}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))

; Generic GNU family regex fragments
(def ^:private fre-gnu-words (lcir/re-concat #"The|GNU|GPL|Genere?al|Pub?lic|Licen[cs]ed?([\s\-–—]+Under)?|Open[\s\-–—]+Source|FOSS|OSS" "|" lcir/fre-date))

; AGPL regexes
(def ^:private fre-agpl-words         (lcir/re-concat fre-gnu-words "|" #"AGPL|Affero"))
(def ^:private fre-agpl-words-and-ver (lcir/re-concat fre-agpl-words "|" lcir/fre-ver-and-suf ))
(def re-agpl                          (lcir/re-concat #"(?iuUx)(?<!\w)"  ; Only public for ease of testing
                                                      "\n\n#### Leading words ####\n"
                                                      "((" fre-agpl-words ")" #"[\s\-–—,\(\)]+" ")*"
                                                      "\n\n#### Matching words ####\n"
                                                      #"(?<agpl>(A\s?GPL|Affero))"  ; "Primary key" for AGPL
                                                      "\n\n#### Trailing words and version ####\n"
                                                      "(" #"[\s\-–—,\(\)]*" "(" fre-agpl-words-and-ver "))*"  ; Whitespace needs to be optional here for values such as "AGPLv3+"
                                                      #"(?!\w)"))

; LGPL regexes
(def ^:private fre-lgpl-words         (lcir/re-concat fre-gnu-words "|" #"LGPL|(Lesser([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Library)?)|(Library([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Lesser)?)"))
(def ^:private fre-lgpl-words-and-ver (lcir/re-concat fre-lgpl-words "|" lcir/fre-ver-and-suf ))
(def re-lgpl                          (lcir/re-concat #"(?iuUx)(?<!\w)"  ; Only public for ease of testing
                                                      "\n\n#### Leading words ####\n"
                                                      "((" fre-lgpl-words ")" #"[\s\-–—,\(\)]+" ")*"
                                                      "\n\n#### Matching words ####\n"
                                                      #"(?<lgpl>(L\s?GPL|((GNU|GPL)[\s\-–—,\(\)]+Lesser([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Library)?)|((GNU|GPL)[\s\-–—,\(\)]+Library([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Lesser)?|(Lesser([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Library)?[\s\-–—,\(\)]+(GNU|GPL|General))|(Library([\s\-–—,\(\)]+or[\s\-–—,\(\)]+Lesser)?[\s\-–—,\(\)]+(GNU|GPL|General)))))"  ; "Primary key" for LGPL
                                                      "\n\n#### Trailing words and version ####\n"
                                                      "(" #"[\s\-–—,\(\)]*" "(" fre-lgpl-words-and-ver "))*"  ; Whitespace needs to be optional here for values such as "LGPLv3+"
                                                      #"(?!\w)"))

; GPL regexes
(def ^:private fre-gpl-words         fre-gnu-words)  ; GPL has no extra words
(def ^:private fre-gpl-words-and-ver (lcir/re-concat fre-gpl-words "|" lcir/fre-ver-and-suf))
(def re-gpl                          (lcir/re-concat #"(?iuUx)(?<!\w)"  ; Only public for ease of testing
                                                     "\n\n#### Leading words ####\n"
                                                     "((" fre-gpl-words ")" #"[\s\-–—,\(\)]+" ")*"
                                                     "\n\n#### Matching words ####\n"
                                                     #"(?<gpl>(GNU|GPL|(Genere?al([\s\-–—]+Pub?lic)?([\s\-–—]+Licen[cs]e)?)))"  ; "Primary key" for GPL
                                                     "\n\n#### Trailing words and version ####\n"
                                                     "(" #"[\s\-–—,\(\)]*" "(" fre-gpl-words-and-ver "))*"  ; Whitespace needs to be optional here for values such as "GPLv3+"
                                                     #"(?!\w)"))

(def ^:private pairs-d (delay (concat ; AGPL matching pairs
                                      [[re-agpl (partial match->ei "AGPL")]]
                                      (lcisu/spdx-match-pairs @agpl-license-ids-d)  ;####TODO: IS THIS EVEN NEEDED?
                                      ; LGPL matching pairs
                                      [[re-lgpl (partial match->ei "LGPL")]]
                                      (lcisu/spdx-match-pairs @lgpl-license-ids-d)  ;####TODO: IS THIS EVEN NEEDED?
                                      ; GPL matching pairs (these must go after AGPL and LGPL)
                                      [[re-gpl (partial match->ei "GPL")]]
                                      (lcisu/spdx-match-pairs @gpl-license-ids-d)  ;####TODO: IS THIS EVEN NEEDED?
                                      )))

(defn sub
  "Substitutes any GNU family licenses found in the `String`s in `coll` with an
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
  @pairs-d
  nil)
