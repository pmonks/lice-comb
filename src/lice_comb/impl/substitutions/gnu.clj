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
  "Compares GNU family ids, by putting the `-only` variants first."
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

(defn- gnu-ei-fn
  "Construct an expression-info map from `m`, a map returned from a rencg regex
  match/find."
  [variant m]
  (let [match              (:match m)
        version-present?   (boolean (lcisu/get-rencgs m ["versionNumber"] false))
        version            (lcisu/get-rencgs m ["versionNumber"] (if (= variant "LGPL") "2.0" "1.0"))  ; Note: on the advice of the SPDX technical team, default to earliest version when version not present
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
        [suffix confidence-explanations]
                           (cond (contains? m "orLater") ["or-later" confidence-explanations]
                                 (contains? m "only")    ["only"     confidence-explanations]
                                 :else                   [(if version-present? "only" "or-later")  ; Note: on the advice of SPDX technical team, default to "or later" variant if version suffix not present
                                                          (set/union #{:missing-version-suffix} confidence-explanations)])
        id                 (str variant "-" version  "-" suffix)]
    (merge {:id         (lcisu/assert-listed-id id)
            :type       :concluded
            :confidence confidence
            :strategy   :regex-matching
            :source     (list match)}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))

;####TODO: REMOVE THIS ONCE IT HAS BEEN SUPERCEDED!!!!
; The regex for the GNU family is a nightmare, so we build it up (and test it) in pieces
;(def agpl-re          #"(?<agpl>AGPL|Affero)(\s+GNU)?(\s+Genere?al)?(\s+Pub?lic)?(\s+Licen[cs]e)?(\s+\(?AGPL\)?)?")
;(def lgpl-re          #"(?<lgpl>(GNU\s+)?((Genere?al\s+)?(Library\s+or\s+Lesser|Lesser\s+or\s+Library|Library|Lesser))|((Library\s+or\s+Lesser|Lesser\s+or\s+Library|Library|Lesser)\s+(GNU|GPL|Genere?al)|(L(esser\s)?\s*GPL)))(\s+Genere?al)?(\s+Pub?lic)?(\s+Licen[cs]e)?(\s+\(?L\s*GPL\)?)?")
;(def gpl-re           #"(?<!(Affero|Lesser|Library)\s+)(?<gpl>GNU(?!\s+Classpath)|(?<!(L|A)\s*)GPL|Genere?al\s+Pub?lic\s+Licen[cs]e)(?!\s+(Affero|Library|Lesser|Genere?al\s+Lesser|Genere?al\s+Library|LGPL|AGPL))((\s+General)?(?!\s+(Affero|Lesser|Library))\s+Pub?lic\s+Licen[cs]e)?(\s+\(?GPL\)?)?")
;(def version-re       #"[\s,\-]*(_?V(ersion)?)?[\s\._]*(?<version>\d+([\._]\d+)?)?")
;(def only-or-later-re #"[\s,\-]*((?<only>\(?only\)?)|(\(?or(\s+\(?at\s+your\s+(option|discretion)\)?)?(\s+any)?)?([\s\-]*(?<orLater>lat[eo]r|newer|greater|\+)))?")
;(def gnu-re           (lciu/re-concat "(?x)(?i)(?<!\\w)(\n# Alternative 1: AGPL\n"
;                                      agpl-re
;                                      "\n# Alternative 2: LGPL\n|"
;                                      lgpl-re
;                                      "\n# Alternative 3: GPL\n|"
;                                      gpl-re
;                                      "\n)\n# Version\n"
;                                      version-re
;                                      "\n# Only/or-Later suffix\n"
;                                      only-or-later-re
;                                      #"(?!\w)"))

; only/or-later suffix
(def ^:private suffix-re #"[\s\-–—]*((?<only>only)?|(?<orLater>\+|\(?or[\s\-–—\(]*(at[\s\-–—]+your[\s\\-–—]+(option|discretion))?([\s\-–—\)]*any)?[\s\-–—]+(lat[eo]r|newer)([\s\-–—]+(v|ver|versions?))?\)?)?)?")

; AGPL regexes
(def ^:private agpl-res-d (delay [
  ;####TODO: IMPLEMENT ME!!!!
  ]))

; LGPL regexes
(def ^:private lgpl-res-d (delay [
  ;####TODO: IMPLEMENT ME!!!!
;  #"(?iuU)(?<!\w)(?<lgpl>(The[\s\-–—]+)?(GNU[\s\-–—]+)?(Library|Less[eo]r|Library[\s\-–—]+or[\s\-–—]+Less[eo]r|Less[eo]r[\s\-–—]+or[\s\-–—]+Library)[\s\-–—]+General[\s\-–—]+Public[\s\-–—]+Licen[cs]e([\s\-–—]+\(?L[\s\-–—]*GPL([\s\-–—]*v)?[\s\d\.]*\))?[\s\-–—,]+((v|ver|versions?)[\s\-–—]*)?(?<version>0*\d+(\.d+)*)[\s\-–—]*(?<only>\+|\(?only\)?)?)(?!\w)"
;  #"(?iuU)(?<!\w)(?<lgpl>(The[\s\-–—]+)?(GNU[\s\-–—]+)?(Library|Less[eo]r|Library[\s\-–—]+or[\s\-–—]+Less[eo]r|Less[eo]r[\s\-–—]+or[\s\-–—]+Library)[\s\-–—]+General[\s\-–—]+Public[\s\-–—]+Licen[cs]e([\s\-–—]+\(?L[\s\-–—]*GPL([\s\-–—]*v)?[\s\d\.]*\))?[\s\-–—,]+((v|ver|versions?)[\s\-–—]*)?(?<version>0*\d+(\.d+)*)[\s\-–—]*(?<orLater>\+|\(?or[\s\-–—]*later\)?)?)(?!\w)"
  ]))

; GPL regexes
(def ^:private gpl-res-d (delay [
  ;####TODO: IMPLEMENT ME!!!!
  (lcir/re-concat #"(The[\s\-–—]*)?(GNU[\s\-–—]*)?(GPL|General[\s\-–—]*Public[\s\-–—]*Licen[cs]e)" suffix-re)
  (lcir/re-concat #"(The[\s\-–—]*)?GNU[\s\-–—]*Public[\s\-–—]*Licen[cs]e"                          suffix-re)
  ]))

(def ^:private pairs-d (delay (concat ; AGPL matching pairs
                                      (lcisu/spdx-match-pairs @agpl-license-ids-d)
                                      (map vector @agpl-res-d (repeat (partial gnu-ei-fn "AGPL")))
                                      ; LGPL matching pairs
                                      (lcisu/spdx-match-pairs @lgpl-license-ids-d)
                                      (map vector @lgpl-res-d (repeat (partial gnu-ei-fn "LGPL")))
                                      ; GPL matching pairs (these must go last)
                                      (lcisu/spdx-match-pairs @gpl-license-ids-d)
                                      (map vector @gpl-res-d (repeat (partial gnu-ei-fn "GPL"))))))

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
