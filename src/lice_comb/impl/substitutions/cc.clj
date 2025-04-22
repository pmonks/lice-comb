;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.cc
  "Helper functionality related to substituting matches for the Creative Commons
  family of licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [wreck.api                          :as re]
            [spdx.licenses                      :as sl]
            [spdx.expressions                   :as sexp]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (concat ["CC0-1.0"] (map :id (filter #(s/starts-with? (:id %) "CC-") @lcis/full-license-list-d))))))


;;
;; EXPRESSION-INFO CONSTRUCTION FROM A MATCH
;;


(defn- valid-clauses
  "Determines a set of valid clauses from match m, returning a tuple of
  `[clauses valid-clauses?]`."
  [m]
  (let [zero?          (or (get m "zero")
                           (get m "publicDomainBefore")  ; Treat "public domain" in the prefix as if it were CC0 (since that's the only combo that makes sense)
                           (get m "publicDomainAfter"))  ; Treat "public domain" in the suffix as if it were CC0 (since that's the only combo that makes sense)
        pddc?          (get m "pddc")
        pdm?           (get m "pdm")
        nc?            (get m "nonCommercial")
        nd?            (get m "noDerivatives")
        sa?            (get m "shareAlike")
        valid-clauses? (not ; Checking for _in_valid clauses is easier than the inverse
                         (or
                           (and nd? sa?)
                           (and zero? (or pddc? pdm? nc? nd? sa?))
                           (and pddc? (or zero? pdm? nc? nd? sa?))
                           (and pdm?  (or zero? pddc? nc? nd? sa?))))
        clauses        (cond
                         zero? ["ZERO"]
                         pddc? ["PDDC"]
                         pdm?  ["PDM"]
                         :else (concat [] (when nc? ["NC"]) (when nd? ["ND"]) (when sa? ["SA"])))]
    [clauses valid-clauses?]))

(def ^:private cc-versions #{"1.0" "2.0" "2.5" "3.0" "4.0"})

(defn- version-number-elements
  "Returns a version number as a tuple of [major minor] found in match `m`,
  stripping any leading zeroes.  One or both elements in the tuple may be `nil`."
  [m]
  (let [raw-version-number (get m "versionNumber")]
    (when-not (s/blank? raw-version-number)
      (let [[_ major minor] (re-matches #"0*(\d+)(?:\.0*(\d+))?(?:\.0+)*" raw-version-number)]
        (when-not (and (s/blank? major) (s/blank? minor))
          [(when-not (s/blank? major) major) (when-not (s/blank? minor) minor)])))))

(defn- valid-version-number
  "Determines a valid version number from match m, based on the clauses,
  returning a tuple of `[version-number version-present? valid-version-number?]`."
  [m clauses]
  (let [[major minor]    (version-number-elements m)
        version-present? (or (not (nil? major)) (not (nil? minor)))
        clauses-set      (set clauses)]
    (if (contains? clauses-set "PDDC")
      ; CC-PDDC doesn't have a version
      [nil true (not version-present?)]  ; This looks wrong, but isn't...
      (if (or (contains? clauses-set "ZERO")
              (contains? clauses-set "PDM"))
        ; Hardcode version 1.0 for CC0, and CC-PDM, as that's their only version
        ["1.0" version-present? (and (= major "1") (= minor "0"))]
        ; For everything else, use the version in the regex match, or 4.0 if there isn't one
        (let [version-number (str (or major "4") "." (or minor "0"))
              version-number (if (contains? cc-versions version-number)
                               version-number
                               (let [version-number (str (or major "4") ".0")]  ; Check if we got something nonsensical like "1.5" and correct to "1.0"
                                 (if (contains? cc-versions version-number)
                                   version-number
                                   "4.0")))]
          [version-number version-present? true])))))

(defn- valid-suffix
  "Returns the (single) valid suffix that was found in match `m`, or `nil` if
  there wasn't one or if the suffix is valid but doesn't result in a distinct
  suffix in the SPDX identifier (e.g. `Generic`)."
  [m]
  (cond
    (get m "generic")       nil   ; Technically redundant, but we call it out just for clarity with the regexes
    (get m "unported")      nil   ; Technically redundant, but we call it out just for clarity with the regexes
    (get m "igo")           "IGO"
    (get m "international") nil   ; Technically redundant, but we call it out just for clarity with the regexes
    (get m "universal")     nil   ; Technically redundant, but we call it out just for clarity with the regexes
    (get m "australia")     "AU"
    (get m "austria")       "AT"
    (get m "englandWales")  "UK"
    (get m "france")        "FR"
    (get m "germany")       "DE"
    (get m "japan")         "JP"
    (get m "netherlands")   "NL"
    (get m "usa")           "US"))

(defn- build-id
  "Builds an id from the given clauses, version-number, and suffix.  Note that
  the resulting id may *NOT* be valid - this is simply a construction function."
  [clauses version-number suffix]
  (str (cond
         (= "ZERO" (first clauses)) "CC0"
         (= "PDDC" (first clauses)) "CC"
         (= "PDM"  (first clauses)) "CC"
         :else                      "CC-BY")
       (when (seq (filter #(not= "ZERO" %) clauses)) (str "-" (s/join "-" clauses)))  ; Remove "ZERO" if present, since it's not actually a valid CC clause component - it's just a marker we use internally
       (when version-number                          (str "-" version-number))
       (when suffix                                  (str "-" suffix))))

(defn- valid-id
  "Builds a valid CC id from a valid set of clauses, a valid version number, and
  a valid suffix (optional).  Returns a tuple of
  `[id valid-clauses? valid-suffix?]`."
  [clauses version-number suffix valid-clauses?]
  (loop [clauses           clauses
         valid-clauses?    valid-clauses?]
    (let [id (build-id clauses version-number suffix)]
      (if (sl/listed-id? id)
        ; id with clauses and suffix was valid, so return it
        [id valid-clauses? true]
        (let [id (build-id clauses version-number nil)]
          (if (sl/listed-id? id)
            ; id with clauses but not suffix was valid, so return it
            [id valid-clauses? false]
            (if (seq clauses)
              ; We still have clauses left, so drop the last one and try again
              (recur (drop-last clauses) false)
              ; No clauses left to drop, so something's wrong...
              ;####TODO: DO BETTER IN THIS CASE!!!!
              (throw (ex-info "UNABLE TO FIND VALID CC ID!!" {})))))))))

(defn- determine-confidence-and-explanations
  [version-present? valid-version? valid-clauses? valid-suffix?]
  (let [version-explanation (case [version-present? valid-version?]
                              [true true]   nil
                              [true false]  :invalid-verson
                              [false true]  :missing-version
                              [false false] :missing-version)]
    (case [(and version-present? valid-version?) valid-clauses? valid-suffix?]
      [true  true  true]  [:high   nil]
      [true  true  false] [:medium #{:invalid-cc-suffix}]
      [true  false true]  [:medium #{:invalid-cc-clauses}]
      [true  false false] [:low    #{:invalid-cc-clauses :invalid-cc-suffix}]
      [false true  true]  [:medium #{version-explanation}]
      [false true  false] [:low    #{version-explanation :invalid-cc-suffix}]
      [false false true]  [:low    #{version-explanation :invalid-cc-clauses}]
      [false false false] [:low    #{version-explanation :invalid-cc-clauses :invalid-cc-suffix}])))

(defn- match->ei
  [m]
  (let [match                                (:match m)
        [clauses valid-clauses?]             (valid-clauses m)
        [version-number version-present? valid-version-number?]
                                             (valid-version-number m clauses)
        suffix                               (valid-suffix m)
        [id valid-clauses? valid-suffix?]    (valid-id clauses version-number suffix valid-clauses?)
        [confidence confidence-explanations] (determine-confidence-and-explanations version-present? valid-version-number? valid-clauses? valid-suffix?)]
    (merge {:id         (lcisu/assert-listed-id (sexp/canonicalise id))
            :type       :concluded
            :confidence confidence
            :strategy   :regex-matching
            :source     (list match)}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))


;;
;; CC REGEX CONSTRUCTION
;;

(defn- re-prefixes
  "Possible CC prefixes"
  []
  (re/ncg "publicDomainBefore" #"Public[\s\-–—]+domain(?:[\s\-–—,]+per)?"))

(defn- re-clauses
  "Possible CC clauses - there can be zero or more of these, and we accept them
  in any order."
  []
  (re/zom-grp
    (re/join
      lcir/fre-ows
      (re/alt-grp
        (re/ncg "nonCommercial" (re/alt (re/join #"Non?"  lcir/fre-ows "Commercial")
                                        "NC"))
        (re/ncg "noDerivatives" (re/alt (re/join "No"     lcir/fre-ows #"Deriv(?:ative)?s?")
                                        "ND"))
        (re/ncg "shareAlike"    (re/alt (re/join "Share"  lcir/fre-ows "Alike")
                                        "SA"))
        (re/ncg "zero"          (re/alt "Zero"
                                        "0"))
        (re/ncg "pddc"          (re/alt (re/join "Public" lcir/fre-ows "Domain" lcir/fre-ows "Dedication" lcir/fre-ows #"(?:and|&)?" lcir/fre-ows "Certification")
                                        "PDDC"))
        (re/ncg "pdm"           (re/alt (re/join "Public" lcir/fre-ows "Domain" lcir/fre-ows "Mark")
                                        "PDM"))))))

(defn- re-suffix
  "Possible CC suffixes (normally only 1, but can be more)"
  []
  (re/zom-grp
    (re/join
      lcir/fre-ows
        (re/alt-grp
          (re/ncg "generic"           "Generic")
          (re/ncg "unported"          "Unported")
          (re/ncg "igo"               "IGO")
          (re/ncg "international"     "International")
          (re/ncg "publicDomainAfter" (re/or-grp #"\(?CC0(?:[\s\-–—]*1\.0)?\)?" #"Public[\s\-–—]+Domain(?:[\s\-–—]+Dedication)?"))
          (re/ncg "universal"         "Universal")
          (re/ncg "australia"         (re/alt "Australia" "AU"))
          (re/ncg "austria"           (re/alt "Austria" "AT"))
          (re/ncg "englandWales"      (re/alt #"Eng(?:land)?[\s\-–—]+(?:and|&)?[\s\-–—]+Wales" "GB" "UK"))
          (re/ncg "france"            (re/alt "France" "FR"))
          (re/ncg "germany"           (re/alt "Germany" "DE" "Deutsche"))
          (re/ncg "japan"             (re/alt "Japan" "JP"))
          (re/ncg "netherlands"       (re/alt "Netherlands" "NL"))
          (re/ncg "usa"               (re/alt #"United[\s\-–—]+States(?:[\s\-–—]+of[\s\-–—]+America)?" #"USA?"))))))

(def re (re/join #"(?iuUx)(?<!\w)(?:The[\s\-–—]+)?"  ; Only public for ease of testing
                 "\n\n#### Prefix ####\n"
                 (re/opt-grp (re-prefixes) lcir/fre-mws)
                 "\n\n#### Matching word ####\n"
                 (re/or-grp #"\(?CC(?:[\s\-–—]+BY)?\)?"
                            (re/or-grp "Attribution"
                                       (re/join "Creative" lcir/fre-ows "Commons"
                                                (re/opt-grp lcir/fre-ows "Legal" lcir/fre-ows "Code"))
                                       lcir/fre-ows)  ; We use "Attribution" as a matching word because of https://repo.clojars.org/spectrum/spectrum/0.2.5/spectrum-0.2.5.pom
                            lcir/fre-ows)
                 "\n\n#### Clauses ####\n"
                 (re-clauses)
                 "\n\n#### Version ####\n"
                 (re/opt-grp lcir/fre-version)
                 "\n\n#### Suffix ####\n"
                 (re-suffix)
                 "\n\n#### Random dingleberries ####\n"
                 (re/opt-grp lcir/fre-ows #"\(?CC.{0,13}\)?")
                 (re/opt-grp lcir/fre-mws #"licen[cs]e")
                 "\n\n#### Coda ####\n"
                 #"(?!\w)"))

(def ^:private pairs-d (delay (concat
;  (lcisu/spdx-match-pairs @ids-d)  ; Generic license regexes handle some cases
  [[re match->ei]])))

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
