;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.cc
  "Creative Commons family license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [wreck.api                                         :as re]
            [spdx.licenses                                     :as sl]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.spdx                               :as lcis]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.version-expression                 :as verexp]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(def ids-d (delay (concat ["CC0-1.0"] (filter #(s/starts-with? % "CC-") @lcis/license-ids-d))))


;;
;; CC REGEX CONSTRUCTION
;;

(defn- prefixes
  "Possible CC prefixes"
  []
  (re/ncg "publicDomainBefore" (re/join ref/public ref/mws "domain" (re/opt-grp ref/mws "per"))))

(defn- clauses
  "Possible CC clauses - there can be zero or more of these, and we accept them in any order."
  []
  (re/zom-grp
    (re/join
      ref/ows
      (re/alt-grp
        (re/ncg "nonCommercial" (re/alt (re/join #"Non?" ref/ows "Commercial") "NC"))
        (re/ncg "noDerivatives" (re/alt (re/join "No" ref/ows #"Deriv(?:ative)?s?") "ND"))
        (re/ncg "shareAlike"    (re/alt (re/join "Share" ref/ows "Alike") "SA"))
        (re/ncg "zero"          (re/alt "Zero" "0"))
        (re/ncg "pddc"          (re/alt (re/join "Public" ref/ows "Domain" ref/ows "Dedication" ref/ows ref/ands ref/ows "Certification") "PDDC"))
        (re/ncg "pdm"           (re/alt (re/join "Public" ref/ows "Domain" ref/ows "Mark") "PDM"))))))

(defn- suffixes
  "Possible CC suffixes (normally only 1, but can be more)"
  []
  (re/zom-grp
    (re/join
      ref/ows
      (re/alt-grp
        (re/ncg "generic"           "Generic")
        (re/ncg "unported"          "Unported")
        (re/ncg "igo"               "IGO")
        (re/ncg "international"     "International")
        (re/ncg "publicDomainAfter" (re/or-grp (re/join #"\(?CC0" (re/opt-grp ref/ows "1.0") #"\)?") (re/join ref/public ref/mws "Domain" (re/opt-grp ref/mws "Dedication"))))
        (re/ncg "universal"         "Universal")
        (re/ncg "australia"         ref/au)
        (re/ncg "austria"           ref/at)
        (re/ncg "englandWales"      ref/gb)
        (re/ncg "france"            ref/fr)
        (re/ncg "germany"           ref/de)
        (re/ncg "japan"             ref/jp)
        (re/ncg "netherlands"       ref/nl)
        (re/ncg "usa"               ref/us)))))

; Only public for ease of testing
(def re (re/fgrp "ix"
                 ref/nwb
                 (re/opt-grp "The" ref/mws)
                 "\n\n#### Prefix ####\n"
                 (re/opt-grp (prefixes) ref/mws)
                 "\n\n#### Matching word ####\n"
                 (re/or-grp (re/join #"\(?CC" (re/opt-grp ref/mws "BY") #"\)?")
                            (re/or-grp (re/join "(?<!" (re/alt-grp "No" "With" #"\s*\-\s*" "Public" #"Data\s+Commons") ")\\s*Attribution")
                                       (re/join "Creative" ref/ows "Commons"
                                                (re/opt-grp ref/ows "Legal" ref/ows "Code"))
                                       ref/ows)  ; We use "Attribution" as a matching word because of https://repo.clojars.org/spectrum/spectrum/0.2.5/spectrum-0.2.5.pom
                            ref/ows)
                 "\n\n#### Clauses ####\n"
                 (clauses)
                 "\n\n#### Version ####\n"
                 (re/opt-grp ref/ows (verexp/expression-regex "cc" ["1.0" "2.0" "2.5" "3.0" "4.0"]))  ; Note: this doesn't technically have to be exhaustive - just enough to emit an appropriate regex for CC version numbers
                 "\n\n#### Suffixes ####\n"
                 (suffixes)
                 "\n\n#### Random dingleberries ####\n"
                 (re/opt-grp ref/ows "CC" (re/n2m-grp 0 13 #"." ref/ows))
                 (re/opt-grp ref/mws ref/license)
                 "\n\n#### Coda ####\n"
                 ref/nwa))


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

(def ^:private cc-versions #{"1.0" "2.0" "2.1" "2.5" "3.0" "4.0"})

(defn- version-number-elements
  "Returns a version number as a tuple of [major minor] found in match `m`,
  stripping any leading zeroes.  One or both elements in the tuple may be `nil`."
  [m]
  (let [raw-version-number (get m "ccVersionNumber")]
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
        (let [valid-version-number? (contains? cc-versions (str major "." minor))
              version-number        (str (or major "4") "." (or minor "0"))
              version-number        (if (contains? cc-versions version-number)
                                      version-number
                                      (let [version-number (str (or major "4") ".0")]  ; Check if we got something nonsensical like "1.5" and correct to "1.0"
                                        (if (contains? cc-versions version-number)
                                          version-number
                                          "4.0")))]
          [version-number version-present? valid-version-number?])))))

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

(defn- determine-confidence-explanations
  [version-present? valid-version? valid-clauses? valid-suffix?]
  (let [version-explanation (case [version-present? valid-version?]
                              [true true]   nil
                              [true false]  :invalid-version
;                              [false true]  :missing-version  ; This is logically impossible
                              [false false] :missing-version)]
    (case [(and version-present? valid-version?) valid-clauses? valid-suffix?]
      [true  true  true]  nil
      [true  true  false] #{:invalid-cc-suffix}
      [true  false true]  #{:invalid-cc-clauses}
      [true  false false] #{:invalid-cc-clauses :invalid-cc-suffix}
      [false true  true]  #{version-explanation}
      [false true  false] #{version-explanation :invalid-cc-suffix}
      [false false true]  #{version-explanation :invalid-cc-clauses}
      [false false false] #{version-explanation :invalid-cc-clauses :invalid-cc-suffix})))


(defn- cc-match->expression-info
  [m]
  (let [[clauses valid-clauses?]          (valid-clauses m)
        [version-number version-present? valid-version-number?]
                                          (valid-version-number m clauses)
        suffix                            (valid-suffix m)
        [id valid-clauses? valid-suffix?] (valid-id clauses version-number suffix valid-clauses?)
        confidence-explanations           (determine-confidence-explanations version-present? valid-version-number? valid-clauses? valid-suffix?)]
    (mp/listed-match->expression-info @ids-d id "Creative Commons regex" confidence-explanations m)))

(defn detect
  "Detects any Creative Commons licenses found in the strings in `coll`, and
  replaces them with an expression-info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll re cc-match->expression-info))
