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


(defn- suffix
  "Returns the (single) suffix that was found in match `m`, or `nil` if there
  wasn't one or if the suffix is valid but doesn't result in a distinct suffix
  in the SPDX identifier (e.g. `Generic`)."
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

(defn- clauses
  "Returns a set of the pre-version clauses that were found in match `m`, or
  `nil` if there weren't any."
  [m]
  (some-> (seq (concat []
                       (when (get m "nonCommercial")      ["NC"])
                       (when (get m "noDerivatives")      ["ND"])
                       (when (get m "shareAlive")         ["SA"])
                       (when (get m "publicDomainBefore") ["ZERO"])  ; Treat "public domain" in the prefix as if it were CC0 (since that's the only combo that makes sense)
                       (when (get m "publicDomainAfter")  ["ZERO"])  ; Treat "public domain" in the suffix as if it were CC0 (since that's the only combo that makes sense)
                       (when (get m "zero")               ["ZERO"])
                       (when (get m "pddc")               ["PDDC"])
                       (when (get m "pdm")                ["PDM"])))
          set))

(defn- valid-clauses?
  "Is the given set of `clauses` valid in a CC license?"
  [clauses]
  (not
    ; Checking for _in_valid clauses is easier than the inverse
    (or (and (contains? clauses "ND")   (contains? clauses "SA"))  ; ND and SA are incompatible
        (and (contains? clauses "ZERO") (> (count clauses) 1))     ; Zero cannot have any other clauses
        (and (contains? clauses "PDDC") (> (count clauses) 1))     ; PDDC cannot have any other clauses
        (and (contains? clauses "PDM")  (> (count clauses) 1)))))  ; PDM cannot have any other clauses

(defn- valid-clauses
  "Determines valid clauses, taking `version-number` into account."
  [clauses version-number]
  (when (seq clauses)
    (case version-number
      "1.0" (disj clauses "PDDC")
      (disj clauses "ZERO" "PDDC" "PDM"))))

(defn- version-number-elements
  "Returns a version number as a tuple of [major minor] found in match `m`,
  stripping all leading zeroes.  One or both elements in the tuple may be `nil`."
  [m]
  (let [raw-version-number (get m "versionNumber")]
    (if-not (s/blank? raw-version-number)
      (let [[_ major minor]    (re-matches #"0*([123456789])(?:\.0*(\d*))?" raw-version-number)]
        [(when-not (s/blank? major) major) (when-not (s/blank? minor) minor)])
      [nil nil])))

(defn- valid-version?
  "Is `s` (a `String`) a valid CC-BY version number?  Note: only checks overall
  validity - doesn't check for valid version/clause/suffix combinations."
  [s]
  (or (= s "1.0")
      (= s "2.0")
      (= s "2.5")
      (= s "3.0")
      (= s "4.0")))

(defn- determine-confidence-and-explanations
  [valid-version? valid-clauses? valid-suffix?]
  (case [valid-version? valid-clauses? valid-suffix?]
    [true  true  true]  [:high   nil]
    [true  true  false] [:medium #{:invalid-cc-suffix}]
    [true  false true]  [:medium #{:invalid-cc-clauses}]
    [true  false false] [:medium #{:invalid-cc-clauses :invalid-cc-suffix}]
    [false true  true]  [:low    #{:invalid-version}]
    [false true  false] [:low    #{:invalid-version :invalid-cc-suffix}]
    [false false true]  [:low    #{:invalid-version :invalid-cc-clauses}]
    [false false false] [:low    #{:invalid-version :invalid-cc-clauses :invalid-cc-suffix}]))

(defn- build-id
  [clauses version-number suffix]
  (str "CC-BY"
       (when (seq clauses)
         (str "-" (s/join "-" clauses)))
       "-" version-number
       (when suffix (str "-" suffix))))

(defn- build-valid-id
  [clauses version-number suffix]
  (loop [clauses           clauses
         id-with-suffix    (build-id clauses version-number suffix)
         id-without-suffix (build-id clauses version-number nil)
         valid-clauses?    true]
    (if (or (nil? clauses)
            (sl/listed-id? id-with-suffix)
            (sl/listed-id? id-without-suffix))
      ; We found a valid id, so return it
      (if (sl/listed-id? id-with-suffix)
        [id-with-suffix valid-clauses? true]
        (when (sl/listed-id? id-without-suffix)
          [id-without-suffix valid-clauses? false]))
      ; No valid ids, so drop a clause and try again
      (let [new-clauses (seq (drop-last clauses))]
        (recur new-clauses (build-id clauses version-number suffix) (build-id clauses version-number nil) false)))))

(defn- match->ei
  [m]
  (let [match                   (:match m)
        ; Version number
        [major minor]           (version-number-elements m)
        version-number          (str (or major "4") "." (or minor "0"))
        version-number-present? (boolean (get m "versionNumber"))
        valid-version-number?   (valid-version? version-number)
        version-number          (if valid-version-number?
                                  version-number
                                  (if (valid-version? (str major ".0"))  ; Check if we got something nonsensical like "1.5" and correct to "1.0"
                                    (str major ".0")
                                    "4.0"))
        ; Clauses (NC, ND, SA, etc.)
        clauses                 (clauses m)
        valid-clauses?          (valid-clauses? clauses)
        valid-clauses           (valid-clauses clauses version-number)
        ; Suffix (Generic, International, IGO, Australia, Austria, etc.)
        suffix                  (suffix m)
        suffix-present?         (not (s/blank? suffix))

        ; Id construction and overall validity checking
        [id confidence confidence-explanations]
                                (cond
                                  ; Special case CC0-1.0
                                  (contains? valid-clauses "ZERO")
                                    (let [valid-CC0-version-number? (= "1.0" version-number)]
                                      (concat ["CC0-1.0"]
                                              (determine-confidence-and-explanations valid-CC0-version-number? valid-clauses? (not suffix-present?))))

                                  ; Special case CC-PDDC
                                  (contains? valid-clauses "PDDC")
                                    (concat ["CC-PDDC"]
                                            (determine-confidence-and-explanations version-number-present? valid-clauses? (not suffix-present?)))

                                  ; Special case CC-PDM-1.0
                                  (contains? valid-clauses "PDM")
                                    (let [valid-CC-PDM-version-number? (= "1.0" version-number)]
                                       (concat ["CC-PDM-1.0"]
                                               (determine-confidence-and-explanations valid-CC-PDM-version-number? valid-clauses? (not suffix-present?))))

                                  ; Special case CC-SA-1.0
                                  (and (= "1.0" version-number) (contains? valid-clauses "SA"))
                                    (concat ["CC-SA-1.0"]
                                            (determine-confidence-and-explanations valid-version-number? (= 1 (count valid-clauses)) (not suffix-present?)))

                                  ; Generic case
                                  :else
                                    (let [[id build-time-valid-clauses? valid-suffix?] (build-valid-id valid-clauses version-number suffix)]
                                      (concat [id] (determine-confidence-and-explanations valid-version-number?
                                                                                          (and valid-clauses? build-time-valid-clauses?)
                                                                                          valid-suffix?))))]
    (merge {:id         (lcisu/assert-listed-id (sexp/normalise id))
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
  "Possible CC suffix - there can be only one of these"
  []
  (re/opt
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
      (re/ncg "usa"               (re/alt #"United[\s\-–—]+States(?:[\s\-–—]+of[\s\-–—]+America)?" #"USA?")))))

(def re (re/join #"(?iuUx)(?<!\w)(?:The[\s\-–—]+)?"  ; Only public for ease of testing
                 "\n\n#### Prefix ####\n"
                 (re/opt-grp (re-prefixes) lcir/fre-mws)
                 "\n\n#### Matching word ####\n"
                 (re/or-grp #"\(?CC(?:[\s\-–—]+BY)?\)?" (re/or-grp "Attribution" (re/join "Creative" lcir/fre-ows "Commons") lcir/fre-ows) lcir/fre-ows)  ; We use "Attribution" as a matching word because of https://repo.clojars.org/spectrum/spectrum/0.2.5/spectrum-0.2.5.pom
                 "\n\n#### Clauses ####\n"
                 (re-clauses)
                 "\n\n#### Version ####\n"
                 (re/opt-grp lcir/fre-version)
                 "\n\n#### Suffix ####\n"
                 (re/opt-grp lcir/fre-mws (re-suffix))
                 "\n\n#### Random dingleberries ####\n"
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
