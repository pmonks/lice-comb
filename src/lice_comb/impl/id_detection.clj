;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.id-detection
  "Helper functionality focused on detecting SPDX id(s) from a (short) string.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string       :as s]
            [clojure.set          :as set]
            [medley.core          :as med]
            [rencg.api            :as rencg]
            [embroidery.api       :as e]
            [lice-comb.impl.spdx  :as lcis]
            [lice-comb.impl.utils :as lciu]))

(defn- get-rencgs
  "Get a value for an re-ncg, potentially looking at multiple ncgs in order
  until a non-blank value is found. Returns `default` when no non-blank value is
  found (and which defaults to `nil` if not provided). Trims and lower-cases the
  value, and replaces all whitespace with a single space."
  ([m names] (get-rencgs m names nil))
  ([m names default]
    (loop [f (first names)
           r (rest  names)]
      (if f
        (let [value (get m f)]
          (if (s/blank? value)
            (recur (first r) (rest r))
            (-> value
                (s/trim)
                (s/lower-case)
                (s/replace #"\s+" " "))))
        default))))

(defn- assert-listed-id
  "Checks that the id is a listed SPDX identifier (license or exception) and
  throws if not. Returns the id."
  [id]
  (if (or (contains? @lcis/license-ids-d   id)
          (contains? @lcis/exception-ids-d id))
    id
    (throw (ex-info (str "Invalid SPDX id constructed: '" id
                         "' - please raise an issue at "
                         "https://github.com/pmonks/lice-comb/issues/new?assignees=pmonks&labels=bug&template=Invalid_id_constructed.md&title=Invalid+SPDX+identifer+constructed:+" id)
                    {:id id}))))

(defn- generic-id-constructor
  "A generic SPDX id constructor which works for many simple license name
  regexes."
  ([id-prefix latest-ver m] (generic-id-constructor id-prefix latest-ver "0" m))
  ([id-prefix latest-ver pad-ver-with m]
   (when m
     (let [version (get-rencgs m ["version"])
           [confidence confidence-explanations]
                   (if (s/blank? latest-ver)
                     [:high]  ; We didn't need a version
                     (if (s/blank? version)
                       [:low #{:missing-version}]
                       (if (s/includes? version ".")
                         [:high]  ; We got a full version
                         [:medium #{:partial-version}])))  ; We got a partial version
           version (if (s/blank? version)
                     latest-ver
                     version)
           version (if (s/includes? version ".")
                      version
                      (str version "." pad-ver-with))
           id      (str id-prefix (when-not (s/blank? version) (str "-" version)))]
       [(assert-listed-id id) confidence confidence-explanations]))))

(defn- bsd-id-constructor
  "An SPDX id constructor specific to the BSD family of licenses."
  [m]
  (let [clause-count1             (let [s (get-rencgs m ["clausecount1"])
                                        n (lciu/digit-name-to-number s)]
                                    (if n
                                      n
                                      s))
        clause-count2             (let [s (get-rencgs m ["clausecount2"])
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
        suffix                    (case (get-rencgs m ["suffix" "clausecount2"])  ; Note: when the clause count is missing, the suffix can end up being captured by the clausecount2 capturing group
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
        [(assert-listed-id base-id) :low (set/union #{:invalid-suffix} confidence-explanations)])  ; We got a suffix but it wasn't valid, which lowers our confidence
      [(assert-listed-id base-id) confidence confidence-explanations])))                       ; We didn't get a suffix

(defn- cc-id-constructor
  "An SPDX id constructor specific to the Creative Commons family of licenses."
  [m]
  (let [nc?            (not (s/blank? (get-rencgs m ["noncommercial"])))
        nd?            (not (s/blank? (get-rencgs m ["noderivatives"])))
        sa?            (not (s/blank? (get-rencgs m ["sharealike"])))
        version        (get-rencgs m ["version"] "")
        version        (s/replace version #"\p{Punct}+" ".")
        [confidence confidence-explanations]
                       (if (s/blank? version)
                         [:low #{:missing-version}]
                         (if (s/includes? version ".")
                           [:high]
                           [:medium #{:partial-version}]))
        version        (if (s/blank? version)
                         "4.0"
                         version)
        version        (if (s/includes? version ".")
                         version
                         (str version ".0"))
        base-id        (str "CC-BY-"
                            (when nc?                 "NC-")
                            (when nd?                 "ND-")
                            (when (and (not nd?) sa?) "SA-")   ; SA and ND are incompatible (and have no SPDX id as a result), and if both are (erroneously) specified we conservatively choose ND
                            version)
        region         (case (get-rencgs m ["region"])
                         "australia"                                            "AU"
                         "austria"                                              "AT"
                         ("england" "england and wales" "england & wales" "uk") "UK"
                         "france"                                               "FR"
                         "germany"                                              "DE"
                         "igo"                                                  "IGO"
                         "japan"                                                "JP"
                         "netherlands"                                          "NL"
                         ("united states" "usa" "us")                           "US"
                         nil)
        id-with-region (str base-id (when-not (s/blank? region) (str "-" region)))]
    (if region
      (if (contains? @lcis/license-ids-d id-with-region)  ; Not all license variants and versions have a region specific identifier, so check that it's valid before returning it
        [id-with-region confidence confidence-explanations]
        [(assert-listed-id base-id) :low (set/union #{:invalid-region} confidence-explanations)])
      [(assert-listed-id base-id) confidence confidence-explanations])))

(defn gpl-id-constructor
  "An SPDX id constructor specific to the GNU family of licenses."
  [m]
  (let [variant            (cond (contains? m "agpl") "AGPL"
                                 (contains? m "lgpl") "LGPL"
                                 (contains? m "gpl")  "GPL")
        version-present?   (boolean (get-rencgs m ["version"] false))
        version            (get-rencgs m ["version"] (if (= variant "LGPL") "2.0" "1.0"))  ; Note: on the advice of the SPDX technical team, default to earliest version when version not present
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
    [(assert-listed-id id) confidence confidence-explanations]))

; The regex for the GNU family is a nightmare, so we build it up (and test it) in pieces
(def agpl-re          #"(?<agpl>AGPL|Affero)(\s+GNU)?(\s+Genere?al)?(\s+Pub?lic)?(\s+Licen[cs]e)?(\s+\(?AGPL\)?)?")
(def lgpl-re          #"(?<lgpl>(GNU\s+)?((Genere?al\s+)?(Library\s+or\s+Lesser|Lesser\s+or\s+Library|Library|Lesser))|((Library\s+or\s+Lesser|Lesser\s+or\s+Library|Library|Lesser)\s+(GNU|GPL|Genere?al)|(L(esser\s)?\s*GPL)))(\s+Genere?al)?(\s+Pub?lic)?(\s+Licen[cs]e)?(\s+\(?L\s*GPL\)?)?")
(def gpl-re           #"(?<!(Affero|Lesser|Library)\s+)(?<gpl>GNU(?!\s+Classpath)|(?<!(L|A)\s*)GPL|Genere?al\s+Pub?lic\s+Licen[cs]e)(?!\s+(Affero|Library|Lesser|Genere?al\s+Lesser|Genere?al\s+Library|LGPL|AGPL))((\s+General)?(?!\s+(Affero|Lesser|Library))\s+Pub?lic\s+Licen[cs]e)?(\s+\(?GPL\)?)?")
(def version-re       #"[\s,\-]*(_?V(ersion)?)?[\s\._]*(?<version>\d+([\._]\d+)?)?")
(def only-or-later-re #"[\s,\-]*((?<only>\(?only\)?)|(\(?or(\s+\(?at\s+your\s+(option|discretion)\)?)?(\s+any)?)?([\s\-]*(?<orLater>lat[eo]r|newer|greater|\+)))?")
(def gnu-re           (lciu/re-concat "(?x)(?i)(?<!\\w)(\n# Alternative 1: AGPL\n"
                                      agpl-re
                                      "\n# Alternative 2: LGPL\n|"
                                      lgpl-re
                                      "\n# Alternative 3: GPL\n|"
                                      gpl-re
                                      "\n)\n# Version\n"
                                      version-re
                                      "\n# Only/or-Later suffix\n"
                                      only-or-later-re
                                      #"(?!\w)"))

(def ^:private license-family-matching-d (delay
  {:AFL {
     :regex #"(?i)(?<!\w)Academic(\s+Free)?(\s+Licen[cs]e)?[\s,\-]*(\s*V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "AFL" "3.0")}
   :Apache {
     :regex #"(?i)(?<!\w)(ASL|Apache)(\s+Software)?(\s+Licen[cs]e(s)?)?[\s,\-]*(\s*V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!.*acknowledgment\s+clause\s+removed)(?!\w)"
     :fn    (partial generic-id-constructor "Apache" "2.0")}
   :Artistic {
     :regex #"(?i)(?<!\w)Artistic\s+Licen[cs]e(\s*V(ersion)?)?[\s,\-]*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "Artistic" "2.0")}
   :Beerware {
     :regex #"(?i)(?<!\w)Beer[\s\-]*ware(?!\w)"
     :fn    (constantly ["Beerware" :high])}
   :BSL {
     :regex #"(?i)(?<!\w)Boost(\s+Software)?(\s+Licen[cs]e)?[\s,\-]*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "BSL" "1.0")}
   :BSD {
     :regex #"(?i)(?<!\w)(?<clausecount1>\p{Alnum}+)?[\s,\-]*(C(lause)?|Type)?[\s\-]*BSD[\s\-]*\(?(Licen[cs]e|Type|C(lause)?)?[\s\-]*(?<clausecount2>\p{Alnum}+)?([\s\-]+Clause)?(?<suffix>\s+(Patent|Views|Attribution|Clear|LBNL|HP|Sun|flex|FreeBSD|NetBSD|Modification|No\s+Military\s+Licen[cs]e|No\s+Nuclear\s+Licen[cs]e([\s\-]+2014)?|No\s+Nuclear\s+Warranty|Open\s+MPI|Shortened|UC|Darwin|acpica))?(?!\w)"
     :fn    bsd-id-constructor}
   :CC {
     :regex #"(?i)(?<!\w)(CC[\s\-]BY|Creative[\s\-]+Commons(?![\s\-]+CC0)(?!([\s\-]+Legal[\s\-]+Code)?[\s\-]+Attribution)|(Creative[\s\-]+Commons[\s\-]+([\s\-]+Legal[\s\-]+Code)?)?(?<!BSD[\s\-]+(\d|two|three|four)[\s\-]+Clause\s+)Attribution)(\s+Licen[cs]e)?([\s,\-]*((?<noncommercial>Non\s*Commercial|NC)|(?<noderivatives>No[\s\-]*Deriv(ative)?s?|ND)|(?<sharealike>Share[\s\-]*Alike|SA)))*(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?\s*(?<region>Australia|Austria|England((\s+and|\&)?\s+Wales)?|France|Germany|IGO|Japan|Netherlands|UK|United\s+States|USA?)?(?!\w)"
     :fn    cc-id-constructor}
   :CC0 {
     :regex #"(?i)(?<!\w)CC[\s\-]*0(?!\w)"
     :fn    (constantly ["CC0-1.0" :high])}
   :CECILL {
     :regex #"(?i)(?<!\w)CeCILL(\s+Free)?(\s+Software)?(\s+Licen[cs]e)?(\s+Agreement)?[\s,\-]*(\s*V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "CECILL" "2.1")}
   :Classpath-exception {
     :regex #"(?<!\w)(CPE|(?i)(Classpath[\s\-]+exception(\s*V(ersion)?)?[\s\-]*(?<version>\d+(\.\d+)?)?))(?!\w)"
     :fn    (partial generic-id-constructor "Classpath-exception" "2.0" true)}
   :CDDL {
     :regex #"(?i)(?<!\w)(CDDL|Common\s+Development\s+(and|\&)?\s+Distribution\s+Licen[cs]e)(\s+\(?CDDL\)?)?[\s,\-]*(\s*V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "CDDL" "1.1")}
   :CPL {
     :regex #"(?i)(?<!\w)Common\s+Public\s+Licen[cs]e[\s,\-]*(\s*V(ersion)?)?(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "CPL" "1.0")}
   :EPL {
     :regex #"(?i)(?<!\w)(EPL|Eclipse(\s+Public)?(\s+Licen?[cs]e)?)(\s*\(EPL\))?[\s,\-]*(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"   ; Note: optional "n" in "license" is because of a known typo
     :fn    (partial generic-id-constructor "EPL" "2.0")}
   :EUPL {
     :regex #"(?i)(?<!\w)European\s+Union(\s+Public)?(\s+Licen[cs]e)?[\s,\-]*(\(?EUPL\)?)?[\s,\-]*(V(ersion)?)?(\.)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "EUPL" "1.2")}
   :FreeBSD {
     :regex #"(?i)(?<!\w)Free[\s\-]*BSD(?!\w)"
     :fn    (constantly ["BSD-2-Clause-FreeBSD" :high])}
   :GNU {
     :regex gnu-re
     :fn    gpl-id-constructor}
   :Hippocratic {
     :regex #"(?i)(?<!\w)Hippocratic(?!\w)"
     :fn    (constantly ["Hippocratic-2.1" :high])}  ; There are no other listed versions of this license
   :LLVM-exception {
     :regex #"(?i)(?<!\w)LLVM[\s\-]+Exception(?!\w)"
     :fn    (constantly ["LLVM-exception" :high])}
   :MIT {
     :regex #"(?i)(?<!\w)(MIT|Bouncy\s+Castle)(?![\s/]*(X11|ISC))(\s+Public)?(\s+Licen[cs]e)?(?!\w)"
     :fn    (constantly ["MIT" :high])}
   :MPL {
     :regex #"(?i)(?<!\w)(MPL|Mozilla)(\s+Public)?(\s+Licen[cs]e)?[\s,\-]*(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "MPL" "2.0")}
   :MX4J {
     :regex #"(?i)(?<!\w)MX4J\s+Licen[cs]e(,?\s+v(ersion)?\s*1\.0)?(?!\w)"
     :fn    (constantly ["Apache-1.1" :high])}  ; See https://github.com/spdx/license-list-XML/pull/594 - the MX4J license *is* the Apache-1.1 license, according to SPDX
   :NASA {
     :regex #"(?i)(?<!\w)NASA(\s+Open)?(\s+Source)?(\s+Agreement)?[\s,\-]+(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?(?!\w)"
     :fn    (partial generic-id-constructor "NASA" "1.3" "3")}
   :Plexus {
     :regex #"(?i)(?<!\w)Apache\s+Licen[cs]e(\s+but)?(\s+with)?(\s+the)?\s+acknowledgment\s+clause\s+removed(?!\w)"
     :fn    (constantly ["Plexus" :medium [:inferred-license-name]])}
   :proprietary-commercial {
     :regex #"(?i)(?<!\w)(Propriet[aoe]ry|Commercial|(Copyright\s+.{0,20})?All\s+Rights\s+Reserved|Private)(\s+Licen[cs]e\s*)?[\-\.]*(?!\w)"  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
     :fn    (constantly [(lcis/proprietary-commercial) :high])}
   :public-domain {
     :regex #"(?i)(?<!\w)Public\s+Domain[\-\.]*(?![\s/\\\(]*CC\s*0\s*\)?)"  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
     :fn    (constantly [(lcis/public-domain) :high])}
   :Ruby {
     :regex #"(?i)(?<!\w)Ruby(\s+Licen[cs]e)?(?!\w)"
     :fn    (constantly ["Ruby" :high])}
   :SGI-B {
     :regex #"(?i)(?<!\w)SGI(\s+Free)?(\s+Software)?(\s+Licen[cs]e)?([\s,\-]+(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?)?(?!\w)"
     :fn    (partial generic-id-constructor "SGI-B" "2.0")}
   :Unlicense {
     :regex #"(?i)(?<!\w)(The\s+)?Unlicen[cs]e(?!\w)"
     :fn    (constantly ["Unlicense" :high])}
   :UPL {
     :regex #"(?i)(?<!\w)Universal\s+Permissive(\s+Licen[cs]e)?([\s,\-]+(V(ersion)?)?\s*(?<version>\d+(\.\d+)?)?)?(?!\w)"
     :fn    (constantly ["UPL-1.0" :high])}  ; There are no other listed versions of this license
   :WTFPL {
     :regex #"(?i)(?<!\w)(WTFPL|DO[\s\-]+(WTF|What[\s\-]+The[\s\-]+[f*][u*][c*][k*])[\s\-]+(U|YOU)[\s\-]+WANT[\s\-]+(2|TO))([\s\-]+Public)?([\s\-]+Licen[cs]e)?([\s\-,]+Version[\s\-]+\d+)?(?!\w)"
     :fn    (constantly ["WTFPL" :high])}
   :X11 {
     :regex #"(?i)(?<!\w)(MIT)?[\s,\-\/\\]+X11(\s+Public)?(\s+Licen[cs]e)?(?!\w)"
     :fn    (constantly ["X11" :high])
   }
   :Zlib {
     :regex #"(?i)(?<!\w)zlib(?![\s/]+libpng)(?!\w)"
     :fn    (constantly ["Zlib" :high])}
    }))

(defn- detect-id-internal
  "Detects an id in `s` for the given `elem`, optionally based on a discovery
  \"mode\", either `:match` (default) or `:find`.

  If a result is found, returns a map containing the following keys:
  * :id         The SPDX license or exception identifier that was determined
  * :type       The 'type' of match - will always have the value :concluded
  * :confidence The confidence of the match: either :high, :medium, or :low
  * :strategy   The matching strategy - will always have the value :regex-matching
  * :source     A list of strings containing source information (specifically
                the portion of the string s that matched this regex element)
  *: start      The start index of the given match within s (to allow sorting)

  Returns `nil` if nothing is found."
  ([s elem] (detect-id-internal s elem :match))
  ([s elem mode]
   (let [f (case mode
             :find rencg/re-find-ncg
             rencg/re-matches-ncg)]
     (when-let [match (f (:regex elem) s)]
       (let [[id confidence confidence-explanations] ((:fn elem) match)
             source                                  (list (s/trim (:match match)))]
         (merge {:id         id
                 :type       :concluded
                 :confidence (if (= source id) :high confidence)
                 :strategy   :regex-matching
                 :source     source
                 :start      (:start match)}
                (when (seq confidence-explanations) {:confidence-explanations confidence-explanations})))))))

(defn supported-families
  "Set of supported 'families' (as `:keyword`s) available for use with
  [[find-family]], [[match-family]], [[replace-family]], etc."
  []
  (some-> @license-family-matching-d
          keys
          set))

(defn find-id
  "Attempts to find the specific SPDX identifier for `family` (a `:keyword`),
  in `s` (a `String`).  Returns `nil` if the `family` wasn't detected in `s`, or
  `family` is `nil` or invalid (see [[supported-families]])."
  [family s]
  (when family
    (when-let [elem (get @license-family-matching-d family)]
      (detect-id-internal s elem :find))))

(defn match-id
  "Attempts to match the specific SPDX identifier for `family` (a `:keyword`),
  against `s` (a `String`).  Returns `nil` if the `family` wasn't detected in
  `s`, or `family` is `nil` or invalid (see [[supported-families]])."
  [family s]
  (when family
    (when-let [elem (get @license-family-matching-d family)]
      (detect-id-internal s elem :match))))

;####TODO: REMOVE THESE TWO FNS!!!!
(comment
(defn- replace-info
  "Similar to `clojure.string/replace`, but returns a tuple where the first
  element is the new `String`, and the second element is a sequence of
  expression-info maps for each replacement.  This sequence will be `nil` if no
  replacements were performed."
  [^CharSequence s ^java.util.regex.Pattern re replacement-fn]
  (let [m    (re-matcher re s)
        ncgs (rencg/re-named-groups re)]
    (if (.find m)
      (let [buffer (StringBuffer. (.length s))]
        (loop [found true
               eis   []]
          (if found
            (let [groups                                  (rencg/re-groups-ncg m ncgs)
                  match                                   (:match groups)
                  [id confidence confidence-explanations] (replacement-fn groups)]
              (.appendReplacement m buffer (java.util.regex.Matcher/quoteReplacement id))
              (recur (.find m) (conj eis (merge {:id         id
                                                 :type       :concluded
                                                 :confidence confidence
                                                 :strategy   :regex-replacement
                                                 :source     (list match)}
                                                (when confidence-explanations {:confidence-explanations confidence-explanations})))))
            (do
              (.appendTail m buffer)
              [(.toString buffer) eis]))))
      [s nil])))

(defn replace-ids
  "Replaces values in `s` with any values that match the regex for `family` (a
  `:keyword` from [[supported-families]]). Returns a tuple where the first
  element is the new `String`, and the second element is a sequence of
  expression-info maps for each replacement. This sequence will be `nil` if no
  replacements were performed."
  [family s]
  (when (and family s)
    (when-let [elem (get @license-family-matching-d family)]
      (let [re (:regex elem)
            f  (:fn    elem)]
        (replace-info s re f)))))
)

(defn- replace-id
  "####TODO: DOCUMENT ME!!!!"
  [f m]
  (let [match                                   (:match m)
        [id confidence confidence-explanations] (f m)]
    (merge {:id         id
            :type       :concluded
            :confidence confidence
            :strategy   :regex-replacement
            :source     (list match)}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))

(defn replace-ids
  "Replaces values in `s` with any values that match the regex for `family` (a
  `:keyword` from [[supported-families]]). Returns a sequence as per
  [[lice-comb.impl.id-detection/replacing-split]], where replacements (if any)
  are expression-info maps.  Returns a singleton sequence containing `s` if
  `family` is invalid (does not identify a family from [[supported-families]].
  Returns `nil` if `family` or `s` are nil."
  [family s]
  (when (and family s)
    (if-let [elem (get @license-family-matching-d family)]
      (let [re (:regex elem)
            f  (:fn    elem)]
        (lciu/replacing-split s re (partial replace-id f)))
      [s])))

(defn find-ids
  "Returns a sequence (NOT A SET!) of expression-info maps.

  Results are in the order in which they appear in `s` (hence why this fn
  returns a sequence not a set), and returns `nil` if there were no matches.

  Note:
  * Only uses the custom regexes in this namespace - does not look for all
    possible SPDX identifiers, LicenseRefs, or AdditionRefs.  For that, use
    [[lice-comb.impl.spdx/find-ids]]"
  [s]
  (when-let [matches (seq (filter identity (e/pmap* #(find-id % s) (keys @license-family-matching-d))))]
    (some->> matches
             (med/distinct-by :id)    ;####TODO: THINK ABOUT MERGING INSTEAD OF DROPPING (e.g. if the same id is detected in two different places in s, and we want to preserve the two eis)
             (sort-by :start)
             (map #(merge {:id         (:id %)   ; We duplicate this here in case the result gets merged into an expression
                           :type       (:type %)
                           :confidence (:confidence %)
                           :strategy   (:strategy %)
                           :source     (:source %)}
                           (when (seq (:confidence-explanations %))
                             {:confidence-explanations (:confidence-explanations %)}))))))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  @license-family-matching-d
  nil)
