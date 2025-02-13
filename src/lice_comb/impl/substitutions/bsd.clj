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
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (concat ["0BSD"] (map :id (filter #(s/starts-with? (:id %) "BSD-") @lcis/full-license-list-d))))))

(comment
(defn- match->ei
  "Construct an expression-info map from `m`, a map returned from a rencg regex
  match/find match."
  [m]
  (let [clause-count1             (let [s (lcisu/get-rencgs m ["clauseCount"])
                                        n (lciu/digit-name-to-number s)]
                                    (if n
                                      n
                                      s))
        clause-count2             (let [s (lcisu/get-rencgs m ["clausecount2"])
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
        suffix                    (case (lcisu/get-rencgs m ["suffix" "clausecount2"])  ; Note: when the clause count is missing, the suffix can end up being captured by the clausecount2 capturing group
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
        [(lcisu/assert-listed-id base-id) :low (set/union #{:invalid-suffix} confidence-explanations)])  ; We got a suffix but it wasn't valid, which lowers our confidence
      [(lcisu/assert-listed-id base-id) confidence confidence-explanations])))                       ; We didn't get a suffix
)

(def ^:private number->name {
  0 "Zero"
  1 "One"
  2 "Two"
  3 "Three"
  4 "Four"})

(defn- re-bsd-numeric-clause
  "Returns a regex that will match a numeric rendition of BSD `n` clause."
  [n]
  (lcir/re-concat "(" #"(C(lause)?|Type)" lcir/fre-ows ")?"
                   "(" (number->name n) "|0*" n ")"
                   "(" lcir/fre-ows "Clause)?"))

(defn- re-bsd-textual-clause
  "Returns a regex that will match the textual rendition of a BSD clause count,
  expressed as a sequence of clause count identifying names."
  [clause-names]
  (when (seq clause-names)
    (if (= 1 (count clause-names))
      (lcir/re-concat lcir/fre-oquote (first clause-names) lcir/fre-oquote)
      (let [alt-group (str "(" (s/join "|" (map lcir/re-escape clause-names)) ")")]
        (lcir/re-concat lcir/fre-oquote "(" alt-group ")" lcir/fre-oquote
                        "(" lcir/fre-mws "or" lcir/fre-mws lcir/fre-oquote alt-group lcir/fre-oquote ")*")))))

(defn- re-bsd-clause
  "Returns a regex that will match both numeric and textual renditions of a BSD
  clause count."
  ([n] (re-bsd-clause n nil))
  ([n clause-names]
   (if (seq clause-names)
     (lcir/re-or (re-bsd-numeric-clause n) (re-bsd-textual-clause clause-names) lcir/fre-mws)
     (re-bsd-numeric-clause n))))

(defn- re-bsd-any-clause
  "Returns a regex that will match any BSD clause clause, in either numeric or
  textual renditions. Optional `prefix` is a prefix to use for each named
  capturing group, and will be followed by `#Clause` (where `#` is the clause
  count number - 0-4)."
  ([] (re-bsd-any-clause nil))
  ([prefix]
   (lcir/re-any (lcir/re-concat "(" (when prefix (str "?<" prefix "0Clause>")) (re-bsd-clause 0) ")")
                (lcir/re-concat "(" (when prefix (str "?<" prefix "1Clause>")) (re-bsd-clause 1) ")")
                (lcir/re-concat "(" (when prefix (str "?<" prefix "2Clause>")) (re-bsd-clause 2 ["Simplified"]) ")")
                (lcir/re-concat "(" (when prefix (str "?<" prefix "3Clause>")) (re-bsd-clause 3 ["New" "Revised" "Modified" "Standard"]) ")")  ; Note: "Standard" is unofficial, but used by e.g. https://repo.clojars.org/org/cyverse/authy/3.0.1/authy-3.0.1.pom
                (lcir/re-concat "(" (when prefix (str "?<" prefix "4Clause>")) (re-bsd-clause 4 ["Original" "Old"]) ")"))))

(defn- fre-clauses-before
  []
  (lcir/re-concat (re-bsd-any-clause "before") "?"))

(defn- fre-clauses-after
  []
  (lcir/re-concat (re-bsd-any-clause "after") "?(" lcir/fre-mws #"(Pub?lic[\s\-–—]+)licen[cs]e" ")?"))

; Possible prefixes for BSD licenses
(defn- fre-prefix-clauses
  []
  #"(?<systemicsBefore>Systemics(?<w3worksBefore>[\s\-–—]+W3Works)?)")

; Possible suffixes for BSD licenses
(defn- fre-suffix-clauses
  []
  (lcir/re-any
    ; BSD 1-4 clause suffixes
    (lcir/re-ncg "darwin"               #"(Ian[\s\-–—]+)?Darwin")
    (lcir/re-ncg "firstLines"           #"(1st|first)[\s\-–—]+lines([\s\-–—]+req(uirement)?)")
    (lcir/re-ncg "patent"               #"(Plus[\s\-–—]+)?Patent")
    (lcir/re-ncg "views"                #"((with|w/)[\s\-–—]+)?Views([\s\-–—]+sentence)?")
    (lcir/re-ncg "acpica"               #"acpica")
    (lcir/re-ncg "attribution"          #"(with|w/)[\s\-–—]+attribution")
    (lcir/re-ncg "clear"                #"Clear")
    (lcir/re-ncg "flex"                 #"Flex")
    (lcir/re-ncg "HP"                   #"(HP|Hewlett[\s\-–—]+Packard)")
    (lcir/re-ncg "LBNL"                 #"(LBNL|Lawrence[\s\-–—]+Berkeley[\s\-–—]+National[\s\-–—]+Labs)")
    (lcir/re-ncg "modification"         #"Modification")
    (lcir/re-ncg "noMilitary"           #"No[\s\-–—]+Military")
    (lcir/re-ncg "noNuclearWarranty"    #"No[\s\-–—]+Nuclear[\s\-–—]+Warranty")
    (lcir/re-ncg "noNuclearVariant2014" #"No[\s\-–—]+Nuclear([\s\-–—]+(variant|licen[cs]e))*[\s\-–—]+2014")
    (lcir/re-ncg "noNuclearVariant"     #"No[\s\-–—]+Nuclear")
    (lcir/re-ncg "openMPI"              #"Open[\s\-–—]+MPI")
    (lcir/re-ncg "sun"                  #"Sun([\s\-–—]+Microsystems)?")
    (lcir/re-ncg "shortened"            #"Shortened")
    (lcir/re-ncg "uc"                   #"\(?(University[\s\-–—]+of[\s\-–—]+California|UC|Cal)([\s\-–—]+Specific)?\)?")

    ; Suffixes with distinct identifiers, unrelated to 1-4 clause licenses
    (lcir/re-ncg "reno43"               #"4\.3[\s\-–—]+RENO")
    (lcir/re-ncg "tahoe43"              #"4\.3[\s\-–—]+TAHOE")
    (lcir/re-ncg "advertising"          #"Advertising[\s\-–—]+Acknowledge?ment")
    (lcir/re-ncg "attributionHPND"      #"(with|w/)[\s\-–—]+Attribution[\s\-–—]+(and|&)[\s\-–—]+(HPND|Historical[\s\-–—]+Permission[\s\-–—]+Notice[\s\-–—]+(and|&)[\s\-–—]+Disclaimer)[\s\-–—]+disclaimer")
    (lcir/re-ncg "inferno"              #"Inferno[\s\-–—]+Nettverk")
    (lcir/re-ncg "protection"           #"Protection")
    (lcir/re-ncg "scaBOF"               #"Source[\s\-–—]+Code[\s\-–—]+Attribution[\s\-–—]+beginning[\s\-–—]+of[\s\-–—]+file")
    (lcir/re-ncg "sca"                  #"Source[\s\-–—]+Code[\s\-–—]+Attribution")
    (lcir/re-ncg "freeBSD"              #"FreeBSD")
    (lcir/re-ncg "netBSD"               #"NetBSD")

    ; Prefixes, but just in case they ever appear in suffix position (as happens in the identifier)
    #"(?<systemicsAfter>Systemics(?<w3worksAfter>[\s\-–—]+W3Works)?)"))

(def re (lcir/re-concat #"(?iuUx)(?<!\w)(The[\s\-–—]+)?"  ; Only public for ease of testing
                        "\n\n#### Prefix ####\n"
                        "(" (fre-prefix-clauses) lcir/fre-mws ")?"
                        "\n\n#### Leading clause ####\n"
                        "(" (fre-clauses-before) lcir/fre-ows ")?"  ; We use optional ws here to catch values like "0BSD"
                        "\n\n#### Matching word ####\n"
                        #"(BSD)([\s\-–—]*style)?([\s\-–—]*licen[cs]e)?"
                        "\n\n#### Trailing clause ####\n"
                        "(" lcir/fre-mws (fre-clauses-after) ")?"
                        "\n\n#### Suffix ####\n"
                        "(" lcir/fre-mws (fre-suffix-clauses) ")?"
                        "\n\n#### Random dingleberries ####\n"
                        "(" lcir/fre-mws #"(variant|(Pub?lic[\s\-–—]+)licen[cs]e)" ")*"
                        "(" lcir/fre-version ")?"
                        #"(?!\w)"))

(def ^:private pairs-d (delay (concat [
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