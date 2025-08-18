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
            [wreck.api                          :as re]
            [spdx.licenses                      :as sl]
            [spdx.expressions                   :as sexp]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.utils               :as lciu]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (lcis/sort-ids (concat ["0BSD" "AMPAS" "FreeBSD-DOC"] (filter #(s/starts-with? % "BSD-") @lcis/license-ids-d)))))

;;
;; EXPRESSION-INFO CONSTRUCTION FROM A MATCH
;;

(defn- valid-clause-count?
  "Is `clause-count` valid (i.e. 0 to 4)?"
  [clause-count]
  (and (>= clause-count 0)
       (<= clause-count 4)))

(defn- clause-counts
  "Returns all clause counts, as a sequence of numbers, found in match `m`, or
  `nil` if no matches were found.  Duplicates will be removed, but invalid
  clause counts will not be removed."
  [m]
  (let [version-number (when-let [version-number-dbl (lciu/parse-dbl (get m "versionNumber"))] (int version-number-dbl))]
    (seq
      (distinct
        (filter identity
                (vector (when (lcisu/get-rencgs m ["before0Clause" "after0Clause"]) 0)
                        (when (lcisu/get-rencgs m ["before1Clause" "after1Clause"]) 1)
                        (when (lcisu/get-rencgs m ["before2Clause" "after2Clause"]) 2)
                        (when (lcisu/get-rencgs m ["before3Clause" "after3Clause"]) 3)
                        (when (lcisu/get-rencgs m ["before4Clause" "after4Clause"]) 4)
                        (when version-number version-number)))))))

(defn- clause-based-identifier
  "Returns a thruple of [identifier confidence confidence-explanations] if match
  `m` represents a standard BSD N clause identifier (such as `BSD-4-Clause`).
  Returns `nil` is `m` did not match a 'clause based identifier."
  [m]
  (let [clause-counts (clause-counts m)
        [clause-count confidence confidence-explanations]
                      (case (count clause-counts)
                        0     [4 :medium #{:missing-clause-count}]
                        1     (let [clause-count (first clause-counts)]
                                (if (valid-clause-count? clause-count)
                                  [clause-count :high nil]
                                  [4            :low  #{:invalid-clause-count}]))
                        ; Multiple distinct clause counts, so we pick the least restrictive one (i.e. the lowest)
                        (let [clause-count (apply min clause-counts)]
                          (if (valid-clause-count? clause-count)
                            [clause-count :low  #{:conflicting-clause-counts}]
                            ; Invalid clause count found, try again with invalid clause counts filtered out
                            (if-let [valid-clause-counts (seq (filter valid-clause-count? clause-counts))]
                              [(apply min valid-clause-counts) :low #{:conflicting-clause-counts}]
                              [4                               :low #{:conflicting-clause-counts :invalid-clause-count}]))))
        id            (if (= 0  clause-count)
                        "0BSD"
                        (str "BSD-" clause-count "-Clause"))]
    [id confidence confidence-explanations]))  ; We don't assert or canonicalise the identifier here, as that has to happen after we've appended any suffix

(defn- suffix
  "Returns the suffix for the given match (including a leading hyphen) or `nil`
  if there wasn't one.  Does NOT handle 'faux suffixes'."
  [m]
  (cond
    (get m "darwin")               "-Darwin"
    (get m "firstLines")           "-first-lines"
    (get m "patent")               "-Patent"
    (get m "views")                "-Views"
    (get m "acpica")               "-acpica"
    (get m "attribution")          "-Attribution"
    (get m "clear")                "-Clear"
    (get m "flex")                 "-flex"
    (get m "HP")                   "-HP"
    (get m "LBNL")                 "-LBNL"
    (get m "modification")         "-Modification"
    (get m "noMilitary")           "-No-Military-License"
    (get m "noNuclearWarranty")    "-No-Nuclear-Warranty"
    (get m "noNuclearLicense2014") "-No-Nuclear-License-2014"
    (get m "noNuclearLicense")     "-No-Nuclear-License"
    (get m "openMPI")              "-Open-MPI"
    (get m "sun")                  "-Sun"
    (get m "shortened")            "-Shortened"
    (get m "uc")                   "-UC"))

(defn- suffix-based-identifier
  "Returns a thruple of [identifier confidence confidence-explanations] if match
  `m` represents a 'faux suffix' based identifier (such as `BSD-4.3RENO`).
  Returns `nil` is `m` did not match a 'faux suffix' based identifier."
  [m]
  (when-let [id (cond
                  (get m "reno43")                                          "BSD-4.3RENO"
                  (get m "tahoe43")                                         "BSD-4.3TAHOE"
                  (get m "advertising")                                     "BSD-Advertising-Acknowledgement"
                  (get m "attributionHPND")                                 "BSD-Attribution-HPND-disclaimer"
                  (get m "inferno")                                         "BSD-Inferno-Nettverk"
                  (get m "protection")                                      "BSD-Protection"
                  (get m "scaBOF")                                          "BSD-Source-beginning-file"
                  (get m "sca")                                             "BSD-Source-Code"
                  (get m "freeBSDDoc")                                      "FreeBSD-DOC"
                  (get m "beforeHP")                                        "BSD-3-Clause-HP"
                  (get m "beforeLBNL")                                      "BSD-3-Clause-LBNL"
                  (lcisu/get-rencgs m ["beforeW3works"   "afterW3works"])   "BSD-Systemics-W3Works"  ; This must go before systemics, since it will always match both
                  (lcisu/get-rencgs m ["beforeSystemics" "afterSystemics"]) "BSD-Systemics"
                  (lcisu/get-rencgs m ["beforeAMPAS"     "afterAMPAS"])     "AMPAS"
                  (get m "beforeAduna")                                     "BSD-3-Clause"  ; See https://www.d3web.de/Wiki.jsp?page=Aduna-BSD
                  (lcisu/get-rencgs m ["beforeFreeBSD"   "freeBSD"])        "BSD-2-Clause-FreeBSD"
                  (lcisu/get-rencgs m ["beforeNetBSD"    "netBSD"])         "BSD-2-Clause-NetBSD")]
    (concat [(sexp/canonicalise id)]
            (if (or (clause-counts m) (suffix m))
              [:medium #{:invalid-bsd-combination}]
              [:high nil]))))

(defn- match->ei
  [m]
  (let [match (:match m)
        [id confidence confidence-explanations]
              (if-let [sbi (suffix-based-identifier m)]  ; First check if it's a suffix-based identifier
                sbi
                (let [[id con con-exp] (clause-based-identifier m)
                      suffix           (suffix m)
                      id-and-suffix    (str id suffix)]
                  (if (sl/listed-id? id-and-suffix)
                    [id-and-suffix con  con-exp]
                    [id            :low (set/union #{:invalid-bsd-combination} con-exp)])))]
    (merge {:id         (lcisu/assert-listed-id id)
            :type       :concluded
            :confidence confidence
            :strategy   :regex-matching
            :source     (list match)}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))


;;
;; BSD REGEX CONSTRUCTION
;;

(def ^:private number->name {
  0 "Zero"
  1 "One"
  2 "Two"
  3 "Three"
  4 "Four"})

(defn- re-bsd-numeric-clause
  "Returns a regex that will match a numeric rendition of BSD `n` clause."
  [n]
  (re/grp (re/opt-grp (re/grp (re/alt #"C(?:lause)?" "Type")) lcir/fre-ows)
          (re/grp     (re/alt (number->name n) "0*") n)
          (re/opt-grp lcir/fre-ows "Clause")))

(defn- re-bsd-textual-clause
  "Returns a regex that will match the textual rendition of a BSD clause count,
  expressed as a sequence of clause count identifying names."
  [clause-names]
  (when (seq clause-names)
    (if (= 1 (count clause-names))
      (re/grp lcir/fre-oquote (first clause-names) lcir/fre-oquote)
      (let [fre-names (apply re/alt-grp (map re/esc clause-names))]
        (re/grp lcir/fre-oquote
                fre-names
                lcir/fre-oquote
                (re/zom-grp
                  (re/alt-grp (re/grp lcir/fre-mws "or"     lcir/fre-mws)
                              (re/grp lcir/fre-ows #"[\\/]" lcir/fre-ows)
                              lcir/fre-mws)
                  lcir/fre-oquote
                  fre-names
                  lcir/fre-oquote))))))

(defn- re-bsd-clause
  "Returns a regex that will match both numeric and textual renditions of a BSD
  clause count."
  ([n] (re-bsd-clause n nil))
  ([n clause-names]
   (if (seq clause-names)
     (re/or-grp (re-bsd-numeric-clause n) (re-bsd-textual-clause clause-names) lcir/fre-mws)
     (re-bsd-numeric-clause n))))

(defn- re-bsd-any-clause
  "Returns a regex that will match any BSD clause clause, in either numeric or
  textual renditions. `prefix` is a prefix to use for each named capturing
  group, and will be followed by `#Clause` (where `#` is the clause count
  number - 0-4)."
  [prefix]
  (re/alt-grp
    (re/ncg (str prefix "0Clause") (re-bsd-clause 0))
    (re/ncg (str prefix "1Clause") (re-bsd-clause 1))
    (re/ncg (str prefix "2Clause") (re-bsd-clause 2 ["Simplified"]))
    (re/ncg (str prefix "3Clause") (re-bsd-clause 3 ["New" "Revised" "Modified" "Standard"]))  ; Note: "Standard" is unofficial, but used by e.g. https://repo.clojars.org/org/cyverse/authy/3.0.1/authy-3.0.1.pom
    (re/ncg (str prefix "4Clause") (re-bsd-clause 4 ["Original" "Old"]))))

; Possible prefixes for BSD licenses
(defn- re-prefix-clauses
  []
  (re/alt-grp
    (re/ncg "beforeHP"        #"HP|Hewlett[\s\-–—]+Packard")
    (re/ncg "beforeLBNL"      #"LBNL|Lawrence[\s\-–—]+Berkeley[\s\-–—]+National[\s\-–—]+Labs")
    (re/ncg "beforeSystemics" #"Systemics(?<beforeW3works>[\s\-–—]+W3Works)?")
    (re/ncg "beforeAMPAS"     #"AMPAS|Academy[\s\-–—]+of[\s\-–—]+Motion[\s\-–—]+Picture[\s\-–—]+Arts[\s\-–—]+(?:and|&)[\s\-–—]+Sciences")
    (re/ncg "beforeAduna"     #"Aduna")))  ; Not an official prefix, but it appears in some license names and indicates BSD-3-Clause e.g. https://repo.clojars.org/art/uniroma2/it/org/openrdf/sesame/sesame-onejar/2.7.10/sesame-onejar-2.7.10.pom

; Possible suffixes for BSD licenses
(defn- re-suffix-clauses
  []
  (re/alt-grp
    ; BSD 1-4 clause suffixes
    (re/ncg "darwin"               #"(?:Ian[\s\-–—]+)?Darwin")
    (re/ncg "firstLines"           #"(?:1st|first)[\s\-–—]+lines([\s\-–—]+req(uirement)?)")
    (re/ncg "patent"               #"(?:Plus[\s\-–—]+)?Patent")
    (re/ncg "views"                #"(?:(?:with|w/)[\s\-–—]+)?Views(?:[\s\-–—]+sentence)?")
    (re/ncg "acpica"               #"acpica")
    (re/ncg "attribution"          #"(?:with|w/[\s\-–—]+)?attribution")
    (re/ncg "clear"                #"Clear")
    (re/ncg "flex"                 #"Flex")
    (re/ncg "HP"                   #"HP|Hewlett[\s\-–—]+Packard")
    (re/ncg "LBNL"                 #"LBNL|Lawrence[\s\-–—]+Berkeley[\s\-–—]+National[\s\-–—]+Labs")
    (re/ncg "modification"         #"Modification")
    (re/ncg "noMilitary"           #"No[\s\-–—]+Military")
    (re/ncg "noNuclearWarranty"    #"No[\s\-–—]+Nuclear[\s\-–—]+Warranty")
    (re/ncg "noNuclearLicense2014" #"No[\s\-–—]+Nuclear(?:[\s\-–—]+(?:variant|licen[cs]e))*[\s\-–—]+2014")
    (re/ncg "noNuclearLicense"     #"No[\s\-–—]+Nuclear(?:[\s\-–—]+(?:variant|licen[cs]e))*")
    (re/ncg "openMPI"              #"Open[\s\-–—]+MPI")
    (re/ncg "sun"                  #"Sun(?:[\s\-–—]+Microsystems)?")
    (re/ncg "shortened"            #"Shortened")
    (re/ncg "uc"                   #"\(?(?:University[\s\-–—]+of[\s\-–—]+California|UC|Cal)(?:[\s\-–—]+Specific)?\)?")

    ; Suffixes with distinct identifiers, unrelated to 1-4 clause licenses
    (re/ncg "reno43"               #"4\.3[\s\-–—]+RENO")
    (re/ncg "tahoe43"              #"4\.3[\s\-–—]+TAHOE")
    (re/ncg "advertising"          #"Advertising[\s\-–—]+Acknowledge?ment")
    (re/ncg "attributionHPND"      #"(?:with|w/[\s\-–—]+)?Attribution[\s\-–—]+(?:and|&)[\s\-–—]+(?:HPND|Historical[\s\-–—]+Permission[\s\-–—]+Notice[\s\-–—]+(?:and|&)[\s\-–—]+Disclaimer)[\s\-–—]+disclaimer")
    (re/ncg "inferno"              #"Inferno[\s\-–—]+Nettverk")
    (re/ncg "protection"           #"Protection")
    (re/ncg "scaBOF"               #"Source[\s\-–—]+Code[\s\-–—]+Attribution[\s\-–—]+beginning[\s\-–—]+of[\s\-–—]+file")
    (re/ncg "sca"                  #"Source[\s\-–—]+Code[\s\-–—]+Attribution")
    (re/ncg "freeBSDDoc"           #"Doc(?:umentation)?(?:[\s\-–—]+Licen[cs]e)?")
    (re/ncg "freeBSD"              #"FreeBSD")
    (re/ncg "netBSD"               #"NetBSD")

    ; Prefixes, but just in case they ever appear in suffix position (as happens in the identifier for BSD-Systemics)
    #"(?<afterSystemics>Systemics(?<afterW3works>[\s\-–—]+W3Works)?)"
    #"(?<afterAMPAS>AMPAS|Academy[\s\-–—]+of[\s\-–—]+Motion[\s\-–—]+Picture[\s\-–—]+Arts[\s\-–—]+(?:and|&)[\s\-–—]+Sciences)"))

(def re (re/join #"(?iuUx)(?<!\w)(?:The[\s\-–—]+)?"  ; Only public for ease of testing
                 "\n\n#### Prefix ####\n"
                 (re/opt-grp (re-prefix-clauses) lcir/fre-mws)
                 "\n\n#### Leading clause ####\n"
                 (re/opt-grp (re-bsd-any-clause "before") lcir/fre-ows)  ; We use optional ws here to catch values like "0BSD"
                 "\n\n#### Matching word ####\n"
                 (re/opt (re/alt-grp (re/ncg "beforeFreeBSD" "Free")
                                     (re/ncg "beforeNetBSD"  "Net")))
                 "BSD"
                 (re/opt-grp #"[\s\-–—]*(?:style|like)")
                 "\n\n#### Trailing clause ####\n"
                 (re/opt-grp lcir/fre-mws (re-bsd-any-clause "after"))
                 "\n\n#### Suffix ####\n"
                 (re/opt-grp lcir/fre-mws (re-suffix-clauses))
                 "\n\n#### Random dingleberries ####\n"
                 (re/zom-grp lcir/fre-mws #"(?:variant|(?:Pub?lic[\s\-–—]+)?licen[cs]e)")
                 (re/opt-grp lcir/fre-ows (lcir/re-version))
                 "\n\n#### Coda ####\n"
                 #"(?!\w)"))

(def ^:private pairs-d (delay (concat [
  [re match->ei]])))

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
