;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.bsd
  "Helper functionality related to substituting matches for the BSD family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [clojure.set                                       :as set]
            [wreck.api                                         :as re]
            [spdx.licenses                                     :as sl]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.spdx                               :as lcis]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.expression-info                    :as ei]
            [lice-comb.impl.utils                              :as lciu]
            [lice-comb.impl.license-detection.match-processing :as mp]))

(def ids-d (delay (concat ["0BSD" "AMPAS" "FreeBSD-DOC"] (filter #(s/starts-with? % "BSD-") @lcis/license-ids-d))))


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
  "Returns a regex that will match a numeric rendition of BSD `n` clause,
  including both the number itself ('1', '2', etc.) and also the English
  language equivalent ('One', 'Two', etc.), and with various prefixes and
  suffixes ('Clause', 'Type', etc.).

  When `n` is `nil` or does not have a listed EnNglish language name,
  returns a regex that will match any digits."
  ([] (re-bsd-numeric-clause nil))
  ([n]
   (re/grp (re/opt-grp (re/grp (re/alt #"C(?:lause)?" "Type")) ref/ows)
           (if-let [nm (number->name n)]
             (re/alt-grp nm (re/join "0*" n))
             #"\d+")
           (re/opt-grp ref/ows "Clause"))))

(defn- re-bsd-textual-clause
  "Returns a regex that will match the textual rendition of a BSD clause count,
  expressed as a sequence of clause count identifying names that aren't numbers
  (e.g. 'Simplified', 'New, 'Original', etc.)."
  [clause-names]
  (when (seq clause-names)
    (if (= 1 (count clause-names))
      (re/grp ref/oqots (first clause-names) ref/oqots)
      (let [fre-names (apply re/alt-grp (map re/esc clause-names))]
        (re/grp ref/oqots
                fre-names
                ref/oqots
                (re/zom-grp
                  (re/alt-grp (re/grp ref/mws "or"     ref/mws)
                              (re/grp ref/ows #"[\\/]" ref/ows)
                              ref/mws)
                  ref/oqots
                  fre-names
                  ref/oqots))))))

(defn- re-bsd-clause
  "Returns a regex that will match both numeric and textual renditions of a BSD
  clause count."
  ([n] (re-bsd-clause n nil))
  ([n clause-names]
   (if (seq clause-names)
     (re/or-grp (re-bsd-numeric-clause n) (re-bsd-textual-clause clause-names) ref/mws)
     (re-bsd-numeric-clause n))))

(defn- re-bsd-any-clause
  "Returns a regex that will match any BSD clause clause, in either numeric or
  textual renditions. `prefix` is a prefix to use for each named capturing
  group, and will be followed by `#Clause` (where `#` is the clause count
  number - 0-4)."
  [prefix]
  (re/alt-grp
    (re/ncg (str prefix "0Clause")            (re-bsd-clause 0))
    (re/ncg (str prefix "1Clause")            (re-bsd-clause 1))
    (re/ncg (str prefix "2Clause")            (re-bsd-clause 2 ["Simplified"]))
    (re/ncg (str prefix "3Clause")            (re-bsd-clause 3 ["New" "Revised" "Modified" "Standard"]))  ; Note: "Standard" is unofficial, but used by e.g. https://repo.clojars.org/org/cyverse/authy/3.0.1/authy-3.0.1.pom
    (re/ncg (str prefix "4Clause")            (re-bsd-clause 4 ["Original" "Old"]))
    (re/ncg (str prefix "InvalidClauseCount") (re-bsd-numeric-clause))))  ; Catch-all for invalid clause counts (e.g. "BSD 999 clause")

(def ^:private ampas (re/alt "AMPAS" (re/join "Academy" ref/mws "of" ref/mws  "Motion" ref/mws "Picture" ref/mws "Arts" ref/mws ref/ands ref/mws "Sciences")))

; Possible prefixes for BSD licenses
(def ^:private prefix-clauses
  (re/alt-grp
    (re/ncg "beforeHP"        (re/alt "HP" (re/join "Hewlett" ref/mws "Packard")))
    (re/ncg "beforeLBNL"      (re/alt "LBNL" (re/join "Lawrence" ref/mws "Berkeley" ref/mws "National" ref/mws "Labs")))
    (re/ncg "beforeSystemics" "Systemics" (re/opt-ncg "beforeW3works" ref/mws "W3Works"))
    (re/ncg "beforeAMPAS"     ampas)
    (re/ncg "beforeAduna"     "Aduna")))  ; Not an official BSD prefix, but it appears in some license names and indicates BSD-3-Clause e.g. https://repo.clojars.org/art/uniroma2/it/org/openrdf/sesame/sesame-onejar/2.7.10/sesame-onejar-2.7.10.pom

; Possible suffixes for BSD licenses
(def ^:private suffix-clauses
  (re/alt-grp
    ; BSD 1-4 clause suffixes
    (re/ncg "darwin"               (re/opt-grp "Ian" ref/mws) "Darwin")
    (re/ncg "firstLines"           (re/join (re/alt-grp "1st" "first") ref/mws) "lines" ref/mws #"req(uirement)?")
    (re/ncg "patent"               (re/opt-grp "Plus" ref/mws) "Patent")
    (re/ncg "views"                ref/withs ref/mws "Views" (re/opt-grp ref/mws "sentence"))
    (re/ncg "acpica"               "acpica")
    (re/ncg "attribution"          (re/opt-grp ref/withs ref/mws) "attribution")
    (re/ncg "clear"                "Clear")
    (re/ncg "flex"                 "Flex")
    (re/ncg "HP"                   (re/alt "HP" (re/join "Hewlett" ref/mws "Packard")))
    (re/ncg "LBNL"                 (re/alt "LBNL" (re/join "Lawrence" ref/mws "Berkeley" ref/mws "National" ref/mws "Labs")))
    (re/ncg "modification"         "Modification")
    (re/ncg "noMilitary"           "No" ref/mws "Military")
    (re/ncg "noNuclearWarranty"    "No" ref/mws "Nuclear" ref/mws "Warranty")
    (re/ncg "noNuclearLicense2014" "No" ref/mws "Nuclear" (re/zom-grp ref/mws (re/alt-grp "variant" ref/license)) ref/mws "2014")
    (re/ncg "noNuclearLicense"     "No" ref/mws "Nuclear" (re/zom-grp ref/mws (re/alt-grp "variant" ref/license)) (re/-la ref/bounded-mws "2014"))
    (re/ncg "openMPI"              "Open" ref/mws "MPI")
    (re/ncg "sun"                  "Sun" (re/opt-grp ref/mws "Microsystems"))
    (re/ncg "tso"                  "Tso")
    (re/ncg "shortened"            "Shortened")
    (re/ncg "uc"                   (re/opt (re/esc "("))
                                   (re/alt-grp (re/join "University" ref/mws "of" ref/mws "California") "UC" "Cal")
                                   (re/opt-grp ref/mws "Specific")
                                   (re/opt (re/esc ")")))

    ; Suffixes with distinct identifiers, unrelated to 1-4 clause licenses
    (re/ncg "reno43"               (re/esc "4.3") ref/mws "RENO")
    (re/ncg "tahoe43"              (re/esc "4.3") ref/mws "TAHOE")
    (re/ncg "advertising"          "Advertising" ref/mws ref/acknowledgement)
    (re/ncg "attributionHPND"      (re/opt-grp ref/withs ref/mws) "Attribution" ref/mws ref/ands ref/mws
                                   (re/alt-grp "HPND"
                                               (re/join "Historical" ref/mws "Permission" ref/mws "Notice" ref/mws ref/ands ref/mws "Disclaimer")))
    (re/ncg "inferno"              "Inferno" ref/mws (re/alt-grp "Nettverk" "Network"))
    (re/ncg "protection"           "Protection")
    (re/ncg "scaBOF"               "Source" ref/mws "Code" ref/mws "Attribution" ref/mws "beginning" ref/mws "of" ref/mws "file")
    (re/ncg "sca"                  "Source" ref/mws "Code" ref/mws "Attribution" (re/-la ref/bounded-mws "beginning" ref/bounded-mws "of" ref/bounded-mws "file"))
    (re/ncg "freeBSDDoc"           #"Doc(?:umentation)?" (re/opt-grp ref/mws ref/license))
    (re/ncg "freeBSD"              "FreeBSD")
    (re/ncg "netBSD"               "NetBSD")
    (re/ncg "markModification"     "Mark" ref/ows "Modifications")

    ; Prefixes, but just in case they ever appear in suffix position (as happens in the identifier for BSD-Systemics)
    (re/ncg "afterSystemics"       "Systemics" (re/opt-ncg "afterW3Works" ref/mws "W3Works"))
    (re/ncg "afterAMPAS"           ampas)))

; Only public for ease of testing
(def re (re/fgrp "ix"
                 ref/nwb
                 (re/opt-grp "The" ref/mws)
                 "\n\n#### Prefix ####\n"
                 (re/opt-grp prefix-clauses ref/mws)
                 "\n\n#### Leading clause ####\n"
                 (re/opt-grp (re-bsd-any-clause "before") ref/ows)  ; We use optional ws here to catch values like "0BSD"
                 "\n\n#### Matching word ####\n"
                 (re/opt (re/alt-grp (re/ncg "beforeFreeBSD" "Free")
                                     (re/ncg "beforeNetBSD"  "Net")))
                 "BSD"
                 (re/opt-grp ref/ows (re/alt "style" "like"))
                 "\n\n#### Trailing clause ####\n"
                 (re/opt-grp ref/mws (re-bsd-any-clause "after"))
                 "\n\n#### Suffix ####\n"
                 (re/opt-grp ref/mws suffix-clauses)
                 "\n\n#### Random dingleberries ####\n"
                 (re/zom-grp ref/mws (re/alt-grp "variant" (re/join (re/opt-grp ref/public ref/mws) ref/license)))
;####TODO: REIMPLEMENT THIS IN TERMS OF THE NEW NAMESPACES
;                 (re/opt-grp ref/ows (lcir/re-version))
                 "\n\n#### Coda ####\n"
                 ref/nwa))


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
                (vector (when (mp/get-rencgs m ["before0Clause" "after0Clause"]) 0)
                        (when (mp/get-rencgs m ["before1Clause" "after1Clause"]) 1)
                        (when (mp/get-rencgs m ["before2Clause" "after2Clause"]) 2)
                        (when (mp/get-rencgs m ["before3Clause" "after3Clause"]) 3)
                        (when (mp/get-rencgs m ["before4Clause" "after4Clause"]) 4)
                        (when version-number version-number)))))))

(defn- clause-based-identifier
  "Returns a tuple of [identifier confidence-explanations] if match
  `m` represents a standard BSD N clause identifier (such as `BSD-4-Clause`).
  Returns `nil` is `m` did not match a 'clause based identifier."
  [m]
  (let [clause-counts         (clause-counts m)
        invalid-clause-count? (boolean (mp/get-rencgs m ["beforeInvalidClauseCount" "afterInvalidClauseCount"]))
        [clause-count confidence-explanations]
                      (case (count clause-counts)
                        0     [4 (if invalid-clause-count? #{:invalid-bsd-clause-count} #{:missing-bsd-clause-count})]
                        1     (let [clause-count (first clause-counts)]
                                (if (valid-clause-count? clause-count)
                                  [clause-count]
                                  [4 #{:invalid-bsd-clause-count}]))
                        ; Multiple distinct clause counts, so we pick the least restrictive one (i.e. the lowest)
                        (let [clause-count (apply min clause-counts)]
                          (if (valid-clause-count? clause-count)
                            [clause-count (into #{:inconsistent-bsd-clause-counts} (when invalid-clause-count? :invalid-bsd-clause-count))]
                            ; Invalid clause count found, try again with invalid clause counts filtered out
                            (if-let [valid-clause-counts (seq (filter valid-clause-count? clause-counts))]
                              [(apply min valid-clause-counts) #{:inconsistent-bsd-clause-counts :invalid-bsd-clause-count}]
                              [4                               #{:inconsistent-bsd-clause-counts :invalid-bsd-clause-count}]))))
        id            (if (= 0  clause-count)
                        "0BSD"
                        (str "BSD-" clause-count "-Clause"))]
    [id confidence-explanations]))  ; We don't assert or canonicalise the identifier here, as that has to happen after we've appended any suffix

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
    (get m "tso")                  "-Tso"
    (get m "shortened")            "-Shortened"
    (get m "uc")                   "-UC"))

(defn- suffix-based-identifier
  "Returns a tuple of [identifier confidence-explanations] if match
  `m` represents a 'faux suffix' based identifier (such as `BSD-4.3RENO`).
  Returns `nil` is `m` did not match a 'faux suffix' based identifier."
  [m]
  (when-let [id (cond
                  (get m "reno43")                                       "BSD-4.3RENO"
                  (get m "tahoe43")                                      "BSD-4.3TAHOE"
                  (get m "advertising")                                  "BSD-Advertising-Acknowledgement"
                  (get m "attributionHPND")                              "BSD-Attribution-HPND-disclaimer"
                  (get m "inferno")                                      "BSD-Inferno-Nettverk"
                  (get m "protection")                                   "BSD-Protection"
                  (get m "scaBOF")                                       "BSD-Source-beginning-file"
                  (get m "sca")                                          "BSD-Source-Code"
                  (get m "freeBSDDoc")                                   "FreeBSD-DOC"
                  (get m "beforeHP")                                     "BSD-3-Clause-HP"
                  (get m "beforeLBNL")                                   "BSD-3-Clause-LBNL"
                  (mp/get-rencgs m ["beforeW3works"   "afterW3works"])   "BSD-Systemics-W3Works"  ; This must go before systemics, since it will always match both
                  (mp/get-rencgs m ["beforeSystemics" "afterSystemics"]) "BSD-Systemics"
                  (mp/get-rencgs m ["beforeAMPAS"     "afterAMPAS"])     "AMPAS"
                  (get m "beforeAduna")                                  "BSD-3-Clause"  ; See https://www.d3web.de/Wiki.jsp?page=Aduna-BSD
                  (mp/get-rencgs m ["beforeFreeBSD"   "freeBSD"])        "BSD-2-Clause-FreeBSD"
                  (mp/get-rencgs m ["beforeNetBSD"    "netBSD"])         "BSD-2-Clause-NetBSD"
                  (get m "markModification")                             "BSD-Mark-Modifications")]
    (into [id]
          (when (or (clause-counts m) (suffix m))
            #{:invalid-bsd-combination}))))

(defn- match->ei
  "Turns a match from the BSD regex into an expression-info map."
  [m]
  (let [matched-text (:match m)
        [id confidence-explanations]
                   (if-let [sbi (suffix-based-identifier m)]  ; First check if it's a suffix-based identifier
                     sbi
                     (let [[id con-exp] (clause-based-identifier m)
                           suffix           (suffix m)
                           id-and-suffix    (str id suffix)]
                       (if (sl/listed-id? id-and-suffix)
                         [id-and-suffix con-exp]
                         [id            (set/union #{:invalid-bsd-combination} con-exp)])))]
    (ei/expression-info id :regex-matching :concluded matched-text confidence-explanations)))

(defn detect
  "Substitutes any BSD licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll re match->ei))
