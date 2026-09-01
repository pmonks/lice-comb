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
  "BSD family license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [wreck.api                                         :as re]
            [lice-comb.impl.spdx                               :as spdx]
            [lice-comb.impl.utils                              :as u]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.regexes.version-expression         :as verexp]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

;(def ids-d (delay (concat ["0BSD" "AMPAS" "FreeBSD-DOC"] (filter #(s/starts-with? % "BSD-") @spdx/license-ids-d))))

; Note: this namespace only handles the various "claused" BSD licenses - things like FreeBSD are handled generically (by lice-comb.impl.license-detection.listed-licenses)
(def ids-d (delay (concat ["0BSD"] (filter (partial re-matches #"BSD-\d-Clause(?:-.*)?") @spdx/license-ids-d))))


;;
;; BSD REGEX CONSTRUCTION
;;

;####TODO: PARSE THESE STRUCTURES OUT OF @ids-d?
(def ^:private bsd-clauses {
  "ZeroClause"  [#"0*0" "zero"]
  "OneClause"   [#"0*1" "one"]
  "TwoClause"   [#"0*2" "two" "simplified"]
  "ThreeClause" [#"0*3" "three" "new" "revised" "modified" "standard"]  ; Note: "Standard" is unofficial, but used by e.g. https://repo.clojars.org/org/cyverse/authy/3.0.1/authy-3.0.1.pom
  "FourClause"  [#"0*4" "four" "original" "old"]
  "OtherClause" [#"0*(?:[5-9]|[1-9]\d+)"]})  ; Catch all for invalid clause counts (e.g. BSD 5 Clause)

(def ^:private bsd-variants {
  "Darwin"                  ["Darwin"                                                                                                                    2]
  "first-lines"             [(re/join (re/alt-grp (re/join #"0*1" ref/ows "st") "first") "lines")                                                        2]
  "Patent"                  ["Patent"                                                                                                                    2]
  "pkgconf-disclaimer"      [(re/join "pcgconf" ref/ows "disclaimer")                                                                                    2]
  "Views"                   ["Views"                                                                                                                     2]
  "FreeBSD"                 ["FreeBSD"                                                                                                                   2]  ; Deprecated
  "NetBSD"                  ["NetBSD"                                                                                                                    2]  ; Deprecated
  "acpica"                  ["acpica"                                                                                                                    3]
  "Attribution"             ["Attribution"                                                                                                               3]
  "Clear"                   ["Clear"                                                                                                                     3]
  "Flex"                    ["Flex"                                                                                                                      3]
  "HP"                      [ref/hewlett-packard                                                                                                         3]
  "LBNL"                    [(re/alt "LBNL" (re/join "Lawrence" ref/ows "Berkeley" ref/ows "National" ref/ows (re/alt-grp "Labs" "Laborator(?:y|ies)"))) 3]
  "Modification"            ["Modification"                                                                                                              3]
  "No-Military-License"     [(re/join "No" ref/ows "Military" ref/ows ref/license)                                                                       3]
  "No-Nuclear-License"      [(re/join "No" ref/ows (re/alt-grp "Nuclear" "Nuke") ref/ows ref/license (re/-la ref/ows "2014"))                            3]
  "No-Nuclear-License-2014" [(re/join "No" ref/ows (re/alt-grp "Nuclear" "Nuke") ref/ows ref/license ref/ows "2014")                                     3]
  "No-Nuclear-Warranty"     [(re/join "No" ref/ows (re/alt-grp "Nuclear" "Nuke") ref/ows "Warranty")                                                     3]
  "Open-MPI"                [(re/join "Open" ref/ows "MPI")                                                                                              3]
  "Sun"                     [ref/sun-oracle                                                                                                              3]
  "Tso"                     ["Tso"                                                                                                                       3]
  "Aduna"                   ["Aduna"                                                                                                                     3]  ; Not an official BSD prefix, but it appears in some license names and indicates BSD-3-Clause e.g. https://repo.clojars.org/art/uniroma2/it/org/openrdf/sesame/sesame-onejar/2.7.10/sesame-onejar-2.7.10.pom
  "Shortened"               ["Shortened"                                                                                                                 4]
  "UC"                      [ref/uc                                                                                                                      4]})

;####TODO: THIS IS THORNY
;(def ^:private inner-bsd-words #{"or" })
(def ^:private bsd-words #{"BSD" "public" ref/license "style" "c(?:lause)?" "type"})

(defn- ncg-for-clause
  [ncg-prefix [ncg-name synonyms]]
  (re/ncg (str ncg-prefix ncg-name)
          (apply re/alt (sort-by #(* -1 (count (re/str' %))) synonyms))))

(defn- ncg-for-variant
  [ncg-prefix [ncg-name [re]]]
  (re/ncg (str ncg-prefix (s/replace ncg-name "-" ""))
          re))

(defn- word-salad
  "Returns a BSD 'word salad' regex using `ncg-prefix` for the meaningful NCGs
  within (e.g. clause counts, variants, etc.)."
  [ncg-prefix]
  (re/alt-grp
    (apply re/alt (map (partial ncg-for-clause  ncg-prefix) bsd-clauses))
    (apply re/alt (map (partial ncg-for-variant ncg-prefix) bsd-variants))
    (apply re/alt bsd-words)))

; Public for ease of testing
(def re (re/fgrp "ix"
                 "\n\n#### Preamble ####\n"
                 ref/nwb
                 (re/opt-grp "The" ref/mws)
                 "\n\n#### Before word salad ####\n"
                 (re/zom-grp (word-salad "before") ref/ows)
                 "\n\n#### Matching word ####\n"
                 "BSD"
                 "\n\n#### After word salad ####\n"
                 ref/ows
                 (re/zom-grp (word-salad "after") ref/ows)
                 ;####TODO: VERSION SHOULD GO IN THE WORD SALAD
                 "\n\n#### Version ####\n"
                 (re/opt-grp ref/ows (verexp/expression-regex ["1.0" "2.0" "3.0" "4.0"]))  ; e.g. for https://repo.clojars.org/org/clojars/ndepalma/jme-game-engine/3.0/jme-game-engine-3.0.pom
                 "\n\n#### Coda ####\n"
                 ref/nwa))


;;
;; FRAGMENT INFO CONSTRUCTION FROM A MATCH
;;

(defn- ncg-from-match
  "Retrieves all values of the given `ncg-name` from match `m`."
  [ncg-name m]
  (seq (distinct (map s/trim (filter identity [(get m (str "before" ncg-name)) (get m (str "after" ncg-name))])))))

(defn- ncg-in-match?
  "Is the given (partial) `ncg-name` in match `m`?  Checks both 'before' and
  'after' the BSD matching word."
  [ncg-name m]
  (boolean
    (or
      (get m (str "before" ncg-name))
      (get m (str "after"  ncg-name)))))

(defn- determine-clause-counts
  "Returns a sequence of the clause count(s) found in match `m`, as integers.
  Note that they are NOT validated."
  [m]
  (if-let [clauses (seq
                     (distinct
                       (filter identity (concat [(when (ncg-in-match? "ZeroClause"  m) 0)
                                                 (when (ncg-in-match? "OneClause"   m) 1)
                                                 (when (ncg-in-match? "TwoClause"   m) 2)
                                                 (when (ncg-in-match? "ThreeClause" m) 3)
                                                 (when (ncg-in-match? "FourClause"  m) 4)]
                                                (map u/parse-lng (ncg-from-match "OtherClause" m))))))]
    ; We found clause(s), so return them in sorted order
    (sort clauses)
    ; We didn't find clause(s), so check for a version number
    (when-let [version (get m "VersionNumber")]
      [(int (u/parse-dbl version))])))  ; version might be something like 3.0, so parse it as a double but then drop the fractional part

(defn- bsd-match->fragment-info
  "Turns a match by the BSD regex into a fragment info map."
  [m]
  (let [clause-counts           (determine-clause-counts m)
        variants                (seq (distinct (filter identity (map #(when (ncg-in-match? (s/replace % "-" "") m) %) (keys bsd-variants)))))
        implied-clause-counts   (seq (distinct (filter identity (map #(second (get bsd-variants %)) variants))))
        valid-clause-counts     (seq (filter #(<= % 4) clause-counts))
        ;####TODO: This seems like a shit way to turn confidence-explanations into a set that doesn't contain nil - see if there's a better way
        confidence-explanations (some->> [(when (empty? clause-counts)                                 :missing-bsd-clause-count)
                                          (when (> (count clause-counts) 2)                            :inconsistent-bsd-clause-counts)
                                          (when (some #(> % 4) clause-counts)                          :invalid-bsd-clause-count)
                                          (when (> (count variants) 1)                                 :multiple-bsd-variants)
                                          (when (and (not (empty? implied-clause-counts))
                                                     (not= implied-clause-counts valid-clause-counts)) :invalid-bsd-clause-count-variant-combination)]
                                         (filter identity)
                                         seq
                                         set)
        final-clause-count      (case [(empty? clause-counts) (empty? implied-clause-counts)]
                                  [true  true]  4
                                  [true  false] (first implied-clause-counts)
                                  [false true]  (first clause-counts)
                                  [false false] (first implied-clause-counts))
        final-variant           (when-not (empty? variants)
                                  (let [variant (first variants)]
                                    (when (not= variant "Aduna")
                                      variant)))
        id                      (if (zero? final-clause-count)
                                  "0BSD"
                                  (str "BSD-" final-clause-count "-Clause" (when final-variant (str "-" final-variant))))]
    (mp/listed-match->fragment-info @ids-d id "BSD regex" confidence-explanations m)))

(defn detect
  "Detects any BSD licenses found in the strings in `coll`, and replaces them
  with a fragment info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll re bsd-match->fragment-info))
