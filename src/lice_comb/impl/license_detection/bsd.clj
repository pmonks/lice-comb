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

; Note: this namespace only handles the various "claused" BSD licenses - things like FreeBSD are handled generically (by lice-comb.impl.license-detection.listed-licenses)
(def ids-d (delay (concat ["0BSD"] (filter (partial re-matches #"BSD-\d-Clause(?:-.*)?") @spdx/license-ids-d))))


;;
;; BSD REGEX CONSTRUCTION
;;

(def ^:private ows+qots (re/zom ref/ws+qots))

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
  "FreeBSD"                 ["FreeBSD"                                                                                                                   2]  ; Deprecated, handled by clj-spdx canonicalisation
  "NetBSD"                  ["NetBSD"                                                                                                                    2]  ; Deprecated, handled by clj-spdx canonicalisation
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

(def ^:private bsd-words ["BSD" "public" ref/license "style" #"c(?:lause)?" "type"])

(defn- ncg-for-clause
  [ncg-prefix [ncg-name synonyms]]
  (let [clause-alt (apply re/alt (sort-by #(* -1 (count (re/str' %))) synonyms))]
    (re/join (re/ncg (str ncg-prefix ncg-name) clause-alt)
             (re/zom-grp ows+qots (re/alt-grp "or" "/" #"\\") ows+qots (re/grp clause-alt)))))

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
                 (re/zom-grp ows+qots (word-salad "before") ows+qots)
                 "\n\n#### Matching word ####\n"
                 "BSD"
                 "\n\n#### After word salad ####\n"
                 ref/ows
                 (re/zom-grp ows+qots (word-salad "after") ows+qots)
                 ;####TODO: VERSION SHOULD GO IN THE WORD SALAD
                 "\n\n#### Version ####\n"
                 (re/opt-grp ref/ows (verexp/expression-regex "bsd" ["1.0" "2.0" "3.0" "4.0"]))  ; e.g. for https://repo.clojars.org/org/clojars/ndepalma/jme-game-engine/3.0/jme-game-engine-3.0.pom
                 "\n\n#### Coda ####\n"
                 ref/nwa))


;;
;; FRAGMENT INFO CONSTRUCTION FROM A MATCH
;;

(defn- ncg-from-match
  "Retrieves all values of the given `ncg-name` from match `m`."
  [ncg-name m]
  (seq (distinct (map s/trim (filter some? [(get m (str "before" ncg-name)) (get m (str "after" ncg-name))])))))

(defn- ncg-in-match?
  "Is the given (partial) `ncg-name` in match `m`?  Checks both 'before' and
  'after' the BSD matching word."
  [ncg-name m]
  (boolean
    (or
      (get m (str "before" ncg-name))
      (get m (str "after"  ncg-name)))))

(def ^:private sort-seq (comp seq sort distinct (partial filter some?)))

(defn- determine-clause-counts
  "Returns a thruple containing:

  1. valid clauses detected in match `m`
  2. other clauses detected in match `m`
  3. a version number found in match `m`

  Any or all of these values may be `nil`."
  [m]
  (let [clauses        (sort-seq [(when (ncg-in-match? "ZeroClause"  m) 0)
                                  (when (ncg-in-match? "OneClause"   m) 1)
                                  (when (ncg-in-match? "TwoClause"   m) 2)
                                  (when (ncg-in-match? "ThreeClause" m) 3)
                                  (when (ncg-in-match? "FourClause"  m) 4)])
        other-clauses  (sort-seq (map u/parse-lng (ncg-from-match "OtherClause" m)))
        version-number (u/sint (u/parse-dbl (get m "bsdVersionNumber")))]
    [clauses other-clauses version-number]))

(defn- bsd-match->fragment-info
  "Turns a match by the BSD regex into a fragment info map."
  [m]
  (let [[clauses
         other-clauses
         version-number]        (determine-clause-counts m)
        all-clauses             (sort-seq (concat clauses other-clauses [version-number]))
        variants                (sort-seq (map #(when (ncg-in-match? (s/replace % "-" "") m) %) (keys bsd-variants)))
        implied-clause-counts   (sort-seq (map #(second (get bsd-variants %)) variants))
        implied-clause-count    (second (get bsd-variants (first variants)))
        final-variant           (when-not (empty? variants)
                                  (let [variant (first variants)]
                                    (when (not= variant "Aduna")
                                      variant)))
        clause-count            (cond
                                  clauses                          (first clauses)
                                  (and (not (nil? version-number))
                                       (<= 1 version-number 4))    version-number)
        final-clause-count      (cond
                                  implied-clause-counts (first implied-clause-counts)  ; Favour implied clause count from variant over the actual clause count, if they're inconsistent
                                  clause-count          clause-count
                                  :else                 4)
        ;####TODO: This seems like a shit way to turn confidence-explanations into a set that doesn't contain nil - see if there's a better way
        confidence-explanations (some->> [(when (and (nil? clauses)
                                                     (nil? other-clauses)
                                                     (nil? version-number))                          :missing-bsd-clause-count)
                                          (when other-clauses                                        :invalid-bsd-clause-count)
                                          (when (> (count all-clauses) 1)                            :inconsistent-bsd-clause-counts)
                                          (when (> (count variants) 1)                               :multiple-bsd-variants)
                                          (when (and (some? version-number)
                                                     (not= final-clause-count version-number))       (if (<= 1 version-number 4)
                                                                                                       :inconsistent-bsd-clause-counts
                                                                                                       :invalid-bsd-clause-count))
                                          (when (and (some? final-variant)
                                                     (some? clause-count)
                                                     (not= final-clause-count implied-clause-count)) :invalid-bsd-clause-count-variant-combination)]
                                         (filter some?)
                                         seq
                                         set)
        id                      (if (zero? final-clause-count)
                                  "0BSD"
                                  (str "BSD-" final-clause-count "-Clause" (when final-variant (str "-" final-variant))))]
    (mp/listed-match->fragment-info @ids-d id "BSD regex" confidence-explanations m)))

(defn detect
  "Detects any BSD licenses found in the strings in `coll`, and replaces them
  with a fragment info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll re bsd-match->fragment-info))
