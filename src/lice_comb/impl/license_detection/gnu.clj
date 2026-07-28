;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.gnu
  "GNU (and Affero Inc.) family license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                    :as s]
            [clojure.set                                       :as set]
            [wreck.api                                         :as re]
            [spdx.licenses                                     :as sl]
            [lice-comb.impl.spdx                               :as spdx]
            [lice-comb.impl.regexes.fragments                  :as ref]
            [lice-comb.impl.regexes.version-expression         :as verexp]
            [lice-comb.impl.parsing.faux-parse                 :as faux]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; Unlike the other detection namespaces, for the GNU family we use a "word salad" strategy for detection.  Basically
; this involves matching any words that are known to appear in a GNU family license *in any order*.  So even nonsensical
; values that don't appear in the wild will match, such as "Open Source License License GNU v3 Licensed Under or later"
;
; For this reason, the `detect` function in this namespace should be called *last, after all other detections have
; already been performed*.
;
; We do this because of the sheer number of variations of GNU family license names in the wild.

(def ids-d (delay (filter #(and (sl/listed-id? %)               ; This namespace only handles GNU license identifiers, not exceptions
                                (or (s/starts-with? % "AGPL-")  ; It also handles AGPL-1.0, despite that license not being published by the FSF/GNU
                                    (s/starts-with? % "LGPL-")
                                    (s/starts-with? % "GPL-")))
                          @spdx/license-ids-d)))

;;
;; GNU REGEXES CONSTRUCTION
;;

; Generic GNU family word regexes
(def ^:private gnu-words               ["The" "GNU" "GPL" ref/general ref/public ref/license
                                        (re/join ref/license #"d?" (re/opt-grp ref/mws "Under"))
                                        (re/join "Open" ref/mws "Source")
                                        "FOSS" "OSS"])
(def ^:private gnu-versions            (verexp/expression-regex "gnu" ["1.0" "2.0" "2.1" "3.0"]))  ; Note: this doesn't technically have to be exhaustive - just enough to emit an appropriate regex for GNU version numbers))
(def ^:private gnu-version-esque-class (re/zom-chcl ref/raw-ws #"v\d\."))

; AGPL specific regexes
(def ^:private re-agpl-words-before (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?AGPL" ref/ows #"\)?") "Affero"]))))
(def ^:private re-agpl-words-after  (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?AGPL" gnu-version-esque-class #"\)?") "Affero"]))))  ; Only include version variants *after* the actual version

; Only public for ease of testing
(def re-agpl (re/fgrp "ix"
                      ref/nwb
                      "\n\n#### Leading word salad ####\n"
                      (re/zom-grp re-agpl-words-before ref/mws)
                      "\n\n#### Matching words ####\n"
                      (re/ncg "agpl" (re/alt (re/join "A" ref/ows (re/alt-grp "GPL" "PGL")) "Affero"))  ; "APGL" seen here: https://repo.clojars.org/cider-ci/open-session/2.0.0-beta.1/open-session-2.0.0-beta.1.pom
                      "\n\n#### Pre-version word salad ####\n"
                      (re/zom-grp ref/mws re-agpl-words-before)
                      "\n\n#### Version and version qualifier ####\n"
                      (re/opt-grp ref/ows gnu-versions)
                      "\n\n#### Post-version word salad ####\n"
                      (re/zom-grp ref/mws re-agpl-words-after)
                      "\n\n#### Date ####\n"
                      (re/opt-grp ref/mws ref/date)
                      "\n\n#### Coda ####\n"
                      ref/nwa))

; LGPL specific regexes
(def ^:private re-lesser-or-library  (re/or-grp "Lesser" "Library" (re/alt-grp (re/join ref/ows "/" ref/ows) (re/join ref/mws "or" ref/mws))))
(def ^:private re-lgpl-words-before  (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?LGPL" ref/ows #"\)?") re-lesser-or-library]))))
(def ^:private re-lgpl-words-after   (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?LGPL" gnu-version-esque-class #"\)?") re-lesser-or-library]))))  ; Only include version variants *after* the actual version

; Only public for ease of testing
(def re-lgpl (re/fgrp "ix"
                      ref/nwb
                      "\n\n#### Leading word salad ####\n"
                      (re/zom-grp re-lgpl-words-before ref/mws)
                      "\n\n#### Matching words ####\n"
                      (re/ncg "lgpl"
                              (re/alt (re/join "L" ref/ows "GPL")
                                      (re/join (re/alt-grp "GNU" "GPL") ref/mws re-lesser-or-library)
                                      (re/join re-lesser-or-library ref/mws (re/alt-grp "GNU" "GPL" ref/general))))
                      "\n\n#### Pre-version word salad ####\n"
                      (re/zom-grp ref/mws re-lgpl-words-before)
                      "\n\n#### Version and version qualifier ####\n"
                      (re/opt-grp ref/ows gnu-versions)
                      "\n\n#### Post-version word salad ####\n"
                      (re/zom-grp ref/mws re-lgpl-words-after)
                      "\n\n#### Date ####\n"
                      (re/opt-grp ref/mws ref/date)
                      "\n\n#### Coda ####\n"
                      ref/nwa))

; GPL specific regexes
(def ^:private re-gpl-words-before (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?GPL" ref/ows #"\)?")]))))
(def ^:private re-gpl-words-after  (re/grp (apply re/alt (concat gnu-words [(re/join #"\(?GPL" gnu-version-esque-class)]))))  ; Only include version variants *after* the actual version

; Only public for ease of testing
(def re-gpl (re/fgrp "ix"
                     ref/nwb
                     "\n\n#### Leading word salad ####\n"
                     (re/zom-grp re-gpl-words-before ref/mws)
                     "\n\n#### Matching words ####\n"
                     (re/ncg "gpl" (re/alt "GNU" "GPL" (re/join ref/general (re/opt-grp ref/mws ref/public) (re/opt-grp ref/mws ref/license))))
                     "\n\n#### Pre-version word salad ####\n"
                     (re/zom-grp ref/mws re-gpl-words-before)
                     "\n\n#### Version and version qualifier ####\n"
                      (re/opt-grp ref/ows gnu-versions)
                     "\n\n#### Post-version word salad ####\n"
                     (re/zom-grp ref/mws re-gpl-words-after)
                     "\n\n#### Date ####\n"
                     (re/opt-grp ref/mws ref/date)
                     "\n\n#### Coda ####\n"
                     ref/nwa))

;;
;; FRAGMENT INFO CONSTRUCTION FROM A MATCH
;;

(defn- gnu-match->fragment-info
  "Construct a fragment info map from `m`, a map returned from a rencg regex
  match/find match using one of the GNU regexes constructed above."
  [variant m]
  (let [version-present?   (boolean (get m "gnuVersionNumber"))
        default-version    (case variant
                             "AGPL" (if (s/includes? (:match m) "GNU")
                                      "3.0"  ; GNU AGPL started at v3.0
                                      "1.0") ; But AGPL by itself (without a "GNU" qualifier) started at v1.0
                             "LGPL" "2.0"
                             "1.0")  ; Note: on the advice of the SPDX technical team, default to earliest GPL version when version not present (unlike most other license families!)
        version            (s/replace (get m "gnuVersionNumber" default-version) #"[\s\p{Punct}]+" ".")  ; Turn other version number point separators into . (undercore appears in at least one license name, for example)
        confidence-explanations
                           (if version-present?
                             (when-not (s/includes? version ".")
                               #{:version-near-match})
                             #{:missing-version})
        version            (if (s/includes? version ".")
                             version
                             (str version ".0"))
        [version-range confidence-explanations]
                           (cond (contains? m "gnuOrLater") ["or-later" confidence-explanations]
                                 (contains? m "gnuOnly")    ["only"     confidence-explanations]
                                 :else                      [(if version-present? "only" "or-later")  ; Note: on the advice of SPDX technical team, default to "or later" version range if version number or range not present (unlike most other license families!)
                                                             (set/union #{:missing-version-range} confidence-explanations)])
        id                 (str variant "-" version  "-" version-range)
        [id confidence-explanations]
                           (if (sl/listed-id? id)
                             [id confidence-explanations]
                             [(str variant "-" default-version "-or-later")  ; Note: on the advice of SPDX technical team, default to "or later" variant if version not valid
                              (set/union #{:invalid-version} confidence-explanations)])]
    (mp/listed-match->fragment-info @ids-d id (str variant " regex") confidence-explanations m)))

(defn detect
  "Detects any Creative Commons licenses found in the strings in `coll`, and
  replaces them with a fragment info map. Returns other elements unchanged."
  [coll]
  (faux/parse coll
              re-agpl (partial gnu-match->fragment-info "AGPL")
              re-lgpl (partial gnu-match->fragment-info "LGPL")
              re-gpl  (partial gnu-match->fragment-info "GPL")))
