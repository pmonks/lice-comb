;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-regexes
  "License (and license exception) regex related functionality. Note: this
  namespace is not part of the public API of lice-comb and may change without
  notice."
  (:require [clojure.string                    :as s]
            [clojure.math.combinatorics        :as combo]
            [spdx.identifiers                  :as si]
            [wreck.api                         :as re]
            [lice-comb.impl.faux-parse         :as faux]
            [lice-comb.impl.regex-fragments    :as ref]
            [lice-comb.impl.version-series     :as verser]
            [lice-comb.impl.version-expression :as verexp]
            [lice-comb.impl.utils              :as lciu]))

(def ^:private re-placeholder-ver  (re/join ref/ows (re/esc verser/placeholder-ver)))
(def ^:private re-placeholder-oool (re/join ref/ows (re/esc verser/placeholder-oool)))

(def ^:private ncg-prefix-id       "id")
(def ^:private ncg-prefix-name     "name")
(def ^:private ncg-suffix-trailing "Trailing")  ;####TODO: decide if "Trailing" is the best term here...


;; Functions involved in license regex construction.

(defn- common-replacements
  "Performs replacements common to both identifiers and names, in `s` (a
  `String), returning a sequence."
  [^String s ^String ncg-prefix versions]
  (when-not (s/blank? s)
    (let [re-vers  (re/opt-grp ref/ows (re/opt-grp ref/single-qots) (verexp/expression-regex ncg-prefix versions))
          re-oool  (re/opt-grp ref/ows (verexp/suffix-regex (str ncg-prefix ncg-suffix-trailing)))
          template (if (re-find re-placeholder-ver s)
                     (lciu/replacing-split s re-placeholder-ver re-vers)
                     [s re-vers])]  ; Always add a version matching component on the end, to handle cases where the version doesn't appear in the name (e.g. Adobe-2006, Arphic-1999)
      (faux/parse (concat [(re/opt-grp #"The" ref/mws)] template)
                  ; only or or-later suffix
                  re-placeholder-oool                   re-oool
                  ; Name alternatives
                  #"(?i:(?<!\w)Apache(?!\w))"           (re/inline (re/alt-grp (re/join "Apache" (re/opt-grp ref/mws (re/alt-grp "Software" "SW"))) "ASL"))
                  #"(?i:(?<!\w)Beerware(?!\w))"         (re/inline (re/join "Beer" ref/ows "ware"))
                  #"(?i:(?<!\w)Classpath\s+exception(?!\w))" (re/inline (re/join (re/opt-grp "GNU" ref/ows) (re/alt-grp "CPE" (re/join "Classpath" ref/ows "exception"))))
                  ; MIT/X11/ISC overlaps
                  #"(?i:(?<!\w)MIT(?!\w))"              (re/inline (let [x11-or-isc (re/alt-grp "X11" "ISC")
                                                                         separator  (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/-lb x11-or-isc separator)
                                                                              "MIT"
                                                                              (re/-la separator x11-or-isc))))
                  #"(?i:(?<!\w)X11(?!\w))"              (re/inline (let [mit       "MIT"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/opt-grp mit separator)
                                                                              "X11"
                                                                              (re/opt-grp separator mit))))
                  #"(?i:(?<!\w)ISC(?!\w))"              (re/inline (let [mit       "MIT"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/opt-grp mit separator)
                                                                              "ISC"
                                                                              (re/opt-grp separator mit))))
                  ; zlib/libpng overlaps
                  #"(?i:(?<!\w)(?<!zlib/)libpng(?!\w))" (re/inline (let [zlib      "zlib"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/-lb zlib separator)
                                                                              "libpng"
                                                                              (re/-la separator zlib))))
                  ; Numbers that aren't part of a version
                  (re/inline (re/ncg "number" #"\d+"))  (fn [m] (let [number (get m "number")] (re/join #"0*" number)))))))

;####TODO: REMOVE ONCE TESTED!!!!
;  (when (seq template)
;    (let [template-without-placeholders (if (seq versions)
;                                          (let [re-vers (re/opt-grp ref/ows (verexp/expression-regex ncg-prefix versions))
;                                                re-oool (re/opt-grp ref/ows (verexp/suffix-regex (str ncg-prefix ncg-suffix-trailing)))]
;                                            (->> template)
;                                                 (lciu/mapcat-str #(if (re-find re-placeholder-ver %)
;                                                                     (lciu/replacing-split % re-placeholder-ver re-vers)
;                                                                     [% re-vers]))
;                                                 (lciu/mapcat-str #(if (re-find re-placeholder-oool %)
;                                                                     (lciu/replacing-split % re-placeholder-oool re-oool)
;                                                                     [% re-oool])))
;                                          template)]

;####TODO: REMOVE ONCE TESTED!!!!
;                                          (faux/parse template
;                                                      re-placeholder-ver   (re/opt-grp ref/ows (verexp/expression-regex ncg-prefix versions))
;                                                      re-placeholder-oool  (re/opt-grp ref/ows (verexp/suffix-regex (str ncg-prefix ncg-suffix-trailing))))
;                                          template)]

;####TODO: REMOVE ONCE TESTED!!!!
;      (faux/parse template-without-placeholders
;                  ; Name alternatives
;                  #"(?i:(?<!\w)Apache(?!\w))"           (re/inline (re/alt-grp (re/join "Apache" (re/opt-grp ref/mws (re/alt-grp "Software" "SW"))) "ASL"))
;                  #"(?i:(?<!\w)Beerware(?!\w))"         (re/inline (re/join "Beer" ref/ows "ware"))
;                  ; MIT/X11/ISC overlaps
;                  #"(?i:(?<!\w)MIT(?!\w))"              (re/inline (let [x11-or-isc (re/alt-grp "X11" "ISC")
;                                                                         separator  (re/n2m 1 4 ref/ws+slashes)]
;                                                                     (re/join (re/-lb x11-or-isc separator)
;                                                                              "MIT"
;                                                                              (re/-la separator x11-or-isc))))
;                  #"(?i:(?<!\w)X11(?!\w))"              (re/inline (let [mit       "MIT"
;                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
;                                                                     (re/join (re/opt-grp mit separator)
;                                                                              "X11"
;                                                                              (re/opt-grp separator mit))))
;                  #"(?i:(?<!\w)ISC(?!\w))"              (re/inline (let [mit       "MIT"
;                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
;                                                                     (re/join (re/opt-grp mit separator)
;                                                                              "ISC"
;                                                                              (re/opt-grp separator mit))))
;                  ; zlib/libpng overlaps
;                  #"(?i:(?<!\w)(?<!zlib/)libpng(?!\w))" (re/inline (let [zlib      "zlib"
;                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
;                                                                     (re/join (re/-lb zlib separator)
;                                                                              "libpng"
;                                                                              (re/-la separator zlib))))
;                  ; Numbers that aren't part of a version
;                  (re/inline (re/ncg "number" #"\d+"))  (fn [m] (let [number (get m "number")] (re/join #"0*" number)))))))

;####TODO: MAKE PRIVATE ONCE TESTED!!!!
(defn id->regex
;(defn- id->regex
  "Returns a regex suitable for matching variations on `id` (a
  `String`), optionally within `version-series`.

  Notes:

  * Callers are expected to wrap these regexes in a case insensitive flag
  * Callers are expected to add word boundary fragments at the start and end of
    these regexes, if separate word matching is required"
  [version-series id]
  (some-> (common-replacements id ncg-prefix-id (:versions version-series))
          (faux/parse ; Special cases for SGI-B without the -B
                      #"(?i:(?<!\w)SGI-B(?!\w))" (re/inline (re/join "SGI" (re/opt-grp ref/mws "B"))))
          ; Remove empty strings
          (->> (filter #(or (not (string? %)) (not (s/blank? %)))))  ;####TODO: Is s/blank? the right predicate here, or should it be (not= "" %) ??
          ; Replace whitespace
          (faux/replace-in-strings #"[\s\-]+" ref/mws)
          ; Cleanup, escape remaining fragments, then combine into a single regex
          (->> (lciu/mapcat-str #(vector (re/esc %)))
               (apply re/join))))


;####TODO: MAKE PRIVATE ONCE TESTED!!!!
(defn name->regex
;(defn- name->regex
  [version-series name]
;####TODO: FIX THESE REGEXES TO USE FRAGMENTS FROM ref NS
  (some-> (common-replacements name ncg-prefix-name (:versions version-series))
          (faux/parse ; Special case for some double and/or weird version components
;                      #"(?i)\(versions 9\.11 to 9\.20\)"                                          #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20\)?"
;                      #"(?i)\(versions 9\.22 and beyond\)"                                        #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*22[\s\-–—]*(\+|(?:and|&)[\s\-–—]*beyond)\)?"
;                      #"(?i)\(for libpng 0\.5 through 1\.6\.35\)"                                 #"\(?for[\s\-–—]+libpng[\s\-–—]+0+\.0*5[\s\-–—]+through[\s\-–—]+0*1\.0*6\.0*35\)?"
;                      #"(?i)\(or possibly 2\.0A and 2\.0B\)"                                      #"\(?or[\s\-–—]+possibly[\s\-–—]+0*2\.0+A[\s\-–—]+(?:and|&)[\s\-–—]+0*2\.0+B\)?"
;                      #"(?i)TORQUE v2\.5\+"                                                       #"TORQUE[\s\-–—]+v0*2\.0*5\+"
                      ; Special cases for certain licenses
;                      #"(?i)(?<!\w)Apache(?!\w)"                                                  #"(?:Apache(?:[\s\-–—]*Software)?|ASL)"
;                      #"(?i)(?<!\w)Beerware(?!\w)"                                                #"Beer[\s\-–—]*Ware"
;                      #"(?i)(?<!\w)No Derivatives(?!\w)"                                          #"No[\s\-–—]*Deriv(?:s|atives)?"
;                      #"(?i)(?<!\w)Share Alike(?!\w)"                                             #"Share[\s\-–—]*Alike"
;                      #"(?i)(?<!\w)MIT(?!\w)"                                                     #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))"
;                      #"(?i)(?<!\w)X11(?!\w)"                                                     #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?"
;                      #"(?i)(?<!\w)ISC(?!\w)"                                                     #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?"
;                      #"(?i)(?<!\w)zlib(/libpng)?(?!\w)"                                          #"(?:libpng[\\/\-\s]{1,4})?zlib([\\/\-\s]{1,4}libpng)?"
;                      #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"                                        #"(?<!zlib[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)"
                      #"(?<!\w)(?i:Hewlett[\s\-]+Packard)(?!\w)"                                  (re/inline (re/alt-grp (re/join "Hewlett" ref/mws "Packard") "HP"))
                      #"(?<!\w)(?i:Microsoft)(?!\w)"                                              (re/inline (re/alt-grp "Microsoft" "MS"))
                      #"(?<!\w)(?i:End[\s\-]user licen[cs]e agreement|EULA)(?!\w)"                (re/inline (re/alt-grp (re/join "End" ref/mws "User" ref/mws #"Licen?[cs]e" ref/mws "Agreement") "EULA"))
                      #"(?<!\w)(?i:Plexus\s+Classworlds\s+Licen[cs]e)(?!\w)"                      #"(?:Plexus(?:[\s\-–—]+Classworlds)?(?:[\s\-–—]+Licen[cs]e)?|Similar[\s\-–—]+to[\s\-–—]+Apache(?:[\s\-–—]+Licen[cs]e)(?:[\s\-–—]+but)?[\s\-–—]+with(?:[\s\-–—]+the)?[\s\-–—]+acknowledge?ment(?:[\s\-–—]+clause)?[\s\-–—]+(?:removed|deleted))"
                      #"\A(?i:Open\s+Software\s+Licen[cs]e)(?!\w)"                                (re/inline (re/join #"Open" ref/mws (re/alt-grp #"Software" #"SW") ref/mws #"Licen?[cs]e"))  ; OSL-x.y
                      #"\A(?i:Open\s+Public\s+Licen[cs]e)(?!\w)"                                  (re/inline (re/join #"Open" ref/mws #"Pub?lic" ref/mws #"Licen?[cs]e"))  ; OPL-x.y
                      #"(?<!\w)Unlicense(?!\w)"                                                   (re/inline (re/join #"Un" ref/ows #"licen?[cs]ed?"))
;                      #"(?i)(?<!\w)(?<!Microsoft[\s\-–—]+)Reciprocal\s+Public\s+Licen[cs]e(?!\w)" #"(?<!Microsoft[\s\-–—]+)Reciprocal(?:[\s\-–—]+Pub?lic)?[\s\-–—]+Licen[cs]e"
                      ; Optional words - we replace them twice to ensure the resulting regex consumes leading whitespace in locations other than the start of input

;####TODO: UPDATE THIS TO FEWER REGEXES THAT HANDLE ALL VARIANTS OF "Licen[cs]e|Lizenz" PREFIXED WITH "Open|Open Source|Public|Software"
; BUT WATCH OUT FOR THE "Open Software License" (OSL-x.y) VERSION SERIES!!!!

                      #"(?i)\s+licen[cs]e[\s\-]agreement(?!\w)"                                   #"(?:[\s\-–—]+Licen?[cs]e)?(?:[\s\-–—]+agreement)?"
                      #"(?i)\s+licen[cs]e(?!\w)"                                                  #"(?:[\s\-–—]+Licen?[cs]e)?"  ; Note: the optional missing `n` is a known misspelling in a POM license name: https://repo.clojars.org/net/unit8/excelebration/excelebration/0.2.0/excelebration-0.2.0.pom
                      #"(?i)licen[cs]e(?!\w)"                                                     #"(?:Licen?[cs]e)?"
                      #"(?i)\s+Lizenz(?!\w)"                                                      #"(?:[\s\-–—]+Lizenz)?"
                      #"(?i)Lizenz(?!\w)"                                                         #"(?:Lizenz)?"
                      #"(?i)\s+free(?!\w)"                                                        #"(?:[\s\-–—]+free)?"
                      #"(?i)free(?!\w)"                                                           #"(?:free)?"
                      #"(?i)\s+public(?!\w)"                                                      #"(?:[\s\-–—]+Pub?lic)?"  ; Note: the optional missing `b` is a known misspelling in a POM license name: e.g. https://repo.clojars.org/org/immutant/immutant-common/1.1.4/immutant-common-1.1.4.pom (there are others too)
                      #"(?i)public(?!\w)"                                                         #"(?:Pub?lic)?"
                      #"(?i)\s+software(?!\w)"                                                    #"(?:[\s\-–—]+Software)?"
                      #"(?i)software(?!\w)"                                                       #"(?:Software)?"
                      #"(?i)\s+hardware(?!\w)"                                                    #"(?:[\s\-–—]+Hardware)?"
                      #"(?i)hardware(?!\w)"                                                       #"(?:Hardware)?"
                      #"(?i)\s+generic(?!\w)"                                                     #"(?:[\s\-–—]+Generic)?"
                      #"(?i)generic(?!\w)"                                                        #"(?:Generic)?"
                      #"(?i)\s+international(?!\w)"                                               #"(?:[\s\-–—]+International)?"
                      #"(?i)international(?!\w)"                                                  #"(?:International)?"
                      ; Alternative spellings
                      #"(?i)\s+Australia(?!\w)"                                                   #"[\s\-–—]+(?:Australia|AU)"
                      #"(?i)\s+Austria(?!\w)"                                                     #"[\s\-–—]+(?:Austria|AT)"
                      #"(?i)\s+England and Wales(?!\w)"                                           #"[\s\-–—]+(?:England[\s\-–—]*(?:and|&)[\s\-–—]*Wales|GB|UK)"
                      #"(?i)\s+France(?!\w)"                                                      #"[\s\-–—]+(?:France|FR)"
                      #"(?i)\s+(Germany|Deutsche)(?!\w)"                                          #"[\s\-–—]+(?:Germany?|DE|Deutsche)"
                      #"(?i)\s+Japan(?!\w)"                                                       #"[\s\-–—]+(?:Japan|JP)"
                      #"(?i)\s+Netherlands(?!\w)"                                                 #"[\s\-–—]+(?:Netherlands|NL)"
                      #"(?i)(?<!\w)(United Kingdom|UK)(?!\w)"                                     #"(?:United[\s\-–—]+Kingdom|GB|UK)"
                      #"(?i)\s+(USA?|United States)(?!\w)"                                        #"[\s\-–—]+(?:United[\s\-–—]+States(?:[\s\-–—]+of[\s\-–—]+America)?|USA?)"
                      #"(?i)(?<!\w)(European Union|EU)(?!\w)"                                     #"(?:European[\s\-–—]+Union|EU)"
                      #"(?i)\s+University of California(?!\w)"                                    #"[\s\-–—]+(?:University[\s\-–—]+of[\s\-–—]+(?:California|CA)|UC|Cal)"
                      #"(?i)(?<!\w)acknowledge?ments?(?!\w)"                                      #"Acknowledge?ments?"
                      #"(?i)(?<!\w)merchant[ai]bility(?!\w)"                                      #"Merchant[ai]bility"
                      #"(?i)(?<!\w)non-?commercial(?!\w)"                                         #"Non[-–—]?commercial"  ; Note: hyphen, en-dash, em-dash
                      ; Common conjunctions etc.
                      #"(?<!\w)(?i:open\s+source)(?!\w)"                                          #"(?:Open[\s\-–—]+Source|FOSS|OSS)"
                      #"(?i)(?<!\w)the\s+"                                                        #"(?:The[\s\-–—]+)?"
                      #"(?i)(?<!\w)w/"                                                            #"(?:w/|with[\s\-–—]+)"
                      #"(?i)(?<!\w)(and|&)(?!\w)"                                                 #"(?:and|&)"
                      ; Character equivalents
                      #"(?iu:é)"                                            #"(?:é|é|e)"  ; As of License List v3.26.0 'é' is the only accented character present, and it can represented 2 ways in Unicode, one of which cannot be used in a regex character class as it's made up of multiple codepoints
                      #"\""                                                 (re/inline (re/opt ref/qots))
                      #"\s*/\s*"                                            (re/inline (re/join ref/ows ref/hyphens+slashes ref/ows))

                      ;####TODO: REPLACE THESE WITH CONSTANTS FROM ref
                      #"[\s\-–]+"                                           ref/mws
                      #"[\(\[\{«‹]+"                                        #"[\(\[\{«‹]*"      ; Make parens optional
                      #"[\)\]\}»›]+"                                        #"[\)\]\}»›]*")
        ; Cleanup, escape, and concat into a single pattern
          (->> (lciu/mapcat-str #(vector (re/esc %)))
               (apply re/join))))

(defn- regexes-impl
  "Returns a sequence of regexes for the given `ids`, `names`, and (if
  applicable) the `version-series` those ids and names are part of.  Each item
  in the sequence is a map, containing two keys:

  * `:name-regex` - a regex based on the name
  * `:id-regex` a regex based on the id

  Note: most licenses / version-series' only have a single id and name, in which
  case the result will contain a single element, but there are a (very) few
  version-series' that have multiple id and/or name patterns, and those will
  have entries in the result."
  ([ids names] (regexes-impl ids names nil))
  ([ids names version-series]
   (let [id-regexes   (seq (map (partial id->regex   version-series) ids))
         name-regexes (seq (map (partial name->regex version-series) names))
         combinations (combo/cartesian-product id-regexes name-regexes)]
     (map (fn [[id-regex name-regex]]
               {:name-regex (re/fgrp "iu" ref/nwb name-regex (re/opt-grp ref/mws id-regex ref/ows) ref/nwa)
                :id-regex   (re/fgrp "iu" ref/nwb id-regex ref/nwa)})
          combinations))))

(defmulti regexes
  "Returns a sequence of regexes for the given license or license exception,
  represented either as an SPDX identifier (a `String`) or as a version series
  (a `Map`, in the format produced by
  [[lice-comb.impl.version-series/version-series]]).

  Returns `nil` if `id-or-version-series` is blank or empty."
  {:arglists '([id-or-version-series])}
  type)

(defmethod regexes java.lang.String
  [^String id]
  (when-not (s/blank? id)
    (when-let [nm (:name (si/id->info id))]
      (regexes-impl [id] [nm]))))

(defmethod regexes java.util.Map
  [{id-formats :id-formats name-formats :name-formats :as version-series}]
  (when (and (seq id-formats) (seq name-formats))
    (regexes-impl id-formats name-formats version-series)))

(defmethod regexes nil
  [_]
  nil)

(defn regexes-for-id
  "Convenience function for testing the regexes for a specific SPDX identifier or
  version series id.  This is not used by lice-comb anywhere."
  [id-or-version-series-id]
  (if-let [vs (get (:version-series (verser/version-series)) id-or-version-series-id)]
    (regexes vs)
    (regexes id-or-version-series-id)))


;; Functions for processing matches from a regex produced by [[regexes]].
;; Note: rencg must have been used to produce the match.

(defn- get-from-match
  "Gets both id and name variants of the given `ncg-suffix` from match `m`,
  produced by a regex returned by [[regexes]], or `nil` if neither exists.
  Filters out blank values and de-duplicates."
  [^String ncg-suffix m]
  (seq (distinct (filter (complement s/blank?) [(get m (str ncg-prefix-id ncg-suffix)) (get m (str ncg-prefix-name ncg-suffix))]))))

(def match->versions
  "Returns the distinct version(s) found in match `m`, produced by a regex
  returned by [[regexes]], or `nil` if no versions were found.  Note that
  there's no guarantee that these version numbers are valid."
  (partial get-from-match verexp/ncg-version-number))

(defn match->or-later?
  "Was an 'or later' suffix found in match `m`, produced by a regex returned by
  [[regexes]]?"
  [m]
  (or (boolean (get-from-match verexp/ncg-or-later m))
      (boolean (get-from-match (str ncg-suffix-trailing verexp/ncg-or-later) m))))  ;####TODO: DOUBLE CHECK THAT THE NCG IS CONSTRUCTED CORRECTLY - good test case is GFDL/invariants

(defn match->only?
  "Was an 'only' suffix found in match `m`, produced by a regex returned by
  [[regexes]]?"
  [m]
  (or (boolean (get-from-match verexp/ncg-only m))
      (boolean (get-from-match (str ncg-suffix-trailing verexp/ncg-only) m))))  ;####TODO: DOUBLE CHECK THAT THE NCG IS CONSTRUCTED CORRECTLY - good test case is GFDL/invariants
