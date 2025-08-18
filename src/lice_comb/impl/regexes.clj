;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.regexes
  "Regexes related functionality. Note: this namespace is not part of the public
  API of lice-comb and may change without notice."
  (:require [clojure.string           :as s]
            [spdx.identifiers         :as si]
            [wreck.api                :as re]
            [lice-comb.impl.3rd-party :as lci3]
            [lice-comb.impl.utils     :as lciu]))

; Note: some of the regexes in this namespace uses classes (e.g. [\\/-\s]{1,4}) instead of alternation (e.g. (\\|/|-|\s){1,4}) due to an apparent bug in the JVM's regex libraries when
; the latter are used in look-behind groups.  See https://stackoverflow.com/questions/24874404/java-regex-look-behind-group-does-not-have-obvious-maximum-length-error/24922107

; Regex fragments - these should all be prefixed with #"(?iuU)" by callers
(def fre-ws     #"[\s\-–—_,\.\(\)]")
(def fre-ows    (re/zom fre-ws))
(def fre-mws    (re/oom fre-ws))
(def fre-quote  #"[\"“”„‟'‘’‚‛`]")
(def fre-oquote (re/opt fre-quote))
(def fre-date   (re/ncg "date" (re/join (re/zom-grp #"\d\d?" fre-ows #"(?:st|nd|rd|th)?")
                               fre-ows
                               (re/alt-grp #"Jan(?:uary)?" #"Feb(?:ruary)?" #"Mar(?:ch)?" #"Apr(?:il)?" #"May" #"June?" #"July?" #"Aug(?:ust)?" #"Sep(?:t(?:ember)?)?" #"Oct(?:ober)?" #"Nov(?:ember)?" #"Dec(?:ember)?")
                               fre-ows #"\d\d(?:\d\d)?" fre-ows)))

; Internal regex fragments
(def ^:private fre-version-label  #"v(?:er(?:sions?)?)?")
(def ^:private fre-only           #"only")
(def ^:private fre-or-later       (re/alt-grp #"\+"
                                              (re/grp (re/opt-grp #"\(" fre-ows)
                                                      "or"
                                                      fre-mws
                                                      (re/opt-grp "at" fre-mws "your" fre-mws (re/alt-grp "option" "discretion") fre-mws)
                                                      (re/opt-grp #"a(?:ny)?" fre-mws)
                                                      (re/alt-grp #"lat[eo]r" "newer")
                                                      (re/opt-grp fre-mws #"(?:v(?:er(?:sions?)?)?)")
                                                      (re/opt-grp fre-ows "at" fre-mws "your" fre-mws (re/alt-grp "option" "discretion"))  ; To handle corner cases such as "Affero General Public License v3 or later (at your option)"
                                                      fre-ows)))

(defn- oncgrp
  "Wraps `re` in a group, either a NCG (when `ncg-name` is not blank) or a non-
  capturing group."
  [ncg-name & res]
  (if (s/blank? ncg-name)
    (apply re/grp res)
    (apply re/ncg ncg-name res)))

(defn- version-number-to-re
  "Emits a regex fragment for matching the given version number (a `String` in
  `x[.y.z & .more]` format)."
  [version-number]
  (let [components (if-let [components (seq (lci3/rdrop-while (partial re-matches #"0+") (s/split version-number #"\.")))]  ; Drop all trailing components that are 0
                     (map #(s/join (drop-while (partial = \0) %)) components)                                               ; Strip leading 0s from each individual component (e.g. "002" -> "2") - we handle this case via a regex fragment below
                     ["0"])]  ; Always make sure we have at least one "hardcoded" version number component
    (re/join (s/join "[-–—_,\\.]" (map #(str "0*" %) components))  ; Allow any number of 0s at the start of each component
             #"(?:[-–—_,\.]0+)*")))  ; Allow any number of ".0" to appear at the end

(defn- re-version-impl
  "Emits a regex fragment for matching a version expression, made up of:

  1. a static version label ('v', 'ver', 'version', and the like)
  2. the version number component itself.  When `version-number` (a `String` in
     `x.y[.z & .more]` format) is provided, wil only match variations of that
     version number.  Otherwise will match any possible version number.  When
     `version-number-ncg-name` is provided, will wrap the version number
     match in an NCG.

  Notes:

  * Does not match suffixes (only, or-later) - for that use
  [[re-version-and-suffix]]."
  ([] (re-version-impl "versionNumber" nil))
  ([version-number-ncg-name] (re-version-impl "versionNumber" version-number-ncg-name))
  ([version-number-ncg-name version-number]
   (let [fre-version-number (if (s/blank? version-number)
                              #"\d+(?:[-–—_,\\.]\d+)*"
                              (version-number-to-re version-number))]
     (re/join (re/opt-grp fre-version-label)
              fre-ows
              (oncgrp version-number-ncg-name fre-version-number)))))
(def re-version (memoize re-version-impl))  ; Memoize as this will be called with the same args a LOT

(defn- re-version-suffix-impl
  "Emits a regex fragment for matching a version suffix component, made up of:

   * when `only?` is `true`: an 'only' component, optionally placed in a NCG
     with the name provided by `only-ncg-name`
   * when `or-later?` is `true`: an 'or-later' component, optionally placed in
     a NCG with the name provided by `or-later-ncg-name`
   * when both `only?` and `or-later?` are `true`: an optional alternation
     group that captures one or the other or neither (but not both)
   * when both `only?` and `or-later?` are `false`: no suffix matching (no
     suffixes will match)"
  ([] (re-version-suffix-impl true "only" true "orLater"))
  ([only?          only-ncg-name
    or-later?      or-later-ncg-name]
    (case [only? or-later?]
      ; Match only suffix only
      [true false] (oncgrp only-ncg-name fre-only)

      ; Match or later suffix only
      [false true] (oncgrp or-later-ncg-name fre-or-later)

      ; Default to matching one of the 2 suffixes, or neither (but never both)
      (re/opt (re/alt-grp (oncgrp only-ncg-name fre-only)
                          (oncgrp or-later-ncg-name fre-or-later))))))
(def re-version-suffix (memoize re-version-suffix-impl))  ; Memoize as this will be called with the same args a LOT

(defn- re-version-and-suffix-impl
  "Emits a regex fragment for matching a version expression, made up of:
  1. a static version label ('version', 'v' or the like)
  2. the version number component itself, matching the number provided by
     `version-number` (a `String` in `x.y[.z...]` format), optionally placed in
     a NCG with the name provided by `version-number-ncg-name` (or a non-
     capturing group when `version-number-ncg-name` is blank)
  3. a suffix component, made up of:
     * when `only?` is `true`: an 'only' component, optionally placed in a NCG
       with the name provided by `only-ncg-name`
     * when `or-later?` is `true`: an 'or-later' component, optionally placed in
       a NCG with the name provided by `or-later-ncg-name`
     * when both `only?` and `or-later?` are `true`: an optional alternation
       group that captures one or the other or neither (but not both)
     * when both `only?` and `or-later?` are `false`: no suffix matching (no
       suffixes will match)"
  ([] (re-version-and-suffix-impl nil "versionNumber", true "only" true "orLater"))
  ([version-number] (re-version-and-suffix-impl version-number "versionNumber", true "only" true "orLater"))
  ([version-number version-number-ncg-name
    only?          only-ncg-name
    or-later?      or-later-ncg-name]
   (re/join (re-version version-number-ncg-name version-number)
            fre-ows
            (re-version-suffix only? only-ncg-name or-later? or-later-ncg-name))))
(def re-version-and-suffix (memoize re-version-and-suffix-impl))  ; Memoize as this will be called with the same args a LOT

(defn- re-version-or-suffix-impl
  "Emits a regex fragment for matching a version expression, made up of any
  combination of:
  1. a static version label ('version', 'v' or the like)
  2. the version number component itself, matching the number provided by
     `version-number` (a `String` in `x.y[.z...]` format), optionally placed in
     a NCG with the name provided by `version-number-ncg-name` (or a non-
     capturing group when `version-number-ncg-name` is blank)
  3. a suffix component, made up of:
     * when `only?` is `true`: an 'only' component, optionally placed in a NCG
       with the name provided by `only-ncg-name`
     * when `or-later?` is `true`: an 'or-later' component, optionally placed in
       a NCG with the name provided by `or-later-ncg-name`
     * when both `only?` and `or-later?` are `true`: an optional alternation
       group that captures one or the other or neither (but not both)
     * when both `only?` and `or-later?` are `false`: no suffix matching (no
       suffixes will match)"
  ([] (re-version-or-suffix-impl nil "versionNumber", true "only" true "orLater"))
  ([version-number] (re-version-or-suffix-impl version-number "versionNumber", true "only" true "orLater"))
  ([version-number version-number-ncg-name
    only?          only-ncg-name
    or-later?      or-later-ncg-name]
   (re/join (re/opt-grp (re-version version-number-ncg-name version-number))
            fre-ows
            (re-version-suffix only? only-ncg-name or-later? or-later-ncg-name))))
(def re-version-or-suffix (memoize re-version-or-suffix-impl))  ; Memoize as this will be called with the same args a LOT



(defn- re-version-replacement
  "Emits a suitable regex for matching the version identified in map `m` (a map
  as returned by rencg). The version number component will be placed in a NCG
  called `version-number-ncg-name`, if that argument is not blank. When
  `suffixes-ncg?` is `true`, the 'only' and 'or later' suffixes will also be
  placed in their own NCGS."
  [version-number-ncg-name suffix-ncgs? m]
  (let [version-number (lciu/strim (get m "versionNumber"))
        only?          (not (s/blank? (get m "only")))
        or-later?      (not (s/blank? (get m "orLater")))]
    (re/join fre-ows
             (re-version-and-suffix version-number version-number-ncg-name only? (when suffix-ncgs? "only") or-later? (when suffix-ncgs? "orLater"))
             fre-ows)))

(defn id->regex
  "Turns `id`, an SPDX license or exception id, into a regex that can be used to
  near-match it.  Returns `nil` if `id` is blank.

  `ncgs?` (default `true`) controls whether named capturing groups are included
  to capture 'only' or 'or-later' suffixes."
  ([id] (id->regex id true))
  ([id ncgs?]
   (when-not (s/blank? id)
     (-> [#"(?iuU)(?<!\w)" (s/trim id) #"(?!\w)"]
         ; Special cases for some double and/or weird version components
         (lciu/replace-in-coll #"9.11-to-9.20"                         #"0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20")
         ; Special cases for certain licenses
         (lciu/replace-in-coll #"(?i)(?<!\w)MIT(?!\w)"                 #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))")
         (lciu/replace-in-coll #"(?i)(?<!\w)X11(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?")
         (lciu/replace-in-coll #"(?i)(?<!\w)ISC(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?")
         (lciu/replace-in-coll #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"    #"(?<!zlib/[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
         (lciu/replace-in-coll #"(?i)(?<!\w)SGI-B(?!\w)"               #"SGI(?:[\s\-–—]+B)?")
         ; Version component
         (lciu/replace-in-coll #"(?i)\-(?<versionNumber>\d+\.\d+(?:\.\d+)*)(?:(?<only>-only)|(?<orLater>\+|-or-later))?(?=(-|\z))"
                               (partial re-version-replacement "versionNumberId" ncgs?))
         ; Character equivalents
         (lciu/replace-in-coll #"[\s\-]+"                              #"[\s\-–—]+")  ; Note: hyphen, en-dash, em-dash
         ; Cleanup and combine into a single pattern
         (->> (filter #(or (not (string? %)) (not (s/blank? %))))   ; Remove empty strings
              (lciu/mapcat-str #(vector (re/esc %)))
              (apply re/join))))))

(defn name->regex
  "Turns `n`, a license or exception name, into a regex that can be used to
  near-match it.  Returns `nil` if `n` is blank."
  [n]
  (when-not (s/blank? n)
    (-> [#"(?iuU)(?<!\w)(The[\s\-–—]+)?" (s/trim n) #"(?!\w)"]
        ; Special case for some double and/or weird version components (this must come first)
        (lciu/replace-in-coll #"(?i)\(versions 9\.11 to 9\.20\)"                    #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20\)?")
        (lciu/replace-in-coll #"(?i)\(versions 9\.22 and beyond\)"                  #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*22[\s\-–—]*(\+|(?:and|&)[\s\-–—]*beyond)\)?")
        (lciu/replace-in-coll #"(?i)\(for libpng 0\.5 through 1\.6\.35\)"           #"\(?for[\s\-–—]+libpng[\s\-–—]+0+\.0*5[\s\-–—]+through[\s\-–—]+0*1\.0*6\.0*35\)?")
        (lciu/replace-in-coll #"(?i)\(or possibly 2\.0A and 2\.0B\)"                #"\(?or[\s\-–—]+possibly[\s\-–—]+0*2\.0+A[\s\-–—]+(?:and|&)[\s\-–—]+0*2\.0+B\)?")
        (lciu/replace-in-coll #"(?i)TORQUE v2\.5\+"                                 #"TORQUE[\s\-–—]+v2\.5\+")
        (lciu/replace-in-coll #"(?i)clause\s+(?<clauseCount>\d+)"                   (fn [m] (re/join #"clause[\s\-–—]*0*" (get m "clauseCount"))))  ; For BSD
         ; Special cases for certain licenses
        (lciu/replace-in-coll #"(?i)(?<!\w)Apache(?!\w)"                            #"(?:Apache(?:[\s\-–—]*Software)?|ASL)")
        (lciu/replace-in-coll #"(?i)(?<!\w)Beerware(?!\w)"                          #"Beer[\s\-–—]*Ware")
        (lciu/replace-in-coll #"(?i)(?<!\w)No Derivatives(?!\w)"                    #"No[\s\-–—]*Deriv(?:s|atives)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)Share Alike(?!\w)"                       #"Share[\s\-–—]*Alike")
        (lciu/replace-in-coll #"(?i)(?<!\w)MIT(?!\w)"                               #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))")
        (lciu/replace-in-coll #"(?i)(?<!\w)X11(?!\w)"                               #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)ISC(?!\w)"                               #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)zlib(/libpng)?(?!\w)"                    #"(?:libpng[\\/\-\s]{1,4})?zlib([\\/\-\s]{1,4}libpng)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"                  #"(?<!zlib[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
        (lciu/replace-in-coll #"(?i)(?<!\w)Hewlett[\s\-]+Packard(?!\w)"             #"(?:Hewlett[\s\-–—]*Packard|HP)")
        (lciu/replace-in-coll #"(?i)\s+(End[\s\-]user licen[cs]e agreement|EULA)\b" #"[\s\-–—]+(?:End[\s\-–—]+User[\s\-–—]+Licen?[cs]e[\s\-–—]+Agreement|EULA)")
        (lciu/replace-in-coll #"(?i)(?<!\w)Plexus\s+Classworlds\s+Licen[cs]e(?!\w)" #"(?:Plexus(?:[\s\-–—]+Classworlds)?(?:[\s\-–—]+Licen[cs]e)?|Similar[\s\-–—]+to[\s\-–—]+Apache(?:[\s\-–—]+Licen[cs]e)(?:[\s\-–—]+but)?[\s\-–—]+with(?:[\s\-–—]+the)?[\s\-–—]+acknowledge?ment(?:[\s\-–—]+clause)?[\s\-–—]+(?:removed|deleted))")
        (lciu/replace-in-coll #"(?i)(?<!\w)(?<!Microsoft[\s\-–—]+)Reciprocal\s+Public\s+Licen[cs]e(?!\w)" #"(?<!Microsoft[\s\-–—]+)Reciprocal(?:[\s\-–—]+Pub?lic)?[\s\-–—]+Licen[cs]e")
        ; Other numbers, especially dates (so that they don't get misidentified as versions)
        (lciu/replace-in-coll #"\d{3,4}(\-\d{2}\-\d{2})?"                           (fn [m] (re-pattern (re/esc (:match m)))))
        ; Version components - 2 & 3 element versions
        (lciu/replace-in-coll #"(?i)\s+((v|ver|versions?)?\s*)?(?<versionNumber>\d+\.\d+(\.\d+)*)([\s\-–—]+((?<only>only)|(?<orLater>or[\s\-–—]+later)))?(?=\z|[\w\s\-–—])"
                              (partial re-version-replacement "versionNumber" true))
        ; Version components - 1 & 2 element versions
        (lciu/replace-in-coll #"(?i)\s+((v|ver|versions?)?\s*)(?<versionNumber>\d+(\.\d+)*)([\s\-–—]+((?<only>only)|(?<orLater>or[\s\-–—]+later)))?(?=\z|[\w\s\-–—])"
                              (partial re-version-replacement "versionNumber" true))
        ; Optional words - we replace them twice to ensure the resulting regex consumes leading whitespace in locations other than the start of input
        (lciu/replace-in-coll #"(?i)\s+licen[cs]e[\s\-]agreement(?!\w)"             #"(?:[\s\-–—]+Licen?[cs]e)?(?:[\s\-–—]+agreement)?")
        (lciu/replace-in-coll #"(?i)\s+licen[cs]e(?!\w)"                            #"(?:[\s\-–—]+Licen?[cs]e)?")  ; Note: the optional missing `n` is a known misspelling in a POM license name: https://repo.clojars.org/net/unit8/excelebration/excelebration/0.2.0/excelebration-0.2.0.pom
        (lciu/replace-in-coll #"(?i)licen[cs]e(?!\w)"                               #"(?:Licen?[cs]e)?")
        (lciu/replace-in-coll #"(?i)\s+Lizenz(?!\w)"                                #"(?:[\s\-–—]+Lizenz)?")
        (lciu/replace-in-coll #"(?i)Lizenz(?!\w)"                                   #"(?:Lizenz)?")
        (lciu/replace-in-coll #"(?i)\s+free(?!\w)"                                  #"(?:[\s\-–—]+free)?")
        (lciu/replace-in-coll #"(?i)free(?!\w)"                                     #"(?:free)?")
        (lciu/replace-in-coll #"(?i)\s+public(?!\w)"                                #"(?:[\s\-–—]+Pub?lic)?")  ; Note: the optional missing `b` is a known misspelling in a POM license name: e.g. https://repo.clojars.org/org/immutant/immutant-common/1.1.4/immutant-common-1.1.4.pom (there are others too)
        (lciu/replace-in-coll #"(?i)public(?!\w)"                                   #"(?:Pub?lic)?")
        (lciu/replace-in-coll #"(?i)\s+software(?!\w)"                              #"(?:[\s\-–—]+Software)?")
        (lciu/replace-in-coll #"(?i)software(?!\w)"                                 #"(?:Software)?")
        (lciu/replace-in-coll #"(?i)\s+hardware(?!\w)"                              #"(?:[\s\-–—]+Hardware)?")
        (lciu/replace-in-coll #"(?i)hardware(?!\w)"                                 #"(?:Hardware)?")
        (lciu/replace-in-coll #"(?i)\s+generic(?!\w)"                               #"(?:[\s\-–—]+Generic)?")
        (lciu/replace-in-coll #"(?i)generic(?!\w)"                                  #"(?:Generic)?")
        (lciu/replace-in-coll #"(?i)\s+international(?!\w)"                         #"(?:[\s\-–—]+International)?")
        (lciu/replace-in-coll #"(?i)international(?!\w)"                            #"(?:International)?")
        ; Alternative spellings
        (lciu/replace-in-coll #"(?i)\s+Australia(?!\w)"                             #"[\s\-–—]+(?:Australia|AU)")
        (lciu/replace-in-coll #"(?i)\s+Austria(?!\w)"                               #"[\s\-–—]+(?:Austria|AT)")
        (lciu/replace-in-coll #"(?i)\s+England and Wales(?!\w)"                     #"[\s\-–—]+(?:England[\s\-–—]*(?:and|&)[\s\-–—]*Wales|GB|UK)")
        (lciu/replace-in-coll #"(?i)\s+France(?!\w)"                                #"[\s\-–—]+(?:France|FR)")
        (lciu/replace-in-coll #"(?i)\s+(Germany|Deutsche)(?!\w)"                    #"[\s\-–—]+(?:Germany?|DE|Deutsche)")
        (lciu/replace-in-coll #"(?i)\s+Japan(?!\w)"                                 #"[\s\-–—]+(?:Japan|JP)")
        (lciu/replace-in-coll #"(?i)\s+Netherlands(?!\w)"                           #"[\s\-–—]+(?:Netherlands|NL)")
        (lciu/replace-in-coll #"(?i)(?<!\w)(United Kingdom|UK)(?!\w)"               #"(?:United[\s\-–—]+Kingdom|GB|UK)")
        (lciu/replace-in-coll #"(?i)\s+(USA?|United States)(?!\w)"                  #"[\s\-–—]+(?:United[\s\-–—]+States(?:[\s\-–—]+of[\s\-–—]+America)?|USA?)")
        (lciu/replace-in-coll #"(?i)\s+University of California(?!\w)"              #"[\s\-–—]+(?:University[\s\-–—]+of[\s\-–—]+(?:California|CA)|UC|Cal)")
        (lciu/replace-in-coll #"(?i)(?<!\w)acknowledge?ment(?!\w)"                  #"Acknowledge?ment")  ; No trailing \b, to handle plurals etc.
        (lciu/replace-in-coll #"(?i)(?<!\w)merchant[ai]bility(?!\w)"                #"Merchant[ai]bility")
        (lciu/replace-in-coll #"(?i)(?<!\w)non-?commercial(?!\w)"                   #"Non[-–—]?commercial")  ; Note: hyphen, en-dash, em-dash
        (lciu/replace-in-coll #"(?i)(?<!\w)open\s+source"                           #"(?:Open[\s\-–—]+Source|FOSS|OSS)")
        (lciu/replace-in-coll #"(?i)(?<!\w)the\s+"                                  #"(?:The[\s\-–—]+)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)w/"                                      #"(?:w/|with[\s\-–—]+)")
        (lciu/replace-in-coll #"(?i)(?<!\w)(and|&)(?!\w)"                           #"(?:and|&)")
        ; Character equivalents
        (lciu/replace-in-coll #"(?i)é"                                              #"[ée]")  ; As of License List v3.26.0 'é' is the only accented character present
        (lciu/replace-in-coll #"\""                                                 fre-quote)
        (lciu/replace-in-coll #"\s*/\s*"                                            #"\s*[\\/\-–—]\s*")  ; hyphen, en-dash, em-dash
        (lciu/replace-in-coll #"[\s\-–]+"                                           #"[\s\-–—]+")        ; hyphen, en-dash, em-dash.  en-dash is in e.g. the name of LiLiQ-R-1.1
        (lciu/replace-in-coll #"[\(\[\{«‹]+"                                        #"[\(\[\{«‹]*")      ; Make parens optional
        (lciu/replace-in-coll #"[\)\]\}»›]+"                                        #"[\)\]\}»›]*")
        ; Cleanup, escape, and concat into a single pattern
        (->> (filter #(or (not (string? %)) (not (s/blank? %))))
             (lciu/mapcat-str #(vector (re/esc %)))
             (apply re/join)))))

(defn id->name->regex
  "Convenience method for obtaining the name regex from an `id`, which is the
  SPDX identifier of a license or exception."
  [id]
  (when-let [info (si/id->info id)]
    (name->regex (:name info))))
;####TODO: ADD (OPTIONAL) ID REGEX ONTO THE END!!!!
(comment
    (re/join (name->regex (:name info))
             (re/opt-grp fre-ows
                         #"\(*"
                         (id->regex id false)
                         #"\)*"))
)
