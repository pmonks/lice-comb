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
  (:require [clojure.string       :as s]
            [spdx.licenses        :as sl]
            [spdx.exceptions      :as se]
            [wreck.api            :as re]
            [lice-comb.impl.utils :as lciu]))

; Regex fragments - these should all be prefixed with #"(?iuU)" by callers
(def fre-ws                       #"[\s\-–—_,\.\(\)]")
(def fre-ows                      (re/zom fre-ws))
(def fre-mws                      (re/oom fre-ws))
(def fre-quote                    #"[\"“”„‟'‘’‚‛`]")
(def fre-oquote                   (re/opt fre-quote))
(def ^:private fre-version-label  (re/grp fre-ows #"v(?:er(?:sions?)?)?"))
(def ^:private fre-version-number (re/ncg "versionNumber" #"\d+(?:[,\._]\d+)*"))
(def fre-version                  (re/join (re/opt fre-version-label) fre-ows fre-version-number))
(def ^:private fre-only           (re/ncg "only" #"only"))
(def ^:private fre-or-later       (re/alt-ncg "orLater"
                                              #"\+"
                                              #"(?:\(?or(?:[\s\-–—,\(]+at[\s\-–—]+your[\s\-–—]+(?:option|discretion)[\),]*)?(?:[\s\-–—]+a(?:ny)?)?[\s\-–—]+(?:lat[eo]r|newer)(?:[\s\-–—,\(]+at[\s\-–—]+your[\s\-–—]+(?:option|discretion)\)?)?(?:[\s\-–—]+(?:v(?:er(?:sions?)?)?))?\)?)"))
(def fre-only-or-later            (re/alt-grp fre-only fre-or-later))
(def fre-date                     (re/ncg "date" (re/join (re/zom-grp #"\d\d?" fre-ows #"(?:st|nd|rd|th)?")
                                                 fre-ows
                                                 (re/alt-grp #"Jan(?:uary)?" #"Feb(?:ruary)?" #"Mar(?:ch)?" #"Apr(?:il)?" #"May" #"June?" #"July?" #"Aug(?:ust)?" #"Sep(?:t(?:ember)?)?" #"Oct(?:ober)?" #"Nov(?:ember)?" #"Dec(?:ember)?")
                                                 fre-ows #"\d\d(?:\d\d)?" fre-ows)))

(defn- re-version-replacement
  "Emits a suitable regex for matching the version identified in map `m` (a map
  as returned by rencg)."
  [m]
  (let [version-number              (get m "versionNumber")
        only?                       (boolean (get m "only"))
        or-later?                   (boolean (get m "orLater"))
        version-components          (seq (s/split version-number #"\."))
        dot-zero?                   (boolean (re-matches #"0+" (last version-components)))
        non-zero-version-components (if dot-zero? (drop-last version-components) version-components)]  ; If version number ends in 0, make last component optional
    (re/join (re/opt-grp fre-version-label)
             fre-ows
             (re/ncg "versionNumber"
                     (s/join "\\." (map #(str "0*" %) non-zero-version-components))
                     #"(?:\.0+)*")  ; Allow any number of ".0" to appear at the end
             fre-ows
             (case [only? or-later?]
               [true false]  (re/opt fre-only)      ; only only
               [false true]  (re/opt fre-or-later)  ; or-later only
               (re/opt fre-only-or-later)))))       ; Undefined, so accept either

; Note: some of the regexes in this namespace uses classes (e.g. [\\/-\s]{1,4}) instead of alternation (e.g. (\\|/|-|\s){1,4}) due to an apparent bug in the JVM's regex libraries when
; the latter are used in look-behind groups.  See https://stackoverflow.com/questions/24874404/java-regex-look-behind-group-does-not-have-obvious-maximum-length-error/24922107

(defn id->regex
  "Turns `id`, an SPDX license or exception id, into a regex that can be used to
  near-match it.  Returns `nil` if `id` is blank."
  [id]
  (when-not (s/blank? id)
    (-> [#"(?iuU)(?<=(\A|\s))" (s/trim id) #"(?=(\s|\z))"]
        ; Special cases for some double and/or weird version components
        (lciu/replace-in-coll #"9.11-to-9.20"                         #"0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20")
        ; Version component
        (lciu/replace-in-coll #"(?i)\-(?<versionNumber>\d+\.\d+(?:\.\d+)*)(?:(?<only>-only)|(?<orLater>\+|-or-later))?(?=(-|\z))"
                                  #(re/join #"[\s\-–—]*" (re-version-replacement %)))  ; Note: we handle leading whitespace slightly differently in id regexes vs name regexes
        ; Special cases for certain licenses
;####TODO: TEST WHETHER THIS IS EVEN NEEDED
;        (lciu/replace-in-coll #"(?i)(?<!\w)AGPL(?!\w)"                #"(?:GNU[\s\-–—]+)?A[\s\-–—]*GPL")
;        (lciu/replace-in-coll #"(?i)(?<!\w)LGPL(?!\w)"                #"(?:GNU[\s\-–—]+)?L[\s\-–—]*GPL")
;        (lciu/replace-in-coll #"(?i)(?<!\w)GPL(?!\w)"                 #"(?:GNU[\s\-–—]+)?[\s\-–—]*GPL")
        (lciu/replace-in-coll #"(?i)(?<!\w)MIT(?!\w)"                 #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))")
        (lciu/replace-in-coll #"(?i)(?<!\w)X11(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)ISC(?!\w)"                 #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"    #"(?<!zlib/[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
;####TODO: TEST WHETHER THIS IS EVEN NEEDED
;        (lciu/replace-in-coll #"(?i)BSD\-(?<clauseCount>\d+)\-Clause" (fn [m] (re/join #"BSD[\s\-–—]*0*" (get m "clauseCount") #"[\s\-–—]*Clause")))  ; For BSD
        ; Character equivalents
        (lciu/replace-in-coll #"[\s\-]+"                              #"[\s\-–—]+")  ; Note: hyphen, en-dash, em-dash
        ; Cleanup and combine into a single pattern
        (->> (filter #(or (not (string? %)) (not (s/blank? %))))   ; Remove empty strings
             (lciu/mapcat-str #(vector (re/esc %)))
             (apply re/join)))))

(defn name->regex
  "Turns `n`, a license or exception name, into a regex that can be used to
  near-match it.  Returns `nil` if `n` is blank."
  [n]
  (when-not (s/blank? n)
    (-> [#"(?iuU)(?<!\w)(The[\s\-–—]+)?" (s/trim n) #"(?!\w)"]
;####TODO: TEST WHETHER THESE ARE EVEN NEEDED
        ; Special case GNU family first, as they're such a massive pita
;        (lciu/replace-in-coll #"(?i)(?<!\w)GNU\s+"                                  #"(?:GNU[\s\-–—]+)?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)Affero General Public License"           #"Affero[\s\-–—]+Genere?al[\s\-–—]+Pub?lic[\s\-–—]+Licen[cs]e(?:[\s\-–—]+\(?A[\s\-–—]*GPL(?:[\s\-–—]*v)?[\s\d\._]*\))?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)Library General Public License"          #"(?:Library|Less[eo]r|Library[\s\-–—]+or[\s\-–—]+Less[eo]r|Less[eo]r[\s\-–—]+or[\s\-–—]+Library)[\s\-–—]+Genere?al[\s\-–—]+Pub?lic[\s\-–—]+Licen[cs]e(?:[\s\-–—]+\(?L[\s\-–—]*GPL(?:[\s\-–—]*v)?[\s\d\._]*\))?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)Lesser General Public License"           #"(?:Library|Less[eo]r|Library[\s\-–—]+or[\s\-–—]+Less[eo]r|Less[eo]r[\s\-–—]+or[\s\-–—]+Library)[\s\-–—]+Genere?al[\s\-–—]+Pub?lic[\s\-–—]+Licen[cs]e(?:[\s\-–—]+\(?L[\s\-–—]*GPL(?:[\s\-–—]*v)?[\s\d\._]*\))?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)General Public License"                  #"Genere?al[\s\-–—]+Pub?lic[\s\-–—]+Licen[cs]e([\s\-–—]+\(?GPL(?:[\s\-–—]*v)?[\s\d\._]*\))?")
;        (lciu/replace-in-coll #"(?i)(?<!\w)\"Original\" or \"Old\" License"         #"(\"?Original\"?(?:[\s\-–—]+or[\s\-–—]+\"?Old\"?)?(?:[\s\-–—]+Licen[cs]e)?)?")  ; BSD-4-Clause
         ; Special cases for certain licenses
        (lciu/replace-in-coll #"(?i)(?<!\w)Apache(?!\w)"                            #"Apache(?:[\s\-–—]*Software)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)Creative Commons(?!\w)"                  #"(?:Creative[\s\-–—]*Commons|CC)")
        (lciu/replace-in-coll #"(?i)(?<!\w)No Derivatives(?!\w)"                    #"No[\s\-–—]*Deriv(s|atives)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)Share Alike(?!\w)"                       #"Share[\s\-–—]*Alike")
        (lciu/replace-in-coll #"(?i)(?<!\w)MIT(?!\w)"                               #"(?<!(?:X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(?:X11|ISC))")
        (lciu/replace-in-coll #"(?i)(?<!\w)X11(?!\w)"                               #"(?:MIT[\\/\-\s]{1,4})?X11(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)ISC(?!\w)"                               #"(?:MIT[\\/\-\s]{1,4})?ISC(?:[\\/\-\s]{1,4}MIT)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)zlib(/libpng)?(?!\w)"                    #"(?:libpng[\\/\-\s]{1,4})?zlib([\\/\-\s]{1,4}libpng)?")
        (lciu/replace-in-coll #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)"                  #"(?<!zlib[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
        (lciu/replace-in-coll #"(?i)(?<!\w)Hewlett[\s\-]+Packard(?!\w)"             #"(?:Hewlett[\s\-–—]*Packard|HP)")
        (lciu/replace-in-coll #"(?i)\s+(End[\s\-]user licen[cs]e agreement|EULA)\b" #"[\s\-–—]+(?:End[\s\-–—]+User[\s\-–—]+Licen?[cs]e[\s\-–—]+Agreement|EULA)")
        ; Special case for some double and/or weird version components
        (lciu/replace-in-coll #"(?i)\(versions 9.11 to 9.20\)"                      #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*11(?:[\s\-–—]+to)?[\s\-–—]+0*9\.0*20\)?")
        (lciu/replace-in-coll #"(?i)\(versions 9.22 and beyond\)"                   #"\(?(?:(?:v|ver|versions?)[\s\-–—]*)?0*9\.0*22[\s\-–—]*(\+|(?:and|&)[\s\-–—]*beyond)\)?")
        (lciu/replace-in-coll #"(?i)\(or possibly 2.0A and 2.0B\)"                  #"\(?or[\s\-–—]+possibly[\s\-–—]+0*2\.0+A[\s\-–—]+(?:and|&)[\s\-–—]+0*2\.0+B\)?")
        (lciu/replace-in-coll #"(?i)TORQUE v2\.5\+"                                 #"TORQUE[\s\-–—]+v2\.5\+")
        (lciu/replace-in-coll #"(?i)clause\s+(?<clauseCount>\d+)"                   (fn [m] (re/join #"clause[\s\-–—]*0*" (get m "clauseCount"))))  ; For BSD
        ; Other numbers, especially dates (so that they don't get misidentified as versions)
        (lciu/replace-in-coll #"\d{3,4}(\-\d{2}\-\d{2})?"                           (fn [m] (re-pattern (re/esc (:match m)))))
        ; Version components - 2 & 3 element versions
        (lciu/replace-in-coll #"(?i)\s+((v|ver|versions?)?\s*)?(?<versionNumber>\d+\.\d+(\.\d+)*)([\s\-–—]+((?<only>only)|(?<orLater>or[\s\-–—]+later)))?(?=\z|[\w\s\-–—])"
                                  re-version-replacement)
        ; Version components - 1 & 2 element versions
        (lciu/replace-in-coll #"(?i)\s+((v|ver|versions?)?\s*)(?<versionNumber>\d+(\.\d+)*)([\s\-–—]+((?<only>only)|(?<orLater>or[\s\-–—]+later)))?(?=\z|[\w\s\-–—])"
                                  re-version-replacement)
        ; Optional words - we replace them twice to ensure the resulting regex consumes leading whitespace in locations other than the start of input
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
        (lciu/replace-in-coll #"(?i)\s+England and Wales(?!\w)"                     #"[\s\-–—]+(?:England[\s\-–—]*(?:and|&)[\s\-–—]*Wales|UK)")
        (lciu/replace-in-coll #"(?i)\s+France(?!\w)"                                #"[\s\-–—]+(?:France|FR)")
        (lciu/replace-in-coll #"(?i)\s+(Germany|Deutsche)(?!\w)"                    #"[\s\-–—]+(?:Germany?|DE|Deutsche)")
        (lciu/replace-in-coll #"(?i)\s+Japan(?!\w)"                                 #"[\s\-–—]+(?:Japan|JP)")
        (lciu/replace-in-coll #"(?i)\s+Netherlands(?!\w)"                           #"[\s\-–—]+(?:Netherlands|NL)")
        (lciu/replace-in-coll #"(?i)(?<!\w)(United Kingdom|UK)(?!\w)"               #"(?:United[\s\-–—]+Kingdom|UK)")
        (lciu/replace-in-coll #"(?i)\s+(USA?|United States)(?!\w)"                  #"[\s\-–—]+(?:United[\s\-–—]+States|USA?)")
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
  (when-let [info (or (sl/id->info id)
                      (se/id->info id))]
    (name->regex (:name info))))
