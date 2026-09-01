;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.regexes.license
  "License (and license exception) regex related functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                            :as s]
            [clojure.math.combinatorics                :as combo]
            [wreck.api                                 :as re]
            [rencg.api                                 :as ncg]
            [spdx.identifiers                          :as si]
            [lice-comb.impl.version-series             :as verser]
            [lice-comb.impl.regexes.version-expression :as verexp]
            [lice-comb.impl.regexes.fragments          :as ref]
            [lice-comb.impl.parsing.faux-parse         :as faux]
            [lice-comb.impl.utils                      :as u]))

(def ^:private re-placeholder-ver  (re/join ref/ows (re/esc verser/placeholder-ver)))
(def ^:private re-placeholder-oool (re/join ref/ows (re/esc verser/placeholder-oool)))

(def ^:private ncg-prefix-id       "id")
(def ^:private ncg-prefix-name     "name")
(def ^:private ncg-suffix-trailing "Trailing")  ;####TODO: decide if "Trailing" is the best term here...


;; Functions involved in license regex construction.

(defn- common-replacements
  "Performs replacements common to both identifiers and names, in `s` (a
  `String`), returning a sequence."
  [^String s ^String ncg-prefix versions]
  (when-not (s/blank? s)
    (let [re-vers  (re/opt-grp ref/ows (re/opt-grp ref/single-qots) (verexp/expression-regex ncg-prefix versions))
          re-oool  (re/opt-grp ref/ows (verexp/suffix-regex (str ncg-prefix ncg-suffix-trailing)))
          template (if (re-find re-placeholder-ver s)
                     (u/replacing-split s re-placeholder-ver re-vers)
                     [s re-vers])]  ; Always add a version matching component on the end, to handle cases where the version doesn't appear in the name (e.g. Adobe-2006, Arphic-1999)
      (faux/parse (concat [(re/opt-grp #"The" ref/mws)]
                          template
                          [(re/opt-grp ref/mws ref/public) (re/opt-grp ref/mws ref/license) (re/opt-grp ref/mws ref/date)])   ;####TODO: CHECK THAT THIS IS GENERALLY APPROPRIATE
                  ; only or or-later suffix
                  re-placeholder-oool                   re-oool
                  ; Name alternatives
                  #"(?<!\w)(?i:Apache)(?!\w)"           (re/inline (re/alt-grp (re/join "Apache" (re/opt-grp ref/mws (re/alt-grp "Software" "SW"))) "ASL"))
                  #"(?<!\w)(?i:Beerware)(?!\w)"         (re/inline (re/join "Beer" ref/ows "ware"))
                  #"(?<!\w)(?i:Classpath\s+exception)(?!\w)" (re/inline (re/join (re/opt-grp "GNU" ref/ows) (re/alt-grp (re/join "Classpath" ref/ows "exception") "CPE")))
                  ; MIT/X11/ISC overlaps
                  #"(?<!\w)(?i:MIT)(?!\w)"              (re/inline (let [x11-or-isc (re/alt-grp "X11" "ISC")
                                                                         separator  (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/-lb x11-or-isc separator)
                                                                              "MIT"
                                                                              (re/-la separator x11-or-isc))))
                  #"(?<!\w)(?i:X11)(?!\w)"              (re/inline (let [mit       "MIT"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/opt-grp mit separator)
                                                                              "X11"
                                                                              (re/opt-grp separator mit))))
                  #"(?<!\w)(?i:ISC)(?!\w)"              (re/inline (let [mit       "MIT"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/opt-grp mit separator)
                                                                              "ISC"
                                                                              (re/opt-grp separator mit))))
                  ; zlib/libpng overlaps
                  #"(?<!\w)(?i:(?<!zlib/)libpng)(?!\w)" (re/inline (let [zlib      "zlib"
                                                                         separator (re/n2m 1 4 ref/ws+slashes)]
                                                                     (re/join (re/-lb zlib separator)
                                                                              "libpng"
                                                                              (re/-la separator zlib))))
                  #"(?<!\w)(?i:zlib(?:/libpng)?)(?!\w)" (re/inline (re/join "zlib" (re/opt-grp ref/ows #"/?" ref/ows "libpng")))
                  ; Numbers that aren't part of a version
                  (re/inline (re/ncg "number" #"\d+"))  (fn [m] (let [number (get m "number")] (re/join #"0*" number)))))))

(defn- id->regex
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
          ; Replace whitespace
          (faux/replace-in-strings #"[\s\-]+" ref/mws)
          ; Cleanup, escape remaining fragments, then combine into a single regex
          (->> (u/mapcat-str #(vector (re/esc %)))
               (apply re/join))))


(defn- name->regex
  [version-series name]
  (some-> (common-replacements name ncg-prefix-name (:versions version-series))
          (faux/parse ; Special cases for certain phrases in licenses
                      #"(?<!\w)(?i:End[\s\-]user[\s\-]licen[cs]e[\s\-]agreement|EULA)(?!\w)"      (re/inline (re/alt-grp (re/join "End" ref/mws "User" ref/mws ref/license ref/mws "Agreement") "EULA"))
;####TODO
;                      #"(?<!\w)(?i:Plexus\s+Classworlds\s+Licen[cs]e)(?!\w)"                      #"(?:Plexus(?:[\s\-–—]+Classworlds)?(?:[\s\-–—]+Licen[cs]e)?|Similar[\s\-–—]+to[\s\-–—]+Apache(?:[\s\-–—]+Licen[cs]e)(?:[\s\-–—]+but)?[\s\-–—]+with(?:[\s\-–—]+the)?[\s\-–—]+acknowledge?ment(?:[\s\-–—]+clause)?[\s\-–—]+(?:removed|deleted))"
                      #"\A(?i:Open\s+Software\s+Licen[cs]e)(?!\w)"                                (re/inline (re/join #"Open" ref/mws ref/software ref/mws ref/license))  ; OSL-x.y
                      #"\A(?i:Open\s+Public\s+Licen[cs]e)(?!\w)"                                  (re/inline (re/join #"Open" ref/mws ref/public ref/mws ref/license))  ; OPL-x.y
                      #"(?<!\w)(?i:Unlicense)(?!\w)"                                              (re/inline (re/join #"Un" ref/ows ref/license #"d?"))
                      #"(?<!\w)(?i:Business\s+Source\s+Licen[cs]e)(?!\w)"                         (re/inline (re/join (re/-lb "Hyperfiddle" ref/bounded-mws) "Business" ref/mws "Source" ref/mws ref/license))

                      ; Words that are handled elsewhere
                      #"\A(?i:the\s+)"                                                            ""  ; A leading, optional "the" is always added to every regex, in common-replacements

                      ; Optional words
                      #"(?<!\w)(?i:licen[cs]e[\s\-]agreement)(?!\w)"                              (re/inline (re/opt-grp ref/license ref/mws "agreement"))
;####TEST!!!!
;                      #"(?<!\w)(?i:licen[cs]e)(?!\w)"                                             ref/license
                      #"(?<!\w)(?i:(?:(?:software|public)\s+)*licen[cs]e)(?!\w)"                  (re/inline (re/opt-grp (re/zom-grp ref/ows (re/alt-grp "style" ref/public ref/software) ref/ows) ref/license))
                      #"(?<!\w)(?i:Lizenz)(?!\w)"                                                 (re/inline (re/opt (re/alt-grp ref/license "Lizenz")))
                      #"(?<!\w)(?i:public)(?!\w)"                                                 (re/inline (re/opt-grp ref/public))
                      #"(?<!\w)(?i:software)(?!\w)"                                               (re/inline (re/opt-grp ref/software))
                      #"(?<!\w)(?i:hardware)(?!\w)"                                               (re/inline (re/opt-grp ref/hardware))
                      #"(?<!\w)(?i:free)(?!\w)"                                                   (re/inline (re/opt-grp "Free"))
                      #"(?<!\w)(?i:generic)(?!\w)"                                                (re/inline (re/opt-grp "Generic"))
                      #"(?<!\w)(?i:international)(?!\w)"                                          (re/inline (re/opt-grp "International"))

                      ; Alternative spellings - organisations
                      #"(?<!\w)(?i:Hewlett[\s\-]+Packard)(?!\w)"                                  ref/hewlett-packard
                      #"(?<!\w)(?i:Microsoft)(?!\w)"                                              ref/microsoft
                      #"(?<!\w)(?i:University of California)(?!\w)"                               ref/uc
                      #"(?<!\w)(?i:Sun)(?!\w)"                                                    ref/sun-oracle

                      ; Alternative spellings - countries
                      #"(?<!\w)(?i:Australia)(?!\w)"                                              ref/au
                      #"(?<!\w)(?i:Austria)(?!\w)"                                                ref/at
                      #"(?<!\w)(?i:England (?:and|&) Wales|England|United Kingdom|UK)(?!\w)"      ref/gb
                      #"(?<!\w)(?i:France)(?!\w)"                                                 ref/fr
                      #"(?<!\w)(?i:Germany|Deutsche)(?!\w)"                                       ref/de
                      #"(?<!\w)(?i:Japan)(?!\w)"                                                  ref/jp
                      #"(?<!\w)(?i:Netherlands)(?!\w)"                                            ref/nl
                      #"(?<!\w)(?i:United States|USA?)(?!\w)"                                     ref/us
                      #"(?<!\w)(?i:European Union|EU)(?!\w)"                                      ref/eu

                      ; Alternative spellings & misspellings
                      #"(?<!\w)(?i:acknowledge?ments?)(?!\w)"                                     ref/acknowledgement
                      #"(?<!\w)(?i:merchant[ai]bility)(?!\w)"                                     ref/merchantability
                      #"(?<!\w)(?i:non-?commercial)(?!\w)"                                        (re/inline (re/join "non" ref/ows "commercial"))
                      #"(?<!\w)(?i:open\s+source(?:\s+software)?)(?!\w)"                          ref/open-source

                      ; Equivalents
                      #"(?<!\w)(?i:and|&)(?!\w)"                                                  ref/ands
                      #"(?<!\w)(?:w/)"                                                            ref/withs
                      #"(?iu:é)"                                                                  #"(?:é|é|e)"  ; As of License List v3.28.0 'é' is the only accented character present, and it can represented 2 ways in Unicode, one of which cannot be used in a regex character class as it's made up of multiple codepoints
                      #"[\"']"                                                                    (re/inline (re/opt ref/qots))
                      #"[/\\]"                                                                    (re/inline (re/opt ref/hyphens+slashes))
                      ref/mopen-parens                                                            ref/oopen-parens
                      ref/mclose-parens                                                           ref/oclose-parens
                      #"[\s\-–]+"                                                                 ref/ows)
        ; Cleanup, escape, and concat into a single pattern
          (->> (u/mapcat-str #(vector (re/esc %)))
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
   ;####TODO: REVISIT THIS POTENTIAL OPTIMISATION, BUT FILTER THE IDS NOT THE NAMES (SINCE THE NAME REGEXES ARE MORE GENERAL)
   ; Any names that are identical to an id, after "license" or "public" is removed, are removed
   (let [;distinct-names (seq (filter identity (map #(when-not (some #{(-> % (s/replace (re/inline (re/fgrp "i" ref/license)) "") (s/replace (re/inline (re/fgrp "i" ref/public)) ""))} ids) %) names)))
         id-regexes   (seq (map (partial id->regex   version-series) ids))
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

(defn regexes-for-id-or-version-series
  "Convenience function that returns the regexes for a specific SPDX identifier
  or version series id.  This function is not used by lice-comb anywhere and is
  solely intended for human use at a REPL - the implementation is inefficient."
  [id-or-version-series-id]
  (if-let [vs (get (:version-series (verser/version-series)) id-or-version-series-id)]
    (regexes vs)
    (regexes id-or-version-series-id)))

(defn re-matches-for-id-or-version-series
  "Convenience function that attempts to re-match the regexes for a specific
  SPDX identifier or version series id against `s` (a `String`).  This function
  is not used by lice-comb anywhere and is solely intended for human use at a
  REPL - the implementation is inefficient."
  [id-or-version-series-id ^CharSequence s]
  (let [res (sort-by #(* -1 (count (str %))) (mapcat vals (regexes-for-id-or-version-series id-or-version-series-id)))]
    (some #(ncg/re-matches % s) res)))

(defn re-find-for-id-or-version-series
  "Convenience function that attempts to re-find the regexes for a specific SPDX
  identifier or version series id against `s` (a `String`).  This function is
  not used by lice-comb anywhere and is solely intended for human use at a
  REPL - the implementation is inefficient."
  [id-or-version-series-id ^CharSequence s]
  (let [res (sort-by #(* -1 (count (str %))) (mapcat vals (regexes-for-id-or-version-series id-or-version-series-id)))]
    (some #(ncg/re-find % s) res)))

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
