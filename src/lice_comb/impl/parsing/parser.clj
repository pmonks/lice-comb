;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.parsing.parser
  "License name, URI, and text parsing functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                          :as s]
            [clojure.java.io                                         :as io]
            [thread-until.core                                       :as tu]
            [spdx.identifiers                                        :as si]
            [spdx.expressions                                        :as sexp]
            [wreck.api                                               :as re]
            [lice-comb.impl.spdx                                     :as spdx]
            [lice-comb.impl.utils                                    :as u]
            [lice-comb.impl.3rd-party                                :as third-party]
            [lice-comb.impl.parsing.faux-parse                       :as faux]
            [lice-comb.impl.parsing.info-maps                        :as info]
            [lice-comb.impl.parsing.correction                       :as correct]
;            [lice-comb.impl.parsing.utils                            :as pu]
            [lice-comb.impl.regexes.fragments                        :as ref]
            [lice-comb.impl.license-detection.listed-licenses        :as listed-licenses]
            [lice-comb.impl.license-detection.spdx-matching          :as spdx-matching]
            [lice-comb.impl.license-detection.spdx-refs              :as spdx-refs]
            [lice-comb.impl.license-detection.spdx-special-forms     :as spdx-special-forms]
            [lice-comb.impl.license-detection.uris                   :as uris]
            [lice-comb.impl.license-detection.bsd                    :as bsd]
            [lice-comb.impl.license-detection.cc                     :as cc]
            [lice-comb.impl.license-detection.gnu                    :as gnu]
            [lice-comb.impl.license-detection.wtf                    :as wtf]
            [lice-comb.impl.license-detection.cursed                 :as cursed]
            [lice-comb.impl.license-detection.mx4j                   :as mx4j]
            [lice-comb.impl.license-detection.bouncy-castle          :as bouncy-castle]
            [lice-comb.impl.license-detection.jdom                   :as jdom]
            [lice-comb.impl.license-detection.like-clojure           :as like-clojure]
            [lice-comb.impl.license-detection.public-domain          :as public-domain]
            [lice-comb.impl.license-detection.proprietary-commercial :as proprietary-commercial]))

;####TODO: REMOVE ME!!!!
(require '[clojure.pprint :as pp])
(defn debug-print
  ([x] (debug-print x nil))
  ([x msg & _]
   (println "⭐️⭐️⭐️ ➡️" msg)
   (pp/pprint x)
   (println "⬅️ ⭐️⭐️⭐️")
   (flush)
   x))
(defn print-detritus
  [coll n]
;  (binding [*out* *err*]
    (when-let [detritus (seq (filter #(and (string? %) (< (count %) 3)) coll))]
      (debug-print detritus (str n " detritus:")))
  coll)

(defmulti match-text
  "Returns an expressions info map for `text` (a `String`, or something that can
  be read), or `nil` if no matches are found or `text` is `nil`."
  {:arglists '([text])}
  class)

(defmethod match-text java.lang.String
  [^String s]
  (when-let [eis (spdx-matching/text->fragment-infos s)]
    (correct/correct (into {} #(hash-map (:fragment %) %) eis))))

(defmethod match-text java.io.Reader
  [^java.io.Reader r]
  (let [sw (java.io.StringWriter.)]
    (io/copy r sw)
    (match-text (str sw))))

(defmethod match-text java.io.InputStream
  [^java.io.InputStream is]
  (match-text (io/reader is)))

(defmethod match-text :default
  [src]
  (when src
    (with-open [r (io/reader src)]
      (doall (match-text r)))))

(defn parse-uri
  "Parses the given license `uri`, returning a fragment info map, or `nil`
  if no matching license ids were found.

  `attempt-download-and-match?` (default `false`) controls whether URIs are
  downloaded and SPDX matching attempted on the content if they're not found in
  the SPDX license list first"
  ([^String uri] (parse-uri uri false))
  ([^String uri ^Boolean attempt-download-and-match?]
   (when-let [eis (uris/uri->fragment-infos uri attempt-download-and-match?)]
     (correct/correct (into {} (map #(vector (:fragment %) (list %)) eis))))))

(def ^:private re-operator (re/fgrp "i"
                                    ref/ows
                                    (re/alt (re/ncg "andOr"        ref/nwb #"and[\s/\\\-]+or" ref/nwa)
                                            (re/ncg "and"          ref/nwb #"and" ref/nwa)
                                            (re/ncg "or"           ref/nwb #"or" (re/-la #"[\s-]lat[eo]r") ref/nwa)   ;####TODO: THE LOOKAHEAD HERE MAY BE REDUNDANT
                                            (re/ncg "with"         ref/nwb (re/alt (re/join #"with" ref/nwa) #"w/"))
                                            (re/ncg "ampersand"    (re/oom #"&"))
                                            (re/ncg "forwardSlash" (re/-lb #"<") (re/oom #"/"))
                                            (re/ncg "backSlash"    (re/oom #"\\")))
                                    ref/ows))

(defn- detect-operators
  "Detects any operators in the Strings in `coll`, and replaces them with a
  keyword representing the detected operator. The possible keyword values are:
  `:and`, `:or`, and `:with`."
  [coll]
  (filter u/not-blank-string?
          (faux/replace-in-strings coll
                                   re-operator
                                   (fn [m]
                                     (cond
                                       (get m "andOr")     :or   ; We assume the least restrictive interpretation
                                       (get m "and")       :and
                                       (get m "or")        :or
                                       (get m "ampersand") :and
                                       (get m "with")      :with
                                       :else               nil)))))  ; Replace slashes with nothing (i.e. break expressions that contain these)

(defn- collapse-runs-of-operators
  "Collapses sequential runs of operator keywords in `coll`, to a single
  keyword; one of:
  * :and
  * :or
  * :with
  * :and-with (further context-dependent processing needed)
  * :or-with (further context-dependent processing needed)

  Non-keyword values are passed through unchanged.

  The collapsing rules are:
  * leading and trailing operator keywords are dropped
  * duplicates are deduped
  * :and :or (any order): replaced with :or (least restrictive interpretation)
  * :and :with (any order): replaced with :and-with
  * :or :with (any order): replaced with :or-with
  : :and :or :with (any order): replaced with :or-with (least restrictive
    interpretation)"
  [coll]
  (when (seq coll)
    ; Step 1: drop leading, trailing operator keywords, and dedupe
    (let [coll (->> coll
                    (drop-while keyword?)
                    (third-party/rdrop-while keyword?)
                    dedupe
                    seq)]
      ; Step 2: collapse sequences of operator keywords to a single operator keyword
      (mapcat #(if (keyword? (first %))
                 ; Keyword(s) - collapse to a single keyword
                 (case (set %)
                   #{:and :or :with} [:or-with]
                   #{:or :with}      [:or-with]
                   #{:and :with}     [:and-with]
                   #{:and :or}       [:or]
                   [(first %)])
                 ; Not a keyword - pass through
                 %)
              (partition-by keyword? coll)))))

(defn- strip-detritus
  "Strips detritus from `coll`."
  [coll]
  (seq (filter #(if (string? %)
                  (not (s/blank? %))
                  (not (nil? %)))
               (->> (-> coll
                        (faux/replace-in-strings (re/inline (re/join ref/nwb "Dual" ref/ows ref/nwa)) nil)
                        (faux/replace-in-strings (re/inline (re/join ref/nwb (re/opt-grp "Double") ref/ows "licensed" ref/ows "under" (re/opt-grp ref/ows "the") ref/nwa)) nil)
                        (faux/replace-in-strings (re/inline (re/join ref/nwb "Open" ref/ows "Source" ref/ows "Initiative" ref/nwa)) nil)
                        (faux/replace-in-strings (re/inline (re/join ref/nwb "Distributed" ref/ows "under" ref/ows (re/alt-grp "the" "an") ref/nwa)) nil))
                    (u/map-str #(let [s (s/trim (s/replace % #"(?U:\W+)" ""))]  ; Strip all Unicode word characters and trim the result
                                 (when (>= (count s) 3)                         ; Then remove anything short
                                   %)))))))

(defn- sub-unidentified-placeholders
  "Replace any non-blank `String`s in `coll` with an unidentified placeholder (a
  Map).  Blank `String`s are filtered out."
  [coll]
  (filter identity
          (u/map-str #(let [s (s/trim %)]
                        (when-not (s/blank? s)
                          {:unidentified (s/trim s)}))
                     coll)))

(defn- unidentified-placeholder?
  "Is `x` an unidentified placeholder?"
  [x]
  (boolean (and (map? x) (:unidentified x))))

(defn- unidentified-fragment-info
  "Constructs a fragment info map for an unidentified ref and src."
  [ref src]
  (info/fragment-info ref
                      src
                      "Unidentified"))

(defn- unidentified-placeholder->license-ref-fragment-info
  "Turn `m`, an unidentified placeholder map, into a fragment info map
  containing a LicenseRef."
  [^java.util.Map m]
  (let [s (:unidentified m)]
    (unidentified-fragment-info (spdx/name->unidentified-license-ref s) s)))

(defn- unidentified-placeholder->addition-ref-fragment-info
  "Turn `m`, an unidentified placeholder map, into a fragment info map
  containing an AdditionRef."
  [^java.util.Map m]
  (let [s (:unidentified m)]
    (unidentified-fragment-info (spdx/name->unidentified-addition-ref s) s)))

(defn- finalise-operators-and-unidentified-placeholders
  "Finalises operator keywords in `coll` and unidentified placeholders, by:
  * Replacing unidentified placeholders with a fragment info containing either
    a LicenseRef or an AdditionRef
  * Resolving `:or-with` and `:and-with` (to either `:or`, `:and` or `:with`)
  * Replacing any operator that appears between a license/LicenseRef and an
    exception/AdditionRef with `:with`"
  [coll]
  (letfn [(finaliser
            [idx elem]
            (if (keyword? elem)
              ; It's an operator - finalise it based on what it's between
              (let [elem-before (nth coll (dec idx) nil)
                    elem-after  (nth coll (inc idx) nil)]
                (case [(spdx/id-position (:fragment elem-before)) (spdx/id-position (:fragment elem-after))]
                  [:license-position :exception-position] :with
                  (case elem
                    :with     (if (unidentified-placeholder? elem-after) :with :and)
                    ; Assume :and or :or, even when elem-after is an unidentified placeholder
                    :or-with  :or
                    :and-with :and
                    elem)))
              (if (unidentified-placeholder? elem)
                ; It's an unidentified placeholder - finalise it based on the prior operator (if any)
                (case (nth coll (dec idx) nil)
                  :with (unidentified-placeholder->addition-ref-fragment-info elem)
                  (unidentified-placeholder->license-ref-fragment-info elem))
                ; It's a fragment info - pass it through as-is
                elem)))]
    (filter identity (map-indexed finaliser coll))))

(defn- group-expressions
  "Groups expressions in `coll` into sequences that can each be turned into
  a valid SPDX expression.

  For example:
  [{:fragment \"Apache-2.0\" ...}]                                                                   -> [[{:fragment \"Apache-2.0\" ...}]]
  [{:fragment \"Apache-2.0\" ...} {:fragment \"MIT\"}]                                               -> [[{:fragment \"Apache-2.0\" ...}] [{:fragment \"MIT\" ...}]]
  [{:fragment \"Apache-2.0\" ...} :or {:fragment \"MIT\" ...}]                                       -> [[\"Apache-2.0\" :or \"MIT\"]]
  [{:fragment \"Apache-2.0\" ...} :and {:fragment \"MIT\" ...} {:fragment \"GPL-2.0-or-later\" ...}] -> [[{:fragment \"Apache-2.0\" ...} :and {:fragment \"MIT\" ...}] [{:fragment \"GPL-2.0-or-later\" ...}]]"
  [coll]
  (loop [result  [[]]
         [f & r] coll]
    (if-not f
      ; Base case
      result
      ; Recursive case
      (let [l (last result)]
        (case [(map? (last l)) (map? f)]
          [true  true]                (recur (conj result [f])                          r) ; map/map, so start a new nested sequence in result
          ([true false] [false true]) (recur (conj (vec (drop-last result)) (conj l f)) r) ; map/keyword or keyword/map, so continue the current last collection in result
;          [false false]  ; Not possible - we've already removed leading and consecutive keywords
          )))))

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the `coll`ection containing
  fragment info maps, unidentified placeholder maps, and operator keywords.
  Returns an expressions map."
  [coll]
  (when (seq coll)
    ; If all we have are unidentifieds, return nil so that the caller can turn the entire string into a single unidentified
    (when-not (every? #(or (keyword? %) (spdx/unidentified? (:fragment %))) coll)
      (let [grouped-expressions (group-expressions coll)
            result              (into {}
                                      (map #(let [raw-expression (s/join " " (map (fn [elem]
                                                                                    (cond
                                                                                      (keyword? elem)
                                                                                        (s/upper-case (name elem))

                                                                                      (map? elem)
                                                                                        (:fragment elem)

                                                                                      :else
                                                                                       (throw (ex-info "Internal logic error: unexpected element in parse tree" {:parse-tree coll}))))
                                                                                  %))
                                                  expression     (if-let [exp (sexp/canonicalise raw-expression)]
                                                                   exp
                                                                   (throw (ex-info (str "Internal logic error: Invalid SPDX expression constructed: " raw-expression) {:raw-expression raw-expression :parse-tree coll})))
                                                  eis            (filter map? %)]
                                              [expression eis])
                                           grouped-expressions))]
        result))))

(defn- done-parsing?
  "Are we done parsing `coll`?"
  [coll]
  (every? #(or (not (string? %)) (re-matches #"\W*" %)) coll))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions map or `nil` when
  `n`ame is blank.

  `attempt-download-and-match?` controls whether URIs are downloaded and SPDX
  matching attempted on the content (if the URI isn't found in the SPDX license
  list or 'well known URI' list first, and only if the URI's hostname is in a
  small set of allowed domains)."
  [^CharSequence n ^Boolean attempt-download-and-match?]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      ; Alternative 1 - an SPDX expression.  In the unlikely event https://github.com/pmonks/clj-spdx/issues/66 is addressed this should move into the parsing sequence in parse-internal.
      (if-let [parse-tree (sexp/parse n)]
        (let [canonicalised-expression (sexp/unparse parse-tree)
              strategy                 (let [ids (sexp/extract-ids parse-tree)]
                                         (if (and (= 1 (count ids))
                                                  (= :license-id (si/id-type (first ids))))
                                           "SPDX identifier"
                                           "SPDX expression"))
              ei                       (info/fragment-info canonicalised-expression n strategy)]
          {canonicalised-expression (list ei)})

        (or ; Alternative 2 - attempt to parse the name
            (some-> [n]
                    ; License detection within n, with short circuiting of steps if we're done early
                    (tu/until-> done-parsing?
                                ; Special cased license detectors
                                cursed/detect
                                spdx-refs/detect
                                (uris/detect attempt-download-and-match?)
                                bsd/detect
                                cc/detect
                                wtf/detect
                                mx4j/detect
                                bouncy-castle/detect
                                jdom/detect
                                like-clojure/detect
                                public-domain/detect
                                proprietary-commercial/detect
                                ; Detector for the bulk of the SPDX license list (in most cases this detector does most of the work)
                                listed-licenses/detect
                                ; Overly broad / general license detectors
                                gnu/detect                  ; Matches very liberally (uses a "word salad" strategy), so needs to go last
                                spdx-special-forms/detect)  ; Very non-specific (e.g. matches "NONE"), so needs to go last
                    ; SPDX expression construction
                    detect-operators
                    collapse-runs-of-operators
                    strip-detritus
                    sub-unidentified-placeholders
                    finalise-operators-and-unidentified-placeholders
                    ; Rebuild the final expression(s)
                    rebuild-expressions
                    (->> (info/prepend-source-to-fims-within-em n)))

            ; Alternative 3. Turn the entire name into a single unidentified LicenseRef, and construct an expressions map with it
            (let [fim (unidentified-fragment-info (spdx/name->unidentified-license-ref n) n)]
              {(:fragment fim) (list fim)}))))))
