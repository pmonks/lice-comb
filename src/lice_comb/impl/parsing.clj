;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.parsing
  "License name, URI, and text parsing functionality. Note: this namespace is
  not part of the public API of lice-comb and may change without notice."
  (:require [clojure.string                                            :as s]
            [clojure.java.io                                           :as io]
            [thread-until.core                                         :as tu]
            [spdx.identifiers                                          :as si]
            [spdx.expressions                                          :as sexp]
            [wreck.api                                                 :as re]
            [lice-comb.impl.regex-fragments                            :as ref]
            [lice-comb.impl.spdx                                       :as lcis]
            [lice-comb.impl.info-maps                                  :as im]
            [lice-comb.impl.correction                                 :as lcic]
            [lice-comb.impl.faux-parse                                 :as faux]
            [lice-comb.impl.utils                                      :as lciu]
            [lice-comb.impl.parsing-utils                              :as lcipu]
            [lice-comb.impl.3rd-party                                  :as lci3]
            [lice-comb.impl.license-detection.listed-licenses          :as listed-licenses]
            [lice-comb.impl.license-detection.spdx-matching-guidelines :as spdx-matching]
            [lice-comb.impl.license-detection.spdx-refs                :as spdx-refs]
            [lice-comb.impl.license-detection.spdx-special-forms       :as spdx-special-forms]
            [lice-comb.impl.license-detection.uris                     :as uris]
            [lice-comb.impl.license-detection.bsd                      :as bsd]
            [lice-comb.impl.license-detection.cc                       :as cc]
            [lice-comb.impl.license-detection.gnu                      :as gnu]
            [lice-comb.impl.license-detection.wtf                      :as wtf]
            [lice-comb.impl.license-detection.cursed                   :as cursed]
            [lice-comb.impl.license-detection.mx4j                     :as mx4j]
            [lice-comb.impl.license-detection.bouncy-castle            :as bouncy-castle]
            [lice-comb.impl.license-detection.jdom                     :as jdom]
            [lice-comb.impl.license-detection.like-clojure             :as like-clojure]
            [lice-comb.impl.license-detection.public-domain            :as public-domain]
            [lice-comb.impl.license-detection.proprietary-commercial   :as proprietary-commercial]))

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
(defn print-fragments
  [coll n]
;  (binding [*out* *err*]
    (if-let [fragments (seq (filter #(and (string? %) (< (count %) 3)) coll))]
      (debug-print fragments (str n " fragments:")))
  coll)

(defmulti match-text
  "Returns an expressions info map for `text` (a `String`, or something that can
  be read), or `nil` if no matches are found or `text` is `nil`."
  {:arglists '([text])}
  class)

(defmethod match-text java.lang.String
  [^String s]
  (when-let [eis (spdx-matching/text->fragment-infos s)]
    (lcic/correct (into {} #(hash-map (:fragment %) %) eis))))

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
     (lcic/correct (into {} (map #(vector (:fragment %) (list %)) eis))))))

(def ^:private placeholder-license-ref (lcis/name->unidentified-license-ref "PLACEHOLDER"))

(defn- make-unidentified-fragment-info
  "Makes a fragment info map for the given license `n`ame, optionally
  including `unidentified-license-ref` in the :fragment key."
  ([^String n] (make-unidentified-fragment-info n nil))
  ([^String n unidentified-license-ref]
   (let [placeholder (if (s/blank? unidentified-license-ref) placeholder-license-ref unidentified-license-ref)]
     (merge (im/fragment-info placeholder n "Unidentified name")
            (when (s/blank? unidentified-license-ref) {:fragment :unidentified})))))  ; Note: this results in a (temporarily) invalid fragment info map, which we fix later (by turning it into either a LicenseRef or AdditionRef)

(def ^:private re-operator (re/fgrp "i"
                                    ref/ows
                                    (re/alt (re/ncg "andOr"        ref/nwb #"and[\s/\\\-]+or" ref/nwa)
                                            (re/ncg "and"          ref/nwb #"and" ref/nwa)
                                            (re/ncg "or"           ref/nwb #"or" (re/-la #"[\s-]lat[eo]r") ref/nwa)
                                            (re/ncg "with"         ref/nwb (re/alt (re/join #"with" ref/nwa) #"w/"))
                                            (re/ncg "ampersand"    (re/oom #"&"))
                                            (re/ncg "forwardSlash" (re/oom #"/"))
                                            (re/ncg "backSlash"    (re/oom #"\\")))
                                    ref/ows))

(defn- detect-operators
  "Detects any operators in the Strings in `coll`, and replaces them with a
  keyword representing the detected operator. The possible keyword values are:
  `:and`, `:or`, and `:with`."
  [coll]
  (filter lciu/not-blank-string?
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
                    (lciu/map-str #(let [s (s/trim (s/replace % #"(?U:\W+)" ""))]  ; Strip all Unicode word characters and trim the result
                                     (when (>= (count s) 3)                        ; Then remove anything short
                                       %)))))))

;####TODO: THIS ISN'T WORKING!!!!
(defn- sub-unidentifieds
  "Replace any `String`s in `coll` with a fragment info map containing an
  unidentified LicenseRef or AdditionRef."
  [coll]
  (let [placeholders              (lciu/map-str #(let [s (lciu/trim-non-word %)]
                                                   (if (s/blank? s)
                                                     (make-unidentified-fragment-info %)
                                                     (make-unidentified-fragment-info s)))
                                                coll)
        unidentified-placeholder? (fn [x] (and (map? x) (= :unidentified (:fragment x))))]
    (loop [[f s & r] placeholders
           result    []]
      (if-not f
        result
        (cond
          ; Very first item in coll is unidentified, so convert it to a LicenseRef
          (unidentified-placeholder? f)
            (recur (concat [s] r)
                   (conj result (assoc f :fragment (lcis/name->unidentified-license-ref (s/trim (last (:source f)))))))

          ; Second item we're currently looking at is unidentified, so convert it based on the preceding item
          (unidentified-placeholder? s)
            (if (= f :with)
              ; Convert to AdditionRef
              (recur (concat [(assoc s :fragment (lcis/name->unidentified-addition-ref (s/trim (last (:source s)))))] r)
                     (conj result :with))
              ; Convert to LicenseRef
              (recur (concat [(assoc s :fragment (lcis/name->unidentified-license-ref (s/trim (last (:source s)))))] r)
                     (conj result
                           (case f
                             :or-with  :or
                             :and-with :and
                             f))))

          ; Neither f nor s are unidentified, so pass through unchanged
          :else
            (recur (concat [s] r)
                   (conj result f)))))))

(defn- group-expressions
  "Groups expressions in `coll` into sequences that can be turned into valid
  SPDX expressions.

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
;          [false false]  ; Not possible - we've already removed leading and consecutive keywords in fragments (in remove-invalid-operator-keywords)
          )))))

(defn- collapse-operators
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
                    (lci3/rdrop-while keyword?)
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

(defn- finalise-operators
  "Finalises operator keywords in `coll`, by replacing `AND` and `OR` with
  `WITH` when they occur between a license or LicenseRef and an exception or an
  AdditionRef."
  [coll]
  (let [coll (collapse-operators coll)
        f    (fn [idx elem]
               (if (keyword? elem)
                 (let [elem-before (nth coll (dec idx) nil)
                       elem-after  (nth coll (inc idx) nil)]
                   (case [(lcis/id-position (:fragment elem-before)) (lcis/id-position (:fragment elem-after))]
                     [:license-position :exception-position] :with
                     (case elem
                       :with     :and
                       :or-with  :or
                       :and-with :and
                       elem)))
                 elem))]
    (filter identity (map-indexed f coll))))

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the `coll`ection containing
  fragment info maps and operator keywords.  Returns an expressions map."
  [coll]
  (when coll
    ; If all we have are unidentifieds, return nil so that the caller can turn the entire string into a single unidentified
    (when-not (every? lcis/unidentified? (map :fragment (filter map? coll)))
      (if (and (= 1 (count coll)) (map? (first coll)))
        {(:fragment (first coll)) coll}  ; Single id detected, so return it
        (let [grouped-expressions (filter #(or (> (count %) 1) (not (lcis/unidentified? (:fragment (first %))))) (group-expressions coll))  ; Remove solitary LicenseRefs
              result              (into {}
                                        (map #(let [raw-expression (s/join " " (map (fn [elem]
                                                                                      (cond
                                                                                        (keyword? elem)
                                                                                          (s/upper-case (name elem))

                                                                                        (map? elem)
                                                                                          (:fragment elem)

                                                                                        :else
                                                                                         (throw (ex-info "Unexpected element in collection" {:collection coll}))))
                                                                                    %))
                                                    expression     (if-let [exp (sexp/canonicalise raw-expression)]
                                                                     exp
                                                                     (throw (ex-info (str "Invalid SPDX expression constructed: " raw-expression) {})))
                                                    eis            (filter map? %)]
                                                [expression eis])
                                             grouped-expressions))]
          result)))))

;####TODO: CAN PROBABLY MOVE THIS INTO parse-name ONCE ITS WORKING!!!!
(defn- parse-internal
  "Parses the given license `n`ame, returning an expressions map or `nil` if no
  expressions can be found."
  [^String n ^Boolean attempt-download-and-match?]
  (when-let [result (-> [n]
                        ; Substitutions, with short circuiting of steps if we're done early
                        ; The order of these steps is important
                        (tu/until-> lcipu/done-parsing?
                                    cursed/detect               ; This must go first
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
                                    listed-licenses/detect      ; This should go after most specific detections, since it's very broad (i.e. detects most of the license & exception lists)
                                    gnu/detect                  ; Except this one, since it matches very liberally ("word salad" strategy)
                                    spdx-special-forms/detect)  ; And this should go last, since it's somewhat non-specific (i.e. matching "NONE")
                        ; At this point we've identified all of the licenses we possibly can
;####TODO: REMOVE ONCE TESTED
;                        deduplicate-identifiers
                        detect-operators
;####TEST!!!!
;(print-fragments n)
;####TEST!!!!
;(debug-print "PRIOR TO STRIPPING DETRITUS")
                        strip-detritus
;(debug-print "AFTER STRIPPING DETRITUS")
                        sub-unidentifieds    ;####TODO: THIS ISN'T WORKING!!!!!
                        finalise-operators
;####TEST!!!!
;(debug-print "PRIOR TO REBUILD")
                        ; Rebuild the final expression(s)
                        rebuild-expressions)]
    (im/prepend-source-to-fims-within-em n result)))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions map or `nil` when
  `n`ame is blank.

  `attempt-download-and-match?` (default `false`) controls whether URIs are
  downloaded and SPDX matching attempted on the content if they're not found in
  the SPDX license list first"
  ([^String n] (parse-name n false))
  ([^String n ^Boolean attempt-download-and-match?]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      ; 1. If it's an SPDX expression, return the canonicalised rendition of it - in the unlikely event https://github.com/pmonks/clj-spdx/issues/66 is addressed this should move into the parsing sequence in parse-internal
      (if-let [parse-tree (sexp/parse n)]
        (let [canonicalised-expression (sexp/unparse parse-tree)
              strategy                 (let [ids (sexp/extract-ids parse-tree)]
                                         (if (and (= 1 (count ids))
                                                  (= :license-id (si/id-type (first ids))))
                                           "SPDX identifier"
                                           "SPDX expression"))
              ei                       (im/fragment-info canonicalised-expression n strategy)]
          {canonicalised-expression (list ei)})
        ; 2. Parse the name
        (or (parse-internal n attempt-download-and-match?)
            ; 3. Turn the entire name into a single unidentified LicenseRef
            (let [unidentified-license-ref (lcis/name->unidentified-license-ref n)]
              {unidentified-license-ref (list (make-unidentified-fragment-info n unidentified-license-ref))})))))))
