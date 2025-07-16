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
  (:require [clojure.string                              :as s]
            [clojure.set                                 :as set]
            [clojure.java.io                             :as io]
            [thread-until.core                           :as tu]
            [spdx.matching                               :as sm]
            [spdx.expressions                            :as sexp]
            [embroidery.api                              :as e]
            [wreck.api                                   :as re]
            [lice-comb.impl.spdx                         :as lcis]
            [lice-comb.impl.expressions-info             :as lciei]
            [lice-comb.impl.http                         :as lcihttp]
            [lice-comb.impl.correction                   :as lcic]
            [lice-comb.impl.utils                        :as lciu]
            [lice-comb.impl.parsing-utils                :as lcipu]
            [lice-comb.impl.regexes                      :as lcir]
            [lice-comb.impl.3rd-party                    :as lci3]
            [lice-comb.impl.substitutions.cursed         :as cursed]
            [lice-comb.impl.substitutions.bsd            :as bsd]
            [lice-comb.impl.substitutions.cc             :as cc]
            [lice-comb.impl.substitutions.cddl           :as cddl]
            [lice-comb.impl.substitutions.cpe            :as cpe]
            [lice-comb.impl.substitutions.epl            :as epl]
            [lice-comb.impl.substitutions.gnu            :as gnu]
            [lice-comb.impl.substitutions.gnu-exceptions :as gnuexc]
            [lice-comb.impl.substitutions.hippocratic    :as hippocratic]
            [lice-comb.impl.substitutions.mpl            :as mpl]
            [lice-comb.impl.substitutions.refs           :as refs]
            [lice-comb.impl.substitutions.wtf            :as wtf]
            [lice-comb.impl.substitutions.custom         :as custom]
            [lice-comb.impl.substitutions.others         :as others]))

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

(defmulti match-text
  "Returns an expressions-info map for the given license text, or nil if no
  matches are found."
  {:arglists '([text])}
  class)

(defmethod match-text java.lang.String
  [s]
  ; clj-spdx's *-within-text APIs are *expensive* but support batching, so we check batches of ids in parallel
  (let [num-cpus             (.availableProcessors (Runtime/getRuntime))
        license-id-batches   (partition num-cpus @lcis/license-ids-d)
        exception-id-batches (partition num-cpus @lcis/exception-ids-d)
        license-ids-found    (apply set/union (e/pmap* #(sm/licenses-within-text   s %) license-id-batches))
        exception-ids-found  (apply set/union (e/pmap* #(sm/exceptions-within-text s %) exception-id-batches))
        expressions-found    (if (and (= 1 (count license-ids-found))
                                      (= 1 (count exception-ids-found)))
                               #{(str (first license-ids-found) " WITH " (first exception-ids-found))}
                               (set/union license-ids-found exception-ids-found))]
    (when expressions-found
      ; Note: we don't need to sexp/canonicalise the keys here, as the only expressions that can be returned are already correctly constructed
      (lcic/correct (into {} (map #(hash-map % (list {:id % :type :concluded :confidence :high :strategy :spdx-matching-guidelines :source (list "<content>")})) expressions-found))))))

(defmethod match-text java.io.Reader
  [r]
  (let [sw (java.io.StringWriter.)]
    (io/copy r sw)
    (match-text (str sw))))

(defmethod match-text java.io.InputStream
  [is]
  (match-text (io/reader is)))

(defmethod match-text :default
  [src]
  (when src
    (with-open [r (io/reader src)]
      (doall (match-text r)))))

(defn parse-uri
  "Parses the given license `uri`, returning an expressions-info map, or `nil`
  if no matching license ids were found."
  [^String uri]
  (when-not (s/blank? uri)
    (when-let [result (or
                        ; 1. Is the URI a close match for any of the URIs in the SPDX license or exception lists?
                        (when-let [id (lcis/best-near-match-uri uri)]
                          {id (list {:id id :type :concluded :confidence :high :strategy :spdx-listed-uri-near-match})})  ; We don't need a :source here since we prepend it below

                        ; 2. attempt to retrieve the text/plain contents of the uri and perform license text matching on it
                        (when-let [license-text (lcihttp/get-text uri)]
                          (match-text license-text)))]
      ; We don't need to sexp/canonicalise the keys here, as we never detect an expression from a URI
      (lciei/prepend-source uri (lcic/correct result)))))

(defn- make-unidentified-ei
  "Makes an expression-info map for the given license `n`ame, and (optionally)
  unidentified license-ref."
  ([^String n] (make-unidentified-ei n (lcis/name->unidentified-license-ref n)))
  ([^String n unidentified-license-ref]
    {:id unidentified-license-ref :type :concluded :confidence :high :strategy :unidentified :source (list n)}))

(defn- make-unidentified-eis-map
  "Makes a (singleton) expressions-info map for the given unidentified license
  `n`ame."
  [^String n]
  (let [unidentified-id (lcis/name->unidentified-license-ref n)]
    {unidentified-id (list (make-unidentified-ei n unidentified-id))}))

; An approximately correct regex for finding http URIs in larger texts - loosely based on https://www.rfc-editor.org/rfc/rfc3986#appendix-B
; Note: not all matches this regex finds are valid as per RFC-3986 - if that's needed, use lciu/valid-http-uri?
(def ^:private approximate-uri-re (re/join #"(?i)(?<!\w)"
                                           #"(?<scheme>https?)://"
                                           #"(?<address>[^/?#\s]+)"
                                           #"(?<path>\/[^?#\s]*)?"
                                           #"(?:\?(?<queryString>[^#\s]*))?"
                                           #"(?:#(?<fragment>[^\s]*))?"
                                           #"(?!\w)"))

(defn- sub-uris
  "Substitutes any uris in the Strings in `coll` with an expression info map.
  We do that here instead of in a separate namespace because of the dependence
  on [[parse-uri]]."
  [coll]
  (flatten  ; In some cases a single URI results in multiple ids/expression infos, so we flatten them here
    (lciu/replace-in-coll
      coll
      approximate-uri-re
      #(let [uri (:match %)]
         (if-let [ei (parse-uri uri)]  ; Note: this may return more than one identifier (and associated info)
           (vals ei)                   ; Unwrap all expressions info maps, and return them as nested sequences (which gets flattened up top)
           (make-unidentified-ei uri))))))

(defn- collapse-multiple-operator-keywords
  "Collapses sequential runs of keywords in `coll`, to a single keyword; one of:
  * :and
  * :or
  * :with
  * :and-with (further context-dependent processing needed)
  * :or-with (further context-dependent processing needed)

  Non-keyword values are passed through unchanged.

  The collapsing rules are:
  * exact duplicates: deduped
  * :and :or (any order): replaced with :or (least restrictive interpretation)
  * :and :with (any order): replaced with :and-with
  * :or :with (any order): replaced with :or-with
  : :and :or :with (any order): replaced with :or-with (least restrictive
    interpretation)"
  [coll]
  (when (seq coll)
    (mapcat #(if (keyword? (first %))
               (case (set %)
                 #{:and :or :with} [:or-with]
                 #{:or :with}      [:or-with]
                 #{:and :with}     [:and-with]
                 #{:and :or}       [:or]
                 [(first %)])
               %)
            (partition-by keyword? coll))))

;####TODO: FOR CASE 2, CONSIDER TURNING "WITH" INTO "AND"
(defn- process-invalid-with-operators
  "Processes invalid :with operators in `coll`, which involves either:
  1. when surrounded by two licenses, turns the :with into an :and
  2. when not preceded by a license and not followed by license exception,
     removing it.
  Other values are passed through unchanged."
  [coll]
  (let [f (fn [idx elem]
            (if (or (= :with elem) (= :or-with elem) (= :and-with elem))
              (let [elem-before (nth coll (dec idx) nil)
                    elem-after  (nth coll (inc idx) nil)]
                (case [(lcis/id-position (:id elem-before)) (lcis/id-position (:id elem-after))]
                  [:license-position :exception-position] :with
                  [:license-position :license-position]   (if (= :or-with elem) :or :and)
                  nil))
              elem))]
    (filter identity (map-indexed f coll))))

(defn- replace-invalid-operator-combos
  "Replaces invalid operator combinations in `coll` with valid alternatives."
  [coll]
  (when coll
        (->> coll
             (drop-while keyword?)
             (lci3/rdrop-while keyword?)
             dedupe
             collapse-multiple-operator-keywords
             process-invalid-with-operators
             seq)))

;####TODO: THIS NEEDS TO BE REVISITED BASED ON REAL WORLD EXTRANEOUS FRAGMENTS!!!
(def ^:private extraneous-fragment-res-d (delay [#"(?i)copyright([\s\-–—,]+\(c\))?([\s\-–—,]*©️)?([\s\-–—,]*©)?"  ; Copyright fragments
                                                 #"(?i)(pub?lic[\s\-–—\\\/]+)?licen[cs]e"                         ; Uncaptured "public license" suffixes
                                                 #"(?i)dual"                                                      ; Uncaptured "dual" prefix
                                                 (re/join #"(?i)" lcir/fre-date)                                  ; Uncaptured dates
                                                 #"(?U)\W+"]))                                                    ; Fragments containing no (Unicode) alphabetic characters i.e. punctuation only

;####TODO: THIS NEEDS TO BE REVISITED BASED ON REAL WORLD EXTRANEOUS FRAGMENTS!!!
(defn- remove-extraneous-fragments
  "Removes 'extraneous' fragments (`String`s) from `coll`."
  [coll]
  (loop [[re & r] @extraneous-fragment-res-d
         coll     coll]
    (if (or (not re)
            (lcipu/done-parsing? coll))
      (filter lciu/not-blank-string? coll)
      (let [new-coll (lciu/map-str #(let [s (s/trim (s/replace % #"(?U)\W+" ""))]  ; Remove all non-alphanumeric ("word") characters and trim the result
                                      (when (and (>= (count s) 4)                  ; Strip anything with fewer than 4 word characters
                                               (not (re-matches re (s/trim %))))   ; or that matches one of the extraneous fragment regexes
                                        %))
                                   coll)]
        (recur r new-coll)))))

(def ^:private operator-re (re/join #"(?i)\s*"
                                    (re/alt #"(?<!\w)(?<andOr>and[\s/\\\-]+or)(?!\w)"
                                            #"(?<!\w)(?<and>and)(?!\w)"
                                            #"(?<!\w)(?<or>or)(?![\s-]lat[eo]r)(?!\w)"  ;####TODO: the -later negative lookahead is likely redundant
                                            #"(?<!\w)(?<with>with(?!\w)|w/)"
                                            #"(?<ampersand>&+)"
                                            #"(?<forwardSlash>/+)"
                                            #"(?<backSlash>\\+)")
                                    #"\s*"))

(defn- sub-operators
  "Substitutes operators in `String` values in `coll`, replacing each one with a
  keyword representing the detected operator. The possible keyword values are:
  `:and`, `:or`, and `:with`."
  [coll]
  (filter lciu/not-blank-string?
          (lciu/replace-in-coll coll
                                operator-re
                                (fn [m]
                                  (cond
                                    (get m "and")       :and
                                    (get m "ampersand") :and
                                    (get m "or")        :or
                                    (get m "andOr")     :or   ; We assume the least restrictive interpretation
                                    (get m "with")      :with
                                    :else               nil)))))

(defn- sub-unidentifieds
  "Replace any `String`s in `coll` with an expression-info map containing an
  unidentified LicenseRef."
  [coll]
  (lciu/map-str #(let [s (lciu/trim-non-word %)]
                   (if (s/blank? s)
                     (make-unidentified-ei %)
                     (make-unidentified-ei s)))
                coll))

(defn- group-expressions
  "Groups expressions in `coll` into sequences that can be turned into valid
  SPDX expressions.

  For example:
  [{:id \"Apache-2.0\" ...}]                                                       -> [[{:id \"Apache-2.0\" ...}]]
  [{:id \"Apache-2.0\" ...} {:id \"MIT\"}]                                         -> [[{:id \"Apache-2.0\" ...}] [{:id \"MIT\" ...}]]
  [{:id \"Apache-2.0\" ...} :or {:id \"MIT\" ...}]                                 -> [[\"Apache-2.0\" :or \"MIT\"]]
  [{:id \"Apache-2.0\" ...} :and {:id \"MIT\" ...} {:id \"GPL-2.0-or-later\" ...}] -> [[{:id \"Apache-2.0\" ...} :and {:id \"MIT\" ...}] [{:id \"GPL-2.0-or-later\" ...}]]"
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

(defn- fix-addition-refs
  "Fixes LicenseRefs in `coll` that appear after a `:with`, by turning them
  into an AdditionRef."
  [coll]
  (loop [[f s & r] coll
         result    []]
    (if-not f
      result
      (if (and (= f :with)
               (map? s)
               (lcis/lice-comb-license-ref? (:id s)))
        (recur (concat [(assoc s :id (lcis/license-ref->addition-ref (:id s)))] r)
               (conj result f))
        (recur (concat [s] r)
               (conj result f))))))

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the `coll`ection containing eis
  and operator keywords.  Returns an expressions-info map."
  [coll]
  ; If all we have are unidentifieds, return nil so that the caller can turn the entire string into a single unidentified
  (when-not (every? lcis/unidentified? (map :id (filter map? coll)))
    (if (= 1 (count coll))
      {(:id (first coll)) coll}  ; Single id detected, so return it
      (let [grouped-expressions (filter #(or (> (count %) 1) (not (lcis/unidentified? (:id (first %))))) (group-expressions coll))  ; Remove solitary LicenseRefs
            result              (into {}
                                      (map #(let [raw-expression (s/join " " (map (fn [elem]
                                                                                    (if (keyword? elem)
                                                                                      (s/upper-case (name elem))
                                                                                      (:id elem)))
                                                                                  %))
                                                  expression     (sexp/canonicalise raw-expression)
                                                  eis            (filter map? %)]
                                              [expression eis])
                                           grouped-expressions))]
        result))))

;####TODO: CAN PROBABLY MOVE THIS INTO parse-name ONCE ITS WORKING!!!!
(defn- parse-internal
  "Parses the given license `n`ame, returning an an expressions-info map or
  `nil` if no expressions can be found."
  [n]
  (when-let [result (-> [n]
                        ; Substitutions, with short circuiting of steps if we're done early
                        ; The order of these steps is important
                        (tu/until-> lcipu/done-parsing?
                                    refs/sub
                                    sub-uris         ; This is here rather than in its own namespace so as to avoid a circular dependency ####TODO: LOOK INTO FIXING THIS
                                    cursed/sub
                                    bsd/sub
                                    cc/sub
                                    cddl/sub
                                    cpe/sub
                                    gnuexc/sub
                                    epl/sub
                                    hippocratic/sub
                                    wtf/sub
                                    others/sub       ; This handles all other SPDX license and exceptions in a generic fashion
                                    custom/sub       ; This has to go after the others, since it matches things like "NCBI Public Domain Notice"
                                    mpl/sub          ; This has to go after the others, since it matches things like "SimPL-2.0"
                                    gnu/sub)         ; This must go last, due to the "word salad" matching approach, and the plethora of non-GNU licenses that have GPL-like names (e.g Nethack General Public License)
                        ; At this point we've identified all of the licenses we can
                        sub-operators
                        ; Cleanup
;                        deduplicate-identifiers  ;####TODO: to fix things like "Eclipse Public License 2.0 (EPL)"
                        remove-extraneous-fragments
                        replace-invalid-operator-combos
                        sub-unidentifieds
                        fix-addition-refs
                        ; Rebuild the final expression(s)
                        rebuild-expressions)]
    (lciei/prepend-source n result)))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions-info map or `nil`
  when `n`ame is blank."
  [n]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      ; 1. If it's an SPDX expression, return the canonicalised rendition of it - this should be replaced if/when https://github.com/pmonks/clj-spdx/issues/66 is addressed
      (if-let [parse-tree (sexp/parse n)]
        (let [canonicalised-expression (sexp/unparse parse-tree)]
          {canonicalised-expression (list {:type     :declared
                                           :strategy (case (count (sexp/extract-ids parse-tree))
                                                       1 :spdx-listed-identifier
                                                       :spdx-expression)
                                           :source (list n)})})
        ; 2. Parse the name
        (if-let [result (parse-internal n)]
          result
          (make-unidentified-eis-map n))))))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  (lcihttp/init!)
  @extraneous-fragment-res-d
  nil)
