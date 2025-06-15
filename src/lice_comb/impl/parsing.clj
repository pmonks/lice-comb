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
            [lice-comb.impl.substitutions.wtf            :as wtf]
            [lice-comb.impl.substitutions.custom         :as custom]
            [lice-comb.impl.substitutions.others         :as others]))

;####TODO: REMOVE ME!!!!
(require '[clojure.pprint :as pp])
(defn debug-print
  ([x] (debug-print x nil))
  ([x msg]
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
  [uri]
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
  ([n] (make-unidentified-ei n (lcis/name->unidentified-license-ref n)))
  ([n unidentified-license-ref]
    {:id unidentified-license-ref :type :concluded :confidence :high :strategy :unidentified :source (list n)}))

(defn- make-unidentified-eis-map
  "Makes a (singleton) expressions-info map for the given unidentified license
  `n`ame."
  [n]
  (let [unidentified-id (lcis/name->unidentified-license-ref n)]
    {unidentified-id (list (make-unidentified-ei n unidentified-id))}))

(defn- collapse-duplicate-operator-keywords
  "Collapses sequential runs of keywords in `coll`, either to 1 keyword if
  the run is identical, or to no keywords if they're heterogeneous. Non-keyword
  values in `coll` are passed through unchanged."
  [coll]
  (filter #(not= ::sentinel %)
    (reduce #(if (and (keyword? (last %1)) (keyword? %2))
               (if (= (last %1) %2)
                 %1
                 (conj (vec (drop-last %1)) ::sentinel))
               (conj %1 %2))
            []
            coll)))

(defn- remove-invalid-operator-combos
  "Strip invalid operator combinations from `coll`."
  [coll]
  (when coll
        (->> coll
             (drop-while keyword?)
             (lci3/rdrop-while keyword?)
             collapse-duplicate-operator-keywords
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
                        ; Parsing, with short circuiting of steps if we're done early
                        ; These generally proceed from longest to shortest (to avoid premature matches), with some exceptions
                        (lciu/until-> lcipu/done-parsing?
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
                                      custom/sub       ; This has to go after the generics, since it matches things like "NCBI Public Domain Notice"
                                      mpl/sub          ; This has to go after the generics, since it matches things like "SimPL-2.0"
                                      gnu/sub          ; This must go last, due to the "word salad" matching approach, and the plethora of non-GNU licenses that have GPL-like names (e.g Nethack General Public License)
                                      )
                        sub-operators
                        ; Cleanup
                        remove-extraneous-fragments
                        remove-invalid-operator-combos
                        sub-unidentifieds
                        fix-addition-refs
;####TEST!!!!
;(debug-print "after parse, before rebuild")
                        ; Rebuild the final expression(s)
                        rebuild-expressions)]
;####TEST!!!!
;(debug-print result "after rebuild")
    (lciei/prepend-source n result)))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions-info map or `nil`
  when `n`ame is blank."
  [n]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      ; 1. If it's a valid SPDX expression, return the canonicalised rendition of it
      (if-let [parse-tree (sexp/parse n)]
        (let [canonicalised-expression (sexp/unparse parse-tree)]
          {canonicalised-expression (list {:type     :declared
                                           :strategy (case (count (sexp/extract-ids parse-tree))
                                                       1 :spdx-listed-identifier
                                                       :spdx-expression)
                                           :source (list n)})})
        ; 2. If it's URI, attempt to parse that
        (if (lciu/valid-http-uri? n)
          (if-let [ids (parse-uri n)]
            ids
            ; It was a URL, but we weren't able to resolve it to any ids, so return it as unidentified
            (make-unidentified-eis-map n))
          ; 3. Parse the name
          (if-let [result (parse-internal n)]
            result
            (make-unidentified-eis-map n)))))))

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
;####TODO: REMOVE ME!!!!
;  @cursed-name-pairs-d
  nil)
