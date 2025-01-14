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
  (:require [clojure.string                  :as s]
            [clojure.set                     :as set]
            [clojure.java.io                 :as io]
            [spdx.licenses                   :as sl]
            [spdx.exceptions                 :as se]
            [spdx.matching                   :as sm]
            [spdx.expressions                :as sexp]
            [spdx.regexes                    :as sre]
            [embroidery.api                  :as e]
            [rencg.api                       :as rencg]
            [lice-comb.impl.spdx             :as lcis]
            [lice-comb.impl.id-detection     :as lciid]
            [lice-comb.impl.expressions-info :as lciei]
            [lice-comb.impl.http             :as lcihttp]
            [lice-comb.impl.data             :as lcid]
            [lice-comb.impl.correction       :as lcic]
            [lice-comb.impl.utils            :as lciu]
            [lice-comb.impl.3rd-party        :as lci3]))

; Names that are so cursed we don't even both trying to parse them
(def ^:private cursed-names-d (delay (lcid/load-edn-resource "lice_comb/names.edn")))

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
      ; Note: we don't need to sexp/normalise the keys here, as the only expressions that can be returned are already correctly constructed
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
      ; We don't need to sexp/normalise the keys here, as we never detect an expression from a URI
      (lciei/prepend-source uri (lcic/correct result)))))

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

(defn- determine-strategy
  "Returns the strategy (a keyword) for the given `match`, matched to
  `listed-name`."
  [match id listed-name]
  (cond
    (= (s/lower-case match) (s/lower-case id))          :spdx-listed-identifier  ; Because some names are also ids (or close enough that a name regex will match)
    (= match listed-name)                               :spdx-listed-name-exact-match
    (= (s/lower-case match) (s/lower-case listed-name)) :spdx-listed-name-case-insensitive-match
    :else                                               :spdx-listed-name-near-match))

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

(def ^:private operator-re #"(?i)\s*((?<!\w)(?<andOr>and\s*/+\\+\s*or)(?!\w)|(?<!\w)(?<and>and)(?!\w)|(?<!\w)(?<or>or)(?!-later)(?!\w)|(?<!\w)(?<with>with)(?!\w)|(?<!\w)w/|(?<ampersand>&+)|(?<forwardSlash>/+)|(?<backSlash>\\+))\s*")

(defn- detect-operators
  "Detects operators in `String` values in `coll`, replacing each one with a
  keyword representing the detected operator. The possible values are: `:and`,
  `:or`, and `:with`."
  [coll]
  (remove-invalid-operator-combos
    (filter lciu/not-blank-string?
            (lciu/mapcat-pred string?
                              #(lciu/replacing-split % operator-re (fn [m]
                                                                     (cond
                                                                       (get m "and")       :and
                                                                       (get m "ampersand") :and
                                                                       (get m "or")        :or
                                                                       (get m "with")      :with
                                                                       :else               nil)))
                              coll))))

(defn- find-ids-in-fragment
  "Attempts to find one or more ids in `fragment` (a `String`).
  For each fragment returns a sequence of expression-info maps, one for each
  detected identifer. If no ids are found in a fragment, returns the fragment in
  a sequence."
  [fragment]
  (let [fragment (s/trim fragment)]
    ; 1. Is it a listed id, LicenseRef or AdditionRef?
    (if-let [ids (seq (map #(get % "Identifier") (rencg/re-seq-ncg (sre/ids-re) fragment)))]
      (map #(hash-map :id (sexp/normalise %) :type :declared :strategy :spdx-listed-identifier :source (list fragment)) ids)
      ; 2. Does it contain any SPDX identifiers?
      (if-let [ids (lcis/find-ids fragment)]
        (map (fn [id] {:id id :TODO "####TODO!"}) ids)  ;####TODO: IMPLEMENT THIS!!!!
        ; 3. Can we detect other ids in it, using custom regexes?
        (if-let [result (lciid/find-ids fragment)]
          result
          ; 4. Give up and return fragment (in a sequence)
          [fragment])))))

(defn- find-ids
  "Attempt to find ids in the `String`s in `coll`. Other values are passed
  through unchanged."
  [coll]
  (lciu/mapcat-pred string? find-ids-in-fragment coll))

(defn- done-parsing?
  "Are we done parsing `coll`?"
  [coll]
  (every? #(or (not (string? %)) (s/blank? %)) coll))

(def ^:private extraneous-fragment-res-d (delay [#"(?i)dual"
                                                 #"(?i)(public[\s-\\\/]+)?licen[cs]e"
                                                 #"(?i)copyright(\s+\(c\))?(\s+©️)?(\s+©)?"
                                                 #"(?i)[\s-,]*version[\s-,]+\d+"]))  ; Some listed names leave dangling versions (e.g. "Do What The Fuck You Want To Public License, Version 2")

(defn- remove-extraneous-fragments
  "Removes 'extraneous' fragments (`String`s) from `coll`."
  [coll]
  (loop [[re & r] @extraneous-fragment-res-d
         coll     coll]
    (if (or (not re)
            (done-parsing? coll))
      (filter lciu/not-blank-string? coll)
      (let [new-coll (filter lciu/not-blank-string?
                             (lciu/map-pred string?
                                            #(when-not (or (< (count %) 4)              ; Strip anything shorter than 4 characters long
                                                           (re-matches re (s/trim %)))  ; or that matches one of the extraneous fragment regexes
                                               %)
                                            coll))]
        (recur r new-coll)))))

(defn- replace-spdx-names
  "Detects SPDX listed license names in the `String`s in `coll`, returning a
  sequence of expression-info maps, one for each detected license name. If no
  names are found in a fragment, `nil` will be returned."
  [coll]
  (loop [[[re id n] & r] @lcis/name-regex-id-pairs-d   ;####TODO: CONSIDER MOVING THAT VAR HERE!!!!
         coll            coll]
    (if (or (not re)
            (not id)
            (done-parsing? coll))  ; coll is fully devoid of strings, so we can terminate early
      coll
      (let [new-coll (lciu/mapcat-pred string?
                                       #(lciu/replacing-split %
                                                              re
                                                              (fn [m]
                                                                (let [id       (str id (when (get m "orLater") "+"))
                                                                      id       (if-let [new-id (sexp/normalise id)] new-id id)  ; Note: exception ids won't normalise
                                                                      strategy (determine-strategy (:match m) id n)]
                                                                  (merge {:id       id
                                                                          :strategy strategy
                                                                          :source   (list (:match m))}
                                                                         (case strategy
                                                                           :spdx-listed-identifier {:type :declared}
                                                                           {:type :concluded :confidence :high})))))
                                       coll)]
        (recur r new-coll)))))

;(defn- replace-lgpl-variant
;  [s]


;(defn- replace-lgpl-variants
;  [coll]
;  (lciu/mapcat-pred string? replace-lgpl-variant coll))

(defn- replace-handcrafted
  "Uses handcrafted regexes to replace certain license names in the `String`s in
  `coll`, returning a new `coll`.  Non-`String` values in `coll` are passed
  through unchanged."
  [coll]
  coll)
;  (-> coll
;      replace-lgpl-variants))

(defn- replace-families
  "Replaces certain families (as defined in lice-comb.impl.id-detection) in the
  `String`s in `coll`, returning a new `coll`.  Non-`String` values in `coll`
  are passed through unchanged."
  [coll]
  (loop [[family & r] [:GNU :CDDL :X11]  ;####TODO: consider other families!
         coll         coll]
    (if (or (not family)
            (done-parsing? coll))  ; coll is fully devoid of strings, so we can terminate early
      coll
      (let [new-coll (lciu/mapcat-pred string?
                                       (partial lciid/replace-ids family)
                                       coll)]
        (recur r new-coll)))))

(defn- replace-unidentifieds
  "Replace any `String`s in `coll` with an expression-info map containing an
  unidentified LicenseRef."
  [coll]
  (lciu/map-pred string?
                 #(let [s (lciu/trim-non-word %)]
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

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the `coll`ection containing eis
  and operator keywords.  Returns an expressions-info map."
  [coll]
  (when (seq coll)
    (if (every? string? coll)
      nil  ; Didn't detect anything, so fall through and mark the entire thing as unidentified
      (let [eis (filter map? coll)]
        (if (every? lcis/unidentified? (map :id eis))
          nil  ; Detected nothing but unidentifieds, so fall through and mark the entire thing as unidentified
          (let [; ####TODO: WHEN THE KEYWORD IS :with ENSURE THE FOLLOWING ELEMENT IS AN EXCEPTION, OR (IF LICENSEREF), CONVERT IT TO AN ADDITIONREF
                grouped-expressions (filter #(or (> (count %) 1) (not (lcis/unidentified? (:id (first %))))) (group-expressions coll))  ; Remove solitary LicenseRefs
;####TEST!!!!
_ (debug-print grouped-expressions "rebuild-expressions 0")
                expressions         (map #(sexp/normalise (s/join " " (map name %))) (map :id grouped-expressions))
                ; Now regroup expression-infos with their associated expression(s)
                ei-lookup           (group-by :id eis)
                result              (into {}
                                          (map #(let [ids (sexp/extract-ids (sexp/parse %) {:include-or-later? true})]
                                                  [% (seq (filter identity (conj (vec (mapcat (fn [id] (distinct (get ei-lookup id))) ids))
                                                                                 (when (> (count ids) 1) {:type :concluded :confidence :high :strategy :expression-inference}))))])
                                               expressions))]


;                expr-ei-pairs       (mapcat #(let [ids (sexp/extract-ids (sexp/parse %) {:include-or-later? true})]
;                                               [% (seq (filter identity (conj (vec (mapcat (fn [id] (distinct (get ei-lookup id))) ids))
;                                                                              (when (> (count ids) 1) {:type :concluded :confidence :high :strategy :expression-inference}))))])
;                                            expressions)
;                result              (apply hash-map expr-ei-pairs)]
            result))))))

;####TODO: CAN PROBABLY MOVE THIS INTO parse-name ONCE ITS WORKING!!!!
(defn- parse-XXXXTODO
  "Parses the given license `n`ame, returning an an expressions-info map or
  `nil` if no expressions can be found."
  [n]
  (when-let [result (-> [n]
                        ; Parsing, with short circuiting of steps if we're done
                        (lciu/until-> done-parsing?
                                      replace-handcrafted  ; Replace specific name variations that are highly problematic first
;####TEST!!!!
;(debug-print "0")
                                      replace-families  ; Replace certain difficult license families (e.g. GNU, CDDL)
;####TEST!!!!
;(debug-print "1")
                                      replace-spdx-names   ; Replace SPDX listed names; this covers the vast majority of "and", "or", "with" in names cases
;####TEST!!!!
;(debug-print "2")
;                                      replace-expressions  ; This covers ids   ; ####PERHAPS UNECESSARY, GIVEN find-ids DOES THIS ANYWAY???
;####TEST!!!!
;(debug-print "3")
                                      detect-operators   ; Split the strings on operators, with confidence that they're truly operators and not part of a name
;####TEST!!!!
;(debug-print "4")
                                      find-ids)
                        ; Cleanup
                        remove-extraneous-fragments
                        remove-invalid-operator-combos
                        replace-unidentifieds
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
      ; 1. If it's cursed, return it
      (if-let [cursed-ids (get @cursed-names-d n)]
        cursed-ids
        ; 2. If it's a valid SPDX expression, return the normalised rendition of it
        (if-let [parse-tree (sexp/parse n)]
          (let [normalised-expression (sexp/unparse parse-tree)]
            {normalised-expression (list {:type     :declared
                                          :strategy (case (count (sexp/extract-ids parse-tree))
                                                      1 :spdx-listed-identifier
                                                      :spdx-expression)
                                          :source (list n)})})
          ; 3. If it's URI, attempt to parse that
          (if (lciu/valid-http-uri? n)
            (if-let [ids (parse-uri n)]
              ids
              ; It was a URL, but we weren't able to resolve it to any ids, so return it as unidentified
              (make-unidentified-eis-map n))
            ; 4. Parse the name
            (if-let [result (parse-XXXXTODO n)]
              result
              (make-unidentified-eis-map n))))))))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  (lciid/init!)
  (lcihttp/init!)
  @cursed-names-d
  @extraneous-fragment-res-d
  nil)
