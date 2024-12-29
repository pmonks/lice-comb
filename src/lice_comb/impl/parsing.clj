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

(defn debug-print
  ([x] (debug-print x nil))
  ([x msg]
   (println "⭐️⭐️⭐️" msg (pr-str x))
   (flush)
   x))

(defn- not-blank-string?
  "`true` when `x` is not a blank `String`."
  [x]
  (or (not (string? x))
      (not (s/blank? x))))

(defn- mapcat-pred
  "mapcat on `coll`, calling `f` for any/all values for which `pred` returns
  logical true, passing through other values unchanged."
  [pred f coll]
  (when (and pred f coll)
    (mapcat #(if (pred %)
               (f %)
               [%])
            coll)))

(comment
(defn- determine-strategy-for-id-match
  "Returns the strategy (a keyword) for the given `m`atch, matched to `id`."
  [match id]
  (cond
    (= (s/lower-case match) (s/lower-case id)) :spdx-listed-identifier
    :else                                      :spdx-listed-identifier-near-match))
)

(defn- determine-strategy
  "Returns the strategy (a keyword) for the given `match`, matched to
  `listed-name`."
  [match id listed-name]
  (cond
    (= (s/lower-case match) (s/lower-case id))          :spdx-listed-identifier  ; Because some names are also ids (or close enough that a name regex will match)
    (= match listed-name)                               :spdx-listed-name-exact-match
    (= (s/lower-case match) (s/lower-case listed-name)) :spdx-listed-name-case-insensitive-match
    :else                                               :spdx-listed-name-near-match))

(comment
(defn- replace-operators-with-keywords
  "Replaces `String`s that represent SPDX expression operators in `coll` with
  an equivalent keyword (`:and`, `:or`, `:with`), or nothing if the 'operator'
  in question is unidentifiable (e.g. `and/or`, `/`, `\\`).  Other values that
  are not operators are preserved in `coll` (but trimmed of whitespace)."
  [coll]
  (filter identity
    (map #(if (string? %)
            (let [trimmed (s/trim %)
                  val     (-> trimmed
                              s/lower-case
                              (s/replace #"(?i)w/"               "with")
                              (s/replace #"&+"                   "and")
                              (s/replace #"(?i)and\s*/+\\+\s*or" "/")
                              (s/replace #"/+"                   "/")
                              (s/replace #"\\+"                  "/"))]
              (case val
                "and"    :and
                "or"     :or
                "with"   :with
                ("/" "") nil
                trimmed))
            %)  ; Not an operator - keep it unchanged
         coll)))
)

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

(def ^:private operator-re #"(?i)((?<!\w)(?<andOr>and\s*/+\\+\s*or)(?!\w)|(?<!\w)(?<and>and)(?!\w)|(?<!\w)(?<or>or)(?!-later)(?!\w)|(?<!\w)(?<with>with)(?!\w)|(?<!\w)w/|(?<ampersand>&+)|(?<forwardSlash>/+)|(?<backSlash>\\+))")

(defn- detect-operators
  "Detects operators in `String` values in `coll`, replacing them with keywords
  and normalising invalid combinations."
  [coll]
  (->> (filter not-blank-string?
               (mapcat-pred string?
                            #(lciu/replacing-split % operator-re (fn [m]
                                                                   (cond
                                                                     (get m "and")       :and
                                                                     (get m "ampersand") :and
                                                                     (get m "or")        :or
                                                                     (get m "with")      :with
                                                                     :else               nil)))
                            coll))
       (drop-while keyword?)
       (lci3/rdrop-while keyword?)
       collapse-duplicate-operator-keywords))

(defn- done-parsing?
  "Are we done parsing `coll`?"
  [coll]
  (every? (complement string?) coll))

(defn- replace-names
  "Detects listed license names in the `String`s in `coll`."
  [coll]
  (loop [[[re id n] & r] @lcis/name-regex-id-pairs-d   ;####TODO: CONSIDER MOVING THAT VAR HERE!!!!
         coll            coll]
    (if (or (not re)
            (not id)
            (done-parsing? coll))  ; coll is fully devoid of strings, so we can terminate early
      coll
      (let [new-coll (filter not-blank-string?
                             (mapcat-pred string?
                                          #(lciu/replacing-split %
                                                                 re
                                                                 (fn [m]
                                                                   (let [strategy (determine-strategy (:match m) id n)]
                                                                     (merge {:id       id
                                                                             :strategy strategy
                                                                             :source   (list (:match m))}
                                                                            (case strategy
                                                                              :spdx-listed-identifier {:type :declared}
                                                                              {:type :concluded :confidence :high})))))
                                          coll))]
        (recur r new-coll)))))

(defn- group-expressions
  "Groups expressions in `coll` into sequences of valid SPDX expressions (albeit
  in sequence form, rather than `String` form.

  For example:
  [\"Apache-2.0\" \"MIT\"]                           -> [[\"Apache-2.0\"] [\"MIT\"]]
  [\"Apache-2.0\" :or \"MIT\"]                       -> [[\"Apache-2.0\" :or \"MIT\"]]
  [\"Apache-2.0\" :and \"MIT\" \"GPL-2.0-or-later\"] -> [[\"Apache-2.0\" :and \"MIT\"] [\"GPL-2.0-or-later\"]]"
  [coll]
  (loop [result  [[]]
         [f & r] coll]
    (if-not f
      ; Base case
      result
      ; Recursive case
      (let [l (last result)]
        (case [(string? (last l)) (string? f)]
          [true  true]                (recur (conj result [f])                          r) ; String/string, so start a new nested sequence in result
          ([true false] [false true]) (recur (conj (vec (drop-last result)) (conj l f)) r) ; String/keyword or keyword/string, so continue the current last collection in result
;          [false false]  ; Not possible - we've already removed leading and consecutive keywords in fragments (in remove-invalid-operator-keywords)
          )))))

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the `coll`ection containing eis
  and operator keywords.  Returns an expressions-info map."
  [coll]
  (when (seq coll)
    (let [eis                 (filter map? coll)
          expr-elements       (map #(if (keyword? %) % (:id %)) coll)
; ####TODO: WHEN THE KEYWORD IS :with ENSURE THE FOLLOWING ELEMENT IS AN EXCEPTION, OR (IF LICENSEREF), CONVERT IT TO AN ADDITIONREF
          expressions         (map #(sexp/normalise (s/join " " (map name %))) (group-expressions expr-elements))
          ; Now regroup expression-infos with their associated expression(s)
          ei-lookup           (group-by :id eis)
          expr-ei-pairs       (mapcat #(let [ids (sexp/extract-ids (sexp/parse %))]
                                         [% (seq (filter identity (conj (vec (mapcat (fn [id] (get ei-lookup id)) ids))
                                                                        (when (> (count ids) 1) {:type :concluded :confidence :high :strategy :expression-inference}))))])
                                      expressions)
          result              (apply hash-map expr-ei-pairs)]
      result)))

(defn- parse-XXXXTODO
  "Parses the given license `n`ame, returning an an expressions-info map or
  `nil` if no expressions can be found."
  [n]
  (when-let [result (-> [n]
                        (lciu/until-> done-parsing?
;####TEST!!!!
;(debug-print "0")
                                      replace-names        ; Replace names first, as this covers the vast majority of "and", "or", "with" in names cases
;####TEST!!!!
;(debug-print "1")
;                                      replace-trickynames  ; Replace other name variations not covered by the standard name regexes
;                                      replace-expressions  ; This covers ids
                                      (-> detect-operators  ; Split the strings on operators, with confidence that they're truly operators and not part of a name
;####TEST!!!!
;(debug-print "2")
;                                          find-ids)
)
;                                     mark-unidentifieds
)
;####TEST!!!!
(debug-print "after parse")
                        rebuild-expressions)]
;####TEST!!!!
(debug-print result "after expression rebuild")
    (lciei/prepend-source n result)))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions-info map or `nil`
  when `n`ame is blank."
  [n]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      ; 1. If it's cursed, return it
      (if-let [cursed-ids (get @cursed-names-d n)]
        (map #(apply hash-map %) cursed-ids)
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
              {(lcis/name->unidentified-license-ref n) (list {:type :concluded :confidence :low :strategy :unidentified :source (list n)})})
            ; 4. Parse the name
            (if-let [result (parse-XXXXTODO n)]
              result
              (let [unidentified-id (lcis/name->unidentified-license-ref n)]
                {unidentified-id (list {:id unidentified-id :type :concluded :confidence :low :strategy :unidentified :source (list n)})}))))))))

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
  nil)






(comment


;####TODO: CONSIDER MOVING THIS TO lice-comb.impl.id-detection
(defn- attempt-to-match-entire-name
  "Attempts to match a single SPDX expression from `s` (a `String`). The
  specific steps involve determining whether `s` is:
  1. a 'cursed' name (see resources/lice_comb/names.edn)
  2. an SPDX expression
  3. an SPDX listed identifier (near match)
  4. an SPDX listed name (near match)
  5. an SPDX listed URL (near match)
  6. proprietary/commercial
  7. public domain

  Returns an expressions-info map or `nil` if `s` is not recognised as the name
  of a single license."
  [s]
  (let [s (s/trim s)]
    ; 1. Is it cursed?
    (if-let [cursed-ids (get @cursed-names-d s)]
      (map #(apply hash-map %) cursed-ids)
      (if-let [normalised-expression (sexp/normalise s)]
        ; 2. s is already an SPDX id / expression
        {normalised-expression (list {:type     :declared
                                      :strategy (if (= 1 (count (sexp/extract-ids (sexp/parse normalised-expression)))) :spdx-listed-identifier :spdx-expression)
                                      :source   (list s)})}
        (if-let [id (lcis/best-near-match-id s)]
          ; 3. ns is a near match for an SPDX identifier
          {id (list {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-id-match s id) :source (list s)})}
          (if-let [id (lcis/best-near-match-name s)]
            ; 4. ns is an SPDX listed name
            {id (list {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-name-match s id) :source (list s)})}
            (if (lciu/valid-http-uri? s)
              (if-let [ei-map (parse-uri s)]
                ; 5.1. s is a URL and it's in the SPDX license or exception list
                ei-map
                ; 5.2. s is a URL but it's not in the SPDX license or exception list
                (let [unidentified-license-ref (lcis/name->unidentified-license-ref s)]
                  {unidentified-license-ref (list {:id unidentified-license-ref :type :concluded :confidence :high :strategy :unidentified :source (list s)})}))
              (if (lciid/match-id :proprietary-commercial s)
                ; 6. s is proprietary / commercial
                (let [prop-comm-license-ref (lcis/proprietary-commercial)]
                  {prop-comm-license-ref (list {:id prop-comm-license-ref :type :concluded :confidence :high :strategy :regex-matching :source (list s)})})
                (when (lciid/match-id :public-domain s)
                  ; 7. s is Public Domain
                  (let [public-domain-license-ref (lcis/public-domain)]
                    {public-domain-license-ref (list {:id public-domain-license-ref :type :concluded :confidence :high :strategy :regex-matching :source (list s)})}))))))))))

(defn- replace-listed-ids-near-match
  "Replaces all near matches of SPDX listed identifiers in `s` with their SPDX
  ids, returning a tuple where the first element is the new `String`, and the
  second element is a sequence of expression info maps or `nil` if no replacements
  occurred."
  [s]
  (if (s/blank? s)
    [s nil]
    (let [[new-s explanations] (lcis/replace-near-match-ids-with-id (s/trim s))
          ei                   (when explanations
                                 (map #(let [[found id] %]
                                         {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-id-match found id) :source (list found)})
                                      explanations))]
      [new-s ei])))

(defn- replace-listed-names-near-match
  "Replaces all near matches of SPDX listed names in `s` with their SPDX ids,
  returning a tuple where the first element is the new `String`, and the second
  element is a sequence of expression info maps or `nil` if no replacements
  occurred."
  [s]
  (if (s/blank? s)
    [s nil]
    (let [[new-s explanations] (lcis/replace-near-match-names-with-id (s/trim s))
          ei                   (when explanations
                                 (map #(let [[found id] %]
                                         {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-name-match found id) :source (list found)})
                                      explanations))]
      [new-s ei])))

(defn- replace-tricky-names
  "Replaces all tricky names in `s` with their SPDX ids, returning a tuple where
  the first element is the (potentially new) `String`, and the second element is
  a sequence of expression info maps or `nil` if no replacements occurred."
  [s]
  (if (s/blank? s)
    [s nil]
    (let [[new-s gnu-eis]  (lciid/replace-ids :GNU s)
          [new-s cddl-eis] (lciid/replace-ids :CDDL new-s)
          [new-s pc-eis]   (lciid/replace-ids :proprietary-commercial new-s)]
          ;####TODO: Check all of the families for trickiness (operators in names, negative look-behinds/aheads, etc.) - e.g. X11
      [new-s (seq (concat gnu-eis cddl-eis pc-eis))])))



(defn- attempt-to-find-ids-in-fragment
  "Attempts to find one or more ids in `fragment` (a `String`).
  For each fragment returns a sequence of maps, where the key(s) are the
  detected identifier(s), and the value(s) are an expression-info map for that
  identifer. If no ids are found in a fragment, the identifier for that fragment
  will be an Unidentified LicenseRef.

  Other elements (i.e. operator keywords) are passed through unchanged)."
  [fragment]
  ; 1. Is it a listed id, LicenseRef or AdditionRef?
  (if (re-matches (sre/ids-re) fragment)
    [{fragment nil}]   ; Don't need an expression-info here, since it will already have one from earlier steps in the parsing process
    ; 2. Does it contain any SPDX identifiers?
    (if-let [result (seq (map (fn [id] {id nil}) (lcis/find-ids fragment)))]  ;####TODO: add eis, as in most _BUT NOT ALL_, cases the ids will already have an expression-info from earlier steps in the parsing process
      result
      ; 3. Can we detect other ids in it, using custom regexes?
      (if-let [result (lciid/find-ids fragment)]
        result
        ; 4. Give up and use the unidentified LicenseRef
        (let [unidentified-license-ref (lcis/name->unidentified-license-ref fragment)]
          [{unidentified-license-ref {:id unidentified-license-ref :type :concluded :confidence :low :strategy :unidentified :source (list fragment)}}])))))

(defn- attempt-to-find-ids-in-fragments
  "Attempts to find one or more ids in the fragments (`String`s) in `coll`.
  For each fragment returns a sequence of maps, where the key(s) are the
  detected identifier(s), and the value(s) are an expression-info map for that
  identifer. If no ids are found in a fragment, the identifier for that fragment
  will be an Unidentified LicenseRef.

  Other elements (i.e. operator keywords) are passed through unchanged)."
  [coll]
  ; This seemingly-redundant let block is only here to facilitate debugging
  (let [result (flatten (map #(if (string? %) (attempt-to-find-ids-in-fragment %) %) coll))]
;####TEST!!!!
;(println "⭐️⭐️⭐️ attempt-to-find-ids-in-fragments result:" (pr-str result))
    result))

(defn- group-expressions
  "Groups expressions in `coll` into sequences of valid SPDX expressions (albeit
  in sequence form, rather than `String` form.

  For example:
  [\"Apache-2.0\" \"MIT\"]                           -> [[\"Apache-2.0\"] [\"MIT\"]]
  [\"Apache-2.0\" :or \"MIT\"]                       -> [[\"Apache-2.0\" :or \"MIT\"]]
  [\"Apache-2.0\" :and \"MIT\" \"GPL-2.0-or-later\"] -> [[\"Apache-2.0\" :and \"MIT\"] [\"GPL-2.0-or-later\"]]"
  [coll]
  (loop [result  [[]]
         [f & r] coll]
    (if-not f
      ; Base case
      result
      ; Recursive case
      (let [l (last result)]
        (case [(string? (last l)) (string? f)]
          [true  true]                (recur (conj result [f])                          r) ; String/string, so start a new nested sequence in result
          ([true false] [false true]) (recur (conj (vec (drop-last result)) (conj l f)) r) ; String/keyword or keyword/string, so continue the current last collection in result
;          [false false]  ; Not possible - we've already removed leading and consecutive keywords in fragments (in remove-invalid-operator-keywords)
          )))))

(defn- rebuild-expressions
  "Rebuilds one or more SPDX expressions from the given `expr` and
  expression-infos (`eis`).  `expr` is a heterogeneous sequence containing
  maps and/or keywords.  Each map represents a detected license, with an SPDX
  identifier as the key and an exression-info map as the value.  Each keyword
  represents one of the SPDX expression operators (`:and`, `:or`, `:with`).

  Returns an expressions-info map."
  [expr existing-eis]
  (let [eis                 (concat existing-eis (filter identity (mapcat #(when (map? %) (vals %)) expr)))
        expr-elements       (mapcat #(if (keyword? %) [%] (keys %)) expr)
        ; ####TODO: WHEN THE KEYWORD IS :with ENSURE THE FOLLOWING ELEMENT IS AN EXCEPTION, OR (IF LICENSEREF), CONVERT IT TO AN ADDITIONREF
        expressions         (map #(sexp/normalise (s/join " " (map name %))) (group-expressions expr-elements))
        ; Now regroup expression-infos with their associated expression(s)
        ei-lookup           (group-by :id eis)
        expr-ei-pairs       (mapcat #(let [ids (sexp/extract-ids (sexp/parse %))]
                                       [% (seq (filter identity (conj (vec (mapcat (fn [id] (get ei-lookup id)) ids))
                                                                      (when (> (count ids) 1) {:type :concluded :confidence :high :strategy :expression-inference}))))])
                                    expressions)
        result              (apply hash-map expr-ei-pairs)]
    result))



(defn- split-and-detect-fragments
  "Splits `s` (a `String`) into fragments based on probable separators (SPDX
  expression operators and various other delimiters commonly seen in license
  names), then detects the license and/or exception identifier(s) in each
  fragment, the finally rebuilds expression(s).  Returns an expressions-info map
  or `nil` if `s` is blank."
  [s eis]
;####TEST!!!!
;(println "⭐️⭐️⭐️ attempt-to-parse-name - input:" (pr-str [s eis]))
  (when-not (s/blank? s)
    (let [expr      (some-> (lciu/retained-split s #"(?i)((?<!\w)and\s*/+\\+\s*or(?!\w)|(?<!\w)and(?!\w)|(?<!\w)or(?!-later)(?!\w)|(?<!\w)with(?!\w)|(?<!\w)w/|&+|/+|\\+)")
                            replace-operators-with-keywords
                            remove-invalid-operator-keywords
                            attempt-to-find-ids-in-fragments)
          fragments (mapcat keys (filter #(not (keyword? %)) expr))]
      (if (every? lcis/lice-comb-ref? fragments)
        ; If we only found lice-comb specific LicenseRefs, normalise them based on what we found (i.e. drop duplicate or redundant unidentified LicenseRefs)
        (let [public-domain?          (boolean (some lcis/public-domain? fragments))
              proprietary-commercial? (boolean (some lcis/proprietary-commercial? fragments))
              result                  (case [public-domain? proprietary-commercial?]
                                        [true true]   {(lcis/proprietary-commercial) eis (lcis/public-domain) eis}   ;####TODO: GROUP eis BY WHICH KEY THEY BELONG TO!!!!
                                        [true false]  {(lcis/public-domain)          eis}
                                        [false true]  {(lcis/proprietary-commercial) eis}
                                        [false false] nil)]
;####TEST!!!!
;(println "⭐️⭐️⭐️ split-and-detect-fragments - result (case 1):" (pr-str result))
          result)
        (let [new-expr (->> expr
;                            (filter #(or (keyword? %) (not (lcis/unidentified? (first (keys %))))))
                            ; Get rid of any dangling keywords after filtering unidentifieds
                            (drop-while keyword?)
                            (lci3/rdrop-while keyword?))
              result   (rebuild-expressions new-expr eis)]
;####TEST!!!!
;(println "⭐️⭐️⭐️ split-and-detect-fragments - result (case 2):" (pr-str result))
          result)))))


(defn- attempt-to-parse-name
  "Attempts to parse `n`ame into one or more SPDX expressions, by:
  1. Replacing listed names with their ids
  2. Replacing listed names with their ids
  3. Replacing 'tricky' names with their ids
  4. Parsing the input for any elements it contains that haven't yet been
     converted into an id

  Returns an expressions-info map or `nil` if parsing fails to find any SPDX
  expressions."
  [n]
;####TEST!!!!
(println "⭐️⭐️⭐️ attempt-to-parse-name - input:" (pr-str n))
  ; 1. Replace near matches for SPDX listed ids
  (let [[new-n eis] (replace-listed-ids-near-match n)]
;####TEST!!!!
(println "⭐️⭐️⭐️ attempt-to-parse-name - result of step 1 (replace ids):" (pr-str [new-n eis]))
    (if-let [normalised-expression (sexp/normalise new-n)]
      (lciei/prepend-source n {normalised-expression eis})
      ; 2. Replace near matches for SPDX listed names
      (let [[new-n new-eis] (replace-listed-names-near-match new-n)
            eis         (concat eis new-eis)]
;####TEST!!!!
(println "⭐️⭐️⭐️ attempt-to-parse-name - result of step 2 (replace names):" (pr-str [new-n eis]))
        (if-let [normalised-expression (sexp/normalise new-n)]
          (lciei/prepend-source n {normalised-expression eis})
          ; 3. Replace tricky names (those with operators in them, primarily)
          (let [[new-n new-eis] (replace-tricky-names new-n )
                eis         (concat eis new-eis)]
;####TEST!!!!
(println "⭐️⭐️⭐️ attempt-to-parse-name - result of step 3 (replace tricky names):" (pr-str [new-n eis]))
            (if-let [normalised-expression (sexp/normalise new-n)]
              (lciei/prepend-source n {normalised-expression eis})
              ; 4. Split on operators then detect fragments - note: this is the (only) point where we can end up with multiple expressions
              (when-let [fully-parsed-result (split-and-detect-fragments new-n eis)]
;####TEST!!!!
(println "⭐️⭐️⭐️ attempt-to-parse-name - result of step 4 (parse):" (pr-str fully-parsed-result))
                (lciei/prepend-source n fully-parsed-result)))))))))



)