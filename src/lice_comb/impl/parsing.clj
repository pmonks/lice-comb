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

(defn- determine-strategy-for-id-match
  "Returns the strategy (a keyword) for the given `m`atch, matched to `id`."
  [m id]
  (cond
    (= (s/lower-case m) (s/lower-case id)) :spdx-listed-identifier
    :else                                  :spdx-listed-identifier-near-match))

(defn- determine-strategy-for-name-match
  "Returns the strategy (a keyword) for the given `n`ame, matched to `id`."
  [n id]
  (let [listed-name (or (:name (sl/id->info id)) (:name (se/id->info id)))]
    (cond
      (= n listed-name)                               :spdx-listed-name-exact-match
      (= (s/lower-case n) (s/lower-case listed-name)) :spdx-listed-name-case-insensitive-match
      :else                                           :spdx-listed-name-near-match)))

;####TODO: CONSIDER MOVING THIS TO lice-comb.impl.id-detection - THIS WOULD RESOLVE THE TODOS INLINE TOO
(defn- attempt-to-match-single-id
  "Attempts to match a single SPDX identifier from `s` (a `String`). The
  specific steps involve identifying whether `s` is:
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
        ; 2. n is already an SPDX id / expression
        {normalised-expression (list {:type     :declared
                                      :strategy (if (= 1 (count (sexp/extract-ids (sexp/parse normalised-expression)))) :spdx-listed-identifier :spdx-expression)
                                      :source   (list s)})}
        (if-let [id (lcis/best-near-match-id s)]
          ; 3. n is a near match for an SPDX identifier
          {id (list {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-id-match s id) :source (list s)})}
          (if-let [id (lcis/best-near-match-name s)]
            ; 4. n is an SPDX listed name
            {id (list {:id id :type :concluded :confidence :high :strategy (determine-strategy-for-name-match s id) :source (list s)})}
            (if (lciu/valid-http-uri? s)
              (if-let [ei-map (parse-uri s)]
                ; 5.1. n is a URL and it's in the SPDX license or exception list
                ei-map
                ; 5.2. n is a URL but it's not in the SPDX license or exception list
                (let [unidentified-license-ref (lcis/name->unidentified-license-ref s)]
                  {unidentified-license-ref (list {:id unidentified-license-ref :type :concluded :confidence :high :strategy :unidentified :source (list s)})}))
              ;####TODO: THIS REGEX IS DUPLICATED FROM lice-comb.impl.id-detection - THERE SHOULD BE A BETTER WAY
              (if (re-matches #"(?i)\b(Propriet[aoe]ry|Commercial|Propriet[aoe]ry\s*[/\-\\]\s*Commercial|All\s+Rights\s+Reserved|Private)\b" s)
                ; 6. n is proprietary / commercial
                (let [prop-comm-license-ref (lcis/proprietary-commercial)]
                  {prop-comm-license-ref (list {:id prop-comm-license-ref :type :concluded :confidence :high :strategy :regex-matching :source (list s)})})
                ;####TODO: THIS REGEX IS DUPLICATED FROM lice-comb.impl.id-detection - THERE SHOULD BE A BETTER WAY
                (when (re-matches #"(?i)\bPublic\s+Domain(?![\s\(]*CC\s*0)" s)
                  ; 7. n is Public Domain
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
  the first element is the new `String`, and the second element is a sequence of
  expression info maps or `nil` if no replacements occurred."
  [s]
  ;####TODO: IMPLEMENT ME!!!!
  [s nil])

;  (if (s/blank? s)
;    [s nil]
;    ;####TODO: LOSING A LOT OF IMPORTANT CONTEXT HERE!!!!!
;    (let [result (lciid/replace-family "GPL"  s)
;          result (lciid/replace-family "CDDL" (first result))
;          result (lciid/replace-family "X11"  (first result))]
;      [(first result) nil])))   ;####TODO: BOGUS EI (second tuple element)

(defn- replace-operators-with-keywords
  "Replaces `String`s that represent SPDX expression operators in `coll` with
  an equivalent keyword (`:and`, `:or`, `:with`), or nothing if the 'operator'
  in question is unidentifiable (e.g. `and/or`, `/`, `\\`)."
  [coll]
  (filter identity
    (map #(let [trimmed (s/trim %)
                val     (-> trimmed
                            s/lower-case
                            (s/replace #"(?i)w/"           "with")
                            (s/replace #"&+"               "and")
                            (s/replace #"/+"               "/")
                            (s/replace #"\\+"              "/")
                            (s/replace #"(?i)and\s*/\s*or" "/"))]
            (case val
              "and"    :and
              "or"     :or
              "with"   :with
              ("/" "") nil
              trimmed))  ; Not a keyword - keep it unchanged (albeit trimmed)
         coll)))

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

(defn- remove-invalid-operator-keywords
  "Removes invalid operator keywords from `coll`. This is defined as all leading
  and trailing keywords, and collapsing sequential runs of keywords (either to
  1 keyword if they're all the same, or no keywords if they're heterogeneous)."
  [coll]
  (->> coll
       (drop-while keyword?)
       (lci3/rdrop-while keyword?)
       collapse-duplicate-operator-keywords))

(defn- attempt-to-find-ids-in-fragments
  "Attempts to find one or more ids in the fragments (Strings) in `coll`.
  For each fragment returns a sequence of maps, where the key(s) are the
  detected identifier(s), and the value(s) are an expression-info map for that
  identifer. If no ids are found in a fragment, the identifier for that fragment
  will be an Unidentified LicenseRef.

  Other elements (i.e. operator keywords) are passed through unchanged)."
  [coll]
  (flatten
    (map #(if (string? %)
            (if (or (sl/listed-id?                %)
                    (se/listed-id?                %)
                    (lcis/lice-comb-license-ref?  %)
                    (lcis/lice-comb-addition-ref? %))
              [{% nil}]   ; Don't need an expression-info here, since it will already have one from earlier steps in the parsing process
              (if-let [ids (lciid/detect-ids %)]
                ids
                (let [unidentified-id (lcis/name->unidentified-license-ref %)]
                  [{unidentified-id {:id unidentified-id :type :concluded :confidence :low :strategy :unidentified :source (list %)}}])))
            %)
         coll)))

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
  "Rebuilds one or more SPDX expressions from the given `fragments` and
  expression-infos (`eis`).  `fragments` is a heterogeneous sequence containing
  maps and/or keywords.  Each map represents a detected license, with an SPDX
  identifier as the key and an exression-info map as the value.  Each keyword
  represents one of the SPDX expression operators (`:and`, `:or`, `:with`).

  It returns a sequence of maps, where the keys are SPDX expressions, and the
  associated value is a sequence of expression-info maps related to that
  expression."
  [fragments existing-eis]
  (let [eis                 (concat existing-eis (filter identity (mapcat #(when (map? %) (vals %)) fragments)))
        expr-elements       (mapcat #(if (keyword? %) [%] (keys %)) fragments)
        expressions         (map #(sexp/normalise (s/join " " (map name %))) (group-expressions expr-elements))
        ; Now regroup expression-infos with their associated expression(s)
        ei-lookup           (group-by :id eis)
        expr-ei-pairs       (mapcat #(let [ids (sexp/extract-ids (sexp/parse %))]
                                       [% (seq (filter identity (conj (vec (mapcat (fn [id] (get ei-lookup id)) ids))
                                                                      (when (> (count ids) 1) {:type :concluded :confidence :high :strategy :expression-inference}))))])
                                    expressions)]
    (apply hash-map expr-ei-pairs)))

(defn- split-and-detect-fragments
  "Splits `s` (a `String`) into fragments based on probable separators (SPDX
  expression operators and various other delimiters commonly seen in license
  names), then detects the license and/or exception identifier(s) in each
  fragment, the finally rebuilds expressions"
  [s eis]
  (let [fragments   (some-> (lciu/retained-split s #"(?i)(\band\s*/+\s*or\b|\band\b|\bor\b|\bwith\b|\bw/|&+|/+|\\+)")
                            replace-operators-with-keywords
                            remove-invalid-operator-keywords
                            attempt-to-find-ids-in-fragments)
        identifiers (mapcat keys (filter #(not (keyword? %)) fragments))]
    ; If we only found unidentifieds, return nil
    (when-not (seq (filter #(lcis/unidentified? %) identifiers))
      (rebuild-expressions fragments eis))))

(defn- attempt-to-parse-name
  "Attempts to parse `n`ame into an SPDX expression, by:
  1. Replacing listed names with their ids
  2. Replacing listed names with their ids
  3. Replacing 'tricky' names with their ids
  4. Parsing the input for any elements it contains that haven't yet been converted into an id

  Returns `nil` if parsing fails."
  [n]
  ; 1. Replace near matches for SPDX listed ids
  (let [[n eis] (replace-listed-ids-near-match n)]
    (if-let [normalised-expression (sexp/normalise n)]
      {normalised-expression eis}
      ; 2. Replace near matches for SPDX listed names
      (let [[n new-eis] (replace-listed-names-near-match n)
            eis         (concat eis new-eis)]
        (if-let [normalised-expression (sexp/normalise n)]
          {normalised-expression eis}
          ; 3. Replace tricky names (those with operators in them, primarily)
          (let [[n new-eis] (replace-tricky-names n)
                eis         (concat eis new-eis)]            
            (if-let [normalised-expression (sexp/normalise n)]
              {normalised-expression eis}
              ; 4. Split on operators then detect fragments - note: this is the (only) point where we can end up with multiple expressions
              (when-let [fully-parsed-result (split-and-detect-fragments n eis)]
                fully-parsed-result))))))))

(defn parse-name
  "Parses the given license `n`ame, returning an expressions-info map or `nil`
  when `n`ame is blank."
  [n]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      (or ; 1. Is it a 'singleton' case?
          (attempt-to-match-single-id n)
          ; 2. Is it a 'complex' case?
          (attempt-to-parse-name n)
          ; 3. Could not parse at all - return a single unidentified LicenseRef
          (let [unidentified-id (lcis/name->unidentified-license-ref n)]
            {unidentified-id (list {:id unidentified-id :type :concluded :confidence :low :strategy :unidentified :source (list n)})})))))

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
