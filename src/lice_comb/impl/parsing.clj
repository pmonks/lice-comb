;
; Copyright © 2023 Peter Monks
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; SPDX-License-Identifier: Apache-2.0
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
            [lice-comb.impl.utils            :as lciu]))

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
;####TODO: CONSIDER ADDING A FLAG FOR "MATCH" MODE (used initially) VS "FIND" MODE (used after an expression has been parsed into fragments)
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
  (if (s/blank? s)
    [s nil]
    ;####TODO: LOSING A LOT OF IMPORTANT CONTEXT HERE!!!!!
    (let [result (lciid/replace-family "GPL" s)
          result (lciid/replace-family "CDDL" (first result))]
      [(first result) nil])))   ;####TODO: BOGUS SECOND TUPLE ELEMENT

(defn- split-and-detect-fragments
  [s]
  ;####TODO: IMPLEMENT ME!
  nil)

(defn- attempt-to-parse-name
  "Attempts to parse `n`ame into an SPDX expression, by:
  1. Replacing listed names with their ids
  2. Replacing listed names with their ids
  3. Replacing 'custom' names with their ids
  4. Parsing out

  4. Identifying operators and canonicalising them (AND, OR, WITH, AND/OR, OR-LATER)
  5. Inferring operators where appropriate (i.e. where a license id is followed by an exception id)
  6. Splitting (if needed) where operators don't exist and cannot be inferred

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
              ; 4. Split on operators then detect fragments
              (let [[n new-eis] (split-and-detect-fragments n)
                    eis         (concat eis new-eis)]
                (when-let [normalised-expression (sexp/normalise n)]
                  {normalised-expression eis})))))))))

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




































; OLD SHIT TO BE DELETED!!!!!!!!!!!!!!!!!!!!!!


(comment







(defn- replace-by-with-ei
  "clojure.string/replace-by, but returning a (singleton) map where the key is
  the replaced string, and the value is an expressions info map."
  [^CharSequence s ^java.util.regex.Pattern re f]
  (let [m (re-matcher re s)]
    (if (.find m)
      (let [buffer (StringBuffer. (.length s))
            match  (re-groups m)
            ei     {f (list {:id f :type :concluded :confidence :high :strategy :spdx-listed-name :source (list match)})}]
        (loop [found true]
          (if found
            (do (.appendReplacement m buffer (java.util.regex.Matcher/quoteReplacement (f (re-groups m))))
                (recur (.find m)))
            (do (.appendTail m buffer)
                {(.toString buffer) ei}))))
      {s nil})))

(def ^:private name-regex-id-pairs-d (delay (concat @lcis/name-regex-id-pairs-d
                                                    [; LGPL name variations containing "or"
                                                     [#"(?i)\bGNU\s+General\s+Library\s+or\s+Less[eo]r\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b" "LGPL"]
                                                     [#"(?i)\bGNU\s+General\s+Less[eo]r\s+or\s+Library\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b" "LGPL"]
                                                     [#"(?i)\bGeneral\s+Library\s+or\s+Less[eo]r\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b"       "LGPL"]
                                                     [#"(?i)\bGeneral\s+Less[eo]r\s+or\s+Library\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b"       "LGPL"]
                                                     [#"(?i)\bGNU\s+Library\s+or\s+Less[eo]r\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b"           "LGPL"]
                                                     [#"(?i)\bGNU\s+Less[eo]r\s+or\s+Library\s+Public\s+Licen[cs]e(\s+\(LGPL\))?\b"           "LGPL"]
                                                     [#"(?i)\bGeneral\s+Library\s+or\s+Less[eo]r(\s+\(LGPL\))?\b"                             "LGPL"]
                                                     [#"(?i)\bGeneral\s+Less[eor]\s+or\s+Library(\s+\(LGPL\))?\b"                             "LGPL"]
                                                     [#"(?i)\bGNU\s+Library\s+or\s+Less[eo]r(\s+\(LGPL\))?\b"                                 "LGPL"]
                                                     [#"(?i)\bGNU\s+Less[eo]r\s+or\s+Library(\s+\(LGPL\))?\b"                                 "LGPL"]
                                                     [#"(?i)\b(\(GNU\)\s+)?Library\s+or\s+Less[eo]r\s+GPL\b"                                  "LGPL"]
                                                     [#"(?i)\b(\(GNU\)\s+)?Less[eo]r\s+or\s+Library\s+GPL\b"                                  "LGPL"]
                                                     ; CDDL name variations containing "and"
                                                     [#"(?i)\bCommon\s+Development\s+(and|\&)?\s+Distribution(\s+Licen[cs]e)?\b"              "CDDL"]
                                                    ])))

;####TODO: USE replace-by-with-ei TO ENSURE EXPRESSIONS INFO IS SYNTHESISED IE BY USING replace-by-with-ei
;####TODO: as part of this, have to figure out what to do about "two step" replacements (e.g. "Common Development & Distribution" -> "CDDL" -> "CDDL-1.1")
(defn replace-names-with-ids
;(defn- replace-names-with-ids
  "Replaces all listed SPDX license and exception names in `s` with their
  identifier, returning a `String`.  Returns `nil` if `s` is `nil`."
  [s]
  (if (s/blank? s)
    s
    (loop [result  s
           [f & r] @name-regex-id-pairs-d]
      (if f
        (recur (s/replace result (first f) (second f)) r)
        ; Base case - return
        result))))

(def op-re          #"\s+(and[/\-\\]or|and|&|or|w/|with|\s+)+\s+")
(def leading-op-re  (lciu/re-concat "(?i)\\A" op-re))
(def trailing-op-re (lciu/re-concat "(?i)" op-re "\\z"))

(defn- strip-leading-and-trailing-operators
  "Strips all leading and trailing operators (and, or, and/or, with) from `s`."
  [s]
  (when s
    (let [new-s (-> (str " " s " ")   ; Ensure s has leading and trailing whitespace, as op-re requires it to exist
                    (s/replace leading-op-re  "")
                    (s/replace trailing-op-re ""))]
      (when-not (s/blank? new-s)
        (s/trim new-s)))))

(def ^:private and-or-splitting-re           #"(?i)(?<=\s)and[/\-\\]or(?=\s)")
(def ^:private abbreviated-with-splitting-re #"(?i)\bw/")
(def ^:private abbreviated-and-splitting-re  #"(?i)&")
(def ^:private and-or-with-splitting-re      #"(?i)\b((and|&)(?![/\-\\]or)|(?<!(and|&)[/\-\\])or(?!\s+lat[eo]r)|with)\b")

(defn- naive-operator-split
  "Naively splits `s` (a `String`) on potential operators. Returns a sequence
  (potentially of one element) of `String`s including both the values between
  operators and the identified operators themselves. Returns `nil` when `s` is
  `nil`."
  [s]
  (when s
    (->>          (lciu/retained-split s and-or-splitting-re)
         (mapcat #(lciu/retained-split % abbreviated-with-splitting-re))
         (mapcat #(lciu/retained-split % abbreviated-and-splitting-re))
         (mapcat #(lciu/retained-split % and-or-with-splitting-re)))))

(defn- keywordise-operators
  "Turns operators in `coll` (a sequence of `String`s) into a keyword
  equivalent.  These keywords are one of:
  * :and
  * :or
  * :with

  Note: and/or 'operators' are removed from the result"
  [coll]
  (filter identity (map #(case (s/lower-case %)
                           ("and" "&")                   :and
                           "or"                          :or
                           ("with" "w/")                 :with
                           ("and/or" "and-or" "and\\or") nil
                           %)
                        coll)))

(defn- string->ids-info
  "Converts the given string (a fragment of a license name) into a **sequence**
  of singleton expressions-info maps (one per expression), ordered in the same
  order of appearance as they appear in s.

  If no listed SPDX license or exception identifiers are found in s, returns a
  sequence containing a single expressions-info map with a String started with
  \"UNIDENTIFIED-\" and with s appended. Callers are expected to turn this value
  into a lice-comb unidentified LicenseRef or AdditionRef, depending on context."
  [s]
  (when-not (s/blank? s)
    (let [s   (s/trim s)
          ids (or
                ; 1. Is it cursed?
                (when-let [cursed-name (get @cursed-names-d s)]
                  (map #(apply hash-map %) cursed-name))

                ; 2. Is it an SPDX license or exception id?
                (when-let [id (lcis/near-match-id s)]
                  (if (= id s)
                    (list {id (list {:id id :type :declared :strategy :spdx-listed-identifier-exact-match :source (list s)})})
                    (list {id (list {:id id :type :concluded :confidence :high :strategy :spdx-listed-identifier-case-insensitive-match :source (list s)})})))

                ; 3. Is it the name of one or more SPDX licenses or exceptions?
                (when-let [ids (lcis/near-match-name s)]
                  (map #(hash-map % (list {:id % :type :concluded :confidence :high :strategy :spdx-listed-name :source (list s)})) ids))

                ; 4. Might it be a URI?  (this is to handle some dumb corner cases that exist in pom.xml files hosted on Clojars & Maven Central)
                (when-let [ids (parse-uri s)]
                  (map #(hash-map (key %) (val %)) ids))

                ; 5. Attempt to detect ids in the string
                (lciid/detect-ids s)

                ; 6. No clue, so return a single info map, but with a made up "UNIDENTIFIED-" value (NOT A LICENSEREF!) instead of an SPDX license or exception identifier
                (let [id (str "UNIDENTIFIED-" s)]
                  (list {id (list {:id id :type :concluded :confidence :low :confidence-explanations [:unidentified] :strategy :unidentified :source (list s)})})))]
      (map (partial lciei/prepend-source s) ids))))

(defn- fix-unidentified
  "Fixes a singleton UNIDENTIFIED- expression info map by converting the id to
  either a lice-comb unidentified LicenseRef or AdditionRef, depending on prev.
  Returns x unchanged if it's neither an expression info map nor has an
  UNIDENTIFIED- id."
  ([x] (fix-unidentified nil x))
  ([prev x]
   (if-not (map? x)
     ; It's not an expression info map (i.e. it's an operator keyword), so let it through unchanged
     x
     ; It's a (singleton) expression info map 
     (let [id (first (keys x))]
       (if (s/starts-with? id "UNIDENTIFIED-")
         ; It's an expression info map with an unidentified id, so we have to fix it
         (let [fixed-id  (if (= :with prev)
                           (lcis/name->unidentified-addition-ref (subs id (count "UNIDENTIFIED-")))
                           (lcis/name->unidentified-license-ref  (subs id (count "UNIDENTIFIED-"))))
               v         (first (vals x))
               fixed-val (map #(if (:id %) (assoc % :id fixed-id) %) v)]
          {fixed-id fixed-val})
         ; It's a (singleton) expression info map but with an identified id, so let it through unchanged
         x)))))

(defn- fix-unidentifieds
  "Fixes all entries in sequence l that have an UNIDENTIFIED- id by converting
  those ids to either a lice-comb unidentified LicenseRef or AdditionRef,
  depending on context (i.e. whether the entry is preceeded by a :with operator
  or not)."
  [l]
  (loop [f      (take 2 l)
         r      (rest l)
         result (list (fix-unidentified (first f)))]
    (if-not (seq f)
      result
      (recur (take 2 r)
             (rest r)
             (if-let [fixed-id-with-info (fix-unidentified (first f) (second f))]
               (concat result [fixed-id-with-info])
               result)))))

(def ^:private push conj)   ; With lists-as-stacks conj == push

(defn- process-expression-element
  "Processes a single new expression element e (either a keyword representing
  an SPDX operator, or a map representing an SPDX identifier) in the context of
  stack (list) s."
  [s e]
  (if (keyword? e)
    ; e is a keyword (SPDX operator): only push a keyword if the prior element was an id, or it's different to the prior keyword
    (if (= (peek s) e)
      s
      (push s e))
    ; e is a singleton map with an SPDX identifier as a key: depending on how many keywords are currently at the top of s...
    (case (count (take-while keyword? s))
      ; No keywords? Push e onto s
      0 (push s e)

      ; One keyword? See if we should "collapse" the prior value, the keyword and e into an SPDX expression fragment and push the result onto s
      1 (let [kw        (peek s)
              operator  (s/upper-case (name kw))
              s-minus-1 (pop s)
              prior     (peek s-minus-1)
              s-minus-2 (pop s-minus-1)]
          (if (nil? prior)
            (push s-minus-2 e)       ; s had one keyword on it (which is invalid), so drop it and push e on
            (if (or (not= :with kw)  ; If the prior keyword was :and or :or, or :with and the current element is a listed exception id or AdditionRef, build an SPDX expression fragment and push the result onto s
                    (se/listed-id?    (first (keys e)))
                    (se/addition-ref? (first (keys e))))
              (let [k                (s/join " " [(first (keys prior)) operator (first (keys e))])
                    expression-infos (concat (first (vals prior)) (first (vals e)))
                    v                (distinct (concat (list {:type :concluded :confidence (lciei/calculate-confidence-for-expression expression-infos) :strategy :expression-inference})
                                                       expression-infos))]
                (push s-minus-2 {k v}))
              (push s-minus-1 e))))  ; We had a :with operator without a valid exception id following it, so simply drop the :with keyword from the stack and push the current element on

      ; Many keywords? That's invalid (since we dedupe them when they get pushed on, so this means they're different), so drop all of them and push e onto s
      (push (drop-while keyword? s) e))))

(defn- build-expressions-info-map
  "Builds an expressions-info map from the given sequence of keywords and SPDX
  expression maps."
  [l]
  (loop [result '()
         f      (first l)
         r      (rest l)]
    (if f
      (recur (process-expression-element result f) (first r) (rest r))
      (manual-fixes (into {} result)))))






)