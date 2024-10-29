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

(ns lice-comb.impl.spdx
  "SPDX-related functionality. Note: this namespace is not part of the public
  API of lice-comb and may change without notice."
  (:require [clojure.string            :as s]
            [embroidery.api            :as e]
            [spdx.licenses             :as sl]
            [spdx.exceptions           :as se]
            [spdx.expressions          :as sexp]
            [lice-comb.impl.sort-utils :refer [by ascending descending]]
            [lice-comb.impl.utils      :as lciu]))

; The subset of SPDX license identifiers that we use, as an unordered set
(def license-ids-d
  (delay
    (disj (set (filter #(not (s/ends-with? % "+")) (sl/ids)))
          "AGPL-1.0" "AGPL-3.0" "GPL-1.0" "GPL-2.0" "GPL-3.0" "LGPL-2.0" "LGPL-2.1" "LGPL-3.0"
          "GPL-2.0-with-autoconf-exception" "GPL-2.0-with-bison-exception" "GPL-2.0-with-classpath-exception"
          "GPL-2.0-with-font-exception" "GPL-2.0-with-GCC-exception" "GPL-3.0-with-autoconf-exception"
          "GPL-3.0-with-GCC-exception" "StandardML-NJ")))

; The subset of SPDX exception identifiers that we use, as an unordered set
(def exception-ids-d
  (delay
    (disj (se/ids)
          "Nokia-Qt-exception-1.1")))

; The license and exception lists (both the full things, and the subsets we use)
; NOTES:
; * large elements (license texts and their variants) are removed, to reduce memory consumption
; * deprecated licenses are sorted last so that they're processed last in any kind of serial processing (e.g. in replace-near-match-ids-with-id & replace-near-match-names-with-id)
; * non-deprecated licenses are sorted longest to shortest so that the least likely match is attempted first in the event of a double match
(def full-license-list-d   (delay (sort (by :deprecated? ascending #(count (:name %)) descending :id ascending) (map #(sl/id->info % {:include-large-text-values? false}) (sl/ids)))))
(def full-exception-list-d (delay (sort (by :deprecated? ascending #(count (:name %)) descending :id ascending) (map #(se/id->info % {:include-large-text-values? false}) (se/ids)))))
(def license-list-d        (delay (sort (by :deprecated? ascending #(count (:name %)) descending :id ascending) (map #(sl/id->info % {:include-large-text-values? false}) @license-ids-d))))
(def exception-list-d      (delay (sort (by :deprecated? ascending #(count (:name %)) descending :id ascending) (map #(se/id->info % {:include-large-text-values? false}) @exception-ids-d))))

; The license refs lice-comb uses (note: the unidentified one usually has a hyphen then a base62 suffix appended)
(def ^:private lice-comb-license-ref-prefix       "LicenseRef-lice-comb")
(def ^:private public-domain-license-ref          (str lice-comb-license-ref-prefix "-PUBLIC-DOMAIN"))
(def ^:private proprietary-commercial-license-ref (str lice-comb-license-ref-prefix "-PROPRIETARY-COMMERCIAL"))
(def ^:private unidentified-license-ref-prefix    (str lice-comb-license-ref-prefix "-UNIDENTIFIED"))

; The addition refs lice-comb uses (note: the unidentified one usually has a hyphen then a base62 suffix appended)
(def ^:private lice-comb-addition-ref-prefix      "AdditionRef-lice-comb")
(def ^:private unidentified-addition-ref-prefix   (str lice-comb-addition-ref-prefix "-UNIDENTIFIED"))

; Map of lower case SPDX id to correctly cased SPDX id
(def ^:private spdx-ids-d (delay (merge (into {} (map #(vec [(s/lower-case %) %]) @license-ids-d))
                                        (into {} (map #(vec [(s/lower-case %) %]) @exception-ids-d)))))

(defn case-insensitive-match-id
  "Returns the (case-corrected) id for the given license or exception id `id`,
  or `nil` if one wasn't found."
  [id]
  (get @spdx-ids-d (s/lower-case id)))

(defn- best-identifier
  "Finds the 'best' identifier in `ids`, a `set` of license or exceptions
  identifiers, `nil` if `ids` is empty. 'Best' is defined as the shortest
  non-deprecated id (if any), or (worst case) the shortest deprecated id."
  [ids]
  (if (<= (count ids) 1)
    (first ids)
    (if-let [non-deprecated-ids (seq (filter #(not (or (sl/deprecated-id? %) (se/deprecated-id? %))) ids))]
      (first (sort-by count non-deprecated-ids))
      (first (sort-by count ids)))))

(defn- id-version-replacement
  "Returns a regex fragment for a version number (e.g. 2.0.0) in an id."
  [[_ version]]
  (let [version-components (s/split version #"\.")]
    (str "-((v|ver|version)-)?"                                  ; Note: - character not escaped because that happens later
         (s/join "." (map #(str "0*" %) version-components)))))  ; Note: . character not escaped because that happens later

; Only public for the unit tests
(defn id->regex
  "Turns `id`, an SPDX license or exception id, into a regex that can be used to
  near-match it.  Returns `nil` if `id` is blank."
  [id]
  (when-not (s/blank? id)
    (-> id
        ; Trim
        s/trim
        ; Add flags and start expressions
        (->> (str "(?i)((?<=\\s)|\\A)"))
        ; Replacements
        (s/replace "+"               "\\+")                                 ; escape + character
        (s/replace #"-(\d+(\.\d+)*)" id-version-replacement)                ; version numbers
        (s/replace #"-"              "[\\\\-\\\\s]*")                       ; hyphens as optional hyphens or whitespace
        (s/replace #"(?i)\blater\b"  "\\\\b(lat[eo]r|newer|greater)\\\\b")  ; alternative "or later" formulations
        (s/replace #"\."             "\\\\.")                               ; escape . character
        ; Add end expressions and remove redundant word boundary matches
        (str "((?=\\s)|\\z)")
        (s/replace #"(\\b)+"         "\\\\b")
        ; And finally turn into a Pattern object
        re-pattern)))

; Notes:
; * we normalise each id so that things like GPL family normalisation are correctly handled (i.e. as per clj-spdx)
; * we use all ids (including deprecated ones) because the real world may include anything
(def ^:private id-regex-id-pairs-d (delay (concat (map #(vec [(id->regex %) (sexp/normalise %)]) (map :id @full-license-list-d))       ; Note: we use the license lists as they're already sorted predictably
                                                  (map #(vec [(id->regex %) %])                  (map :id @full-exception-list-d)))))  ; Note: can't normalise a solitary exception id since they're not a valid expression alone

(defn near-match-id
  "Returns the id(s) (a set) when `s` 'near matches' one or more license or
  exception identifiers, or `nil` if `n` is blank or no near matches were found.
  The result may include deprecated ids."
  [s]
  (when-not (s/blank? s)
    (let [n (s/trim s)]
      (some-> (seq (filter identity (map #(when (re-matches (first %) n) (second %)) @id-regex-id-pairs-d)))
                   set))))

(defn best-near-match-id
  "Returns the 'best match' id (a `String`) for `s`, or `nil` if no ids were
  found.  A 'best match' is defined as the shortest non-deprecated id (if any),
  or (worst case) the shortest deprecated id."
  [s]
  (best-identifier (near-match-id s)))

(defn replace-near-match-ids-with-id
  "Replaces all near matched ids in `s` (a `String`) with their actual (best)
  SPDX id. Result is a tuple containing the modified `s` and a sequence of
  tuples describing the replacements that were performed."
  [s]
  (when s
    (loop [s            s
           replacements []
           [f & r]      @id-regex-id-pairs-d]
      (if-not f
        [s replacements]
        (let [[re id]             f
              [new-s replacement] (lciu/explaining-replace s re id)
              replacement         (seq (filter #(not= (first %) (second %)) replacement))  ; Remove redundant replacements such as ["GPL-2.0-only" "GPL-2.0-only"]
              new-replacements    (if replacement (apply conj replacements replacement) replacements)]
          (recur new-s new-replacements r))))))

(defn- name-version-replacement
  "Returns a regex fragment for a version number (e.g. 2.0.0) in a name."
  [[_ prefix version]]
  (let [license-str        (when (s/starts-with? (s/lower-case prefix) "lic") "License")
        version-components (s/split version #"\\\.")]
    (str license-str " ((v|ver|version)[\\-\\s]*)?"  ; Note: whitespace not escaped because that happens later
         (s/join "\\." (map #(str "0*" %) version-components)))))

; Only public for the unit tests
(defn name->regex
  "Turns `n`, a license or exception name, into a regex that can be used to
  near-match it.  Returns `nil` if `n` is blank."
  [n]
  (when-not (s/blank? n)
    (-> n
        ; Trim & escape
        s/trim
        lciu/escape-re
        ; Start clauses
        (->> (str "(?i)(\\A|\\b)"))  ; Note: technically the \A is redundant (since \b also matches "starts of input"), but it's retained here for clarity with the end fragment - see comment below
        ; "Version" variations (this must come first)
        (s/replace #"(?i)(Licen[cs]e\s|version\s|v)\s*(\d+(\\\.\d+)*)" name-version-replacement)  ; Note: have to match escaped . here
        ; SPDX equivalent words (subset of https://spdx.org/licenses/equivalentwords.txt that have been found in a license or exception name as of SPDX license list 3.25.0)
        (s/replace #"(?i)\blicen[cs]"                                  "Licen[cs]")
        (s/replace #"(?i)\backnowledge?ment"                           "Acknowledge?ment")
        (s/replace #"(?i)\b(and|&)\b"                                  "(and|&)")
        (s/replace #"(?i)\bmerchant[ai]bility\b"                       "Merchant[ai]bility")
        (s/replace #"(?i)\bnon(\\\-)?commercial\b"                     "Non(\\\\-)?commercial")    ; Note: weird syntax in find regex as hyphens have already been escaped
        ; Special cases
        (s/replace #"(?i)\bApache\b"                                   "Apache(\\\\s+Software)?")
        (s/replace #"(?i)\s+Public\b"                                  "(\\\\s+Public)?")
        ; Whitespace variance
        (s/replace #"\s+"                                              "\\\\s+")
        ; End clauses
        (str "(\\b|\\z)")  ; Because \b doesn't match "end of input" for some bizarre reason (even though it *does* match start of input!  🙄)
        ; Remove redundant word boundary matches
        (s/replace "\\s+\\b"                                           "\\s+")
        (s/replace "\\b\\s+"                                           "\\s+")
        ; And finally compile the regex
        re-pattern)))

; Notes:
; * we normalise each id so that things like GPL family normalisation are correctly handled (i.e. as per clj-spdx)
; * we use all ids (including deprecated ones) because the real world may include anything
(def ^:private name-regex-id-pairs-d (delay (concat (map #(vec [(name->regex (:name %)) (sexp/normalise (:id %))]) @full-license-list-d)
                                                    (map #(vec [(name->regex (:name %)) %])                        @full-exception-list-d))))  ; Note: can't normalise a solitary exception id since they're not a valid expression alone

(defn near-match-name
  "Returns the id(s) (a set) when `n`ame 'near matches' one or more license or
  exception names, or `nil` if `n` is blank or no near matches were found. The
  result may include deprecated ids."
  [n]
  (when-not (s/blank? n)
    (let [n (s/trim n)]
      (some-> (seq (filter identity (map #(when (re-matches (first %) n) (second %)) @name-regex-id-pairs-d)))
                   set))))

(defn best-near-match-name
  "Returns the 'best match' id (a `String`) for `n`ame, or `nil` if no ids were
  found.  A 'best match' is defined as the shortest non-deprecated id (if any),
  or (worst case) the shortest deprecated id."
  [n]
  (best-identifier (near-match-name n)))

(defn replace-near-match-names-with-id
  "Replaces all near matched names in `s` (a `String`) with their actual (best)
  SPDX id.  Result is a tuple containing the modified `s` and a sequence of
  explanation tuples as returned by [[lice-comb.impl.utils/explaining-replace]]."
  [s]
  (when s
    (loop [s            s
           replacements []
           [f & r]      @name-regex-id-pairs-d]
      (if-not f
        [s replacements]
        (let [[re id]             f
              [new-s replacement] (lciu/explaining-replace s re id)
              new-replacements    (if replacement (apply conj replacements replacement) replacements)]
          (recur new-s new-replacements r))))))

(defn- urls-to-id-tuples
  "Extracts all urls for a given list (license or exception) entry."
  [list-entry]
  (let [id              (:id list-entry)
        simplified-uris (map lciu/simplify-uri (filter (complement s/blank?) (concat (:see-also list-entry) (get-in list-entry [:cross-refs :url]))))]
    (map #(vec [% id]) simplified-uris)))

(def ^:private index-uri-to-id-d (delay (merge (lciu/mapfonv #(lciu/nset (map second %)) (group-by first (mapcat urls-to-id-tuples @full-license-list-d)))
                                               (lciu/mapfonv #(lciu/nset (map second %)) (group-by first (mapcat urls-to-id-tuples @full-exception-list-d))))))

(defn near-match-uri
  "Returns the id(s) (a set) for the given listed `uri`, or `nil` if no ids were
  found. The result may include deprecated ids."
  [uri]
  (get @index-uri-to-id-d (lciu/simplify-uri uri)))

; This is needed for pathological cases like "http://gnu.org/license/fdl-1.3" (which has 7 (!) ids associated with it)
(defn best-near-match-uri
  "Returns the 'best match' id (a `String`) for `uri`, or `nil` if no ids were
  found.  A 'best match' is defined as the shortest non-deprecated id (if any),
  or (worst case) the shortest deprecated id."
  [uri]
  (best-identifier (near-match-uri uri)))

(defn lice-comb-license-ref?
  "Is the given id one of lice-comb's custom LicenseRefs?"
  [id]
  (s/starts-with? (s/lower-case id) (s/lower-case lice-comb-license-ref-prefix)))

(defn lice-comb-addition-ref?
  "Is the given id one of lice-comb's custom AdditionRefs?"
  [id]
  (s/starts-with? (s/lower-case id) (s/lower-case lice-comb-addition-ref-prefix)))

(defn public-domain?
  "Is the given id lice-comb's custom 'public domain' LicenseRef?"
  [id]
  (= (s/lower-case id) (s/lower-case public-domain-license-ref)))

(def ^{:doc "Constructs a valid SPDX id (a LicenseRef specific to lice-comb)
  representing public domain."
       :arglists '([])}
  public-domain
  (constantly public-domain-license-ref))

(defn proprietary-commercial?
  "Is the given id lice-comb's custom 'proprietary / commercial' LicenseRef?"
  [id]
  (when id
    (= (s/lower-case id) (s/lower-case proprietary-commercial-license-ref))))

(def ^{:doc "Constructs a valid SPDX id (a LicenseRef specific to lice-comb)
  representing a proprietary / commercial license."
       :arglists '([])}
  proprietary-commercial
  (constantly proprietary-commercial-license-ref))

(defn unidentified-license-ref?
  "Is the given id a lice-comb custom 'unidentified' LicenseRef?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case unidentified-license-ref-prefix))))

(defn unidentified-addition-ref?
  "Is the given id a lice-comb custom 'unidentified' AdditionRef?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case unidentified-addition-ref-prefix))))

(defn unidentified?
  "Is the given id a lice-comb custom 'unidentified' LicenseRef or AdditionRef?"
  [id]
  (or (unidentified-license-ref? id) (unidentified-addition-ref? id)))

(defn name->unidentified-license-ref
  "Constructs a valid SPDX id (a LicenseRef specific to lice-comb) for an
  unidentified license, with the given name (if provided) appended as Base62
  (since clj-spdx identifiers are limited to a small superset of Base62)."
  ([] (name->unidentified-license-ref nil))
  ([name]
   (str unidentified-license-ref-prefix (when-not (s/blank? name) (str "-" (lciu/base62-encode name))))))

(defn name->unidentified-addition-ref
  "Constructs a valid SPDX id (an AdditionRef specific to lice-comb) for an
  unidentified license exception, with the given name (if provided) appended as
  Base62 (since clj-spdx identifiers are limited to a small superset of Base62)."
  ([] (name->unidentified-addition-ref nil))
  ([name]
   (str unidentified-addition-ref-prefix (when-not (s/blank? name) (str "-" (lciu/base62-encode name))))))

(defn unidentified-license-ref->name
  "Get the original name of the given unidentified license ref. Returns nil if
  id is nil or is not a lice-comb unidentified LicenseRef."
  [id]
  (when (unidentified-license-ref? id)
    (if (> (count id) (count unidentified-license-ref-prefix))
      (lciu/base62-decode (subs id (inc (count unidentified-license-ref-prefix))))
      "")))

(defn unidentified-addition-ref->name
  "Get the original name of the given unidentified addition ref. Returns nil if
  id is nil or is not a lice-comb unidentified AdditionRef."
  [id]
  (when (unidentified-addition-ref? id)
    (if (> (count id) (count unidentified-license-ref-prefix))
      (lciu/base62-decode (subs id (inc (count unidentified-license-ref-prefix))))
      "")))

(defn unidentified->name
  "Get the original name of the given unidentified license ref or addition ref.
  Returns nil if id is nil or is not a lice-comb unidentified LicenseRef or
  AdditionRef."
  [id]
  (cond
    (unidentified-license-ref?  id) (unidentified-license-ref->name id)
    (unidentified-addition-ref? id) (unidentified-addition-ref->name id)))

(defn unidentified-license-ref->human-readable-name
  "Returns the string 'Unidentified' with the original name of the given
  unidentified license in parens. Returns nil if id is nil or is not a
  lice-comb unidentified LicenseRef."
  [id]
  (when (unidentified-license-ref? id)
    (let [original-name (unidentified->name id)]
      (str "Unidentified (\""
           (if (s/blank? original-name)
             "-original name not available-"
             (s/trim original-name))
           "\")"))))

(defn unidentified-addition-ref->human-readable-name
  "Returns the string 'Unidentified' with the original name of the given
  unidentified license exception in parens. Returns nil if id is nil or
  is not a lice-comb unidentified AdditionRef."
  [id]
  (when (unidentified-addition-ref? id)
    (let [original-name (unidentified->name id)]
      (str "Unidentified (\""
           (if (s/blank? original-name)
             "-original name not available-"
             (s/trim original-name))
           "\")"))))

(defn unidentified->human-readable-name
  "Returns the string 'Unidentified' with the original name of the given
  unidentified license or license exception in parens. Returns nil if id is nil
  or is not a lice-comb unidentified LicenseRef or AdditionRef."
  [id]
  (cond
    (unidentified-license-ref?  id) (unidentified-license-ref->human-readable-name id)
    (unidentified-addition-ref? id) (unidentified-addition-ref->human-readable-name id)))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  ; Parallelise initialisation of the spdx.licenses and spdx.exceptions namespaces, as they're both sloooooooow (can take over a minute)
  (let [sl-init (e/future* (sl/init!))
        se-init (e/future* (se/init!))]
    @sl-init
    @se-init)
  (sexp/init!)

  ; Serially initialise this namespace's dependent state - they're all pretty fast (< 1s)
  @license-ids-d
  @exception-ids-d
  @full-license-list-d
  @full-exception-list-d
  @license-list-d
  @exception-list-d
  @spdx-ids-d
  @name-regex-id-pairs-d
  @index-uri-to-id-d
  nil)
