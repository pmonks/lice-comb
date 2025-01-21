;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.spdx
  "SPDX-related functionality. Note: this namespace is not part of the public
  API of lice-comb and may change without notice."
  (:require [clojure.string           :as s]
            [embroidery.api           :as e]
            [rencg.api                :as rencg]
            [spdx.licenses            :as sl]
            [spdx.exceptions          :as se]
            [spdx.expressions         :as sexp]
            [spdx.regexes             :as sre]
            [lice-comb.impl.3rd-party :refer [by ascending descending] :as lc3]
            [lice-comb.impl.utils     :as lciu]))

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

; The subset of SPDX license identifiers that we use, as an unordered set
(def license-ids-d
  (delay
    (disj (set (filter #(not (s/ends-with? % "+")) (sl/ids)))
          "AGPL-1.0" "AGPL-3.0" "GPL-1.0" "GPL-2.0" "GPL-3.0" "LGPL-2.0" "LGPL-2.1" "LGPL-3.0"
          "GPL-2.0-with-autoconf-exception" "GPL-2.0-with-bison-exception" "GPL-2.0-with-classpath-exception"
          "GPL-2.0-with-font-exception" "GPL-2.0-with-GCC-exception" "GPL-3.0-with-autoconf-exception"
          "GPL-3.0-with-GCC-exception" "BSD-2-Clause-FreeBSD" "BSD-2-Clause-NetBSD" "bzip2-1.0.5"
          "eCos-2.0" "Net-SNMP" "StandardML-NJ" "wxWindows" )))

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

(defn- replace-version
  "Emits a suitable regex for matching the version identified in map `m`
  (a map as returned by rencg)."
  [m]
  (let [version-number     (get m "versionNumber")
        only?              (boolean (get m "only"))
        version-components (seq (s/split version-number #"\."))
        dot-zero?          (boolean (re-matches #"0+" (last version-components)))]
    (re-pattern (str "((v|ver|versions?)[\\s\\-–—]*)?"  ; Note: hyphen, en-dash, em-dash
                     "("
                     (if dot-zero?
                       (s/join "\\." (map #(str "0*" %) (drop-last version-components)))  ; Version number ends in ".0", so make the last component optional
                       (s/join "\\." (map #(str "0*" %) version-components)))             ; Version number ends in a non-zero number, so make the last component mandatory
                     "(\\.0+)*)"                                                          ; Allow any number of ".0" to appear at the end
                     (if only?
                      "[\\s\\-–—]*only"
                      "[\\s\\-–—]*(\\+|or[\\s\\-–—]*later)?")))))  ; Note: hyphen, en-dash, em-dash

(defn- replace-with-re-fragment
  "For each `String` in `coll`, replaces any matches with `re` with
  `replacement`, as per [lice-comb.impl.utils/replacing-split]."
  [coll re replacement]
  (lciu/mapcat-pred string? #(lciu/replacing-split % re replacement) coll))


; Note: some of the regexes in this namespace uses classes (e.g. [\\/-\s]{1,4}) instead of alternation (e.g. (\\|/|-|\s){1,4}) due to an apparent bug in the JVM's regex libraries when
; the latter are used in look-behind groups.  See https://stackoverflow.com/questions/24874404/java-regex-look-behind-group-does-not-have-obvious-maximum-length-error/24922107

; Only public for the unit tests
(defn id->regex
  "Turns `id`, an SPDX license or exception id, into a regex that can be used to
  near-match it.  Returns `nil` if `id` is blank."
  [id]
  (when-not (s/blank? id)
    (-> [#"(?i)(?u)(?U)(?<=(\A|\s))" (s/trim id) #"(?=(\s|\z))"]
        ; Version component
        (replace-with-re-fragment #"(?i)(?<=-)(?<versionNumber>\d+\.\d+(\.\d+)*)(-(?<only>only)|or-later)?(?=(-|\z))"
                                  replace-version)
         ; Special cases for certain licenses
        (replace-with-re-fragment #"(?i)(?<!\w)MIT(?!\w)"              #"(?<!(X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(X11|ISC))")
        (replace-with-re-fragment #"(?i)(?<!\w)X11(?!\w)"              #"(MIT[\\/\-\s]{1,4})?X11([\\/\-\s]{1,4}MIT)?")
        (replace-with-re-fragment #"(?i)(?<!\w)ISC(?!\w)"              #"(MIT[\\/\-\s]{1,4})?ISC([\\/\-\s]{1,4}MIT)?")
        (replace-with-re-fragment #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)" #"(?<!zlib/[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
        ; Character equivalents
        (replace-with-re-fragment #"[\s\-]+"                           #"[\s\-–—]+")  ; Note: hyphen, en-dash, em-dash
        ; Cleanup and combine into a single pattern
        (->> (filter #(or (not (string? %)) (not (s/blank? %))))   ; Remove empty strings
             (lciu/mapcat-pred string? #(vector (lciu/escape-re %)))
             (apply lciu/re-concat)))))

; Notes:
; * we normalise each id so that things like GPL family normalisation are correctly handled (i.e. as per clj-spdx)
; * we use all ids (including deprecated ones) because the real world may include anything
(def ^:private id-regex-id-pairs-d (delay (concat (sort (by #(count (str (first %))) descending) (map #(vec [(id->regex %) (sexp/normalise %)]) (map :id @full-license-list-d)))       ; Note: we use the license lists as they're already sorted predictably
                                                  (sort (by #(count (str (first %))) descending) (map #(vec [(id->regex %) %])                  (map :id @full-exception-list-d))))))  ; Note: can't normalise a solitary exception id since they're not a valid expression alone

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

; Only public for the unit tests
(defn name->regex
  "Turns `n`, a license or exception name, into a regex that can be used to
  near-match it.  Returns `nil` if `n` is blank."
  [n]
  (when-not (s/blank? n)
    (-> [#"(?i)(?u)(?U)(?<!\w)" (s/trim n) #"(?!\w)"]
        ; Version components (2 variants)
        (replace-with-re-fragment #"(?i)(?<=[\s\(])((v|ver|versions?)?\s*)?(?<versionNumber>\d+\.\d+(\.\d+)*)([\s\-–]+((?<only>only)|(or[\s\-]lat[eo]r)))?(?=\w?(\s|\z|\)))"
                                  replace-version)
        (replace-with-re-fragment #"(?i)(?<=[\s\(])((v|ver|versions?)?\s*)(?<versionNumber>\d+(\.\d+)*)([\s\-–]+((?<only>only)|(or[\s\-–]lat[eo]r)))?(?=\w?(\s|\z|\)))"
                                  replace-version)
        ; Alternative spellings, optional words, etc.
        (replace-with-re-fragment #"(?i)\bthe\s+"                      #"(The\s*)?")
        (replace-with-re-fragment #"(?i)(?<!\w)(and|&)(?!\w)"          #"(and|&)")
        (replace-with-re-fragment #"(?i)\s+licen[cs]e"                 #"([\s\-–—]+Licen?[cs]e)?")  ; Note: the optional missing `n` is a known misspelling in a POM license name: https://repo.clojars.org/net/unit8/excelebration/excelebration/0.2.0/excelebration-0.2.0.pom
        (replace-with-re-fragment #"(?i)\s+public\b"                   #"([\s\-–—]+Public)?")
        (replace-with-re-fragment #"(?i)\backnowledge?ment"            #"Acknowledge?ment")  ; No trailing \b, to handle plurals etc.
        (replace-with-re-fragment #"(?i)\bmerchant[ai]bility\b"        #"Merchant[ai]bility")
        (replace-with-re-fragment #"(?i)\bnon-?commercial\b"           #"Non[-–—]?commercial")  ; Note: hyphen, en-dash, em-dash
        (replace-with-re-fragment #"(?i)\bf(u|\\\*)ck"                 #"[f*][u*][c*][k*]")  ; As of SPDX license list v3.25.0, profane names use "F*ck", but we hedge here in case that changes in other versions
        (replace-with-re-fragment #"(?i)\bopen\s+source"               #"(Open[\s\-–—]+Source|OSS|FOSS)")
         ; Special cases for certain licenses
        (replace-with-re-fragment #"(?i)(?<!\w)Apache(?!\w)"           #"Apache([\s\-–—]+Software)?")
        (replace-with-re-fragment #"(?i)(?<!\w)MIT(?!\w)"              #"(?<!(X11|ISC)[\\/\-\s]{1,4})MIT(?![\\/\-\s]{1,4}(X11|ISC))")
        (replace-with-re-fragment #"(?i)(?<!\w)X11(?!\w)"              #"(MIT[\\/\-\s]{1,4})?X11([\\/\-\s]{1,4}MIT)?")
        (replace-with-re-fragment #"(?i)(?<!\w)ISC(?!\w)"              #"(MIT[\\/\-\s]{1,4})?ISC([\\/\-\s]{1,4}MIT)?")
        (replace-with-re-fragment #"(?i)(?<!\w)(?<!zlib/)libpng(?!\w)" #"(?<!zlib/[\\/\-\s]{1,4})libpng(?![\\/\-\s]{1,4}zlib)")
        ; Character equivalents
        (replace-with-re-fragment #"(?i)é"                             #"[ée]")  ; As of License List v3.26.0 'é' is the only accented character present
        (replace-with-re-fragment #"\""                                #"[\"“”„‟'‘’‚‛`]")
        (replace-with-re-fragment #"\s*/\s*"                           #"\s*[\\/\-–—]\s*")  ; Note: hyphen, en-dash, em-dash
        (replace-with-re-fragment #"[\s\-–]+"                          #"[\s\-–—]+")  ; Note: hyphen, en-dash, em-dash. en-dash is in e.g. the name of LiLiQ-R-1.1
        ; Cleanup and combine into a single pattern
        (->> (filter #(or (not (string? %)) (not (s/blank? %))))   ; Remove empty strings
             (lciu/mapcat-pred string? #(vector (lciu/escape-re %)))
             (apply lciu/re-concat)))))

;####TODO: CONSIDER MOVING TO lice-comb.impl.parsing!!  THIS WOULD MEAN REMOVING THE VARIOUS FNS HERE THAT USE THIS STRUCTURE!!!
; Notes:
; * we normalise each id so that things like GPL family normalisation are correctly handled (i.e. as per clj-spdx)
; * we use all ids (including deprecated ones) because the real world may include anything
; * we preserve the listed name of the license in the tuple so that we can determine the precise matching strategy
(def name-regex-id-pairs-d (delay (concat (sort (by #(count (str (first %))) descending) (map #(vec [(name->regex (:name %)) (sexp/normalise (:id %)) (:name %)]) @full-license-list-d))
                                          (sort (by #(count (str (first %))) descending) (map #(vec [(name->regex (:name %)) (:id %)                  (:name %)]) @full-exception-list-d)))))  ; Note: can't normalise a solitary exception id since they're not a valid expression alone

;####TODO: REMOVE IF UNNEEDED!!!!
(comment
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
)

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
  "Is `id` one of lice-comb's custom LicenseRefs?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case lice-comb-license-ref-prefix))))

(defn lice-comb-addition-ref?
  "Is `id` one of lice-comb's custom AdditionRefs?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case lice-comb-addition-ref-prefix))))

(defn lice-comb-ref?
  "Is `id` a lice-comb custom LicenseRef or AdditionRef"
  [id]
  (or (lice-comb-license-ref?  id)
      (lice-comb-addition-ref? id)))

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
  "Is `id` a lice-comb custom 'unidentified' LicenseRef?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case unidentified-license-ref-prefix))))

(defn unidentified-addition-ref?
  "Is `id` a lice-comb custom 'unidentified' AdditionRef?"
  [id]
  (when id
    (s/starts-with? (s/lower-case id) (s/lower-case unidentified-addition-ref-prefix))))

(defn unidentified?
  "Is `id` a lice-comb custom 'unidentified' LicenseRef or AdditionRef?"
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

(defn find-ids
  "Returns a sequence of the distinct listed SPDX license ids, exceptions ids,
  LicenseRefs and AdditionRefs found in `s` (a `String`), in the order they were
  found, or `nil` if no listed ids were found or `s` was `nil`.

  Note: results are NOT normalised."
  [s]
  (when s
    (when-let [matches (map #(get % "Identifier") (rencg/re-seq-ncg (sre/ids-re) s))]
      (seq (distinct matches)))))

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
