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
            [spdx.identifiers         :as si]
            [spdx.licenses            :as sl]
            [spdx.exceptions          :as se]
            [spdx.expressions         :as sexp]
            [lice-comb.impl.families  :as lcif]
            [lice-comb.impl.3rd-party :refer [by ascending descending] :as lc3]
            [lice-comb.impl.utils     :as lciu]))

; The subset of SPDX license identifiers that we use, as an unordered set
(def license-ids-d (delay (sl/ids)))

; The subset of SPDX exception identifiers that we use, as an unordered set
(def exception-ids-d (delay (se/ids)))

(defn id-position
  "Returns the 'position' (expressed as `:license-position` or
  `:exception-position`) of `id` (a license, LicenseRef, exception, or
  AdditionRef) in an SPDX expression, or `nil` if it's none of those things."
  [^String id]
  (when-let [type (si/id-type id)]
    (case type
      :license-id   :license-position
      :license-ref  :license-position
      :exception-id :exception-position
      :addition-ref :exception-position)))

(defn id->info
  "Returns the associated SPDX list info for `id`, which can be either a license
  identifier or an exception identifier, or `nil` if `id` is not a valid SPDX
  listed identifier.

  Notes:

  * does _not_ include large text values in the result
  * assocs a `:type` element with either the value `:license` or the value
    `:exception`"
  [^String id]
  (if-let [license-entry (sl/id->info id {:include-large-text-values? false})]
    (assoc license-entry :type :license)
    (when-let [exception-entry (se/id->info id {:include-large-text-values? false})]
      (assoc exception-entry :type :exception))))

(defn- sort-id-infos
  "Sorts the given id info maps according to lice-comb's preferred sort order:

  1. non-deprecated before deprecated
  2. licenses before exceptions
  3. 'families' of identifiers grouped together
  4. longer names before shorter names, based on the newest identifier in each
     family
  5. by id (newest version first)"
  [id-infos]
  (let [; Split non-deprecated and deprecated, then identify families in each
        non-deprecated-families (lcif/id-infos->families false (filter (complement :deprecated?) id-infos))
        deprecated-families     (lcif/id-infos->families false (filter :deprecated? id-infos))
        ; Sorter function
        sorter                  (by #(:type (last (val %)))         descending  ; licenses first, exceptions second
                                    #(count (:name (last (val %)))) descending
                                    #(:id (last (val %)))           ascending)]
    (concat
      (mapcat #(reverse (second %)) (sort sorter non-deprecated-families))
      (mapcat #(reverse (second %)) (sort sorter deprecated-families)))))

(defn- sort-ids->id-infos
  "Sorts the given SPDX listed ids according to lice-comb's preferred sort
  order, returning a sequence of id info maps for each one. The sort order is:

  1. non-deprecated before deprecated
  2. licenses before exceptions
  3. 'families' of identifiers grouped together
  4. longer names before shorter names, based on the newest identifier in each
     family
  5. by id (newest version first)"
  [ids]
  (sort-id-infos (map id->info ids)))

(defn sort-ids
  "Sorts the given SPDX listed ids according to lice-comb's preferred sort
  order:

  1. non-deprecated before deprecated
  2. licenses before exceptions
  3. 'families' of identifiers grouped together
  4. longer names before shorter names, based on the newest identifier in each
     family
  5. by id (newest version first)"
  [ids]
  (map :id (sort-ids->id-infos ids)))

; The license and exception lists, in the order they should be processed
(def license-list-d   (delay (sort-ids->id-infos @license-ids-d)))
(def exception-list-d (delay (sort-ids->id-infos @exception-ids-d)))

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
  non-deprecated id (if any), or (if not) the shortest deprecated id."
  [ids]
  (if (<= (count ids) 1)
    (first ids)
    (if-let [non-deprecated-ids (seq (filter #(not (or (sl/deprecated-id? %) (se/deprecated-id? %))) ids))]
      (first (sort-by count non-deprecated-ids))
      (first (sort-by count ids)))))

(defn- urls->id-tuples
  "Extracts all urls for a given list (license or exception) entry."
  [list-entry]
  (let [id              (:id list-entry)
        simplified-uris (map lciu/simplify-uri (filter (complement s/blank?) (concat (:see-also list-entry) (get-in list-entry [:cross-refs :url]))))]
    (map #(vec [% id]) simplified-uris)))

(def ^:private index-uri->id-d (delay (merge (lciu/mapfonv #(lciu/nset (map second %)) (group-by first (mapcat urls->id-tuples @license-list-d)))
                                             (lciu/mapfonv #(lciu/nset (map second %)) (group-by first (mapcat urls->id-tuples @exception-list-d))))))

(defn near-match-uri
  "Returns the id(s) (a set) for the given listed `uri`, or `nil` if no ids were
  found. The result may include deprecated ids."
  [uri]
  (get @index-uri->id-d (lciu/simplify-uri uri)))

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
  @license-list-d
  @exception-list-d
  @spdx-ids-d
  @index-uri->id-d
  nil)
