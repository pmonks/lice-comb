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
  (:require [clojure.string       :as s]
            [embroidery.api       :as e]
            [spdx.identifiers     :as si]
            [spdx.licenses        :as sl]
            [spdx.exceptions      :as se]
            [spdx.expressions     :as sexp]
            [lice-comb.impl.utils :as u]))

; The subset of SPDX identifiers that we use, as an unordered set
(def license-ids-d   (delay (some->> (sl/ids)
                                     (filter #(not (s/ends-with? % "+")))                                         ; Remove deprecated "xGPL-y.z+" identifiers
                                     (filter #(not (re-matches #"\AGPL-\d\.0-with-[\p{Alpha}]+-exception\z" %)))  ; Remove deprecated "GPL-x.0-with-xxx-exception" identifiers
                                     seq
                                     set)))
(def exception-ids-d (delay (set (se/ids))))
(def ids-d           (delay (set (concat @license-ids-d @exception-ids-d))))

;####TODO: IS THIS BETTER IN clj-spdx?
(defn id-position
  "Returns the 'position' (expressed as `:license-position` or
  `:exception-position`) of `id` (a license, LicenseRef, exception, AdditionRef,
  or special form) in an SPDX expression, or `nil` if it's none of those things."
  [^CharSequence id]
  (when-let [type (si/id-type id)]
    (case type
      (:license-id :license-ref :special-form) :license-position
      (:exception-id :addition-ref)            :exception-position)))

(defn canonicalise-spdx-expression-fragment
  "Canonicalises `spdx-expression-fragment`, or returns `nil` if it cannot be
  canonicalised (i.e. is not an SPDX expression, listed SPDX identifier, a valid
  ref, or a special form)."
  [^CharSequence spdx-expression-fragment]
  (when-not (s/blank? spdx-expression-fragment)
    (if-let [canonical-expression (sexp/canonicalise spdx-expression-fragment)]  ; First, attempt expression canonicalisation (which handles more cases than identifer canonicalisation)
      canonical-expression
      (si/canonicalise spdx-expression-fragment))))                              ; But if that doesn't work, fall back on identifier canonicalisation

; Custom license and addition refs lice-comb uses (note: the unidentified one usually has a hyphen then a base62 suffix appended)
(def ^:private lice-comb-prefix                   "lice-comb")

(def ^:private public-domain-license-ref          (sl/license-ref (str lice-comb-prefix "-PUBLIC-DOMAIN")))
(def ^:private proprietary-commercial-license-ref (sl/license-ref (str lice-comb-prefix "-PROPRIETARY-COMMERCIAL")))
(def ^:private hippocratic-30-license-ref         (sl/license-ref (str lice-comb-prefix "-Hippocratic-3.0")))  ; Only needed until https://github.com/spdx/license-list-XML/issues/2931 is resolved
(def ^:private unidentified-ref-prefix            (str lice-comb-prefix "-UNIDENTIFIED"))

(defn lice-comb-license-ref?
  "Is `id` one of lice-comb's custom LicenseRefs?"
  [^CharSequence id]
  (boolean
    (when-let [m (sl/string->license-ref-map id)]
      (s/starts-with? (:license-ref m) lice-comb-prefix))))

(defn lice-comb-addition-ref?
  "Is `id` one of lice-comb's custom AdditionRefs?"
  [^CharSequence id]
  (boolean
    (when-let [m (se/string->addition-ref-map id)]
      (s/starts-with? (:addition-ref m) lice-comb-prefix))))

(defn lice-comb-ref?
  "Is `id` a lice-comb custom LicenseRef or AdditionRef"
  [^CharSequence id]
  (or (lice-comb-license-ref?  id)
      (lice-comb-addition-ref? id)))

(def ^{:doc "Is the given id lice-comb's custom 'public domain' LicenseRef?"
       :arglists '([id])}
   public-domain?
   (partial sl/equivalent? public-domain-license-ref))

(def ^{:doc "Constructs a lice-comb specific LicenseRef representing public domain."
       :arglists '([])}
  public-domain
  (constantly public-domain-license-ref))


(def ^{:doc "Is the given id lice-comb's custom 'proprietary / commercial' LicenseRef?"
       :arglists '([id])}
   proprietary-commercial?
   (partial sl/equivalent? proprietary-commercial-license-ref))

(def ^{:doc "Constructs a lice-comb specific LicenseRef representing a proprietary / commercial license."
       :arglists '([])}
  proprietary-commercial
  (constantly proprietary-commercial-license-ref))

(def ^{:doc "Constructs a lice-comb specific LicenseRef representing the Hippocratic-3.0 license."
       :arglists '([])}
  hippocratic-30
  (constantly hippocratic-30-license-ref))

(defn unidentified-license-ref?
  "Is `id` a lice-comb custom 'unidentified' LicenseRef?"
  [^CharSequence id]
  (boolean
    (when-let [m (sl/string->license-ref-map id)]
      (s/starts-with? (:license-ref m) unidentified-ref-prefix))))

(defn unidentified-addition-ref?
  "Is `id` a lice-comb custom 'unidentified' AdditionRef?"
  [^CharSequence id]
  (boolean
    (when-let [m (se/string->addition-ref-map id)]
      (s/starts-with? (:addition-ref m) unidentified-ref-prefix))))

(defn unidentified?
  "Is `id` a lice-comb custom 'unidentified' LicenseRef or AdditionRef?"
  [^CharSequence id]
  (or (unidentified-license-ref? id)
      (unidentified-addition-ref? id)))

(defn name->unidentified-license-ref
  "Constructs a valid SPDX id (a LicenseRef specific to lice-comb) for an
  unidentified license, with the given name (if provided) appended as Base62
  (since the variable tag in an SPDX ref is limited to Base62)."
  ([] (name->unidentified-license-ref nil))
  ([^CharSequence n]
   (sl/license-ref (str unidentified-ref-prefix (when-not (s/blank? n)
                                                  (str "-" (u/base62-encode n)))))))

(defn name->unidentified-addition-ref
  "Constructs a valid SPDX id (an AdditionRef specific to lice-comb) for an
  unidentified license exception, with the given name (if provided) appended as
  (since the variable tag in an SPDX ref is limited to Base62)."
  ([] (name->unidentified-addition-ref nil))
  ([^CharSequence n]
   (se/addition-ref (str unidentified-ref-prefix (when-not (s/blank? n)
                                                   (str "-" (u/base62-encode n)))))))

(defn unidentified-license-ref->name
  "Get the original name of the given unidentified license ref. Returns nil if
  id is nil or is not a lice-comb unidentified LicenseRef, or if the
  unidentified LicenseRef did not have a name."
  [^CharSequence id]
  (when (unidentified-license-ref? id)
    (let [m   (sl/string->license-ref-map id)
          tag (:license-ref m)]
      (when (s/starts-with? tag (str unidentified-ref-prefix "-"))
        (u/base62-decode (subs id (inc (count unidentified-ref-prefix))))))))

(defn unidentified-addition-ref->name
  "Get the original name of the given unidentified addition ref. Returns nil if
  id is nil or is not a lice-comb unidentified AdditionRef, or if the
  unidentified AdditionRef did not have a name."
  [^CharSequence id]
  (when (unidentified-addition-ref? id)
    (let [m   (se/string->addition-ref-map id)
          tag (:addition-ref m)]
      (when (s/starts-with? tag (str unidentified-ref-prefix "-"))
        (u/base62-decode (subs id (inc (count unidentified-ref-prefix))))))))

(defn unidentified->name
  "Get the original name of the given unidentified license ref or addition ref.
  Returns nil if id is nil or is not a lice-comb unidentified Ref, or if the
  Ref did not have a name."
  [^CharSequence id]
  (cond
    (unidentified-license-ref?  id) (unidentified-license-ref->name id)
    (unidentified-addition-ref? id) (unidentified-addition-ref->name id)))

(defn unidentified-license-ref->human-readable-name
  "Returns the string 'Unidentified' with the original name of the given
  unidentified license in parens. Returns nil if id is nil or is not a
  lice-comb unidentified LicenseRef."
  [^CharSequence id]
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
  [^CharSequence id]
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
  [^CharSequence id]
  (cond
    (unidentified-license-ref?  id) (unidentified-license-ref->human-readable-name id)
    (unidentified-addition-ref? id) (unidentified-addition-ref->human-readable-name id)))

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

;####TODO: MOVE THIS TO lice-comb.impl.license-detection.uris
(defn- urls->id-tuples
  "Extracts all urls for a given list (license or exception) entry."
  [list-entry]
  (let [id              (:id list-entry)
        simplified-uris (map u/simplify-uri (filter (complement s/blank?) (concat (:see-also list-entry) (get-in list-entry [:cross-refs :url]))))]
    (map #(vec [% id]) simplified-uris)))

(def ^:private index-uri->id-d (delay (merge (u/mapfonv #(u/nset (map second %)) (group-by first (mapcat urls->id-tuples (map sl/info @license-ids-d))))
                                             (u/mapfonv #(u/nset (map second %)) (group-by first (mapcat urls->id-tuples (map se/info @exception-ids-d)))))))

;####TODO: MOVE THIS TO lice-comb.impl.license-detection.uris
(defn near-match-uri
  "Returns the id(s) (a set) for the given listed `uri`, or `nil` if no ids were
  found. The result may include deprecated ids."
  [uri]
  (get @index-uri->id-d (u/simplify-uri uri)))

;####TODO: MOVE THIS TO lice-comb.impl.license-detection.uris
; This is needed for pathological cases like "http://gnu.org/license/fdl-1.3" (which has 7 (!) ids associated with it)
(defn best-near-match-uri
  "Returns the 'best match' id (a `String`) for `uri`, or `nil` if no ids were
  found.  A 'best match' is defined as the shortest non-deprecated id (if any),
  or (worst case) the shortest deprecated id."
  [uri]
  (best-identifier (near-match-uri uri)))

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
  @index-uri->id-d
  nil)
