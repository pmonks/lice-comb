;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.utils
  "Utility functions for license substitutions.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string               :as s]
            [clojure.set                  :as set]
            [spdx.licenses                :as slic]
            [spdx.exceptions              :as sexc]
            [spdx.expressions             :as sexp]
            [lice-comb.impl.3rd-party     :refer [by ascending descending] :as lc3]
            [lice-comb.impl.regexes       :as lcir]
            [lice-comb.impl.spdx          :as lcis]
            [lice-comb.impl.parsing-utils :as lcip]
            [lice-comb.impl.utils         :as lciu]))

(defn listed-id?
  "Is `id` a listed SPDX identifier (license or exception)?

  Notes:
  * Supports 'identifiers' (technically an SDPX expression) containing an 'or
    later' flag (a single '+' character at the end)"
  [id]
  (let [raw-id (s/replace id #"\+\z" "")]
    (or (contains? @lcis/license-ids-d   raw-id)
        (contains? @lcis/exception-ids-d raw-id))))

(defn assert-listed-id
  "Checks that `id` is a listed SPDX identifier (license or exception) and
  throws if not. Returns `id` unchanged on success.

  Notes:
  * Supports 'identifiers' (technically an SDPX expression) containing an 'or
    later' flag (a single '+' character at the end)"
  [id]
  (if (listed-id? id)
    id
    (throw (ex-info (str "Invalid SPDX id constructed: '" id
                         "' - please raise an issue at "
                         "https://github.com/pmonks/lice-comb/issues/new?assignees=pmonks&labels=bug&template=Invalid_id_constructed.md&title=Invalid+SPDX+identifer+constructed:+" id)
                    {:id id}))))

(defn get-rencgs
  "Get a value for an re-ncg, potentially looking at multiple ncgs in order
  until a non-blank value is found. Returns `default` when no non-blank value is
  found (and which defaults to `nil` if not provided). Trims and lower-cases the
  value, and replaces all whitespace with a single space."
  ([m names] (get-rencgs m names nil))
  ([m names default]
    (loop [f (first names)
           r (rest  names)]
      (if f
        (let [value (get m f)]
          (if (s/blank? value)
            (recur (first r) (rest r))
            (-> value
                (s/trim)
                (s/lower-case)
                (s/replace #"\s+" " "))))
        default))))

(defn sub-res
  "Takes a sequence of regex/substitution pairs (`re-sub-pairs`) and uses
  them to substitute matches in the `String` value in `coll`, returning a new
  `coll`."
  [re-sub-pairs coll]
  (loop [[[re repl] & r] re-sub-pairs
         coll            coll]
    (if (or (not re)
            (not repl)
            (lcip/done-parsing? coll))  ; coll is fully parsed, so we can terminate early
      (seq coll)
      (let [new-coll (lciu/replace-in-coll coll re repl)]
        (recur r new-coll)))))

; These are here because the GNU family are SUCH A HUGE PAIN IN THE ARSE!!!!!
(defn agpl-identifier?
  "Is `id` an AGPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "AGPL-")))

(defn lgpl-identifier?
  "Is `id` an LGPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "LGPL-")))

(defn gpl-identifier?
  "Is `id` a GPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "GPL-")))

(defn gnu-family?
  "Is `id` a GNU family SPDX identifier?"
  [id]
  (boolean
    (when id
      (or (agpl-identifier? id)
          (lgpl-identifier? id)
          (gpl-identifier?  id)))))

(defn- name-id-match-ei-fn
  "Returns a simple expression-info construction function for `id`, when matched
  with an generic regex constructed from an SPDX name or id.  The returned
  function takes a single argument; a rencg find/match match map."
  [id]
  (let [n (:name (or (slic/id->info id) (sexc/id->info id)))]
    (fn [m]
      (let [id                  (str id (when (get m "orLater") "+"))               ; This can end up with things like GPL-2.0-or-later+, but those get canonicalised by clj-spdx in the next step
            id                  (if-let [new-id (sexp/canonicalise id)] new-id id)  ; Note: exception ids won't canonicalise by themselves
            gnu-suffix-missing? (and (gnu-family? id) (not (get m "orLater")) (not (get m "only")))  ; Special case GNU family licenses missing version suffixes
            match               (s/trim (:match m))
            strategy            (cond
                                  (= (s/lower-case match) (s/lower-case id)) :spdx-listed-identifier  ; Because some name regexes will also match the associated id
                                  (= match n)                                :spdx-listed-name-exact-match
                                  (= (s/lower-case match) (s/lower-case n))  :spdx-listed-name-case-insensitive-match
                                  :else                                      :spdx-listed-name-near-match)]
        (merge {:id       id
                :strategy strategy
                :source   (list match)}
               (case strategy
                 :spdx-listed-identifier {:type :declared}
                 (if gnu-suffix-missing?
                   {:type :concluded :confidence :medium :confidence-explanations #{:missing-version-suffix}}
                   {:type :concluded :confidence :high})))))))

(defn spdx-match-pairs
  "Constructs a sequence of regex/fn pairs for the given SPDX identifiers,
  matching first by name regexes, then by id regexes. Sorts the ids as per
  lice-comb.impl.spdx/sort-ids."
  [ids]
  (let [ids (lcis/sort-ids ids)]
    (concat
      ; Names match pairs first
      (map #(vector (lcir/id->name->regex %) (name-id-match-ei-fn %)) ids)
      ; Then id match pairs
      (map #(vector (lcir/id->regex %)       (name-id-match-ei-fn %)) ids))))

(comment
      ; Names match pairs first
      (sort (by #(count (str (first %))) descending)
            (map #(vector (lcir/id->name->regex %) (name-id-match-ei-fn %)) ids))
      ; Then id match pairs
      (sort (by #(count (str (first %))) descending)
            (map #(vector (lcir/id->regex %) (name-id-match-ei-fn %)) ids))
)

(defn simple-regex-match-ei-fn
  "Returns a simple expression-info construction function for `id`, when matched
  with a custom regex.  The returned function takes a single argument; a rencg
  find/match match map."
  ([id] (simple-regex-match-ei-fn id nil nil))
  ([id confidence confidence-explanations]
   (fn [m]
     (let [id (str id (when (get m "orLater") "+"))
           id (if-let [new-id (sexp/canonicalise id)] new-id id)  ; Note: naked exception identifiers won't canonicalise, so this has to be conditional
           match      (s/trim (:match m))]
       (merge {:id         id
               :type       :concluded
               :confidence (or confidence :high)
               :strategy   :regex-matching
               :source     (list match)}
              (when confidence-explanations {:confidence-explanations confidence-explanations}))))))

(defn version-handling-regex-match-ei-fn
  "Returns a expression-info construction function that takes a single argument;
  a rencg find/match match map.  `id-prefix` is the prefix of the SPDX
  identifier to use in constructing the id.  `default-version` is the default
  version to use if none is provided.  `all-versions` is a sequence of all
  versions of the given license."
  [id-prefix default-version all-versions]
  (fn [m]
    (let [has-version-number?     (not (s/blank? (get m "versionNumber")))
          [version-number valid-version? missing-minor-version?]
                                  (if (not has-version-number?)
                                    ; No version number - fall back on the default
                                    [default-version true false]
                                    ; We found a version number - validate it
                                    (let [raw-version-number (s/trim (get m "versionNumber"))]
                                      ; Is it valid?
                                      (if (some #{raw-version-number} all-versions)
                                        [raw-version-number true false]
                                        ; Not valid so try with .0 tacked on the end (e.g. "1" -> "1.0")
                                        (let [raw-version-number (str raw-version-number ".0")]
                                          (if (some #{raw-version-number} all-versions)
                                            [raw-version-number true true]
                                            ; Fall back to the default
                                            [default-version false true])))))
          id                      (str id-prefix version-number (when (get m "orLater") "+"))
          id                      (if-let [new-id (sexp/canonicalise id)] new-id id)  ; Note: naked exception identifiers won't canonicalise, so this has to be conditional
          confidence              (if (and has-version-number? valid-version? (not missing-minor-version?)) :high :medium)
          confidence-explanations (set/union (when (not has-version-number?) #{:missing-version})
                                             (when (not valid-version?)      #{:invalid-version})
                                             (when missing-minor-version?    #{:missing-minor-version}))]
    (merge {:id                      (assert-listed-id id)
            :type                    :concluded
            :confidence              confidence
            :strategy                :regex-matching
            :source                  (list (:match m))}
           (when confidence-explanations {:confidence-explanations confidence-explanations})))))
