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
            (lcip/done-parsing? coll))  ; coll is fully devoid of strings, so we can terminate early
      (seq coll)
      (let [new-coll (lciu/replace-in-coll coll re repl)]
        (recur r new-coll)))))

; These are here because the GNU family are SUCH A HUGE PAIN IN THE ARSE!!!!!
(defn agpl-license?
  "Is `id` an AGPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "AGPL-")))

(defn lgpl-license?
  "Is `id` an LGPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "LGPL-")))

(defn gpl-license?
  "Is `id` a GPL SPDX identifier?"
  [id]
  (when id
    (s/starts-with? id "GPL-")))

(defn gnu-family?
  "Is `id` a GNU family SPDX identifier?"
  [id]
  (boolean
    (when id
      (or (agpl-license? id)
          (lgpl-license? id)
          (gpl-license?  id)))))

(defn- name-id-match-ei-fn
  "Returns a simple expression-info construction function for `id`, when matched
  with an generic regex constructed from an SPDX name or id.  The returned
  function takes a single argument; a rencg find/match match map."
  [id]
  (let [n (:name (or (slic/id->info id) (sexc/id->info id)))]
    (fn [m]
      (let [id                  (str id (when (get m "orLater") "+"))            ; This can end up with things like GPL-2.0-or-later+, but those get normalised in the next step
            id                  (if-let [new-id (sexp/normalise id)] new-id id)  ; Note: exception ids won't normalise
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
  matching first by name regexes, then by id regexes."
  [ids]
  (concat
    ; Names match pairs first
    (sort (by #(count (str (first %))) descending)
          (map #(vector (lcir/id->name->regex %) (name-id-match-ei-fn %)) ids))
    ; Then id match pairs
    (sort (by #(count (str (first %))) descending)
          (map #(vector (lcir/id->regex %) (name-id-match-ei-fn %)) ids))))

(defn regex-match-ei-fn
  "Returns a simple expression-info construction function for `id`, when matched
  with a custom regex.  The returned function takes a single argument; a rencg
  find/match match map."
  [id]
  (fn [m]
    (let [id (str id (when (get m "orLater") "+"))
          id (if-let [new-id (sexp/normalise id)] new-id id)  ; Note: naked exception identifiers won't normalise
          match      (s/trim (:match m))]
      {:id         id
       :type       :concluded
       :confidence :high
       :strategy   :regex-matching
       :source     (list match)})))
