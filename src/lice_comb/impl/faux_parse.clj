;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.faux-parse
  "Functionality related to 'faux parsing' (parsing a String to a heterogeneous
  sequence by successively performing replacements on the string fragments
  remaining after each prior replacement).

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string       :as s]
            [thread-until.core    :as tu]
            [lice-comb.impl.utils :as lciu]))

(defn replace-in-strings
  "For each `String` in `partial-parse` (a sequential), replaces any occurrences
  of `re` (a regex) within the string with `replacement`, as per
  [[lice-comb.impl.utils/replacing-split]]. Returns a new sequential."
  [partial-parse re replacement]
  (when partial-parse
    (if re
      (lciu/mapcat-str #(lciu/replacing-split % re replacement) partial-parse)
      partial-parse)))

(defn consumed?
  "Has `partial-parse` been consumed?  This is defined as having no non-blank
  strings left."
  [partial-parse]
  (every? #(or (not (string? %)) (s/blank? %)) partial-parse))

(defmacro parse-until
  "Faux parses `string-or-sequential` by successively applying
  `replacement-pairs` (pairs of regexes and replacements, as used in
  [[replace-in-strings]]) until `predicate-fn` (default [[consumed?]]) becomes
  true, or it runs out of replacements.  Returns a sequence of 'faux parsed'
  fragments."
  [string-or-sequential predicate-fn & replacement-pairs]
  ; Validate that replacement-pairs isn't empty, and that it only contains pairs
  (let [rpc (count replacement-pairs)]
    (when (or (not (pos? rpc))
              (odd? rpc))
      (throw (ex-info "Wrong number of replacement pairs" {:count rpc}))))
  ; Precalculate some stuff
  (let [pred         (or predicate-fn `consumed?)
        replacements (for [[re replacement] (partition 2 replacement-pairs)]
                       (list `replace-in-strings re replacement))]
    ; Emit the resulting forms
    `(let [sos# ~string-or-sequential
           pp#  (when sos# (if (sequential? sos#) sos# (vector sos#)))]
       (tu/until-> pp#
                   ~pred
                   ~@replacements))))

(defmacro parse
  "As for [[parse-until]] but with the default early termination fn
  ([[consumed?]])."
  [string-or-sequential & replacement-pairs]
  `(parse-until ~string-or-sequential nil ~@replacement-pairs))

(defn parse-with-pairs
  "Similar to [[parse-until]], but accepts a sequence of regex/replacement
  pairs."
  ([regex-replacement-pairs partial-parse] (parse-with-pairs regex-replacement-pairs nil partial-parse))
  ([regex-replacement-pairs predicate-fn partial-parse]
   (when (and (seq regex-replacement-pairs)
              partial-parse)
     (let [pred (or predicate-fn consumed?)]
       (loop [[[regex replacement] & r] regex-replacement-pairs
              result                    partial-parse]
         (if (or (not  regex)
                 (not  replacement)
                 (pred result))  ; Result is fully parsed, so terminate early
           (seq result)
           (recur r (replace-in-strings result regex replacement))))))))

