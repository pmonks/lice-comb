;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.cddl
  "Helper functionality related to substituting matches for the CDDL family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [clojure.set                        :as set]
            [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (map :id (filter #(s/starts-with? (:id %) "CDDL-") @lcis/full-license-list-d)))))

(def ^:private pairs-d (delay (concat
  (lcisu/spdx-match-pairs @ids-d)  ; Generic license regexes handle most cases, except...
  [[(re/join #"(?iuU)"             ; ...when no version is provided
             (re/alt-grp "CDDL"
                         (re/join #"Common[\s\-–—]+Development[\s\-–—]+(?:and|&)[\s\-–—]+Distribution(?:[\s\-–—]+Licen?[cs]e)?"
                                  (re/opt-grp lcir/fre-ows #"\(?CDDL\)?")))
             (re/opt-grp lcir/fre-ows lcir/fre-version)
             (re/opt-grp lcir/fre-ows lcir/fre-only-or-later))
   (fn [m]
     (let [has-version-number?     (boolean (get m "versionNumber"))
           version-number          (s/trim (get m "versionNumber" "1.1"))  ; Default to latest version
           valid-version-number?   (or (= version-number "1.0") (= version-number "1.1"))
           id                      (str "CDDL-"
                                        (if valid-version-number? version-number "1.1")
                                        (when (get m "orLater") "+"))
           confidence              (if (and has-version-number? valid-version-number?) :high :medium)
           confidence-explanations (when-not (and has-version-number? valid-version-number?)
                                     (set/union (when (not has-version-number?)   #{:missing-version})
                                                (when (not valid-version-number?) #{:invalid-version})))]
     (merge {:id                      id
             :type                    :concluded
             :confidence              confidence
             :strategy                :regex-matching
             :source                  (list (:match m))}
            (when confidence-explanations {:confidence-explanations confidence-explanations}))))]])))

(defn sub
  "Substitutes any CDDL licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d  coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it.

  Note: this method has a substantial performance cost."
  []
  (lcis/init!)
  @ids-d
  @pairs-d
  nil)
