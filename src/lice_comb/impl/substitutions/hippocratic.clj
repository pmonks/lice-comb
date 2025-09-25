;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.hippocratic
  "Helper functionality related to substituting matches for the Hippocractic
  family of licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (lcis/sort-ids (filter #(s/starts-with? % "Hippocratic-") @lcis/license-ids-d))))

(def ^:private pairs-d (delay (concat
  (lcisu/spdx-match-pairs @ids-d)                                 ; Generic license regexes handle most cases, except...
  [[(re/flags-grp "iuU"
                  #"Hippocratic(?:[\s\-–—]+Licen?[cs]e)?"
                  (re/opt-grp lcir/fre-ows (lcir/re-version-suffix)))  ; ...when no version is provided
   (fn [m]
     {:id                      (str "Hippocratic-2.1" (when (get m "orLater") "+"))
      :type                    :concluded
      :confidence              :high   ; We opt for :high here because there's only one listed version of the Hippocratic license
      :confidence-explanations #{:missing-version}
      :strategy                :regex-matching
      :source                  (list (:match m))})]])))

(defn sub
  "Substitutes any Hippocratic licenses found in the strings in `coll` with an
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
