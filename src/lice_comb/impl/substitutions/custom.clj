;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.custom
  "Helper functionality related to substituting matches for custom lice-comb
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; This is redundant here, but we include it for consistency with other substutition namespaces
(def ids-d (delay (set '())))

(def ^:private pairs-d (delay [
  ; Proprietary / commercial
  [#"(?i)(?<!\w)(?:Propriet[aoe]ry|Commercial|(?:Copyright\s+.{0,20})?All[\s\-–—]+Rights[\s\-–—]+Reserved|Private)(?:[\s\-–—]+Licen?[cs]e)?[\s\-–—\.]*(?!\w)"  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
   (fn [m]
     {:id         (lcis/proprietary-commercial)
      :type       :concluded
      :confidence :high
      :strategy   :regex-matching
      :source     (list (:match m))})]
  ; Public domain
  [#"(?i)(?<!\w)Public\s+Domain[\s\-–—\.]*(?![\s/\\\(]*CC[\s\-–—]*0)"  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
   (fn [m]
     {:id         (lcis/public-domain)
      :type       :concluded
      :confidence :high
      :strategy   :regex-matching
      :source     (list (:match m))})]]))

(defn sub
  "Substitutes any custom (lice-comb specific) licenses found in the `String`s
  in `coll` with an expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d coll))

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
