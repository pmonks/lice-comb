;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.cursed
  "Helper functionality related to substituting matches for certain cursed
  license names.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; This is redundant here, but we include it for consistency with other substutition namespaces
(def ids-d (delay (set '())))

; Map of name values seen in the wild that are too ambiguous / cursed to support any reasonable form of automated parsing
(def ^:private cursed-names {
  ; Seen in https://repo.maven.apache.org/maven2/com/sun/mail/all/1.4.7/all-1.4.7.pom and other javax.mail/javax.mail-api artifacts
  "GPLv2+CE" '({:id "GPL-2.0-only"            :type :concluded :confidence :high :strategy :manual-verification :source ("GPLv2+CE" "GPLv2")}
               :with
               {:id "Classpath-exception-2.0" :type :concluded :confidence :high :strategy :manual-verification :source ("GPLv2+CE" "CE")})
  })

(def ^:private pairs-d (delay (map #(vector (lcir/re-concat #"(?<!\w)" (lcir/re-escape (key %)) #"(?!\w)")
                                            (val %))
                                   cursed-names)))

(defn sub
  "Substitutes any cursed licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
  [coll]
  (lcisu/sub-res @pairs-d coll))

(defn init!
  "Initialises this namespace upon first call (and does nothing on subsequent
  calls), returning nil. Consumers of this namespace are not required to call
  this fn, as initialisation will occur implicitly anyway; it is provided to
  allow explicit control of the cost of initialisation to callers who need it."
  []
  @ids-d
  @pairs-d
  nil)
