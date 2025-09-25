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
  licenserefs.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.substitutions.utils :as lcisu]))

; This is redundant here, but we include it for consistency with other substutition namespaces
(def ids-d (delay '()))

(def ^:private pairs-d (delay [
  ; Proprietary / commercial
  [(re/flags-grp "i"
                 #"(?<!\w)"
                 (re/alt-grp #"Propriet[aoe]ry(?:[\s\-–—_\.,/\\]+Commercial)?"
                             #"Commercial"
                             #"(?:Copyright\s+.{0,20})?All[\s\-–—]+Rights[\s\-–—]+Reserved"
                             #"Private")
                 (re/opt-grp #"[\s\-–—]+Licen?[cs]e")
                 #"[\s\-–—\.]*(?!\w)")  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
   (lcisu/simple-regex-match-ei-fn (lcis/proprietary-commercial))]
  ; Public domain
  [#"(?i:(?<!\w)Public\s+Domain[\s\-–—\.]*(?![\s/\\\(]*CC[\s\-–—]*0))"  ; We consume - and . so that replacement doesn't leave them in and cause problems later on
   (lcisu/simple-regex-match-ei-fn (lcis/public-domain))]
  ; MX4J - alias for Apache 1.1 - see https://wiki.spdx.org/view/Legal_Team/License_List/Licenses_Under_Consideration#Processed_License_Requests
  [(re/flags-grp "i"
                 #"(?<!\w)"
                 (re/opt-grp #"The[\s\-–—_\.,]*")
                 #"MX4J"
                 (re/opt-grp #"[\s\-–—_\.,]+Pub?lic")
                 (re/opt-grp #"[\s\-–—_\.,]+Licen?[cs]e")
                 (re/opt-grp #"[\s\-–—_\.,]+v(?:er(?:sion)?)?")
                 (re/opt-grp #"[\s\-–—_\.,]*0*1(?:\.0*)")
                 #"(?!\w)")
   (lcisu/simple-regex-match-ei-fn "Apache-1.1")]
   ; Bouncy Castle - alias for MIT - see https://github.com/spdx/license-list-XML/issues/910
   [#"(?i:Bouncy[\s\-–—_\.,]+Castle([\s\-–—_\.,]+Licen[cs]e)?)"
    (lcisu/simple-regex-match-ei-fn "MIT")]]))

(defn sub
  "Substitutes any custom (lice-comb specific) 'licenses' found in the `String`s
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
