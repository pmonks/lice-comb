;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.wtf
  "Helper functionality related to substituting matches for the WTFPL family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ^:private wtf-id "WTFPL")
(def ids-d (delay (lcis/sort-ids #{wtf-id})))

(def ^:private pairs-d (delay (concat [
  [(re/join #"(?iuU)"
            (re/alt-grp
              #"(?:\(?WTFPL\)?[\s\-–—]+)?(?:The[\s\-–—]+)?Do[\s\-–—]+(?:WTF|What[\s\-–—]+The[\s\-–—]+[f\*][u\*][c\*][k\*])[\s\-–—]+(?:You|U)[\s\-–—]+Want[\s\-–—]+(?:To|2)"
              "WTFPL")
            (re/opt-grp lcir/fre-mws "Public")
            (re/opt-grp lcir/fre-mws #"Licen?[cs]e")
            (re/opt-grp lcir/fre-ows #"(?:v|ver|version)" lcir/fre-ows #"[\d\.]+")   ; We don't care about capturing the version number for WTFPL, as there are no official versions
            #"(?!\w)")
   (lcisu/simple-regex-match-ei-fn wtf-id)]])))

(defn sub
  "Substitutes any WTFPL licenses found in the strings in `coll` with an
  expression-info map. Returns other elements unchanged."
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
