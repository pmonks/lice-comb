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
            [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (map :id (filter #(s/starts-with? (:id %) "CDDL-") @lcis/full-license-list-d)))))

(def re (re/join #"(?iuU)"  ; Only public for ease of testing
                 (re/alt-grp "CDDL"
                             (re/join #"Common[\s\-–—]+Development[\s\-–—]+(?:and|&)[\s\-–—]+Distribution(?:[\s\-–—]+Licen?[cs]e)?"
                                      (re/opt-grp lcir/fre-ows #"\(?CDDL\)?")))
                 (re/opt-grp lcir/fre-ows lcir/fre-version)
                 (re/opt-grp lcir/fre-ows lcir/fre-only-or-later)))

(def ^:private pairs-d (delay (concat
  [[re (lcisu/version-handling-regex-match-ei-fn "CDDL-" "1.1" ["1.0" "1.1"])]]  ; Match custom regex first, so default ones don't partially consume
  (lcisu/spdx-match-pairs @ids-d))))

(defn sub
  "Substitutes any CDDL licenses found in the strings in `coll` with an
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
