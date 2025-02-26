;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.epl
  "Helper functionality related to substituting matches for the EPL family of
  licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [wreck.api                          :as re]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.substitutions.utils :as lcisu]))

(def ids-d (delay (set (map :id (filter #(s/starts-with? (:id %) "EPL-") @lcis/full-license-list-d)))))

(def re (re/join #"(?iuU)"  ; Only public for ease of testing
                 (re/alt-grp "EPL"
                             (re/join (re/opt-grp "Some" lcir/fre-mws)
                                      #"Eclipse"
                                      (re/opt-grp lcir/fre-mws #"Pub?lic")
                                      (re/opt-grp lcir/fre-mws #"Licen?[cs]e")
                                      (re/opt-grp lcir/fre-mws #"\(?EPL[\s\-–—v\d\.]*\)?")))
                 (re/opt-grp lcir/fre-ows lcir/fre-version)
                 (re/opt-grp lcir/fre-ows lcir/fre-only-or-later)
                 (re/opt-grp lcir/fre-mws #"\(?EPL[\s\-–—v\d\.]*\)?")
                 (re/opt-grp lcir/fre-mws (re/opt-grp "the") lcir/fre-mws "same" lcir/fre-mws "as" lcir/fre-mws "Clojure")))

(def ^:private pairs-d (delay [[re (lcisu/version-handling-regex-match-ei-fn "EPL-" "2.0" ["1.0" "2.0"])]]))

(defn sub
  "Substitutes any EPL licenses found in the strings in `coll` with an
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
