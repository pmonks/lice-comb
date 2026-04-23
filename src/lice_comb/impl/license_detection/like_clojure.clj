;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.like-clojure
  "'like Clojure' license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [wreck.api                                         :as re]
            [lice-comb.impl.faux-parse                         :as faux]
            [lice-comb.impl.regex-fragments                    :as ref]
            [lice-comb.impl.license-detection.match-processing :as mp]))

; Clojure's license is EPL-1.0 (https://github.com/clojure/clojure/) and seems unlikely to move to EPL-2.0+ (https://www.eclipse.org/lists/epl-discuss/msg00043.html)

(def ^:private re-clojure
  (re/fgrp "i"
           ref/nwb
           ref/ows
           (re/opt-grp (re/join "Eclipse" "public" #"licen?[cs]e", ))
           (re/opt-grp (re/alt-grp (re/join (re/opt-grp "the" ref/mws) "same" ref/mws "as")
                                   (re/join (re/opt-grp "just" ref/mws) "like"))
                       ref/ows)
           "Clojure"
           ref/ows
           ref/nwa))

(defn detect
  "Detects any proprietary/commercial identifiers found inside the `String`s in
  `coll` and replaces them with an expression-info map in that location. Returns
  other elements unchanged."
  [coll]
  (faux/parse coll re-clojure (partial mp/unversioned-match->expression-info "EPL-1.0" :custom-regex)))
