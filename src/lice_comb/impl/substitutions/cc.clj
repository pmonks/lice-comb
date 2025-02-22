;
; Copyright © 2025 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.substitutions.cc
  "Helper functionality related to substituting matches for the Creative Commons
  family of licenses.
  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                     :as s]
            [clojure.set                        :as set]
            [wreck.api                          :as re]
            [spdx.licenses                      :as sl]
            [spdx.expressions                   :as sexp]
            [lice-comb.impl.spdx                :as lcis]
            [lice-comb.impl.regexes             :as lcir]
            [lice-comb.impl.utils               :as lciu]
            [lice-comb.impl.substitutions.utils :as lcisu]))

;####TODO: IMPLEMENT ME!!!!
(def ids-d (delay (set '())))

(defn sub
  "Substitutes any Creative Commons family licenses found in the strings in
  `coll` with an expression-info map. Returns other elements unchanged."
  [coll]
  ;####TODO: IMPLEMENT ME!!!!
  coll)
