;
; Copyright © 2023 Peter Monks
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; SPDX-License-Identifier: Apache-2.0
;

(ns lice-comb.impl.correction
  "Corrects conceptually invalid/nonsensical combinations of license identifiers.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [spdx.licenses                   :as sl]
            [spdx.exceptions                 :as se]
            [lice-comb.impl.spdx             :as lcis]))

;####TODOS:
; * Update to only work on expressions-info maps - there is no need to support sets, since they're only emitted at the final step
; * Update to support whatever intermediate data structure the parsing code ends up using

(def ^:private gpl-ids-with-only-or-later #{"AGPL-1.0"
                                            "AGPL-3.0"
                                            "GFDL-1.1"
                                            "GFDL-1.2"
                                            "GFDL-1.3"
                                            "GPL-1.0"
                                            "GPL-2.0"
                                            "GPL-3.0"
                                            "LGPL-2.0"
                                            "LGPL-2.1"
                                            "LGPL-3.0"})

(defn- dis
  "Remove the given key(s) from the associative collection (set or map)."
  [associative & ks]
  (cond (set? associative) (apply disj   associative ks)
        (map? associative) (apply dissoc associative ks)))

(defn- fix-gpl-only-or-later
  "If the keys of expressions includes both an 'only' and an 'or-later' variant
  of the same underlying GNU family identifier, remove the 'only' variant."
  [expressions]
  (loop [result expressions
         f      (first gpl-ids-with-only-or-later)
         r      (rest  gpl-ids-with-only-or-later)]
    (if f
      (recur (if (and (contains? result (str f "-only"))
                      (contains? result (str f "-or-later")))
               (dis result (str f "-only"))
               result)
             (first r)
             (rest r))
      result)))

(defn- fix-public-domain-cc0
  "If the keys of expressions includes both CC0-1.0 and lice-comb's public
  domain LicenseRef, remove the LicenseRef as it's redundant."
  [expressions]
  (if (and (contains? expressions (lcis/public-domain))
           (contains? expressions "CC0-1.0"))
    (dis expressions (lcis/public-domain))
    expressions))

(defn- fix-mpl-2
  "If the keys of expressions includes both MPL-2.0 and
  MPL-2.0-no-copyleft-exception, remove MPL-2.0-no-copyleft-exception as it's
  redundant."
  [expressions]
  (if (and (contains? expressions "MPL-2.0")
           (contains? expressions "MPL-2.0-no-copyleft-exception"))
    (dis expressions "MPL-2.0-no-copyleft-exception")
    expressions))

(defn- fix-license-id-with-exception-id
  "Combines instances where there are two keys, one of them a license identifier
  and the other an exception identifier."
  [expressions]
  (if (= 2 (count expressions))
    (if (set? expressions)
      ; expressions is a set
      (let [license-id   (first (seq (filter #(or (sl/listed-id? %) (sl/license-ref?  %)) expressions)))
            exception-id (first (seq (filter #(or (se/listed-id? %) (se/addition-ref? %)) expressions)))]
        (if (and license-id exception-id)
          #{(str license-id " WITH " exception-id)}
          expressions))
      ; expressions is a map
      (let [exprs        (keys expressions)
            license-id   (first (seq (filter #(or (sl/listed-id? %) (sl/license-ref?  %)) exprs)))
            exception-id (first (seq (filter #(or (se/listed-id? %) (se/addition-ref? %)) exprs)))]
        (if (and license-id exception-id)
          {(str license-id " WITH " exception-id) (reduce concat (vals expressions))}
          expressions)))
    expressions))


(defn correct
  "Corrects certain invalid/nonsensical combinations of license identifiers in a
  set or map of expressions."
  [expressions]
  (some-> expressions
          fix-gpl-only-or-later
          fix-public-domain-cc0
          fix-mpl-2
          fix-license-id-with-exception-id))
