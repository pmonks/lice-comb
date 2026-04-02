;;;; lice_comb.impl.3rd_party.clj
;;;
;;; Code obtained from third party sources, but not readily available as a
;;; library via standard package-consumption mechanisms (i.e. a Maven artifact).
;;;
;;; Copyright and license information is on a per-code-snippet basis, and
;;; is communicated inline via further comments.
;;;
(ns lice-comb.impl.3rd-party)


;; rdrop-while is copyright © Joshua Suskalo (https://github.com/IGJoshua) 2023 and licensed as "CC0-1.0 OR MIT"
;;
;; Source: https://discord.com/channels/729136623421227082/732641743723298877/1141786961875583097
;; Link to request access: https://discord.gg/discljord
;;
;; Note that the lice-comb project elects to consume this code under the MIT license
;;
;; SPDX-License-Identifier: CC0-1.0 OR MIT
;;
(defn rdrop-while
  "As for [clojure.core/drop-while](https://clojuredocs.org/clojure.core/drop-while),
  but drops from the end of `coll` backwards, rather than the front forwards.
  More efficient when provided with a vector rather than a list."
  ([pred coll]
   (if (reversible? coll)
     (take (- (count coll) (count (take-while pred (rseq coll)))) coll)
     (reverse (drop-while pred (reverse coll)))))
  ([pred]
   (fn [rf]
     (let [stash (volatile! [])]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc elt]
          (if (pred elt)
            (do (vswap! stash conj elt)
                acc)
            (let [res (reduce rf acc (conj @stash elt))]
              (vreset! stash [])
              res))))))))

;; when-pred is copyright © Joshua Suskalo (https://github.com/IGJoshua) 2022 and licensed as "CC0-1.0 OR MIT"
;;
;; Source: https://discord.com/channels/729136623421227082/878465827979010079/961333852821942322
;; Link to request access: https://discord.gg/discljord
;;
;; Note that the lice-comb project elects to consume this code under the MIT license
;;
;; SPDX-License-Identifier: CC0-1.0 OR MIT
;;
(defn when-pred
  "If `pred?` is met when supplied with `value`, apply `then` (a function) to
  `value` and return the result. Otherwise just return `value` as-is.

  Useful when used with thread-first macros."
  [value pred? then]
  (if (pred? value) (then value) value))


;; by, ascending, and descending are copyright © fredoverflow 2022
;;
;; Obtained from https://www.reddit.com/r/Clojure/comments/ufa8e0/comment/i6s7zt5/ /
;; https://youtu.be/bihh8nPGixo?si=XwMwKZL16jMokn1B, where the author states:
;;
;; "I don't care about the code. Feel free to use and evolve it for any purpose,
;; including commercial. No attribution required."
;;
;; SPDX-License-Identifier: LicenseRef-lice-comb-PUBLIC-DOMAIN
;;
(defn by
  "For use with `clojure.core/sort` to provide composite sorts.

  For example:
  ```clojure
  (sort (by :deprecated? ascending :id ascending) license-id)
  ```"
  [& keys-orderings]
  (fn [a b]
    (loop [[key-fn ordering & keys-orderings] keys-orderings]
      (let [order (ordering (key-fn a) (key-fn b))]
        (if (and (zero? order) keys-orderings)
          (recur keys-orderings)
          order)))))

(defn ascending
  "For use with [[by]] to indicate ascending sort order."
  [a b]
  (clojure.lang.Util/compare a b))

(defn descending
  "For use with [[by]] to indicate descending sort order."
  [a b]
  (clojure.lang.Util/compare b a))
