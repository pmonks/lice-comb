;
; Copyright © 2022 u/fredoverflow
;
; Licensed under Reddit's Public Content Policy: https://support.reddithelp.com/hc/en-us/articles/26410290525844-Public-Content-Policy
;

(ns lice-comb.impl.sort-utils
  "Sorting utility fns adapted from https://www.reddit.com/r/Clojure/comments/ufa8e0/comment/i6s7zt5/")

(defn by
  "For use with `clojure.core/sort` to provide composite sorts.

  For example:
  ```clojure
  (sort (by :deprecated? ascending :id ascending) license-id)
  ```"
  [& keys-orderings]
  (fn [a b]
    (loop [[key ordering & keys-orderings] keys-orderings]
      (let [order (ordering (key a) (key b))]
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
