;
; Copyright © 2021 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.utils
  "General purpose utility fns that I seem to end up needing in every single
  project I write... Note: this namespace is not part of the public API of
  lice-comb and may change without notice."
  (:require [clojure.string  :as s]
            [clojure.java.io :as io]
            [clj-base62.core :as base62]
            [embroidery.api  :as e]
            [rencg.api       :as rencg]))

(defn mapfonk
  "Returns a new map where f has been applied to all of the keys of m."
  [f m]
  (when m
    (into {}
          (for [[k v] m]
            [(f k) v]))))

(defn mapfonv
  "Returns a new map where f has been applied to all of the values of m."
  [f m]
  (when m
    (into {}
          (for [[k v] m]
            [k (f v)]))))

(defn map-pad
  "`map`, but when presented with multiple collections of different lengths,
  'pads out' the missing elements with `nil` rather than terminating early."
  [f & cs]
  (loop [result nil
         firsts (map first cs)
         rests  (map rest  cs)]
    (if-not (seq (keep identity firsts))
      result
      (recur (cons (apply f firsts) result)
             (map first rests)
             (map rest  rests)))))

(defn map-pred
  "`map` on `coll`, calling `f` for any/all values for which `pred` returns
  logical true, passing through other values unchanged."
  [pred f coll]
  (when (and pred f coll)
    (map #(if (pred %)
            (f %)
            %)
         coll)))

(defn map-str
  "[map-pred] but with `pred` hardcoded to be `string?`."
  [f coll]
  (map-pred string? f coll))

(defn mapcat-pred
  "`mapcat` on `coll`, calling `f` for any/all values for which `pred` returns
  logical true, passing through other values unchanged."
  [pred f coll]
  (when (and pred f coll)
    (mapcat #(if (pred %)
               (f %)
               [%])
            coll)))

(defn mapcat-str
  "[mapcat-pred] but with `pred` hardcoded to be `string?`."
  [f coll]
  (mapcat-pred string? f coll))

(defn not-blank-string?
  "`true` when `x` is not a blank `String`."
  [x]
  (boolean
    (when x
      (or (not (string? x))
          (not (s/blank? x))))))

(defn strim
  "`nil` safe version of `clojure.string/trim`"
  [^String s]
  (when s (s/trim s)))

(defn trim-non-word
  "Like `clojure.string/trim`, but trims `s` of all leading and trailing
  characters that aren't alphanumeric (`[\\w]` regex class, with Unicode
  support enabled)."
  [s]
  (when s
    (-> s
        (s/replace #"(?U)\A[^\w]+" "")
        (s/replace #"(?U)[^\w]+\z" ""))))

(defn is-digits?
  "Does `s` contains digits only?"
  [^String s]
  (boolean
    (when-not (s/blank? s)
      (every? #(Character/isDigit ^Character %) s))))

(defn parse-dbl
  "Parses `s` (a `String`) as a double, returning `nil` if it can't be parsed."
  [s]
  (when (not (s/blank? s))
    (let [s (s/trim s)]
      (when (re-matches #"-?\d+(\.\d+)?" s)
        (java.lang.Double/parseDouble s)))))  ; We use interop instead of clojure.core/parse-double for backwards compatibility with older Clojure versions

(defn nset
  "`nil` preserving version of `clojure.core/set`"
  [coll]
  (some-> (seq coll)
          set))

(defn replace-ncg
  "As for `clojure.string/replace`, but uses rencg for regex processing.

  Notes:

  * uses [rencg](https://github.com/pmonks/rencg), so `replacement-fn` must
    accept a single argument that is a rencg-style map
  * only supports a regex as the second argument (for obvious reasons...)
  * only supports a function as the third argument (for obvious reasons...)"
  [^CharSequence s ^java.util.regex.Pattern re replacement-fn]
  (let [m    (re-matcher re s)
        ncgs (rencg/re-named-groups re)]
    (if (.find m)
      (let [buffer (StringBuffer. (.length s))]
        (loop [found true]
          (if found
            (do
              (.appendReplacement m buffer (java.util.regex.Matcher/quoteReplacement (replacement-fn (rencg/re-groups-ncg m ncgs))))
              (recur (.find m)))
            (do
              (.appendTail m buffer)
              (str buffer)))))
      s)))

(defn replacing-split
  "As for `clojure.string/split`, but replaces whatever `re` matched with
  `replacement`, which can be a value or a function of one argument.

  Notes:

  * replacement doesn't have to return a `String`, though not doing so will
    result in a heterogeneous collection.
  * uses [rencg](https://github.com/pmonks/rencg), so if `replacement` is a
    function it must accept a map, not a sequence.
  * does not support the `$1` syntax (as supported by Clojure and the JVM) - use
    a function instead"
  [^CharSequence s ^java.util.regex.Pattern re replacement]
  (let [replacement-fn (if (fn? replacement) replacement (constantly replacement))
        m              (re-matcher re s)
        ncgs           (rencg/re-named-groups re)]
    (loop [result []
           index  0
           f      (.find m)]
      (if f
        (let [match       (rencg/re-groups-ncg m ncgs)
              match-start (long (:start match))
              match-end   (long (:end   match))
              rep         (replacement-fn match)]
          (if (= index match-start)
            (recur (conj result rep) match-end (.find m))  ; Back-to-back matches
            (recur (vec (concat result [(subs s index match-start) rep])) match-end (.find m))))
        (if (= index (count s))
          result  ; The last find consumed to the end of the input
          (conj result (subs s index (count s))))))))  ; There's some trailing text - make sure to preserve it

(defn retained-split
  "As for `clojure.string/split`, but retains whatever `re` matched as distinct
  elements in the result."
  [^CharSequence s ^java.util.regex.Pattern re]
  (replacing-split s re #(get % :match)))   ; Can't use :match literally here, since `(fn? :keyword)` is always false

(defn replace-in-coll
  "For each `String` in `coll`, replaces any matches with `re` with
  `replacement`, as per [replacing-split]. Returns a new coll."
  [coll re replacement]
  (mapcat-str #(replacing-split % re replacement) coll))

(defn digit-name-to-number
  "Converts the English name of a single digit (a `String`) to that number (as a
  `Long`). e.g. `\"two\"` -> `2`.  Returns `nil` if `s` is not the name of a
  digit."
  [^String s]
  (when s
    (case (s/lower-case (s/trim s))
      "zero"  0
      "one"   1
      "two"   2
      "three" 3
      "four"  4
      "five"  5
      "six"   6
      "seven" 7
      "eight" 8
      "nine"  9
      nil)))

(def ^java.nio.charset.Charset utf8-charset java.nio.charset.StandardCharsets/UTF_8)

(defn utf8-bytes
  "The UTF-8 encoded bytes of `s` (a `String`), as a Java `byte[]`."
  [^String s]
  (.getBytes s utf8-charset))

(defn base62-encode
  "Encodes the given string to Base62/UTF-8."
  [^String s]
  (when s
    (base62/encode (utf8-bytes s))))

(defn base62-decode
  "Decodes the given Base62/UTF-8 string."
  [^String s]
  (when s
    (if (re-matches #"\p{Alnum}*" s)
      (java.lang.String. ^bytes (base62/decode s) utf8-charset)
      (throw (ex-info (str "Invalid BASE62 value provided: " s) {})))))   ; Because clj-base62 has crappy error messages

(defn html->text
  "Converts `html` (a String) to plain text (also a String)."
  [^String html]
  (when html
    (s/trim (.text (org.jsoup.Jsoup/parse html)))))

(defmulti html-file->text
  "Converts `f`, a file-like thing (input-stream, or anything that can be
  slurped) to plain text (a String)."
  class)

(defmethod html-file->text nil
  [_])

(defmethod html-file->text java.io.InputStream
  [is]
  (let [sw (java.io.StringWriter.)]
    (io/copy is sw)
    (html->text (str sw))))

(defmethod html-file->text :default
  [f]
  (when f
    (html->text (slurp f))))

(defn valid-http-uri?
  "Returns true if given string is a valid HTTP or HTTPS URI."
  [^String s]
  ; Note: no nil check needed since the isValid method handles null sanely
  (.isValid (org.apache.commons.validator.routines.UrlValidator. ^"[Ljava.lang.String;" (into-array String ["http" "https"])) s))

(defn simplify-uri
  "Simplifies a URI (which can be a string, java.net.URL, or java.net.URI) if
  possible, returning a String. Returns nil if the input is nil or blank."
  [uri]
  (let [uri (str uri)]
    (when-not (s/blank? uri)
      (let [luri (s/lower-case (s/trim uri))]
        (if (valid-http-uri? luri)
          (-> luri
              (s/replace #"\Ahttps?://(www\.)?"     "http://")    ; Normalise to http and strip any www. extension on hostname
              (s/replace #"licen[cs]es?"            "license")    ; Alternative spelling and plurals of "license"
              (s/replace #"\.\p{Alpha}\p{Alnum}*\z" "")           ; Strip file type extension (if any)
              (s/replace #"/+\z"                    ""))          ; Strip all trailing forward slash (/) characters
          luri)))))

(defn readable-dir?
  "Is d (a String or File) a readable directory?"
  [d]
  (let [d (io/file d)]
    (and d
         (.exists d)
         (.canRead d)
         (.isDirectory d))))

(defmulti readable-file?
  "Is f (a String, File, InputStream, or Reader) a readable file?"
  class)

(defmethod readable-file? nil
  [_])

(defmethod readable-file? java.io.File
  [^java.io.File f]
  (and f
       (.exists f)
       (.canRead f)
       (not (.isDirectory f))))

(defmethod readable-file? java.lang.String
  [s]
  (or (valid-http-uri? s)
      (readable-file? (io/file s))))

(defmethod readable-file? java.io.InputStream
  [_]
  true)

(defmethod readable-file? java.io.Reader
  [_]
  true)

(defmethod readable-file? java.net.URL
  [_]
  true)

(defmethod readable-file? java.net.URI
  [_]
  true)

(defmulti filepath
  "Returns the full path and name of the given file-like thing (String, File,
  ZipEntry, URI, URL)."
  class)

(defmethod filepath nil
  [_])

(defmethod filepath java.io.File
  [^java.io.File f]
  (.getPath f))

(defmethod filepath java.lang.String
  [s]
  (when s
    (let [s (s/trim s)]
      (if (valid-http-uri? s)
        (filepath (io/as-url s))
        (filepath (io/file   s))))))

(defmethod filepath java.net.URI
  [^java.net.URI uri]
  (str uri))

(defmethod filepath java.net.URL
  [^java.net.URL url]
  (str url))

(defmethod filepath java.util.zip.ZipEntry
  [^java.util.zip.ZipEntry ze]
  (.getName ze))

(defmethod filepath java.io.InputStream
  [_]
  (throw (ex-info "Cannot determine filepath of an InputStream - did you forget to provide it separately?" {})))

(defmulti filename
  "Returns just the name component of the given file-like thing (String, File,
  ZipEntry, URI, URL), excluding any parents."
  class)

(defmethod filename nil
  [_])

(defmethod filename java.io.File
  [^java.io.File f]
  (.getName f))

(defmethod filename java.lang.String
  [s]
  (when s
    (let [s (s/trim s)]
      (if (valid-http-uri? s)
        (filename (io/as-url s))
        (filename (io/file s))))))

(defmethod filename java.net.URI
  [^java.net.URI uri]
  (filename (.getPath uri)))

(defmethod filename java.net.URL
  [^java.net.URL url]
  (filename (.getPath url)))

(defmethod filename java.util.zip.ZipEntry
  [^java.util.zip.ZipEntry ze]
  (filename (.getName ze)))

(defmethod filename java.io.InputStream
  [_]
  (throw (ex-info "Cannot determine filename of an InputStream - did you forget to provide it separately?" {})))

(defn filter-file-seq*
  "As for clojure.core/file-seq, but with support for filtering.  pred must be
  a predicate that accepts one argument of type java.io.File.  Files for which
  `pred` returns `false` will not be included in the result, and directories for
  which `pred` returns `false` will also not be recused into (so `pred` must be
  able to handle both cases).

  Note also that `dir` is always included in the result, even if `pred` returns
  `false` for it."
  [^java.io.File dir pred]
  (let [pred   (or pred (constantly true))
        filter (reify java.io.FileFilter (accept [_ f] (boolean (pred (.getCanonicalFile ^java.io.File f)))))]  ; Use the canonical file, otherwise we will get tripped up by "." being "hidden" according to the JVM when running on a Unix 🤡
    (tree-seq
      (fn [^java.io.File f] (.isDirectory f))
      (fn [^java.io.File d] (seq (.listFiles d filter)))
      dir)))

(defn filter-file-seq
  "As for `clojure.core/file-seq`, but with support for filtering.  `dir-pred`
  controls which directories will be included in the result and recursed into.
  `file-pred` controls which files will be included in the result.  Both must be
  a predicate of one argument of type `java.io.File`.

  Note also that `dir` is always included in the result, even if `dir-pred`
  returns `false` for it."
  [dir dir-pred file-pred]
  (let [dir-pred  (or dir-pred  (constantly true))
        file-pred (or file-pred (constantly true))
        pred      (fn [^java.io.File f]
                    (or (and (.isDirectory f) (dir-pred f))
                        (file-pred f)))]
    (filter-file-seq* dir pred)))

(defn filter-file-only-seq
  "As for `clojure.core/file-seq`, with support for filtering and only returns
  files (but not any directories that were traversed during the seq).
  `dir-pred` controls which directories will be recursed into. `file-pred`
  controls which files will be included in the result.  Both must be a predicate
  of one argument of type `java.io.File`."
  [dir dir-pred file-pred]
  (seq (filter #(.isFile ^java.io.File %) (filter-file-seq dir dir-pred file-pred))))

(defn getenv
  "Obtain the value of environment variable `var`, returning `default` if it
  isn't set (defaults to `nil` if not specified)."
  ([var] (getenv var nil))
  ([var default]
    (let [val (System/getenv var)]
      (if-not (s/blank? val)
        val
        default))))

; Note: we could use OSHI to determine the actual number of possible open file
; handles on the runtime environment, but it seems like overkill to bring in
; such a large dependency for this one feature, especially when lice-comb
; typically won't get close to opening this many files.
(defn file-handle-bounded-pmap
  "bounded-pmap* hardcoded to no more than 8192 virtual threads. This size is
  determined conservatively from macOS, since it's the least common denominator
  of the major OSes in terms of number of possible open file handles per
  process."
  [f coll]
  (e/bounded-pmap* 8192 f coll))
