;
; Copyright © 2026 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.license-detection.uris
  "URI-based license detection.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string                                            :as s]
            [wreck.api                                                 :as re]
            [rencg.api                                                 :as ncg]
            [spdx.identifiers                                          :as si]
            [lice-comb.impl.spdx                                       :as lcis]
            [lice-comb.impl.http                                       :as lcihttp]
            [lice-comb.impl.utils                                      :as lciu]
            [lice-comb.impl.info-maps                                  :as im]
            [lice-comb.impl.faux-parse                                 :as faux]
            [lice-comb.impl.regex-fragments                            :as ref]
            [lice-comb.impl.license-detection.spdx-matching-guidelines :as spdx-matching]))

; An approximately correct regex for finding http URIs in larger texts - loosely based on https://www.rfc-editor.org/rfc/rfc3986#appendix-B
; Note: not all matches that this regex finds are valid URIs as per RFC-3986 - if that level of validity is needed, use lciu/valid-http-uri?
(def ^:private re-uri (re/fgrp "i"
                               ref/nwb
                               (re/grp     (re/ncg "scheme" #"https?") #"://")
                               (re/ncg     "address" #"[^/?#\s]+")
                               (re/opt-ncg "path"    #"\/[^?#\s]*")
                               (re/opt-grp #"\?" (re/opt-ncg "queryString" #"[^#\s]*"))
                               (re/opt-grp #"#"  (re/opt-ncg "fragment"    #"[^\s]*"))
                               ref/nwa))

(defn- match-well-known-uri
  "Match other well known license URIs that aren't included in the SPDX license
  list's `:see-also` field, based solely on the URI itself."
  [^String uri]
  (when-let [suri (lciu/simplify-uri uri)]
    (when-let [uri-components (ncg/re-matches re-uri suri)]
      (let [host (s/lower-case (get uri-components "address"))]
        (cond
          ; choosealicense.com
          (= host "choosealicense.com")
            (when-let [path (get uri-components "path")]
              (let [path-elements (filter (complement s/blank?) (s/split path #"/"))]
                (when (and (>= (count path-elements) 2)
                           (re-matches #"(?i:licen[cs]es?)" (first path-elements)))
                  (si/canonicalise (second path-elements)))))  ; URI pattern is https://choosealicense.com/licenses/${id}

          ; *.mit-license.org
          (s/ends-with? host "mit-license.org")
            "MIT")))))

;####TODO: CONSIDER MAKING THIS CONFIGURABLE
; Set of allowed hosts
(def ^:private host-download-allow-list #{
  "github.com"
  "gitlab.com"
  "codeberg.org"})

;####TODO: CONSIDER MAKING THIS CONFIGURABLE
; Set of allowed host suffixes (hostnames that end in these values)
(def ^:private host-suffix-download-allow-list #{
;  "sourceforge.net"  ; SourceForge is a mess...
  "github.io"
  "sr.ht"})  ; SourceHut repo hosting

(defn- download-allowed?
  "Are we allowed to download from `uri`?"
  [uri]
  (when-let [suri (lciu/simplify-uri uri)]
    (when-let [uri-components (ncg/re-matches re-uri suri)]
      (let [host (get uri-components "address")]
        (or (contains? host-download-allow-list host)
            (some (partial s/ends-with? host) host-suffix-download-allow-list))))))

(defn- attempt-to-download-and-match
  "If `uri` is in the allowlist, attempts to download it as plain text and
  perform SPDX matching on it."
  [uri]
  (when (download-allowed? uri)
    (when-let [license-text (lcihttp/get-text uri)]
      (map #(im/prepend-source-to-fim uri %) (spdx-matching/text->fragment-infos license-text)))))  ; Note: spdx-matching/text->fragment-infos returns a sequence of fragment info maps

(defn uri->fragment-infos
  "Returns a sequence of fragment info maps for the given URI, or `nil` if
  `uri` is blank.

  `attempt-download-and-match?` controls whether URIs are downloaded and SPDX
  matching attempted on the content if they're not found in the SPDX license
  list first

  Notes:

  * This returns a sequence because some URIs can return multiple matches (e.g.
    if the content is downloaded and found to contain multiple licenses)
  * This is public because URIs can be detected both directly, and from within
    parsing of a name."
  [^String uri ^Boolean attempt-download-and-match?]
  (when-not (s/blank? uri)
    (or
      ; 1. Is the URI a close match for any of the URIs in the SPDX license or exception lists?
      (when-let [id (lcis/best-near-match-uri uri)]
        (list (im/fragment-info id uri "SPDX listed URI near match")))

      ; 2. Is the URI a close match for any of the "well known" URIs we additionally support?  (choosealicense, etc.)
      (when-let [id (match-well-known-uri uri)]
        (list (im/fragment-info id uri "Well known license URI near match")))

      ; 3. Optionally attempt to retrieve the text/plain contents of the uri and perform license text matching on it (expensive, so off by default)
      (when attempt-download-and-match?
        (attempt-to-download-and-match uri))

      ; 4. Return an unidentified ei (inside a list)
      (list (im/fragment-info (lcis/name->unidentified-license-ref uri) uri "Unidentified URI")))))

(defn detect
  "Detects any URIs found inside the `String`s in `coll` and replaces them with
  one or more fragment info maps in that location. Returns other elements
  unchanged.

  `attempt-download-and-match?` controls whether URIs may be downloaded and SPDX
  matching attempted on the content (if they're not found in the SPDX license
  list, or lice-comb's own 'well known URI' list first)."
  [coll ^Boolean attempt-download-and-match?]
  (faux/parse coll re-uri #(uri->fragment-infos (:match %) attempt-download-and-match?)))
