;
; Copyright © 2023 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns lice-comb.impl.http
  "HTTP helper functionality.

  Note: this namespace is not part of the public API of lice-comb and may change
  without notice."
  (:require [clojure.string       :as s]
            [clojure.java.io      :as io]
            [hato.client          :as hc]
            [lice-comb.impl.utils :as u]))

(def ^:private user-agent    "https://github.com/pmonks/lice-comb")
(def ^:private http-client-d (delay (hc/build-http-client {:connect-timeout 1000
                                                           :redirect-policy :always
                                                           :cookie-policy   :none})))

(defn uri-resolves?
  "Does `uri` resolve i.e. does the resource it points to exist?

  Notes:

  * Only supports http(s) URIs
  * Uses an HTTP HEAD request
  * Does not throw - returns false on errors"
  [uri]
  (boolean
    (when (u/valid-http-uri? (str uri))
      (try
        (when-let [response (hc/head (str uri)
                                     {:http-client @http-client-d
                                      :header      {"User-Agent" user-agent}})]
          (= 200 (:status response)))
        (catch Exception _
          false)))))

(defn- cdn-uri
  "Converts raw URIs into CDN URIs, for these 'known' hosts:

  * github.com e.g. https://github.com/pmonks/lice-comb/blob/main/LICENSE -> https://raw.githubusercontent.com/pmonks/lice-comb/main/LICENSE

  If the given URI is not known, returns the input unchanged."
  [uri]
  (if-let [^java.net.URL url-obj (try (io/as-url uri) (catch Exception _ nil))]
    (case (s/lower-case (.getHost url-obj))
      "github.com" (if (s/includes? uri "/blob/")
                     (-> uri
                         (s/replace-first #"(?i)github\.com" "raw.githubusercontent.com")
                         (s/replace-first "/blob/"           "/"))
                     uri)
      uri)
    uri))

(defn get-text
  "Attempts to retrieve the content of `uri` as plain text, returning a `String`
  or `nil` if unable to do so (including for error conditions - there is no way
  to disambiguate errors from non-text content, for example).

  Notes:

  * Automatically uses CDN URLs when known (as per `cdn-uri`)
  * HTML responses are automatically converted to plain text (using JSoup)"
  [uri]
  (when (u/valid-http-uri? uri)
    (try
      (when-let [response (hc/get (cdn-uri uri)
                                  {:http-client @http-client-d
                                   :accept      "text/plain;q=1,text/html;q=0.5,application/xhtml+xml;q=0.5,*/*;q=0"
                                   :header      {"User-Agent" user-agent}})]
        (case (:content-type response)
           :text/plain                         (:body response)
           (:text/html :application/xhtml+xml) (u/html->text (:body response))))
      (catch Exception _
        nil))))
