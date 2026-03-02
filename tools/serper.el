;;; serper.el --- Serper API web tools for llm-api -*- lexical-binding: t; -*-
;;
;; Provides web_search and web_scrape tools using the Serper API.
;; Both tools are fully async via `url-retrieve'.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)

(defvar llm-api--serper-api-key "830cfd0baa93cdfff158921a2d1e805eff651292"
  "API key for the Serper API.")

(defvar llm-api--serper-search-url "https://google.serper.dev/search"
  "URL for Serper search API.")

(defvar llm-api--serper-scrape-url "https://scrape.serper.dev"
  "URL for Serper scrape API.")

;; Register tools via the global registry

(defun llm-api--serper-search (query callback)
  "Search the web for QUERY using Serper API, call CALLBACK with results."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("X-API-KEY" . ,llm-api--serper-api-key)))
        (url-request-data (encode-coding-string
                           (json-encode `((:q . ,query)))
                           'utf-8)))
    (url-retrieve
     llm-api--serper-search-url
     (lambda (status cb)
       (condition-case err
           (if (plist-get status :error)
               (funcall cb (format "[Error: HTTP request failed: %S]"
                                   (plist-get status :error)))
             (goto-char url-http-end-of-headers)
             (let* ((json-str (decode-coding-string
                               (buffer-substring-no-properties (point) (point-max))
                               'utf-8))
                    (json (json-parse-string json-str
                                            :object-type 'plist :array-type 'list))
                    (organic (plist-get json :organic))
                    (results (seq-take organic 5))
                    (formatted
                     (mapconcat
                      (lambda (r)
                        (format "Title: %s\nSnippet: %s\nURL: %s"
                                (or (plist-get r :title) "N/A")
                                (or (plist-get r :snippet) "N/A")
                                (or (plist-get r :link) "N/A")))
                      results
                      "\n\n")))
               (kill-buffer (current-buffer))
               (funcall cb (if (string-empty-p formatted)
                               "[No results found]"
                             formatted))))
         (error
          (ignore-errors (kill-buffer (current-buffer)))
          (funcall cb (format "[Error: %S]" err)))))
     (list callback)
     t)))

(defun llm-api--serper-scrape (url callback)
  "Scrape URL using Serper API, call CALLBACK with page text."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("X-API-KEY" . ,llm-api--serper-api-key)))
        (url-request-data (encode-coding-string
                           (json-encode `((:url . ,url)))
                           'utf-8)))
    (url-retrieve
     llm-api--serper-scrape-url
     (lambda (status cb)
       (condition-case err
           (if (plist-get status :error)
               (funcall cb (format "[Error: HTTP request failed: %S]"
                                   (plist-get status :error)))
             (goto-char url-http-end-of-headers)
             (let* ((json-str (decode-coding-string
                               (buffer-substring-no-properties (point) (point-max))
                               'utf-8))
                    (json (json-parse-string json-str
                                            :object-type 'plist :array-type 'list))
                    (text (or (plist-get json :text)
                              (plist-get json :markdown)
                              (plist-get json :content)
                              "")))
               (kill-buffer (current-buffer))
               (funcall cb (if (> (length text) 15000)
                               (substring text 0 15000)
                             text))))
         (error
          (ignore-errors (kill-buffer (current-buffer)))
          (funcall cb (format "[Error: %S]" err)))))
     (list callback)
     t)))

(llm-api-register-tool
 "web_search"
 (llm-api--make-tool
  "web_search"
  "Search the web using Google. Returns top results with titles, snippets, and URLs."
  `((:type . "object")
    (:properties . ((:query . ((:type . "string")
                                (:description . "The search query")))))
    (:required . ["query"])))
 (lambda (parsed-args callback)
   (llm-api--serper-search (plist-get parsed-args :query) callback)))

(llm-api-register-tool
 "web_scrape"
 (llm-api--make-tool
  "web_scrape"
  "Scrape a web page and return its text content."
  `((:type . "object")
    (:properties . ((:url . ((:type . "string")
                              (:description . "The URL to scrape")))))
    (:required . ["url"])))
 (lambda (parsed-args callback)
   (llm-api--serper-scrape (plist-get parsed-args :url) callback)))

(provide 'llm-api-serper)
;;; serper.el ends here
