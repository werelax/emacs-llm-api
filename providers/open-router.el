;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--open-router (:include llm-api--platform)
                                    (:constructor llm-api--open-router-create)
                                    (:copier nil)))

;; dynamic model list

(defvar *open-router-models* nil)

(defvar *open-router-price-labels*
  `((0 . :free)
    (0.5 . :cheap)
    (1 . :medium)
    (2 . :costly)
    (5 . :expensive)
    (,most-positive-fixnum . :prohibitive)))

(defun llm-api--open-router-get-price-label (price)
  (let ((price-pair (-find (lambda (p) (<= price (car p)))
                           *open-router-price-labels*)))
    (cdr price-pair)))

(defun llm-api--open-router-refresh-models ()
  (interactive)
  (spinner-start)
  (let* ((url "https://openrouter.ai/api/v1/models")
         (response (plz 'get url :as #'json-read))
         (models-data (alist-get 'data response))
         ;; process the data a little bit
         (models (mapcar (lambda (m)
                           ;; :id must be called :model
                           (let* ((id (alist-get 'id m))
                                  (pricing (alist-get 'pricing m))
                                  (prompt (string-to-number (alist-get 'prompt pricing)))
                                  (completion (string-to-number (alist-get 'completion pricing)))
                                  (mean-1M (* (/ (+ prompt completion) 2) 1000000))
                                  (price-label (llm-api--open-router-get-price-label mean-1M)))
                             `(:model ,id
                               :name ,(concat (format "$%0.2f [%s] " mean-1M price-label)
                                              (alist-get 'name m)))))
                         models-data)))
    (spinner-stop)
    (setq *open-router-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--open-router))
  (or *open-router-models* (llm-api--open-router-refresh-models))
  (setf (llm-api--platform-available-models platform) *open-router-models*)
  (mapcar (lambda (m) (plist-get m :name)) *open-router-models*))

;; {"id":"gen-zzxbiiFVvze5xbXnax7rOufQPNkr","model":"databricks/dbrx-instruct","object":"chat.completion.chunk","created":1712077962,
;; "choices":[{"index":0,"delta":{"role":"assistant","content":"\n"},"finish_reason":null}]}’

;; (cl-defmethod llm-api--response-filter ((platform llm-api--open-router) on-data _process output)
;;   (let ((lines (split-string output "?\n")))
;;     (dolist (line lines)
;;       (when (string-prefix-p  "data: " line)
;;         (setq line (substring line (length "data: ")))
;;         ;; (message "llm-api--response-filter DATA-LINE: '%s'" line)
;;         (when (and (not (string-empty-p line))
;;                    (not (string= line "[DONE]")))
;;           (let ((chunk (json-parse-string line :object-type 'plist :array-type 'list)))
;;             ;; the ONLY think different from the default implementation is:
;;             ;;  - this line: (string= "chat.completion" (plist-get chunk :object)))
;;             ;;  - becomes: (eq nil (plist-get chunk :finish_reason)))
;;             ;; becuase the response is not fully compatible :/
;;             (when (and (listp chunk)
;;                        (eq nil (plist-get chunk :finish_reason)))
;;               (let ((choices (plist-get chunk :choices)))
;;                 (when (and (listp choices)
;;                            (> (length choices) 0))
;;                   (let* ((choice (car choices))
;;                          (delta (plist-get choice :delta))
;;                          (finish-reason (plist-get choice :finish_reason))
;;                          (content-delta (plist-get delta :content)))
;;                     ;; store last response (full response on last filter call)
;;                     (message "* FINISH: %s" chunk)
;;                     (when (stringp content-delta)
;;                       (cl-callf concat (llm-api--platform-last-response platform) content-delta))
;;                     ;; stream the deltas
;;                     (when (and (stringp content-delta)
;;                                (functionp on-data))
;;                       (funcall on-data content-delta))))))))))))

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--open-router))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :transforms) '("middle-out"))
    (setf (plist-get payload :max_tokens) (* 128 1024))
    ;; (setf (plist-get payload :max_tokens) 4000)
    payload))

(defun llm--create-open-router-platform (token)
  (llm-api--open-router-create
   :name "open-router"
   :url "https://openrouter.ai/api/v1/chat/completions"
   :token token
   :selected-model "gryphe/mythomist-7b"
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
