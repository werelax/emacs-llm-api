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

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--open-router))
  (let ((payload (cl-call-next-method platform)))
    (plist-put payload :transforms '("middle-out"))
    (plist-put payload :n 512)
    payload))

(defun llm--create-open-router-platform (token)
  (llm-api--open-router-create
   :name "open-router"
   :url "https://openrouter.ai/api/v1/chat/completions"
   :token token
   :selected-model "gryphe/mythomist-7b"
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.2)))
