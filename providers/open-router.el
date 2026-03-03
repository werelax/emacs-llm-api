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
                                  (price-label (llm-api--open-router-get-price-label mean-1M))
                                  (ctx (alist-get 'context_length m))
                                  (top-provider (alist-get 'top_provider m))
                                  (max-out (or (alist-get 'max_completion_tokens top-provider)
                                               (alist-get 'max_output_tokens top-provider))))
                             `(:model ,id
                               :name ,(concat (format "$%0.2f [%s] " mean-1M price-label)
                                              (alist-get 'name m))
                               :context-window ,ctx
                               :max-output-tokens ,max-out
                               :source :provider-api)))
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
    (setf (plist-get payload :transforms) '("middle-out"))
    (setf (plist-get payload :max_tokens) (* 128 1024))
    ;; (setf (plist-get payload :max_tokens) 4000)
    payload))

(defun llm--create-open-router-platform (token &optional selected-model)
  (llm-api--open-router-create
   :name "open-router"
   :url "https://openrouter.ai/api/v1/chat/completions"
   :token token
   :selected-model (or selected-model "anthropic/claude-3.5-sonnet")
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))

;; REASONING HELPERS

(defun my/llm-openrouter-set-reasoning (effort &optional max-tokens exclude)
  "Enable reasoning tokens for OpenRouter with EFFORT level.
EFFORT can be 'high', 'medium', or 'low'.
MAX-TOKENS specifies exact token allocation (alternative to effort).
EXCLUDE when t uses reasoning internally without returning it."
  (interactive (list (completing-read "Reasoning effort: " '("high" "medium" "low") nil t)))
  (let ((reasoning-config (cond
                           (max-tokens `(:max_tokens ,max-tokens))
                           (effort `(:effort ,effort))))
        (platform llm-chat--active-platform))
    (when exclude
      (plist-put reasoning-config :exclude t))
    (plist-put (llm-api--platform-params platform) :reasoning reasoning-config)
    (message "OpenRouter reasoning set to: %s" reasoning-config)))

(defun my/llm-openrouter-disable-reasoning ()
  "Disable reasoning tokens for OpenRouter."
  (interactive)
  (let ((params (llm-api--platform-params llm-chat--active-platform)))
    (when (plist-member params :reasoning)
      (setq params (cl-loop for (key value) on params by #'cddr
                            unless (eq key :reasoning)
                            collect key and collect value))
      (setf (llm-api--platform-params llm-chat--active-platform) params)
      (message "OpenRouter reasoning disabled"))))
