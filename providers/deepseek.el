;;; ../sync/doom.d/lib/llm-api/providers/togetherai.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--deepseek (:include llm-api--platform)
                                 (:constructor llm-api--deepseek-create)
                                 (:copier nil)))

(defvar *deepseek-models* nil "List of available models.")

(defun llm-api--deepseek-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.deepseek.com/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *deepseek-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--deepseek))
  (or *deepseek-models* (llm-api--deepseek-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *deepseek-models*)
  *deepseek-models*)

(cl-defmethod llm-api--format-continuation-message ((platform llm-api--deepseek) last-response)
  "Format LAST-RESPONSE as a continuation message for chat history."
  `((:role . :assistant) (:content . ,last-response) (:prefix . t)))

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--deepseek))
  (let ((payload (cl-call-next-method platform)))
    ;; (setf (plist-get payload :max_tokens) 16384)
    payload))

(defun llm--create-deepseek-platform (token &optional selected-model)
  (llm-api--deepseek-create
   :name "deepseek"
   :url "https://api.deepseek.com/beta/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *deepseek-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '()))
