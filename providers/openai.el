;;; ../sync/doom.d/lib/llm-api/providers/togetherai.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--openai (:include llm-api--platform)
                               (:constructor llm-api--openai-create)
                               (:copier nil)))

(defvar *openai-models* nil "List of available models.")

(defun llm-api--openai-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.openai.com/v1/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *openai-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--openai))
  (or *openai-models* (llm-api--openai-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *openai-models*)
  *openai-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--openai))
  (let ((payload (cl-call-next-method platform)))
    ;; (setf (plist-get payload :max_tokens) 16384)
    payload))

(defun llm--create-openai-platform (token &optional selected-model)
  (llm-api--openai-create
   :name "openai"
   :url "https://api.openai.com/v1/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *openai-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
