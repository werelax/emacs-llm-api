;;; ../sync/doom.d/lib/llm-api/providers/togetherai.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--openai (:include llm-api--platform)
                               (:constructor llm-api--openai-create)
                               (:copier nil))
  (models-cache nil))  ; Per-instance cache for available models

(defun llm-api--openai-refresh-models (platform)
  "Fetch available models from the provider's /models endpoint."
  (interactive)
  (spinner-start)
  (let* ((base-url (llm-api--platform-url platform))
         ;; Derive models URL from chat completions URL
         ;; e.g., https://api.example.com/v1/chat/completions -> https://api.example.com/v1/models
         (models-url (replace-regexp-in-string "/chat/completions.*" "/models" base-url))
         (response (plz 'get models-url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setf (llm-api--openai-models-cache platform) models)
    models))

(cl-defmethod llm-api--get-available-models ((platform llm-api--openai))
  "Return cached models or fetch from API if not available."
  (or (llm-api--openai-models-cache platform)
      (llm-api--openai-refresh-models platform))
  (setf (llm-api--platform-available-models platform)
        (llm-api--openai-models-cache platform))
  (llm-api--openai-models-cache platform))

(cl-defmethod llm-api--invalidate-model-cache ((platform llm-api--openai))
  "Invalidate OpenAI-family model caches for PLATFORM."
  (setf (llm-api--openai-models-cache platform) nil)
  (cl-call-next-method))

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
   :selected-model (or selected-model "gpt-5.2")
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
