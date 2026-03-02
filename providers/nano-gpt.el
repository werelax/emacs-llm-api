;;; nano-gpt.el --- NanoGPT provider -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)

(cl-defstruct (llm-api--nano-gpt (:include llm-api--openai)
                                  (:constructor llm-api--nano-gpt-create)
                                  (:copier nil)))

;; NanoGPT requires Accept: text/event-stream for SSE streaming
(cl-defmethod llm-api--get-request-headers ((_platform llm-api--nano-gpt))
  '("Content-Type: application/json"
    "Accept: text/event-stream"))

;; NanoGPT uses a custom endpoint for model listing
(cl-defmethod llm-api--get-available-models ((platform llm-api--nano-gpt))
  "Fetch available models from NanoGPT's subscription endpoint."
  (or (llm-api--openai-models-cache platform)
      (progn
        (spinner-start)
        (let* ((url "https://nano-gpt.com/api/subscription/v1/models")
               (response (plz 'get url
                           :as #'json-read
                           :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
               (models-data (alist-get 'data response))
               (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
          (spinner-stop)
          (setf (llm-api--openai-models-cache platform) models)
          (setf (llm-api--platform-available-models platform) models)
          models))))

(defun llm--create-nano-gpt-platform (token &optional selected-model)
  (llm-api--nano-gpt-create
   :name "nano-gpt"
   :url "https://nano-gpt.com/api/v1/chat/completions"
   :token token
   :selected-model (or selected-model "deepseek-v3.2")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
