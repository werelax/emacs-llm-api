;;; nano-gpt.el --- NanoGPT provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--nano-gpt (:include llm-api--platform)
                                  (:constructor llm-api--nano-gpt-create)
                                  (:copier nil)))

;; NanoGPT requires Accept: text/event-stream for SSE streaming
(cl-defmethod llm-api--get-request-headers ((_platform llm-api--nano-gpt))
  '("Content-Type: application/json"
    "Accept: text/event-stream"))

(defun llm--create-nano-gpt-platform (token &optional selected-model)
  (llm-api--nano-gpt-create
   :name "nano-gpt"
   :url "https://nano-gpt.com/api/v1/chat/completions"
   :token token
   :selected-model (or selected-model "deepseek-v3.2")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
