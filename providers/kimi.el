;;; kimi.el --- Kimi (Moonshot AI) provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--kimi (:include llm-api--openai)
                              (:constructor llm-api--kimi-create)
                              (:copier nil)))

;; Kimi For Coding requires a specific User-Agent header
(cl-defmethod llm-api--get-request-headers ((_platform llm-api--kimi))
  '("Content-Type: application/json"
    "Accept: application/json"
    "User-Agent: claude-code/0.1.0"))

(defun llm--create-kimi-platform (token &optional selected-model)
  (llm-api--kimi-create
   :name "kimi"
   :url "https://api.kimi.com/coding/v1/chat/completions"
   :token token
   :selected-model (or selected-model "kimi-for-coding")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.6)))
