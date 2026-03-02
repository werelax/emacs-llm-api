;;; kimi.el --- Kimi (Moonshot AI) provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--kimi (:include llm-api--openai)
                              (:constructor llm-api--kimi-create)
                              (:copier nil)))

(defun llm--create-kimi-platform (token &optional selected-model)
  (llm-api--kimi-create
   :name "kimi"
   :url "https://api.moonshot.ai/v1/chat/completions"
   :token token
   :selected-model (or selected-model "kimi-k2.5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.6)))
