;;; zai.el --- Z.AI (Zhipu) provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--zai (:include llm-api--openai)
                             (:constructor llm-api--zai-create)
                             (:copier nil)))

(defun llm--create-zai-platform (token &optional selected-model)
  (llm-api--zai-create
   :name "zai"
   :url "https://api.z.ai/api/coding/paas/v4/chat/completions"
   :token token
   :selected-model (or selected-model "glm-5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
