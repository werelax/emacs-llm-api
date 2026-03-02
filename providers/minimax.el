;;; minimax.el --- MiniMax provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--minimax (:include llm-api--platform)
                                 (:constructor llm-api--minimax-create)
                                 (:copier nil)))

(defun llm--create-minimax-platform (token &optional selected-model)
  (llm-api--minimax-create
   :name "minimax"
   :url "https://api.minimax.io/v1/chat/completions"
   :token token
   :selected-model (or selected-model "MiniMax-M2.5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
