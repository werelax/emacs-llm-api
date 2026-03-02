;;; minimax.el --- MiniMax provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--minimax (:include llm-api--openai)
                                 (:constructor llm-api--minimax-create)
                                 (:copier nil)))

;; MiniMax doesn't have a /models endpoint, so we return a static list
(cl-defmethod llm-api--get-available-models ((_platform llm-api--minimax))
  "Return static list of MiniMax models."
  '("MiniMax-M2.5"
    "MiniMax-M2.5-highspeed"
    "MiniMax-M2.1"
    "MiniMax-M2.1-highspeed"
    "MiniMax-M2"))

(defun llm--create-minimax-platform (token &optional selected-model)
  (llm-api--minimax-create
   :name "minimax"
   :url "https://api.minimax.io/v1/chat/completions"
   :token token
   :selected-model (or selected-model "MiniMax-M2.5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
