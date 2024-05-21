;;; -*- lexical-binding: t; -*-

(defun llm--create-pplx-platform (token)
  (llm-api--platform-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("llama-3-sonar-small-32k-chat"
                       "llama-3-sonar-small-32k-online"
                       "llama-3-sonar-large-32k-chat"
                       "llama-3-sonar-large-32k-online"
                       "llama-3-8b-instruct"
                       "llama-3-70b-instruct"
                       "mixtral-8x7b-instruct"
                       "mixtral-8x22b-instruct")
   :selected-model "sonar-medium-online"
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
