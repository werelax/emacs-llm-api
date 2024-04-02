;;; -*- lexical-binding: t; -*-

(defun llm--create-pplx-platform (token)
  (llm-api--platform-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("sonar-small-chat"
                       "sonar-small-online"
                       "sonar-medium-chat"
                       "sonar-medium-online"
                       "mistral-7b-instruct"
                       "mixtral-8x7b-instruct"
                       "codellama-70b-instruct")
   :selected-model "sonar-medium-online"
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.2)))
