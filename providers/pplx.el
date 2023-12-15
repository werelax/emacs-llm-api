;;; -*- lexical-binding: t; -*-

(defun llm--create-pplx-platform (token)
  (llm-api--platform-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("codellama-34b-instruct"
                       "llama-2-70b-chat"
                       "mistral-7b-instruct"
                       "mixtral-8x7b-instruct"
                       "pplx-7b-chat"
                       "pplx-70b-chat"
                       "pplx-7b-online"
                       "pplx-70b-online")
   :selected-model "openhermes-2.5-mistral-7b"
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.2)))
