;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; TODO: write similar functions for other popular chat formats (ChatML, etc, ...)
;; TODO: add more together.ai models (with their corresponding prompts formatting)

(cl-defstruct (llm-api--prompt-format
               (:constructor llm-api--prompt-format-create)
               (:copier nil))
  system-prompt
  (system-turn-format "SYSTEM: %s")
  (user-turn-format "USER: %s")
  (assistant-turn-format "ASSISTANT: %s")
  (separator "\n\n")
  turn-separator
  turn-prefix
  turn-suffix
  prompt-suffix)

(cl-defstruct (llm-api--together-ai
               (:include llm-api--platform)
               (:constructor llm-api--together-ai-create)
               (:copier nil)))

(defun llm-api--format-history (system-prompt history prompt-format)
  "Format SYSTEM-PROMPT and HISTORY according to PROMPT-FORMAT."
  (let ((default-system-prompt (llm-api--prompt-format-system-prompt prompt-format))
        (system-turn-format (llm-api--prompt-format-system-turn-format prompt-format))
        (user-turn-format (llm-api--prompt-format-user-turn-format prompt-format))
        (assistant-turn-format (llm-api--prompt-format-assistant-turn-format prompt-format))
        (separator (llm-api--prompt-format-separator prompt-format))
        (turn-prefix (llm-api--prompt-format-turn-prefix prompt-format))
        (turn-suffix (llm-api--prompt-format-turn-suffix prompt-format))
        (turn-separator (llm-api--prompt-format-turn-separator prompt-format))
        (prompt-suffix (llm-api--prompt-format-prompt-suffix prompt-format)))
    (when-let ((system (or system-prompt default-system-prompt)))
      (setq history (cons `(:role :system :content ,system) history)))
    ;; TODO: turn-prefix, turn-suffix, turn-separator
    ;; TODO: must limit turn history to :seq-length / 2 (estimating tokens), then preppend system prompt
    (let ((prompt (mapconcat (lambda (turn)
                               (let ((role (cdar turn))
                                     (text (cdadr turn)))
                                 (cond ((eq role :user) (format user-turn-format text))
                                       ((eq role :assistant) (format assistant-turn-format text))
                                       ((eq role :system) (format system-turn-format text))
                                       (t ""))))
                             history separator)))
      ;; one more time, after last turn
      (setq prompt (concat prompt separator))
      ;; return value
      (concat prompt (if (stringp prompt-suffix)
                         prompt-suffix
                       (format assistant-turn-format ""))))))



(cl-defmethod llm-api--get-history ((platform llm-api--together-ai) prompt-format)
  (llm-api--format-history (llm-api--platform-system-prompt platform)
                           (llm-api--platform-history platform)
                           prompt-format))

;; "model": "togethercomputer/RedPajama-INCITE-7B-Instruct",
;; "prompt": "The capital of France is",
;; "max_tokens": 128,
;; "stop": ".",
;; "temperature": 0.7,
;; "top_p": 0.7,
;; "top_k": 50,
;; "repetition_penalty": 1

(defun llm-api--together-ai-words-to-tokens (n-words)
  "Terrible approximation of how many tokens N-WORDS is."
  (* n-words 0.65))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--together-ai))
  ;; (message "\n> history: %s\n\n" (llm-api--get-history platform))
  (let* ((params (llm-api--platform-params platform))
         (model (llm-api--platform-selected-model platform))
         (model-params (plist-get model :params))
         (model-id (if (stringp model) model (plist-get model :model)))
         (get-param (lambda (key) (or (plist-get model-params key)
                                      (plist-get model key)
                                      (plist-get params key))))
         (prompt-format (funcall get-param :prompt-format))
         (temperature (funcall get-param :temperature))
         (token-length (funcall get-param :seq-length))
         (stop-string (funcall get-param :stop-string)))
    ;; (message ">> %s" (llm-api--get-history platform prompt-format))
    `(:model ,model-id
      :prompt ,(llm-api--get-history platform prompt-format)
      :temperature ,temperature
      ,@(when-let ((stop (funcall get-param :stop))) `(:stop ,stop))
      ,@(when-let ((top-p (funcall get-param :top-p))) `(:top_p ,top-p))
      ,@(when-let ((top-k (funcall get-param :top-k))) `(:top_k ,top-k))
      ,@(when-let ((repetition-penalty (funcall get-param :repetition-penalty))) `(:repetition_penalty ,repetition-penalty))
      ;; TODO: this is a shitty calculation
      :max_tokens ,(/ (or token-length 4000) 3)
      :stop ,(or stop-string "</s>")
      :stream t)))

(cl-defmethod llm-api--response-filter ((platform llm-api--together-ai) on-data _process output)
  ;; (message ">> %s output" output)
  (let ((lines (split-string output "?\n")))
    (dolist (line lines)
      (when (string-prefix-p  "data: " line)
        (setq line (substring line (length "data: "))))
      (when (and (not (string-empty-p line))
                 (not (string= line "[DONE]")))
        (let ((chunk (json-parse-string line :object-type 'plist :array-type 'list)))
          (let ((choices (plist-get chunk :choices)))
            (when (and (listp choices)
                       (> (length choices) 0))
              (let* ((choice (car choices))
                     (text (plist-get choice :text)))
                ;; store last response (full response on last filter call)
                (when (stringp text)
                  (cl-callf concat (llm-api--together-ai-last-response platform) text)
                  ;; stream the deltas
                  (when (functionp on-data)
                    (funcall on-data text)))))))))))

(cl-defmethod llm-api--add-generated-message-to-history ((platform llm-api--together-ai))
  (let* ((last-msg-content (llm-api--platform-last-response platform))
         (msg `((:role . :assistant) (:content . ,last-msg-content))))
    (llm-api--add-to-history platform msg)
    (setf (llm-api--together-ai-last-response platform) nil)))

(setq llm-api--alpaca-prompt-format
      (llm-api--prompt-format-create :system-prompt nil
                                     :user-turn-format "### User:\n%s"
                                     :assistant-turn-format "### Assistant:\n%s"
                                     :system-turn-format "### System:\n%s\n"
                                     :separator "\n\n"
                                     :prompt-suffix "### Assistant:\n"))

(setq llm-api--mistral-instruct-prompt-format
      (llm-api--prompt-format-create :system-prompt nil
                                     :user-turn-format "<s>[INST] %s [/INST]"
                                     :assistant-turn-format "%s</s> "
                                     :system-turn-format ""
                                     :separator ""
                                     :prompt-suffix ""))


(setq llm-api--stripped-hyena-nous-prompt-format
      ;; ### Instruction:\n{prompt}\n\n### Response:\n{response}
      (llm-api--prompt-format-create :system-prompt nil
                                     :user-turn-format "### Instruction:\n%s\n\n"
                                     :assistant-turn-format "### Response:%s\n"
                                     :system-turn-format ""
                                     :separator ""
                                     :prompt-suffix "### Response:"))

(defun llm--create-together-ai-platform (token)
  (llm-api--together-ai-create
   :token token
   :name "together.ai"
   :url "https://api.together.xyz/inference"
   :available-models
   `((:name "SOLAR v0 (70B)" :model "upstage/SOLAR-0-70b-16bit" :seq-length 4096)
     (:name "Stripped Hyena Nous (7B)" :model "togethercomputer/StripedHyena-Nous-7B"
      :seq-length 32768 :prompt-format ,llm-api--stripped-hyena-nous-prompt-format
      :stop "###")
     (:name "Alpaca (7B)" :model "togethercomputer/alpaca-7b" :seq-length 2048)
     (:name "Chronos Hermes (13B)" :model "Austism/chronos-hermes-13b" :seq-length 2048)
     (:name "Code Llama Instruct (13B)" :model "togethercomputer/CodeLlama-13b-Instruct" :seq-length 8192)
     (:name "Code Llama Instruct (34B)" :model "togethercomputer/CodeLlama-34b-Instruct" :seq-length 8192)
     (:name "Code Llama Instruct (7B)" :model "togethercomputer/CodeLlama-7b-Instruct" :seq-length 8192)
     (:name "UAE Falcon Instruct (40B)" :model "togethercomputer/falcon-40b-instruct" :seq-length 2048)
     (:name "UAE Falcon Instruct (7B)" :model "togethercomputer/falcon-7b-instruct" :seq-length 2048)
     (:name "GPT-NeoXT-Chat-Base (20B)" :model "togethercomputer/GPT-NeoXT-Chat-Base-20B" :seq-length 2048)
     (:name "LLaMA-2 Chat (13B)" :model "togethercomputer/llama-2-13b-chat" :seq-length 4096)
     (:name "LLaMA-2 Chat (70B)" :model "togethercomputer/llama-2-70b-chat" :seq-length 4096)
     (:name "LLaMA-2 Chat (7B)" :model "togethercomputer/llama-2-7b-chat" :seq-length 4096)
     (:name "LLaMA-2-7B-32K-Instruct (7B)" :model "togethercomputer/Llama-2-7B-32K-Instruct" :seq-length 32768)
     (:name "Mistral (7B) Instruct"  :model "mistralai/Mistral-7B-Instruct-v0.1"
      :seq-length 4096 :prompt-format ,llm-api--mistral-instruct-prompt-format)
     (:name "Mixtral (8x7B) Instruct"  :model "mistralai/Mixtral-8x7B-Instruct-v0.1"
      :seq-length 32768 :prompt-format ,llm-api--mistral-instruct-prompt-format)
     (:name "MythoMax-L2 (13B)" :model "Gryphe/MythoMax-L2-13b" :seq-length 4096)
     (:name "Nous Hermes LLaMA-2 (7B)" :model "NousResearch/Nous-Hermes-llama-2-7b" :seq-length 4096)
     (:name "Research Nous Hermes Llama-2 (13B)" :seq-length 4096)
     (:name "OpenHermes-2-Mistral (7B)" :model "teknium/OpenHermes-2-Mistral-7B" :seq-length 4096)
     (:name "OpenHermes-2.5-Mistral (7B)" :model "teknium/OpenHermes-2p5-Mistral-7B" :seq-length 4096)
     (:name "OpenOrca Mistral (7B) 8K"  :model "Open-Orca/Mistral-7B-OpenOrca" :seq-length 8192)
     (:name "Platypus2 Instruct (70B)" :model "garage-bAInd/Platypus2-70B-instruct" :seq-length 4096)
     (:name "Pythia-Chat-Base (7B)" :model "togethercomputer/Pythia-Chat-Base-7B-v0.16" :seq-length 2048)
     (:name "Qwen-Chat (7B)" :model "togethercomputer/Qwen-7B-Chat" :seq-length 8192)
     (:name "RedPajama-INCITE Chat (3B)" :model "togethercomputer/RedPajama-INCITE-Chat-3B-v1" :seq-length 2048)
     (:name "RedPajama-INCITE Chat (7B)" :model "togethercomputer/RedPajama-INCITE-7B-Chat" :seq-length 2048)
     (:name "Sys Vicuna v1.5 (13B)" :model "lmsys/vicuna-13b-v1.5" :seq-length 4096)
     (:name "UAE Falcon (40B)" :model "togethercomputer/falcon-40b" :seq-length 2048)
     (:name "UAE Falcon (7B)" :model "togethercomputer/falcon-7b" :seq-length 2048)
     (:name "GPT-JT (6B)" :model "togethercomputer/GPT-JT-6B-v1" :seq-length 2048)
     (:name "GPT-JT-Moderation (6B)" :model "togethercomputer/GPT-JT-Moderation-6B" :seq-length 2048)
     (:name "LLaMA (65B)" :model "huggyllama/llama-65b" :seq-length 2048)
     (:name "LLaMA-2 (13B)" :model "togethercomputer/llama-2-13b" :seq-length 4096)
     (:name "LLaMA-2 (70B)" :model "togethercomputer/llama-2-70b" :seq-length 4096)
     (:name "LLaMA-2 (7B)" :model "togethercomputer/llama-2-7b" :seq-length 4096)
     (:name "LLaMA-2-32K (7B)" :model "togethercomputer/LLaMA-2-7B-32K" :seq-length 32768)
     (:name "Llemma (7B)" :model "EleutherAI/llemma_7b" :seq-length 4096)
     (:name "ML MPT (30B)" :model "togethercomputer/mpt-30b" :seq-length 2048)
     (:name "Mistral (7B)" :model "mistralai/Mistral-7B-v0.1" :seq-length 4096)
     (:name "Qwen (7B)" :model "togethercomputer/Qwen-7B" :seq-length 8192)
     (:name "RedPajama-INCITE (3B)" :model "togethercomputer/RedPajama-INCITE-Base-3B-v1" :seq-length 2048)
     (:name "RedPajama-INCITE (7B)" :model "togethercomputer/RedPajama-INCITE-7B-Base" :seq-length 2048)
     (:name "RedPajama-INCITE Instruct (3B)" :seq-length 2048)
     (:name "RedPajama-INCITE Instruct (7B)" :seq-length 2048)
     (:name "WizardLM v1.0 (70B)" :model "WizardLM/WizardLM-70B-V1.0" :seq-length 4096))
   :params `(:temperature 0.2
             :prompt-format ,llm-api--alpaca-prompt-format)))
