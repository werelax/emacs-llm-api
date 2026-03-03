;;; nano-gpt.el --- NanoGPT provider -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)

(cl-defstruct (llm-api--nano-gpt (:include llm-api--openai)
                                  (:constructor llm-api--nano-gpt-create)
                                  (:copier nil)))

;; NanoGPT requires Accept: text/event-stream for SSE streaming
(cl-defmethod llm-api--get-request-headers ((_platform llm-api--nano-gpt))
  '("Content-Type: application/json"
    "Accept: text/event-stream"))

;; NanoGPT uses a custom endpoint for model listing
(cl-defmethod llm-api--get-available-models ((platform llm-api--nano-gpt))
  "Fetch available models from NanoGPT detailed models endpoint."
  (or (llm-api--openai-models-cache platform)
      (progn
        (spinner-start)
        (let* ((url "https://nano-gpt.com/api/v1/models?detailed=true")
               (response (plz 'get url
                           :as #'json-read
                           :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
               (models-data (alist-get 'data response))
               (included-models
                (cl-remove-if-not
                 (lambda (m)
                   (let* ((subscription (alist-get 'subscription m))
                          (included (alist-get 'included subscription)))
                     (not (memq included '(nil :json-false :false false)))))
                 models-data))
               (models (mapcar (lambda (m)
                                 (let ((id (alist-get 'id m))
                                       (name (alist-get 'name m))
                                       (description (alist-get 'description m))
                                       (context-length (alist-get 'context_length m))
                                       (max-output (alist-get 'max_output_tokens m)))
                                   (list :model id
                                         :name id
                                         :display-name name
                                         :description description
                                         :context-window context-length
                                         :max-output-tokens max-output
                                         :source :provider-api)))
                               included-models)))
          (spinner-stop)
          (setf (llm-api--openai-models-cache platform) models)
          (setf (llm-api--platform-available-models platform) models)
          ;; Keep selected model valid after filtering by subscription.included.
          (let* ((selected (llm-api--platform-selected-model platform))
                 (selected-id (if (stringp selected)
                                  selected
                                (and (consp selected)
                                     (or (plist-get selected :model)
                                         (plist-get selected :name)))))
                 (exists (and selected-id
                              (seq-find (lambda (m)
                                          (or (string= (or (plist-get m :model) "") selected-id)
                                              (string= (or (plist-get m :name) "") selected-id)))
                                        models))))
            (when (and models (not exists))
              (setf (llm-api--platform-selected-model platform) (car models))))
          models)))
  (setf (llm-api--platform-available-models platform)
        (llm-api--openai-models-cache platform))
  (mapcar (lambda (m) (if (stringp m) m (plist-get m :name)))
          (llm-api--openai-models-cache platform)))

(defun llm--create-nano-gpt-platform (token &optional selected-model)
  (llm-api--nano-gpt-create
   :name "nano-gpt"
   :url "https://nano-gpt.com/api/v1/chat/completions"
   :token token
   :selected-model (or selected-model "deepseek-v3.2")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
