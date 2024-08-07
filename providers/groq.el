;;; ../sync/doom.d/lib/llm-api/providers/groq.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--groq (:include llm-api--platform)
                             (:constructor llm-api--groq-create)
                             (:copier nil)))

(defvar *groq-models* nil "List of available models.")

(defun llm-api--groq-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.groq.com/openai/v1/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *groq-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--groq))
  (or *groq-models* (llm-api--groq-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *groq-models*)
  *groq-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--groq))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) nil)
    payload))

(defun llm--create-groq-platform (token &optional selected-model)
  (llm-api--groq-create
   :name "groq"
   :url "https://api.groq.com/openai/v1/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *groq-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
