;;; ../sync/doom.d/lib/llm-api/providers/infermatic.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--infermatic (:include llm-api--platform)
                                   (:constructor llm-api--infermatic-create)
                                   (:copier nil)))

(defvar *infermatic-models* nil "List of available models.")

(defun llm-api--infermatic-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.totalgpt.ai/v1/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *infermatic-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--infermatic))
  (or *infermatic-models* (llm-api--infermatic-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *infermatic-models*)
  *infermatic-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--infermatic))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 4096)
    payload))

(defun llm--create-infermatic-platform (token &optional selected-model)
  (llm-api--infermatic-create
   :name "infermatic"
   :url "https://api.totalgpt.ai/v1/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *infermatic-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
