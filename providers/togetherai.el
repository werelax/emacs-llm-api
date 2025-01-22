;;; ../sync/doom.d/lib/llm-api/providers/togetherai.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--togetherai (:include llm-api--platform)
                                   (:constructor llm-api--togetherai-create)
                                   (:copier nil)))

(defvar *togetherai-models* nil "List of available models.")

(defun llm-api--togetherai-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.together.xyz/v1/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models (mapcar (lambda (m) (alist-get 'id m)) response)))
    (spinner-stop)
    (setq *togetherai-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--togetherai))
  (or *togetherai-models* (llm-api--togetherai-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *togetherai-models*)
  *togetherai-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--togetherai))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 16384)
    payload))

(defun llm--create-togetherai-platform (token &optional selected-model)
  (llm-api--togetherai-create
   :name "togetherai"
   :url "https://api.together.xyz/v1/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *togetherai-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
