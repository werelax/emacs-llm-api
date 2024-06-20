;;; ../sync/doom.d/lib/llm-api/providers/featherless.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--featherless (:include llm-api--platform)
                                    (:constructor llm-api--featherless-create)
                                    (:copier nil)))

(defvar *featherless-models* nil "List of available models.")

(defun llm-api--featherless-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.featherless.ai/v1/models")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *featherless-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--featherless))
  (or *featherless-models* (llm-api--featherless-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *featherless-models*)
  *featherless-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--featherless))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 4096)
    (message "featherless get request payload %s" payload)
    payload))

(defun llm--create-featherless-platform (token)
  (message "TOKEN %s" token)
  (llm-api--featherless-create
   :name "featherless"
   :url "https://api.featherless.ai/v1"
   :token token
   :selected-model (plist-get (car *featherless-models*) :model)
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
