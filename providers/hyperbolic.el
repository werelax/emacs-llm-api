;;; ../sync/doom.d/lib/llm-api/providers/hyperbolic.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--hyperbolic (:include llm-api--platform)
                                   (:constructor llm-api--hyperbolic-create)
                                   (:copier nil)))

(defvar *hyperbolic-models*
  '("deepseek-ai/DeepSeek-R1"
    "deepseek-ai/DeepSeek-R1-Zero"
    "deepseek-ai/DeepSeek-V3")
  "List of available models.")

(defun llm-api--hyperbolic-refresh-models (platform)
  (interactive)
  (spinner-start)
  (let* ((url "https://api.hyperbolic.xyz/v1/chat/completions")
         (response (plz 'get url
                     :as #'json-read
                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-api--platform-token platform))))))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *hyperbolic-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--hyperbolic))
  (or *hyperbolic-models* (llm-api--hyperbolic-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *hyperbolic-models*)
  *hyperbolic-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--hyperbolic))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 131072)
    payload))

(defun llm--create-hyperbolic-platform (token &optional selected-model)
  (llm-api--hyperbolic-create
   :name "hyperbolic"
   :url "https://api.hyperbolic.xyz/v1/chat/completions"
   :token token
   :selected-model (or selected-model (plist-get (car *hyperbolic-models*) :model))
   :system-prompt ""
   :params '(:temperature 0.7)))
