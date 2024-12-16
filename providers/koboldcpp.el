;;; ../sync/doom.d/lib/llm-api/providers/koboldcpp.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--koboldcpp (:include llm-api--platform)
                                  (:constructor llm-api--koboldcpp-create)
                                  (:copier nil)))

(defvar *koboldcpp-models* nil "List of available models.")

(defun llm-api--koboldcpp-refresh-models (_)
  (interactive)
  (spinner-start)
  (let* ((url "https://localhost:50001/api/v1/models")
         (response (plz 'get url :as #'json-read))
         (models-data (alist-get 'data response))
         (models (mapcar (lambda (m) (alist-get 'id m)) models-data)))
    (spinner-stop)
    (setq *koboldcpp-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--koboldcpp))
  (or *koboldcpp-models* (llm-api--koboldcpp-refresh-models platform))
  (setf (llm-api--platform-available-models platform) *koboldcpp-models*)
  *koboldcpp-models*)

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--koboldcpp))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 4096)
    payload))

(defun llm--create-koboldcpp-platform (_ &optional selected-model)
  (llm-api--koboldcpp-create
   :name "koboldcpp"
   :url "https://localhost:5001/api/v1/models"
   :selected-model (or selected-model (plist-get (car *koboldcpp-models*) :model))
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
