;;; kimi.el --- Kimi (Moonshot AI) provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--kimi (:include llm-api--openai)
                              (:constructor llm-api--kimi-create)
                              (:copier nil)))

(defvar *kimi-model-capabilities*
  '(("kimi-for-coding/k2p5"             . (:context-window 262144 :source :provider-default))
    ("kimi-for-coding/kimi-k2-thinking" . (:context-window 262144 :source :provider-default)))
  "Hardcoded Kimi model capabilities for canonical model IDs.")

(defvar *kimi-model-aliases*
  '(("kimi-k2.5" . "kimi-for-coding/k2p5")
    ("kimi-for-coding" . "kimi-for-coding/k2p5"))
  "Aliases for legacy/short Kimi model names.")

(defun llm-api--kimi-normalize-model-id (model-id)
  "Normalize Kimi MODEL-ID aliases to canonical endpoint model IDs."
  (if (not (stringp model-id))
      model-id
    (or (alist-get model-id *kimi-model-aliases* nil nil #'string=)
        model-id)))

(cl-defmethod initialize-instance :after ((platform llm-api--kimi) &rest _)
  "Normalize initial selected model aliases for PLATFORM."
  (let ((selected (llm-api--platform-selected-model platform)))
    (when (stringp selected)
      (setf (llm-api--platform-selected-model platform)
            (llm-api--kimi-normalize-model-id selected)))))

(cl-defmethod llm-api--get-available-models ((platform llm-api--kimi))
  "Return Kimi models from hardcoded capability table."
  (let ((models (mapcar (lambda (pair)
                          (let* ((id (car pair))
                                 (caps (cdr pair)))
                            (append (list :model id :name id)
                                    caps)))
                        *kimi-model-capabilities*)))
    (setf (llm-api--platform-available-models platform) models)
    (mapcar (lambda (m) (plist-get m :name)) models)))

(cl-defmethod llm-api--set-selected-model ((platform llm-api--kimi) model-name)
  "Set MODEL-NAME for PLATFORM, normalizing aliases first."
  (cl-call-next-method platform (llm-api--kimi-normalize-model-id model-name)))

(cl-defmethod llm-api--get-selected-model ((platform llm-api--kimi))
  "Get selected MODEL for PLATFORM with alias normalization."
  (llm-api--kimi-normalize-model-id (cl-call-next-method platform)))

(cl-defmethod llm-api--get-model-name ((_platform llm-api--kimi) model)
  "Get display model name for Kimi MODEL with alias normalization."
  (llm-api--kimi-normalize-model-id
   (if (stringp model)
       model
     (or (plist-get model :name)
         (plist-get model :model)))))

(cl-defmethod llm-api--get-model-capabilities ((platform llm-api--kimi) &optional model)
  "Get capability plist for Kimi MODEL from hardcoded values + overrides."
  (let* ((model (or model (llm-api--platform-selected-model platform)))
         (model-id-raw (if (stringp model)
                           model
                         (or (plist-get model :model)
                             (plist-get model :id)
                             (plist-get model :name))))
         (model-id (llm-api--kimi-normalize-model-id model-id-raw))
         (provider-caps (alist-get model-id *kimi-model-capabilities* nil nil #'string=))
         (from-provider (and provider-caps
                             (append (list :model model-id)
                                     provider-caps)))
         (fallback (cl-call-next-method platform model-id)))
    (llm-api--plist-merge from-provider fallback)))

;; Kimi For Coding requires a specific User-Agent header
(cl-defmethod llm-api--get-request-headers ((_platform llm-api--kimi))
  '("Content-Type: application/json"
    "Accept: application/json"
    "User-Agent: claude-code/0.1.0"))

(defun llm--create-kimi-platform (token &optional selected-model)
  (llm-api--kimi-create
   :name "kimi"
   :url "https://api.kimi.com/coding/v1/chat/completions"
   :token token
   :selected-model (or selected-model "kimi-for-coding/k2p5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.6)))
