;;; zai.el --- Z.AI (Zhipu) provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--zai (:include llm-api--openai)
                             (:constructor llm-api--zai-create)
                             (:copier nil)))

(defvar *zai-model-capabilities*
  '(("glm-5"         . (:context-window 205000 :max-output-tokens 128000))
    ("glm-4.7"       . (:context-window 205000 :max-output-tokens 128000))
    ("glm-4.7-flash" . (:context-window 200000 :max-output-tokens 128000))
    ("glm-4.6"       . (:context-window 205000 :max-output-tokens 128000))
    ("glm-4.6v"      . (:context-window 128000 :max-output-tokens 32000))
    ("glm-4.5"       . (:context-window 131000 :max-output-tokens 96000))
    ("glm-4.5-air"   . (:context-window 131000 :max-output-tokens 96000))
    ("glm-4.5-flash" . (:context-window 131000 :max-output-tokens 96000))
    ("glm-4.5v"      . (:context-window 64000  :max-output-tokens 16000)))
  "Hardcoded Z.AI model capabilities.")

(defun llm-api--zai-model-plist (model-id)
  "Build model plist for MODEL-ID from `*zai-model-capabilities*'."
  (let ((caps (alist-get model-id *zai-model-capabilities* nil nil #'string=)))
    (append (list :model model-id :name model-id :source :provider-default)
            caps)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--zai))
  "Return Z.AI models from hardcoded capability table."
  (let ((models (mapcar (lambda (pair)
                          (llm-api--zai-model-plist (car pair)))
                        *zai-model-capabilities*)))
    (setf (llm-api--platform-available-models platform) models)
    (mapcar (lambda (m) (plist-get m :name)) models)))

(cl-defmethod llm-api--get-model-capabilities ((platform llm-api--zai) &optional model)
  "Get capability plist for Z.AI MODEL from hardcoded values + overrides."
  (let* ((model (or model (llm-api--platform-selected-model platform)))
         (model-id (if (stringp model)
                       model
                     (or (plist-get model :model)
                         (plist-get model :id)
                         (plist-get model :name))))
         (provider-caps (alist-get model-id *zai-model-capabilities* nil nil #'string=))
         (from-provider (and provider-caps
                             (append (list :model model-id :source :provider-default)
                                     provider-caps)))
         (fallback (cl-call-next-method platform model)))
    (llm-api--plist-merge from-provider fallback)))

(defun llm--create-zai-platform (token &optional selected-model)
  (llm-api--zai-create
   :name "zai"
   :url "https://api.z.ai/api/coding/paas/v4/chat/completions"
   :token token
   :selected-model (or selected-model "glm-5")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
