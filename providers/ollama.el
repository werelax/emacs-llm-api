;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)

(cl-defstruct (llm-api--ollama
               (:include llm-api--platform)
               (:constructor llm-api--ollama-create)
               (:copier nil)))

;; dynamic model list

(defvar *ollama-models* nil "List of available models.")
(defvar *ollama-num-ctx* nil "Override size of the model's context window.")

(defun get-until (haystack needle)
  (car (split-string haystack needle)))

(defun extract-number (s)
  (if (string-match "\\([0-9]+\\)" s)
      (match-string 1 s)
    nil))

(defun llm-api--ollama-refresh-models ()
  (interactive)
  (spinner-start)
  (let* ((url (concat *ollama-server* "/api/tags"))
         (response (plz 'get url :as #'json-read))
         (models-data (alist-get 'models response))
         ;; Filter out models with "M" in parameter_size
         (filtered-models (cl-remove-if
                           (lambda (m)
                             (let* ((details (alist-get 'details m))
                                    (param-size (alist-get 'parameter_size details)))
                               (string-match-p "M" param-size)))
                           models-data))
         (models (mapcar (lambda (m) (let ((details (alist-get 'details m))
                                           (name (alist-get 'name m)))
                                       `(:model ,name
                                         :name  ,(format "%s:%s %s"
                                                         (format "%+2sB" (extract-number (alist-get 'parameter_size details)))
                                                         (format "%s" (downcase (get-until
                                                                                 (alist-get 'quantization_level details)
                                                                                 "_")))
                                                         (get-until name ":")))))
                         filtered-models)))
    (spinner-stop)
    (setq *ollama-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--ollama))
  (or *ollama-models* (llm-api--ollama-refresh-models))
  (setf (llm-api--platform-available-models platform) *ollama-models*)
  (cl-call-next-method))

(cl-defmethod llm-api--invalidate-model-cache ((platform llm-api--ollama))
  "Invalidate Ollama global model cache for PLATFORM."
  (setq *ollama-models* nil)
  (cl-call-next-method))

;; select server

(defvar *ollama-inference-servers* '((:local . "http://localhost:11434")))

(defvar *ollama-server* (cdar *ollama-inference-servers*))

(defun llm-api--ollama-select-server ()
  (interactive)
  (let* ((server-name (completing-read "ollama server:" *ollama-inference-servers*))
         (server-url (alist-get (intern server-name) *ollama-inference-servers*)))
    (setq *ollama-server* server-url)
    (llm-api--ollama-refresh-models)))

;; payload

(cl-defmethod llm-api--get-curl-url ((platform llm-api--ollama))
  (concat *ollama-server* "/api/chat"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--ollama))
  (let* ((params (llm-api--platform-params platform))
         (model (llm-api--platform-selected-model platform))
         (model-params (plist-get model :params))
         (model-id (if (stringp model) model (plist-get model :model)))
         (get-param (lambda (key) (or (plist-get model-params key) (plist-get params key))))
         ;; read num_ctx from platform :ctx-size or *ollama-num-ctx*
         (num-ctx (or (plist-get params :context-size) *ollama-num-ctx*))
         (temperature (funcall get-param :temperature)))
    ;; the actual payload
    `(:model ,model-id
      :messages ,(llm-api--get-history platform)
      :keep_alive -1
      :options ((:temperature . ,temperature)
                ,@(when num-ctx `((:num_ctx . ,num-ctx)))
                (:min_p . 0.2))
      :stream t)))

;; NDJSON response handling (Ollama uses newline-delimited JSON, not SSE)

(cl-defmethod llm-api--response-filter ((platform llm-api--ollama) on-data _process output)
  "Filter Ollama NDJSON OUTPUT using buffered parser."
  (let ((state (llm-api--platform-sse-state platform)))
    (llm-api--ndjson-parse state output
                           (lambda (payload)
                             (when payload
                               (llm-api--handle-sse-data platform on-data payload))))))

(cl-defmethod llm-api--flush-stream ((platform llm-api--ollama))
  "Flush remaining NDJSON buffer content for Ollama."
  (let ((state (llm-api--platform-sse-state platform)))
    (when state
      (llm-api--ndjson-flush state
                             (lambda (payload)
                               (llm-api--handle-sse-data platform nil payload))))))

(cl-defmethod llm-api--handle-sse-data ((platform llm-api--ollama) on-data payload)
  "Handle a single NDJSON line PAYLOAD for Ollama."
  (condition-case err
      (let ((chunk (json-parse-string payload :object-type 'plist :array-type 'list)))
        (when (listp chunk)
          ;; Check for Ollama error response (e.g. model not found)
          (let ((err-data (plist-get chunk :error)))
            (if (stringp err-data)
                (progn
                  (setf (llm-api--sse-state-errorp (llm-api--platform-sse-state platform)) err-data)
                  (message "llm-api (ollama) error: %s" err-data))
              ;; Normal chunk processing
              (let* ((message (plist-get chunk :message))
                     (content-delta (plist-get message :content))
                     (done (plist-get chunk :done)))
                (when (stringp content-delta)
                  (cl-callf concat (llm-api--platform-last-response platform) content-delta))
                (when (and (stringp content-delta) (functionp on-data))
                  (funcall on-data content-delta)))))))
    (json-parse-error
     (message "llm-api (ollama): JSON parse error: %S" err))))

;; factory

(defun llm--create-ollama-platform (&optional selected-model)
  (llm-api--ollama-create
   :name "ollama"
   :selected-model (or selected-model "phi4")
   :params `(:temperature 0.6)))
