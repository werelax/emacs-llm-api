;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)

(cl-defstruct (llm-api--ollama
               (:include llm-api--platform)
               (:constructor llm-api--ollama-create)
               (:copier nil)))

;; for incomplete json lines

(defvar partial-line "")

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

;; select server

(defvar *ollama-inference-servers* '((:local . "http://localhost:11434")))

(defvar *ollama-server* (cdar *ollama-inference-servers*))

(defun llm-api--ollama-select-server ()
  (interactive)
  (let* ((server-name (completing-read "ollama server:" *ollama-inference-servers*))
         (server-url (alist-get (intern server-name) *ollama-inference-servers*)))
    (setq *ollama-server* server-url)
    (llm-api--ollama-refresh-models)))

;; history

(cl-defmethod llm-api--on-clear-history ((_platform llm-api--ollama))
  "Hook called when the history is cleared for PLATFORM."
  (setq partial-line "")
  (cl-call-next-method))

;; payload

(cl-defmethod llm-api--get-curl-url ((platform llm-api--ollama))
  (concat *ollama-server* "/api/chat"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--ollama))
  ;; TODO:
  ;; (message "\n> history: %s\n\n" (llm-api--get-history platform))
  (let* ((params (llm-api--platform-params platform))
         (model (llm-api--platform-selected-model platform))
         (model-params (plist-get model :params))
         (model-id (if (stringp model) model (plist-get model :model)))
         (get-param (lambda (key) (or (plist-get model-params key) (plist-get params key))))
         (temperature (funcall get-param :temperature)))
    ;; the actual payload
    `(:model ,model-id
      :messages ,(llm-api--get-history platform)
      :keep_alive -1
      :options ((:temperature . ,temperature)
                ;; to avoid modifying every Modelfile
                ,@(when *ollama-num-ctx* `((:num_ctx . ,*ollama-num-ctx*)))
                ;; :top_k . 20
                ;; :top_p . 0.9
                ;; :tfs_z . 0.5
                ;; :typical_p . 0.7
                ;; :repeat_last_n . 33
                ;; :repeat_penalty . 1.2
                ;; :presence_penalty . 1.5
                ;; :frequency_penalty . 1.0
                ;; :mirostat . 1
                ;; :mirostat_tau . 0.8
                ;; :mirostat_eta . 0.6
                ;; :penalize_newline . true
                ;; :stop . ("\n" "user:")
                ;; :numa . :json-false
                ;; :num_ctx . 4
                ;; :num_batch . 2
                ;; :logits_all . false
                ;; :vocab_only . false
                ;; :embedding_only . false
                ;; :rope_frequency_base . 1.1
                ;; :rope_frequency_scale . 0.8
                )
      :stream t)))

;; request filter

(cl-defmethod llm-api--response-filter ((platform llm-api--ollama) on-data _process output)
  ;; (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "
?\n")))
    (dolist (line lines)
      (when (not (string-empty-p line))
        (let ((chunk (json-parse-string line :object-type 'plist :array-type 'list)))
          (when (and (listp chunk))
            (let* ((message (plist-get chunk :message))
                   (content-delta (plist-get message :content))
                   (done (plist-get chunk :done)))
              (when (stringp content-delta)
                (cl-callf concat (llm-api--platform-last-response platform) content-delta))
              (when (and (stringp content-delta)
                         (functionp on-data))
                (funcall on-data content-delta)))))))))

;; factory

(defun llm--create-ollama-platform (&optional selected-model)
  (llm-api--ollama-create
   :name "ollama"
   :selected-model (or selected-model "gemma2:27b-instruct-q4_K_M")
   :params `(:temperature 0.2)))
