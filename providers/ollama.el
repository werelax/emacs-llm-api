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

;; select server

(defvar *ollama-inference-servers* '((:local . "http://localhost:11434")))

(defvar *ollama-server* (cdar *ollama-inference-servers*))

(defun llm-api--ollama-select-server ()
  (interactive)
  ()
  (let* ((server-name (completing-read "ollama server:" *ollama-inference-servers*))
         (server-url (alist-get (intern server-name) *ollama-inference-servers*)))
    (setq *ollama-server* server-url)))

;; dynamic model list

(defvar *ollama-models* nil)

(defun llm-api--ollama-refresh-models ()
  (interactive)
  (spinner-start)
  (let* ((url (concat *ollama-server* "/api/tags"))
         (response (plz 'get url :as #'json-read))
         (models-data (alist-get 'models response))
         (models (mapcar (lambda (m) (alist-get 'name m)) models-data)))
    (spinner-stop)
    (setq *ollama-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--ollama))
  (or *ollama-models* (llm-api--ollama-refresh-models))
  (setf (llm-api--platform-available-models platform) *ollama-models*)
  *ollama-models*)

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
    ;; sneak the partial-line cleansing
    (setq partial-line "")
    ;; the actual payload
    `(:model ,model-id
      :messages ,(llm-api--get-history platform)
      :options ((:temperature . ,temperature)
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
  (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "?\n")))
    (dolist (line lines)
      (when (string-prefix-p  "data: " line)
        (setq line (substring line (length "data: ")))
        (message "llm-api--response-filter DATA-LINE: '%s'" line)
        (when (and (not (string-empty-p line))
                   (not (string= line "[DONE]")))
          (condition-case nil
              (let ((chunk (json-parse-string line :object-type 'plist :array-type 'list)))
                (setq partial-line "")
                (when (and (listp chunk))
                  (let* ((message (plist-get chunk :message))
                         (content-delta (plist-get message :content))
                         (done (plist-get chunk :done)))
                    (when (stringp content-delta)
                      (cl-callf concat (llm-api--platform-last-response platform) content-delta))
                    (when (and (stringp content-delta)
                               (functionp on-data))
                      (funcall on-data content-delta)))))
            (error
             ;; probably the json line wasn't complete
             (setq partial-line line))))))))

;; factory

(defun llm--create-ollama-platform ()
  (llm-api--ollama-create
   :name "ollama"
   :selected-model "dolphin2.1-mistral"
   :params `(:temperature 0.2)))
