;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--ollama-comp
               (:include llm-api--platform)
               (:constructor llm-api--ollama-comp-create)
               (:copier nil))
  (context []))

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

(defun llm-api--ollama-comp-refresh-models ()
  (interactive)
  (spinner-start)
  (let* ((url (concat *ollama-server* "/api/tags"))
         (response (plz 'get url :as #'json-read))
         (models-data (alist-get 'models response))
         (models (mapcar (lambda (m) (alist-get 'name m)) models-data)))
    (spinner-stop)
    (setq *ollama-models* models)))

(cl-defmethod llm-api--get-available-models ((platform llm-api--ollama-comp))
  (or *ollama-models* (llm-api--ollama-comp-refresh-models))
  (setf (llm-api--platform-available-models platform) *ollama-models*)
  *ollama-models*)

;; payload

(cl-defmethod llm-api--get-curl-url ((platform llm-api--ollama-comp))
  (concat *ollama-server* "/api/generate"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--ollama-comp))
  (let* ((params (llm-api--platform-params platform))
         (model (llm-api--platform-selected-model platform))
         (model-params (plist-get model :params))
         (model-id (if (stringp model) model (plist-get model :model)))
         (get-param (lambda (key) (or (plist-get model-params key) (plist-get params key))))
         (temperature (funcall get-param :temperature))
         (history (llm-api--get-history platform))
         (prompt (cdadr (-last-item history))))
    ;; sneak the partial-line cleansing
    (setq partial-line "")
    ;; the actual payload
    `(:model ,model-id
      :prompt ,prompt
      ,@(when-let ((system-prompt (llm-api--platform-system-prompt platform)))
          `(:system ,system-prompt))
      :context ,(llm-api--ollama-comp-context platform)
      :options (,@(when temperature `((:temperature . ,temperature)))
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

;; history

(cl-defmethod llm-api--on-clear-history ((platform llm-api--ollama-comp))
  "Hook called when the history is cleared for PLATFORM."
  (setq partial-line "")
  (setf (llm-api--ollama-comp-context platform) '())
  (cl-call-next-method))

;; request filter

(cl-defmethod llm-api--response-filter ((platform llm-api--ollama-comp) on-data _process output)
  ;; (message "llm-api--response-filter: '%s'" output)
  ;; if it ends with a partial line (not \n at the end) then remove it and save it
  (when-let ((idx (string-match-p "\n[^\n]+\\'" output)))
    (setq partial-line (concat partial-line (substring output (1+ idx))))
    (setq output (substring output 0 idx)))
  ;; split by newline
  (let ((lines (split-string output "
?\n")))
    (dolist (line lines)
      ;; (message "llm-api--response-filter DATA-LINE: '%s'" line)
      ;; newline token
      (when (and (not (string-empty-p line))
                 (not (string= line "[DONE]")))
        (condition-case nil
            (let ((chunk (json-parse-string
                          (concat partial-line line)
                          :object-type 'plist :array-type 'list)))
              (setq partial-line "")
              (when (and (listp chunk))
                (let* ((raw-content-delta (plist-get chunk :response))
                       ;; newline token
                       (content-delta (s-replace "\u003c0x0A\u003e" "\n" raw-content-delta))
                       (context (plist-get chunk :context))
                       (done (plist-get chunk :done)))
                  (when (stringp content-delta)
                    (cl-callf concat (llm-api--platform-last-response platform) content-delta))
                  (when (eq done t)
                    (setf (llm-api--ollama-comp-context platform) context))
                  (when (and (stringp content-delta)
                             (functionp on-data))
                    (funcall on-data content-delta)))))
          (error
           ;; probably the json line wasn't complete
           (setq partial-line line)))))))

;; factory

(defun llm--create-ollama-comp-platform ()
  (llm-api--ollama-comp-create
   :name "ollama-completion"
   :selected-model "dolphin2.1-mistral"
   :params `(:temperature 0.2)))
