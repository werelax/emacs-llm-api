;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'spinner)
(require 'plz)
(require 's)

(cl-defstruct (llm-api--awan-llm (:include llm-api--platform)
                                 (:constructor llm-api--awan-llm-create)
                                 (:copier nil)))

(setq *awan-llm-models*
      '((:name "Llama3 8B Instruct #8k (50t/s)" :model "Meta-Llama-3-8B-Instruct")
        (:name "(soon) Llama3 8B Dolfin #8k (50t/s)" :model "Meta-Llama-3-8B-Instruct-Dolfin")
        (:name "(soon) WizardLM2 8x22B #64k (15t/s)" :model "WizardLM-2-8x22B")
        (:name "(soon) Llama3 70B Instruct #8k (10t/s)" :model "Meta-Llama-3-70B-Instruct")
        (:name "(soon) Mixtral 8x7b #8k (20t/s)" :model "Mixtral-8x7B-Instruct")
        (:name "(soon) MythoMax-L2-13b #8k (40t/s)" :model "MythoMax-L2-13b")
        (:name "(soon) Mistral 7B #32k (50t/s)" :model "Mistral-7B-Instruct")))

(cl-defmethod llm-api--get-available-models ((platform llm-api--awan-llm))
  ;; (or *awan-llm-models* (llm-api--awan-llm-refresh-models))
  (setf (llm-api--platform-available-models platform) *awan-llm-models*)
  (mapcar (lambda (m) (plist-get m :name)) *awan-llm-models*))

(defun llm-api--response-filter-process-line (platform chunk on-data)
  "Process one line of json (CHUNK) and maybe call ON-DATA."
  ;; (message "CHUNK: %s" chunk)
  (when (and (listp chunk)
             (eq nil (plist-get chunk :finish_reason)))
    (let ((choices (plist-get chunk :choices)))
      (when (and (listp choices)
                 (> (length choices) 0))
        (let* ((choice (car choices))
               (delta (plist-get choice :delta))
               (content-delta (plist-get delta :content)))
          ;; store last response (full response on last filter call)
          (when (stringp content-delta)
            (cl-callf concat (llm-api--platform-last-response platform) content-delta))
          ;; stream the deltas
          (when (and (stringp content-delta)
                     (functionp on-data))
            (funcall on-data content-delta)))))))

(cl-defmethod llm-api--response-filter ((platform llm-api--awan-llm) on-data _process output)
  (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "?\n"))
        (partial-line ""))
    (dolist (line lines)
      (when (string-prefix-p  "data: " line)
        (setq line (substring line (length "data: ")))
        (message "llm-api--response-filter DATA-LINE: '%s'" line)
        (when (and (not (string-empty-p line))
                   (not (string= line "[DONE]")))
          (condition-case nil
              (let ((chunk (json-parse-string (concat partial-line line) :object-type 'plist :array-type 'list)))
                ;; (message "CHUNK: %s" chunk)
                (llm-api--response-filter-process-line platform chunk on-data)
                (setq partial-line ""))
            (error
             ;; (message "ERR: %s" err)
             ;; probably the json line wasn't complete
             (setq partial-line line))))))))

;; fix the payload a little bit

(cl-defmethod llm-api--get-request-payload ((platform llm-api--awan-llm))
  (let ((payload (cl-call-next-method platform)))
    ;; (setf (plist-get payload :transforms) '("middle-out"))
    ;; (setf (plist-get payload :max_tokens) (* 128 1024))
    (setf (plist-get payload :max_tokens) nil)
    payload))

(defun llm--create-awan-llm-platform (token)
  (message "TOKEN: %s" token)
  (llm-api--awan-llm-create
   :name "awan-llm"
   :url "https://api.awanllm.com/v1/chat/completions"
   :token token
   :selected-model (plist-get (car *awan-llm-models*) :model)
   :system-prompt "You are a sentient superintelligent AI assistant.
 Help the user with precision."
   :params '(:temperature 0.7)))
