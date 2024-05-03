;;; ../sync/doom.d/lib/llm-api/providers/default.el -*- lexical-binding: t; -*-

(cl-defmethod llm-api--get-available-models ((platform llm-api--platform))
  "Get the list of names of available models for PLATFORM."
  (let ((models (llm-api--platform-available-models platform)))
    (mapcar (lambda (m)
              (if (consp m) (plist-get m :name) m))
            models)))

(cl-defmethod llm-api--set-selected-model ((platform llm-api--platform) model-name)
  "Set MODEL as selected for PLATFORM."
  (let* ((models (llm-api--platform-available-models platform))
         (model (seq-find (lambda (m) (if (stringp m)
                                          (string= m model-name)
                                        (string= (plist-get m :name) model-name)))
                          models)))
    (setf (llm-api--platform-selected-model platform) model)))

(cl-defmethod llm-api--get-selected-model ((platform llm-api--platform))
  "Get selected MODEL for PLATFORM."
  (let* ((model (llm-api--platform-selected-model platform)))
    (if (stringp model)
        model
      (plist-get model :model))))

(cl-defmethod llm-api--get-model-name ((_platform llm-api--platform) model)
  "Get the model name for PLATFORM."
  (if (stringp model)
      model
    (plist-get model :name)))

(cl-defmethod llm-api--kill-process ((platform llm-api--platform))
  "Kill the process for PLATFORM."
  (when (process-live-p (llm-api--platform-process platform))
    (delete-process (llm-api--platform-process platform))))

(cl-defmethod llm-api--on-clear-history ((_ llm-api--platform))
  "Hook called when the history is cleared for PLATFORM."
  (message "History cleared"))

(cl-defmethod llm-api--clear-history ((platform llm-api--platform))
  "Clear the history for PLATFORM."
  ;; TODO: system prompt?
  (setf (llm-api--platform-history platform) '())
  (llm-api--on-clear-history platform))

(cl-defmethod llm-api--get-history ((platform llm-api--platform))
  "Get the history for PLATFORM."
  (let ((history (llm-api--platform-history platform)))
    (when-let ((system-prompt (llm-api--platform-system-prompt platform)))
      (push `((:role . :system) (:content . ,system-prompt)) history))
    ;; (message "HISTORY: %s" history)
    history))

(cl-defmethod llm-api--add-to-history ((platform llm-api--platform) message)
  "Add MESSAGE to the history for PLATFORM."
  (let ((history (llm-api--platform-history platform)))
    (setf (llm-api--platform-history platform) (nconc history (list message)))))

(cl-defmethod llm-api--remove-last-from-history ((platform llm-api--platform))
  "Remove the last message from history for the given PLATFORM."
  (let ((history (llm-api--platform-history platform)))
    (when history  ; Ensure the history is not empty.
      (setf (llm-api--platform-history platform) (butlast history)))))

(cl-defmethod llm-api--add-generated-message-to-history ((platform llm-api--platform))
  "Add generated :assistant response to PLATFORM chat history."
  (let ((last-response (llm-api--platform-last-response platform)))
    (llm-api--add-to-history platform `((:role . :assistant) (:content . ,last-response)))
    (setf (llm-api--platform-last-response platform) nil)))

(cl-defmethod llm-api--on-generation-finish-hook ((_platform llm-api--platform))
  "Empty for this implementation for PLATFORM."
  nil)

(cl-defmethod llm-api--response-filter ((platform llm-api--platform) on-data _process output)
  "Filter the OUTPUT of the PROCESS for PLATFORM and call ON-DATA."
  ;; (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "?\n")))
    (dolist (line lines)
      (when (string-prefix-p  "data: " line)
        (setq line (substring line (length "data: ")))
        ;; (message "llm-api--response-filter DATA-LINE: '%s'" line)
        (when (and (not (string-empty-p line))
                   (not (string= line "[DONE]")))
          (let ((chunk (json-parse-string line :object-type 'plist :array-type 'list)))
            (when (and (listp chunk)
                       (string= "chat.completion" (plist-get chunk :object)))
              (let ((choices (plist-get chunk :choices)))
                (when (and (listp choices)
                           (> (length choices) 0))
                  (let* ((choice (car choices))
                         (msg (plist-get choice :message))
                         (delta (plist-get choice :delta))
                         (role (plist-get msg :role))
                         (content (plist-get msg :content))
                         (content-delta (plist-get delta :content)))
                    ;; store last response (full response on last filter call)
                    (when (stringp content-delta)
                      (cl-callf concat (llm-api--platform-last-response platform) content-delta))
                    ;; stream the deltas
                    (when (and (stringp content-delta)
                               (functionp on-data))
                      (funcall on-data content-delta))))))))))))

(cl-defmethod llm-api--process-sentinel ((platform llm-api--platform) on-finish process event)
  "Process sentinel function to handle EVENT for PROCESS and PLATFORM. Call ON-FINISH on end."
  ;; (message "llm-api--server sentinel: %s" event)
  (when (string-match-p "killed" event)
    (signal-process process 'SIGTERM))
  (when (string-match-p "finished\\|exited" event)
    (funcall on-finish)))

(cl-defmethod llm-api--get-request-headers ((_platform llm-api--platform))
  "Get the request headers for PLATFORM."
  '("Content-Type: application/json"
    "Accept: application/json"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--platform))
  "Generate request payload for PLATFORM."
  (let ((params (llm-api--platform-params platform)))
    `(:model ,(llm-api--get-selected-model platform)
      :temperature ,(or (plist-get params :temperature) 0.2)
      :messages ,(llm-api--get-history platform)
      ;; :max_tokens ,(or (plist-get params :max_tokens) 4096)
      :max_tokens 512
      ;; :n 512
      ;; :transforms ["middle-out"]
      ;; :top_p 1
      ;; :frequency_penalty 0.0
      ;; :presence_penalty 0.0
      ;; :stop ["\n"]
      :stream t)))

(cl-defmethod llm-api--get-curl-params ((_platform llm-api--platform))
  "Get the curl command params for PLATFORM."
  '("-s" "-N" "-X" "POST"))

(cl-defmethod llm-api--get-curl-url ((platform llm-api--platform))
  "Get the curl command url for PLATFORM."
  (llm-api--platform-url platform))

(cl-defmethod llm-api--generate-streaming ((platform llm-api--platform) (prompt string) &rest args)
  "Query PLATFORM for PROMPT. ARGS for more control."
  ;; add user message to history
  ;; TODO: now I see that this is a mistake!
  ;;       it would be very convenient if the function that generates the
  ;;       response does *not* take a prompt paramter, and uses the history
  ;;       as it is instead. It would make history manipulations easier
  ;;       like *regenerate* or *generation trees* (like sillytavern).
  ;; add string to history when non-empty
  (when (not (string-empty-p prompt))
    (llm-api--add-to-history platform `((:role . :user) (:content . ,prompt))))
  ;; call the API
  (let* ((on-data (plist-get args :on-data))
         (on-finish (plist-get args :on-finish))
         (filter (lambda (process line)
                   (llm-api--response-filter platform on-data process line)))
         (sentinel (lambda (process event)
                     (llm-api--process-sentinel platform on-finish process event)))
         (request-payload (llm-api--get-request-payload platform))
         (request-headers (llm-api--get-request-headers platform))
         (curl-params (llm-api--get-curl-params platform)))
    ;; headers
    (dolist (header request-headers)
      (push header curl-params)
      ;; push adds at the *front*!
      (push "-H" curl-params))
    ;; api token
    (when-let ((token (llm-api--platform-token platform)))
      (push (format "Authorization: Bearer %s" (llm-api--platform-token platform)) curl-params)
      ;; push adds at the *front*!
      (push "-H" curl-params))
    ;; full command
    ;; (message "curl params: %s" (json-encode request-payload))
    (let ((temp-file (make-temp-file "llm-api-payload-")))
      (with-temp-file temp-file
        (insert (json-encode request-payload)))
      (let ((curl-command `("curl"
                            ,(llm-api--get-curl-url platform)
                            ,@curl-params
                            "-d" ,(concat "@" temp-file))))
        (let ((process (make-process :name (format "llm-api--server-%s" (llm-api--platform-name platform))
                                     :buffer (llm-api--platform-process-buffer-name platform)
                                     :command curl-command
                                     :filter filter
                                     :sentinel (lambda (process event)
                                                 (funcall sentinel process event)
                                                 ;; delete the temporary file
                                                 (delete-file temp-file))
                                     ;; :sentinel sentile
                                     :connection-type 'pipe
                                     :noquery t)))
          (setf (llm-api--platform-process platform) process))))))
