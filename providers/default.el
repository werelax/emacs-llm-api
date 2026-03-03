;;; ../sync/doom.d/lib/llm-api/providers/default.el -*- lexical-binding: t; -*-

(cl-defmethod llm-api--get-available-models ((platform llm-api--platform))
  "Get the list of names of available models for PLATFORM."
  (let ((models (llm-api--platform-available-models platform)))
    (mapcar (lambda (m)
              (if (consp m) (plist-get m :name) m))
            models)))

(cl-defmethod llm-api--invalidate-model-cache ((_platform llm-api--platform))
  "Invalidate cached model data for PLATFORM.
Default is a no-op to preserve static model lists and stale fallback data.
Providers with external model caches should override this method."
  nil)

(cl-defmethod llm-api--refresh-model-metadata ((platform llm-api--platform))
  "Force refresh model metadata/list for PLATFORM and return available models."
  (llm-api--invalidate-model-cache platform)
  (llm-api--get-available-models platform))

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

(defun llm-api--maybe-number (value)
  "Convert VALUE to number when possible, else nil."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
    (string-to-number value))
   (t nil)))

(cl-defmethod llm-api--get-model-capabilities ((platform llm-api--platform) &optional model)
  "Get capability plist for MODEL on PLATFORM.
Default implementation reads known keys from model plist + user overrides."
  (let* ((model (or model (llm-api--platform-selected-model platform)))
         (model-id (cond
                    ((stringp model) model)
                    ((consp model)
                     (or (plist-get model :model)
                         (plist-get model :id)
                         (plist-get model :name)))))
         (model-meta (cond
                      ((consp model) model)
                      ((and (stringp model-id)
                            (listp (llm-api--platform-available-models platform)))
                       (seq-find (lambda (m)
                                   (and (consp m)
                                        (or (string= (or (plist-get m :model) "") model-id)
                                            (string= (or (plist-get m :id) "") model-id)
                                            (string= (or (plist-get m :name) "") model-id))))
                                 (llm-api--platform-available-models platform)))))
         (from-model (when (consp model-meta)
                       (let ((ctx (llm-api--maybe-number
                                   (or (plist-get model-meta :context-window)
                                       (plist-get model-meta :context_length)
                                       (plist-get model-meta :tokenLimit))))
                             (max-out (llm-api--maybe-number
                                       (or (plist-get model-meta :max-output-tokens)
                                           (plist-get model-meta :max_completion_tokens)
                                           (plist-get model-meta :max_tokens))))
                             (src (or (plist-get model-meta :source) :provider-api)))
                         (when (or ctx max-out)
                           (list :context-window ctx
                                 :max-output-tokens max-out
                                 :source src)))))
         (override (llm-api--lookup-model-capabilities-override platform model-id))
         (caps (llm-api--plist-merge from-model override)))
    (when (and model-id caps)
      (plist-put caps :model model-id))))

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
    (when-let* ((system-prompt (llm-api--platform-system-prompt platform)))
      (when (not (string-empty-p system-prompt))
        (let ((system-prompt-role (llm-api--platform-system-prompt-role platform)))
          (push `((:role . ,system-prompt-role) (:content . ,system-prompt)) history))))
    history))

(cl-defmethod llm-api--add-to-history ((platform llm-api--platform) message)
  "Add MESSAGE to the history for PLATFORM."
  (let ((history (llm-api--platform-history platform)))
    ;; TODO: for each item in history: clear any previous formatting (like continuation)
    (setf (llm-api--platform-history platform) (nconc history (list message)))))

(cl-defmethod llm-api--remove-last-from-history ((platform llm-api--platform))
  "Remove the last message from history for the given PLATFORM."
  (let ((history (llm-api--platform-history platform)))
    (when history  ; Ensure the history is not empty.
      (setf (llm-api--platform-history platform) (butlast history)))))

(cl-defmethod llm-api--add-response-to-history ((platform llm-api--platform) &rest args)
  "Add the last response to PLATFORM's chat history.
Handles regular responses, continuation messages (ARGS contains :continuation),
and tool call messages (ARGS contains :tool-calls LIST)."
  (let* ((is-continuation (memq :continuation args))
         (tool-calls-arg (plist-get args :tool-calls))
         (last-response (llm-api--platform-last-response platform)))
    (cond
     ;; Tool calls: assistant message with tool_calls array
     (tool-calls-arg
      (let ((content (if (and last-response (not (string-empty-p last-response)))
                         last-response
                       :json-null)))
        (llm-api--add-to-history platform
          `((:role . :assistant)
            (:content . ,content)
            (:tool_calls . ,(llm-api--format-tool-calls-for-history tool-calls-arg)))))
      (setf (llm-api--platform-last-response platform) nil))
     ;; Regular or continuation (don't add empty responses)
     ((and last-response (not (string-empty-p last-response)))
      (if is-continuation
          (llm-api--add-to-history platform (llm-api--format-continuation-message platform last-response))
        (llm-api--add-to-history platform `((:role . :assistant) (:content . ,last-response))))
      (setf (llm-api--platform-last-response platform) nil)))))

(cl-defmethod llm-api--format-continuation-message ((_ llm-api--platform) last-response)
  "Format LAST-RESPONSE as a continuation message for chat history.
Default implementation returns a simple assistant message format. Platforms
may override this if they require special continuation message formatting."
  `((:role . :assistant) (:content . ,last-response)))

(defun llm-api--add-user-message (platform prompt)
  "Append user PROMPT to PLATFORM history."
  (llm-api--add-to-history platform `((:role . :user) (:content . ,prompt))))

(defun llm-api--rewrite-last-assistant-as-continuation (platform)
  "Rewrite last assistant message in PLATFORM history as continuation format.
Used for manual continuation flows. No-op when last message is not assistant."
  (let* ((history (llm-api--platform-history platform))
         (last-msg (car (last history))))
    (when (and last-msg (eq (alist-get :role last-msg) :assistant))
      (setf (llm-api--platform-history platform) (butlast history))
      (llm-api--add-to-history platform
                               (llm-api--format-continuation-message platform
                                                                     (alist-get :content last-msg))))))

(cl-defmethod llm-api--on-generation-finish-hook ((_platform llm-api--platform) _on-data)
  "Empty for this implementation for PLATFORM."
  nil)

;; Tool calling helpers

(defun llm-api--collect-tool-calls (tc-table)
  "Convert TC-TABLE hash-table to sorted list of tool call plists.
Returns nil if TC-TABLE is nil or empty."
  (when tc-table
    (let ((entries '()))
      (maphash (lambda (idx tc) (push (cons idx tc) entries)) tc-table)
      (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
      (mapcar #'cdr entries))))

(defun llm-api--format-tool-calls-for-history (tool-calls)
  "Format TOOL-CALLS plist list as a vector of alists for json-encode."
  (vconcat (mapcar (lambda (tc)
                     `((:id . ,(plist-get tc :id))
                       (:type . ,(plist-get tc :type))
                       (:function . ((:name . ,(plist-get tc :name))
                                     (:arguments . ,(plist-get tc :arguments))))))
                   tool-calls)))

(defun llm-api--make-tool (name description parameters)
  "Create a tool definition alist for NAME with DESCRIPTION and PARAMETERS.
PARAMETERS should be a JSON Schema alist (e.g. with :type, :properties, :required)."
  `((:type . "function")
    (:function . ((:name . ,name)
                  (:description . ,description)
                  (:parameters . ,parameters)))))

;; Async tool execution helpers

(defvar llm-api--tool-timeout 30
  "Timeout in seconds for async tool execution.")

(defun llm-api--executor-is-async-p (executor)
  "Return non-nil if EXECUTOR accepts 4+ arguments (async signature).
Async executors take (name parsed-args raw-args callback).
Sync executors take (name parsed-args raw-args)."
  (let ((max-args (cdr (func-arity executor))))
    (or (eq max-args 'many) (and (numberp max-args) (>= max-args 4)))))

(defun llm-api--wrap-sync-executor (executor)
  "Wrap sync 3-arg EXECUTOR as async 4-arg executor.
Returns a function that calls EXECUTOR and immediately passes the
result to CALLBACK."
  (lambda (name parsed-args raw-args callback)
    (let ((result (funcall executor name parsed-args raw-args)))
      (funcall callback result))))

;; SSE response handling

(cl-defmethod llm-api--response-filter ((platform llm-api--platform) on-data _process output)
  "Filter the OUTPUT of the PROCESS for PLATFORM and call ON-DATA.
Delegates SSE framing to `llm-api--sse-parse' and JSON handling to
`llm-api--handle-sse-data'."
  (let ((state (llm-api--platform-sse-state platform)))
    (llm-api--sse-parse state output
                        (lambda (payload)
                          (when payload
                            (llm-api--handle-sse-data platform on-data payload))))))

(cl-defmethod llm-api--flush-stream ((platform llm-api--platform))
  "Flush remaining SSE buffer content for PLATFORM."
  (let ((state (llm-api--platform-sse-state platform)))
    (when state
      (llm-api--sse-flush state))))

(cl-defmethod llm-api--handle-sse-data ((platform llm-api--platform) on-data payload)
  "Handle a single SSE data PAYLOAD for PLATFORM (OpenAI chat completions format)."
  (condition-case err
      (let ((chunk (json-parse-string payload :object-type 'plist :array-type 'list)))
        ;; Check for API error response
        (let ((err-data (plist-get chunk :error)))
          (if err-data
              (let ((msg (if (and (listp err-data) (plist-get err-data :message))
                             (plist-get err-data :message)
                           (format "%s" err-data))))
                (setf (llm-api--sse-state-errorp (llm-api--platform-sse-state platform)) msg)
                (message "llm-api error: %s" msg))
            ;; Normal chunk processing
            (when (and (listp chunk)
                       (plist-get chunk :object)
                       (string-prefix-p "chat.completion" (plist-get chunk :object)))
              ;; save the last api response (for debug, reference and citations)
              (setf (llm-api--platform-last-api-response platform) chunk)
              (let ((choices (plist-get chunk :choices)))
                (when (and (listp choices) (> (length choices) 0))
                  (let* ((choice (car choices))
                         (delta (plist-get choice :delta))
                         (finish-reason (plist-get choice :finish_reason))
                         (content-delta (plist-get delta :content))
                         (reasoning-delta (plist-get delta :reasoning_content))
                         (state (llm-api--platform-sse-state platform)))
                    ;; reasoning_content delta (DeepSeek, QwQ, etc.)
                    (when (stringp reasoning-delta)
                      (setf (llm-api--sse-state-reasoning-active state) t)
                      (when-let* ((on-reasoning (llm-api--sse-state-on-reasoning state)))
                        (funcall on-reasoning reasoning-delta)))
                    ;; finalize reasoning when content starts
                    (when (and (stringp content-delta)
                               (llm-api--sse-state-reasoning-active state))
                      (setf (llm-api--sse-state-reasoning-active state) nil)
                      (when-let* ((finalize-fn (llm-api--sse-state-on-reasoning-finalize state)))
                        (funcall finalize-fn)))
                    ;; store last response (full response on last filter call)
                    (when (stringp content-delta)
                      (cl-callf concat (llm-api--platform-last-response platform) content-delta))
                    ;; stream the deltas
                    (when (and (stringp content-delta) (functionp on-data))
                      (funcall on-data content-delta))
                    ;; tool call delta accumulation
                    (let ((tc-deltas (plist-get delta :tool_calls)))
                      (when (consp tc-deltas)
                        (let ((tc-table (or (llm-api--sse-state-tool-calls
                                             (llm-api--platform-sse-state platform))
                                            (let ((ht (make-hash-table :test 'eql)))
                                              (setf (llm-api--sse-state-tool-calls
                                                     (llm-api--platform-sse-state platform)) ht)
                                              ht))))
                          (dolist (tc-delta tc-deltas)
                            (let* ((idx (plist-get tc-delta :index))
                                   (existing (gethash idx tc-table))
                                   (fn (plist-get tc-delta :function)))
                              (unless existing
                                (setq existing (list :id (plist-get tc-delta :id)
                                                     :type (or (plist-get tc-delta :type) "function")
                                                     :name (and fn (plist-get fn :name))
                                                     :arguments ""))
                                (puthash idx existing tc-table))
                              (when-let* ((arg-frag (and fn (plist-get fn :arguments))))
                                (when (stringp arg-frag)
                                  (plist-put existing :arguments
                                             (concat (plist-get existing :arguments) arg-frag)))))))))
                    ;; save the finish reason
                    (when (not (eq finish-reason :null))
                      (setf (llm-api--platform-finish-reason platform) finish-reason)))))))))
    (json-parse-error
     (message "llm-api: JSON parse error: %S (payload: %.100s)" err payload))))

;; Process sentinel

(cl-defmethod llm-api--process-sentinel ((platform llm-api--platform) on-finish on-error on-continue on-tool-calls process event)
  "Process sentinel for PLATFORM handling EVENT from PROCESS.
Calls ON-ERROR with (error-message partial-response) when an error occurred,
ON-CONTINUE when finish-reason is \"length\", ON-TOOL-CALLS when finish-reason
is \"tool_calls\", or ON-FINISH on normal completion.
If ON-ERROR is nil, falls back to calling ON-FINISH on error (backward compat)."
  (when (string-match-p "killed" event)
    (signal-process process 'SIGTERM))
  (when (string-match-p "finished\\|exited" event)
    ;; Flush any remaining buffered content before checking for errors
    (llm-api--flush-stream platform)
    (let ((state (llm-api--platform-sse-state platform)))
      ;; Try to parse error-buffer if we never got valid SSE data
      (when (and state
                 (not (llm-api--sse-state-header-parsed state))
                 (not (string-empty-p (llm-api--sse-state-error-buffer state))))
        (let ((raw (string-trim (llm-api--sse-state-error-buffer state))))
          (condition-case nil
              (let* ((json (json-parse-string raw :object-type 'plist :array-type 'list))
                     (err-data (plist-get json :error))
                     (msg (if (and (listp err-data) (plist-get err-data :message))
                              (plist-get err-data :message)
                            (format "%s" (or err-data json)))))
                (setf (llm-api--sse-state-errorp state) msg)
                (message "llm-api error: %s" msg))
            ;; Non-JSON error body (e.g. HTML from Cloudflare)
            (error
             (let ((msg (format "Unexpected response (not SSE): %.200s" raw)))
               (setf (llm-api--sse-state-errorp state) msg)
               (message "llm-api error: %s" msg))))))
      ;; Route to error, continue, or finish
      (cond
       ;; Error occurred
       ((and state (llm-api--sse-state-errorp state))
        (let ((err-msg (llm-api--sse-state-errorp state))
              (partial (or (llm-api--platform-last-response platform) "")))
          (if on-error
              (funcall on-error err-msg partial)
            ;; Backward compat: no on-error provided, fall back to on-finish
            (funcall on-finish))))
       ;; Continue generation (finish_reason: "length")
       ((and state (equal (llm-api--platform-finish-reason platform) "length"))
        (funcall on-continue))
       ;; Tool calls (finish_reason: "tool_calls")
       ((and state on-tool-calls
             (equal (llm-api--platform-finish-reason platform) "tool_calls"))
        (funcall on-tool-calls))
       ;; Normal completion
       (t (funcall on-finish))))))

;; Request construction

(cl-defmethod llm-api--get-request-headers ((_platform llm-api--platform))
  "Get the request headers for PLATFORM."
  '("Content-Type: application/json"
    "Accept: application/json"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--platform))
  "Generate request payload for PLATFORM, automatically including all params."
  `(:model ,(llm-api--get-selected-model platform)
    :messages ,(llm-api--get-history platform)
    ,@(llm-api--platform-params platform)
    ,@(when-let* ((tools (or (llm-api--platform-tools platform)
                              llm-api-default-tools)))
        (list :tools tools))
    :stream t))

(cl-defmethod llm-api--get-curl-params ((_platform llm-api--platform))
  "Get the curl command params for PLATFORM."
  '("-s" "-N" "-X" "POST" "--max-time" "0"))

(cl-defmethod llm-api--get-curl-url ((platform llm-api--platform))
  "Get the curl command url for PLATFORM."
  (llm-api--platform-url platform))

;; Streaming generation

(cl-defmethod llm-api--generate-streaming ((platform llm-api--platform) (prompt string) &rest args)
  "Backward-compatible generation entrypoint using PROMPT.
Mutates history based on PROMPT, then delegates to
`llm-api--generate-streaming-from-history'."
  (if (string-empty-p prompt)
      (llm-api--rewrite-last-assistant-as-continuation platform)
    (llm-api--add-user-message platform prompt))
  (apply #'llm-api--generate-streaming-from-history platform args))

(cl-defmethod llm-api--generate-streaming-from-history ((platform llm-api--platform) &rest args)
  "Generate from PLATFORM current history. ARGS for more control."
  ;; Initialize fresh state for this generation
  (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
  (setf (llm-api--platform-last-response platform) "")
  (setf (llm-api--platform-finish-reason platform) nil)
  ;; call the API
  (let* ((on-data (plist-get args :on-data))
         (on-finish (plist-get args :on-finish))
         (on-error (plist-get args :on-error))
         (on-tool-calls (plist-get args :on-tool-calls))
         (on-tool-start (plist-get args :on-tool-start))
         (on-tool-done (plist-get args :on-tool-done))
         (on-reasoning (plist-get args :on-reasoning))
         (on-reasoning-finalize (plist-get args :on-reasoning-finalize))
         (tool-loop-depth (or (plist-get args :tool-loop-depth) 0))
         (max-tool-loops (or (plist-get args :max-tool-loops) 10))
         ;; Store reasoning callbacks on sse-state
         (_ (let ((state (llm-api--platform-sse-state platform)))
              (when on-reasoning
                (setf (llm-api--sse-state-on-reasoning state) on-reasoning))
              (when on-reasoning-finalize
                (setf (llm-api--sse-state-on-reasoning-finalize state) on-reasoning-finalize))))
         (continue-generation (lambda ()
                                (llm-api--add-response-to-history platform :continuation)
                                (apply #'llm-api--generate-streaming-from-history platform args)))
         (handle-tool-calls
          (lambda ()
            (let* ((state (llm-api--platform-sse-state platform))
                   (tc-table (and state (llm-api--sse-state-tool-calls state)))
                   (tool-calls (llm-api--collect-tool-calls tc-table))
                   ;; If platform defines its own tools, respect its executor choice
                   ;; (nil = manual mode).  Only fall back to registry defaults
                   ;; when the platform has no per-platform tools.
                   (executor (if (llm-api--platform-tools platform)
                                 (llm-api--platform-tool-executor platform)
                               llm-api-default-tool-executor)))
              (if (and executor tool-calls (< tool-loop-depth max-tool-loops))
                  ;; Auto-loop: execute tools async, barrier for completion
                  (progn
                    (llm-api--add-response-to-history platform :tool-calls tool-calls)
                    (let* ((async-executor (if (llm-api--executor-is-async-p executor)
                                              executor
                                            (llm-api--wrap-sync-executor executor)))
                           (n (length tool-calls))
                           (remaining (cons n nil))
                           (results (make-vector n nil))
                           (tc-list tool-calls)
                           (next-args (append (list :tool-loop-depth (1+ tool-loop-depth)) args))
                           (barrier-fn
                            (lambda ()
                              ;; Fallback display when no widget callbacks
                              (when (and (null on-tool-start) (functionp on-data))
                                (dotimes (i n)
                                  (let* ((tc (nth i tc-list))
                                         (tc-name (plist-get tc :name))
                                         (tc-args (plist-get tc :arguments))
                                         (result-str (aref results i)))
                                    (funcall on-data (format "\n\n[tool: %s(%s) → %s]\n\n"
                                                             tc-name
                                                             (truncate-string-to-width tc-args 60)
                                                             (truncate-string-to-width result-str 100))))))
                              ;; Add all results to history
                              (dotimes (i n)
                                (let* ((tc (nth i tc-list))
                                       (result-str (aref results i)))
                                  (llm-api--add-to-history platform
                                    `((:role . :tool)
                                      (:tool_call_id . ,(plist-get tc :id))
                                      (:content . ,result-str)))))
                              ;; Continue generation
                              (apply #'llm-api--generate-streaming-from-history platform next-args))))
                      ;; Launch all tools in parallel
                      (dotimes (idx n)
                        (let* ((i idx) ; fresh binding for closures
                               (tc (nth i tc-list))
                               (name (plist-get tc :name))
                               (args-str (plist-get tc :arguments))
                               (args-parsed (condition-case nil
                                                (json-parse-string args-str :object-type 'plist)
                                              (error nil)))
                               (timeout-timer nil)
                               (completed (list nil)))
                          ;; Notify tool start
                          (when on-tool-start (funcall on-tool-start i name args-str))
                          ;; Start timeout
                          (setq timeout-timer
                                (run-at-time llm-api--tool-timeout nil
                                             (lambda ()
                                               (unless (car completed)
                                                 (setcar completed t)
                                                 (let ((result-str (format "[Timeout: %s after %ds]"
                                                                           name llm-api--tool-timeout)))
                                                   (aset results i result-str)
                                                   (when on-tool-done
                                                     (funcall on-tool-done i name result-str))
                                                   (cl-decf (car remaining))
                                                   (when (= (car remaining) 0)
                                                     (funcall barrier-fn)))))))
                          ;; Execute tool
                          (funcall async-executor name args-parsed args-str
                                   (lambda (result)
                                     (unless (car completed)
                                       (setcar completed t)
                                       (when timeout-timer (cancel-timer timeout-timer))
                                       (let ((result-str (if (stringp result)
                                                             result
                                                           (json-encode result))))
                                         (aset results i result-str)
                                         (when on-tool-done
                                           (funcall on-tool-done i name result-str))
                                         (cl-decf (car remaining))
                                         (when (= (car remaining) 0)
                                           (funcall barrier-fn))))))))))
                ;; Manual mode: callback or fallback to on-finish
                (if on-tool-calls
                    (funcall on-tool-calls tool-calls)
                  (llm-api--on-generation-finish-hook platform on-data)
                  (funcall on-finish))))))
         (filter (lambda (process line)
                   (llm-api--response-filter platform on-data process line)))
         (sentinel (lambda (process event)
                     (llm-api--process-sentinel platform
                                                (lambda ()
                                                  (llm-api--on-generation-finish-hook platform on-data)
                                                  (funcall on-finish))
                                                on-error
                                                continue-generation
                                                handle-tool-calls
                                                process event)))
         (request-payload (llm-api--get-request-payload platform))
         (request-headers (llm-api--get-request-headers platform))
         (curl-params (llm-api--get-curl-params platform)))
    ;; headers
    (dolist (header request-headers)
      (push header curl-params)
      ;; push adds at the *front*!
      (push "-H" curl-params))
    ;; api token
    (when-let* ((token (llm-api--platform-token platform)))
      (push (format "Authorization: Bearer %s" (llm-api--platform-token platform)) curl-params)
      ;; push adds at the *front*!
      (push "-H" curl-params))
    ;; full command
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
                                     :connection-type 'pipe
                                     :noquery t)))
          (setf (llm-api--platform-process platform) process))))))
