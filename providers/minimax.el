;;; minimax.el --- MiniMax provider -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--minimax (:include llm-api--openai)
                                 (:constructor llm-api--minimax-create)
                                 (:copier nil))
  ;; Streaming parser state for MiniMax <think> ... </think> tags.
  (think-open nil)
  (think-carry ""))

;; MiniMax doesn't have a /models endpoint, so we return a static list
(cl-defmethod llm-api--get-available-models ((platform llm-api--minimax))
  "Return static list of MiniMax models."
  (let ((models '("MiniMax-M2.5-highspeed"
                  "MiniMax-M2.5"
                  "MiniMax-M2.1-highspeed"
                  "MiniMax-M2.1"
                  "MiniMax-M2")))
    (setf (llm-api--platform-available-models platform) models)
    models))

(defun llm-api--minimax--split-partial-marker (text marker)
  "Split TEXT into (EMIT . CARRY) for possible partial MARKER at the end.
CARRY is the longest suffix of TEXT that matches a prefix of MARKER."
  (let* ((max-k (min (length text) (1- (length marker))))
         (carry-len 0))
    (cl-loop for k from max-k downto 1
             when (string-suffix-p (substring marker 0 k) text)
             do (setq carry-len k)
             and return t)
    (cons (substring text 0 (- (length text) carry-len))
          (if (> carry-len 0)
              (substring text (- (length text) carry-len))
            ""))))

(defun llm-api--minimax--emit-normal (platform text on-data)
  "Emit assistant TEXT as normal content for PLATFORM."
  (when (> (length text) 0)
    (cl-callf concat (llm-api--platform-last-response platform) text)
    (when (functionp on-data)
      (funcall on-data text))))

(defun llm-api--minimax--emit-reasoning (platform text)
  "Emit reasoning TEXT callbacks for PLATFORM."
  (when (> (length text) 0)
    (let ((state (llm-api--platform-sse-state platform)))
      (when state
        (setf (llm-api--sse-state-reasoning-active state) t)
        (when-let* ((on-reasoning (llm-api--sse-state-on-reasoning state)))
          (funcall on-reasoning text))))))

(defun llm-api--minimax--finalize-reasoning (platform)
  "Finalize reasoning callbacks for PLATFORM if active."
  (let ((state (llm-api--platform-sse-state platform)))
    (when state
      (setf (llm-api--sse-state-reasoning-active state) nil)
      (when-let* ((finalize-fn (llm-api--sse-state-on-reasoning-finalize state)))
        (funcall finalize-fn)))))

(defun llm-api--minimax--process-content (platform content-delta on-data)
  "Process MiniMax CONTENT-DELTA with inline <think> tags.
Reasoning blocks are dispatched through reasoning callbacks; normal content
is appended to response and streamed via ON-DATA."
  (let* ((open "<think>")
         (close "</think>")
         (text (concat (or (llm-api--minimax-think-carry platform) "") content-delta))
         (in-think (llm-api--minimax-think-open platform))
         (carry ""))
    (while (> (length text) 0)
      (if in-think
          (let ((close-pos (string-match (regexp-quote close) text)))
            (if close-pos
                (progn
                  (llm-api--minimax--emit-reasoning platform (substring text 0 close-pos))
                  (llm-api--minimax--finalize-reasoning platform)
                  (setq in-think nil)
                  ;; MiniMax often emits extra newlines right after </think>.
                  ;; Drop leading whitespace here so collapsed reasoning widgets
                  ;; don't leave large visible gaps before following content/tools.
                  (setq text (replace-regexp-in-string
                              "\\`[ \t\n\r]+" ""
                              (substring text (+ close-pos (length close))))))
              (pcase-let* ((`(,emit . ,new-carry)
                            (llm-api--minimax--split-partial-marker text close)))
                (llm-api--minimax--emit-reasoning platform emit)
                (setq carry new-carry)
                (setq text ""))))
        (let ((open-pos (string-match (regexp-quote open) text)))
          (if open-pos
              (progn
                (llm-api--minimax--emit-normal platform (substring text 0 open-pos) on-data)
                (setq in-think t)
                (setq text (substring text (+ open-pos (length open)))))
            (pcase-let* ((`(,emit . ,new-carry)
                          (llm-api--minimax--split-partial-marker text open)))
              (llm-api--minimax--emit-normal platform emit on-data)
              (setq carry new-carry)
              (setq text ""))))))
    (setf (llm-api--minimax-think-open platform) in-think
          (llm-api--minimax-think-carry platform) carry)))

(cl-defmethod llm-api--generate-streaming-from-history :before ((platform llm-api--minimax) &rest _args)
  "Reset MiniMax think parser state before each generation request."
  (setf (llm-api--minimax-think-open platform) nil
        (llm-api--minimax-think-carry platform) ""))

(cl-defmethod llm-api--on-clear-history :after ((platform llm-api--minimax))
  "Reset MiniMax think parser state when clearing history."
  (setf (llm-api--minimax-think-open platform) nil
        (llm-api--minimax-think-carry platform) ""))

(cl-defmethod llm-api--on-generation-finish-hook ((platform llm-api--minimax) _on-data)
  "Finalize dangling MiniMax reasoning block if a stream ends inside <think>."
  (when (llm-api--minimax-think-open platform)
    (llm-api--minimax--finalize-reasoning platform))
  (setf (llm-api--minimax-think-open platform) nil
        (llm-api--minimax-think-carry platform) ""))

(cl-defmethod llm-api--handle-sse-data ((platform llm-api--minimax) on-data payload)
  "Handle a single SSE data PAYLOAD for MiniMax (OpenAI-compatible + <think> tags)."
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
                    ;; reasoning_content delta (if provider sends it directly)
                    (when (stringp reasoning-delta)
                      (setf (llm-api--sse-state-reasoning-active state) t)
                      (when-let* ((on-reasoning (llm-api--sse-state-on-reasoning state)))
                        (funcall on-reasoning reasoning-delta)))
                    ;; content delta may contain <think>...</think>
                    (when (stringp content-delta)
                      (llm-api--minimax--process-content platform content-delta on-data))
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

(defun llm--create-minimax-platform (token &optional selected-model)
  (llm-api--minimax-create
   :name "minimax"
   :url "https://api.minimax.io/v1/chat/completions"
   :token token
   :selected-model (or selected-model "MiniMax-M2.5-highspeed")
   :system-prompt "You are a helpful assistant."
   :params '(:temperature 0.7)))
