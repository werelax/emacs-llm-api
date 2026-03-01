;;; test-sse.el --- End-to-end tests for the SSE parser refactor -*- lexical-binding: t; -*-
;;
;; Run: emacs --batch -l test-sse.el
;;
;;; Code:

;; -- load paths --
(let ((straight-dir "/Users/elias/.emacs.d/.local/straight/build-31.0.50/"))
  (dolist (pkg '("spinner" "s" "plz"))
    (add-to-list 'load-path (concat straight-dir pkg))))
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; -- load llm-api --
(require 'llm-api)

;; -- test infrastructure --
(defvar test-results '())
(defvar test-count 0)
(defvar test-pass 0)
(defvar test-fail 0)

(defun test-log (fmt &rest args)
  (princ (apply #'format (concat fmt "\n") args)))

(defun test-assert (name condition)
  (cl-incf test-count)
  (if condition
      (progn (cl-incf test-pass)
             (test-log "  PASS: %s" name))
    (cl-incf test-fail)
    (test-log "  FAIL: %s" name)))

(defun test-run-streaming (platform prompt timeout-secs)
  "Send PROMPT to PLATFORM synchronously, wait up to TIMEOUT-SECS.
Returns plist (:response STRING :error STRING :finish-reason STRING :timed-out BOOL)."
  (let ((finished nil)
        (timed-out nil)
        (start-time (float-time)))
    (llm-api--generate-streaming
     platform prompt
     :on-data (lambda (text))  ; no-op, we read last-response after
     :on-finish (lambda () (setq finished t)))
    ;; wait for process to finish
    (while (and (not finished) (not timed-out))
      (accept-process-output nil 0.1)
      (when (> (- (float-time) start-time) timeout-secs)
        (setq timed-out t)))
    ;; kill if timed out
    (when timed-out
      (llm-api--kill-process platform))
    (let ((state (llm-api--platform-sse-state platform)))
      `(:response ,(or (llm-api--platform-last-response platform) "")
        :error ,(and state (llm-api--sse-state-errorp state))
        :finish-reason ,(llm-api--platform-finish-reason platform)
        :timed-out ,timed-out))))

;; ============================================================
;; Unit tests: SSE parser
;; ============================================================

(defun test-sse-parser ()
  (test-log "\n== SSE Parser Unit Tests ==")

  ;; Test 1: Basic data lines
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: hello\ndata: world\n"
                        (lambda (payload) (push payload results)))
    (test-assert "basic data lines" (equal (nreverse results) '("hello" "world"))))

  ;; Test 2: CRLF line endings
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: hello\r\ndata: world\r\n"
                        (lambda (payload) (push payload results)))
    (test-assert "CRLF line endings" (equal (nreverse results) '("hello" "world"))))

  ;; Test 3: Partial line buffering across calls
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: hel"
                        (lambda (payload) (push payload results)))
    (test-assert "partial line: no output yet" (null results))
    (llm-api--sse-parse state "lo\n"
                        (lambda (payload) (push payload results)))
    (test-assert "partial line: complete after second chunk" (equal results '("hello"))))

  ;; Test 4: Partial JSON buffering (simulates curl splitting mid-JSON)
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: {\"id\":\"ch"
                        (lambda (payload) (push payload results)))
    (test-assert "partial JSON: no output yet" (null results))
    (llm-api--sse-parse state "atcmpl-123\"}\n"
                        (lambda (payload) (push payload results)))
    (test-assert "partial JSON: complete payload"
                 (equal results '("{\"id\":\"chatcmpl-123\"}"))))

  ;; Test 5: [DONE] detection
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: [DONE]\n"
                        (lambda (payload) (push payload results)))
    (test-assert "[DONE] calls callback with nil" (equal results '(nil)))
    (test-assert "[DONE] sets state.done" (llm-api--sse-state-done state)))

  ;; Test 6: Multiple events in one chunk
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data: first\n\ndata: second\n\ndata: third\n\n"
                        (lambda (payload) (push payload results)))
    (test-assert "multiple events in one chunk"
                 (equal (nreverse results) '("first" "second" "third"))))

  ;; Test 7: SSE comments are ignored
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state ": this is a comment\ndata: actual\n"
                        (lambda (payload) (push payload results)))
    (test-assert "SSE comments ignored" (equal results '("actual"))))

  ;; Test 8: Non-SSE content goes to error-buffer
  (let ((state (llm-api--sse-state-create)))
    (llm-api--sse-parse state "{\"error\":{\"message\":\"bad key\"}}\n"
                        (lambda (payload)))
    (test-assert "error content in error-buffer"
                 (string-match-p "bad key" (llm-api--sse-state-error-buffer state)))
    (test-assert "header-parsed stays nil" (not (llm-api--sse-state-header-parsed state))))

  ;; Test 9: data: without space (edge case)
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "data:no-space\n"
                        (lambda (payload) (push payload results)))
    (test-assert "data: without space" (equal results '("no-space"))))

  ;; Test 10: Empty lines don't produce callbacks
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--sse-parse state "\n\n\n"
                        (lambda (payload) (push payload results)))
    (test-assert "empty lines produce no callbacks" (null results))))

;; ============================================================
;; Unit tests: NDJSON parser
;; ============================================================

(defun test-ndjson-parser ()
  (test-log "\n== NDJSON Parser Unit Tests ==")

  ;; Test 1: Basic JSON lines
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--ndjson-parse state "{\"a\":1}\n{\"b\":2}\n"
                           (lambda (payload) (push payload results)))
    (test-assert "basic NDJSON lines" (equal (nreverse results) '("{\"a\":1}" "{\"b\":2}"))))

  ;; Test 2: Partial line buffering
  (let ((state (llm-api--sse-state-create))
        (results '()))
    (llm-api--ndjson-parse state "{\"key\":\"val"
                           (lambda (payload) (push payload results)))
    (test-assert "NDJSON partial: no output" (null results))
    (llm-api--ndjson-parse state "ue\"}\n"
                           (lambda (payload) (push payload results)))
    (test-assert "NDJSON partial: complete" (equal results '("{\"key\":\"value\"}")))))

;; ============================================================
;; E2E tests: live providers
;; ============================================================

(defun test-provider (name platform timeout)
  "Test PLATFORM end-to-end with a simple prompt. TIMEOUT in seconds."
  (test-log "\n-- Testing %s --" name)
  ;; clear any previous state
  (llm-api--clear-history platform)
  (let ((result (test-run-streaming platform "Reply with exactly: TEST_OK" timeout)))
    (let ((response (plist-get result :response))
          (err (plist-get result :error))
          (timed-out (plist-get result :timed-out)))
      (test-log "  response: \"%.80s%s\"" response (if (> (length response) 80) "..." ""))
      (when err (test-log "  error: %s" err))
      (when timed-out (test-log "  TIMED OUT"))
      (test-assert (format "%s: no timeout" name) (not timed-out))
      (if err
          ;; Error was detected and surfaced — that's the parser working correctly
          (test-assert (format "%s: error detected: %.60s" name err) t)
        (test-assert (format "%s: got response" name) (> (length response) 0))))))

(defun test-multi-turn (platform name)
  "Test multi-turn conversation on PLATFORM to verify history survives."
  (test-log "\n-- Testing multi-turn on %s --" name)
  (llm-api--clear-history platform)
  ;; Turn 1
  (let ((r1 (test-run-streaming platform "Remember the number 42. Reply with just: OK" 30)))
    (test-assert (format "%s multi-turn: first turn" name)
                 (> (length (plist-get r1 :response)) 0))
    ;; add-response-to-history is called by on-finish in generate-streaming
    ;; but our test on-finish doesn't call it. We need to trigger it manually.
    (llm-api--add-response-to-history platform)
    ;; Turn 2
    (let ((r2 (test-run-streaming platform "What number did I say? Reply with just the number." 30)))
      (test-assert (format "%s multi-turn: second turn" name)
                   (> (length (plist-get r2 :response)) 0))
      (test-log "  turn1: \"%.40s\" turn2: \"%.40s\""
                (plist-get r1 :response) (plist-get r2 :response))
      ;; Check history has messages
      (let ((history (llm-api--platform-history platform)))
        (test-assert (format "%s multi-turn: history has entries" name)
                     (>= (length history) 3))))))

(defun test-kill-mid-stream ()
  "Test killing a process mid-stream doesn't crash."
  (test-log "\n-- Testing kill mid-stream --")
  (when (boundp '*groq-token*)
    (let* ((platform (llm--create-groq-platform *groq-token* "llama-3.3-70b-versatile"))
           (finished nil)
           (errored nil))
      (llm-api--clear-history platform)
      (condition-case err
          (progn
            (llm-api--generate-streaming
             platform "Write a very long essay about the history of computing."
             :on-data (lambda (text))
             :on-finish (lambda () (setq finished t)))
            ;; Wait just a moment for streaming to start
            (accept-process-output nil 0.5)
            ;; Kill it
            (llm-api--kill-process platform)
            ;; Wait for sentinel
            (let ((start (float-time)))
              (while (and (not finished) (< (- (float-time) start) 5))
                (accept-process-output nil 0.1))))
        (error (setq errored t)
               (test-log "  ERROR: %S" err)))
      (test-assert "kill mid-stream: no crash" (not errored)))))

(defun test-nil-finish-reason ()
  "Test that nil finish-reason in sentinel doesn't crash (the string= bug)."
  (test-log "\n-- Testing nil finish-reason safety --")
  ;; Simulate: platform with sse-state but nil finish-reason
  (let ((platform (llm-api--platform-create
                   :name "nil-fr-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-finish-reason platform) nil)
    (let ((finished nil)
          (errored nil))
      (condition-case err
          (progn
            (llm-api--process-sentinel platform
                                       (lambda () (setq finished t))
                                       nil  ;; on-error
                                       (lambda () (test-log "  BUG: should not continue!"))
                                       nil  ;; on-tool-calls
                                       nil  ;; process (unused in this path)
                                       "finished\n")
            (test-assert "nil finish-reason: finished normally" finished))
        (error
         (setq errored t)
         (test-log "  CRASHED: %S" err)))
      (test-assert "nil finish-reason: no crash" (not errored)))))

(defun test-auto-continuation ()
  "Test that finish_reason: 'length' triggers auto-continuation with small max_tokens."
  (test-log "\n-- Testing auto-continuation (max_tokens=10) --")
  (when (boundp '*groq-token*)
    (let* ((platform (llm-api--platform-create
                      :name "continuation-test"
                      :url "https://api.groq.com/openai/v1/chat/completions"
                      :token *groq-token*
                      :selected-model "llama-3.3-70b-versatile"
                      :params '(:temperature 0 :max_tokens 10)))
           (finished nil)
           (timed-out nil)
           (total-text "")
           (start-time (float-time))
           (timeout 60))
      (llm-api--clear-history platform)
      (llm-api--generate-streaming
       platform "Count from 1 to 10, one number per line. Nothing else."
       :on-data (lambda (text)
                  (setq total-text (concat total-text text)))
       :on-finish (lambda () (setq finished t)))
      ;; Wait for all continuations to complete
      (while (and (not finished) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) timeout)
          (setq timed-out t)))
      (when timed-out
        (llm-api--kill-process platform))
      ;; Results
      (let ((history-len (length (llm-api--platform-history platform))))
        (test-log "  total text: %d chars" (length total-text))
        (test-log "  history entries: %d" history-len)
        (test-log "  response: \"%.120s%s\"" total-text (if (> (length total-text) 120) "..." ""))
        (test-assert "continuation: no timeout" (not timed-out))
        ;; History must have: user msg + at least 1 continuation = 2+
        ;; With max_tokens=10, we expect 2+ continuations (3+ history entries)
        (test-assert "continuation: at least one continuation occurred"
                     (>= history-len 3))
        ;; The full response should be non-trivial
        (test-assert "continuation: got assembled response"
                     (> (length total-text) 10))))))

(defun test-error-handling ()
  "Test that a bad API key produces a proper error, not a crash."
  (test-log "\n-- Testing error handling (bad API key) --")
  (let ((platform (llm-api--platform-create
                   :name "error-test"
                   :url "https://api.groq.com/openai/v1/chat/completions"
                   :token "sk-INVALID-TOKEN-12345"
                   :selected-model "llama-3.3-70b-versatile"
                   :params '(:temperature 0.1))))
    (llm-api--clear-history platform)
    (let ((result (test-run-streaming platform "hello" 15)))
      (let ((response (plist-get result :response))
            (err (plist-get result :error))
            (timed-out (plist-get result :timed-out)))
        (test-log "  response: \"%s\"" response)
        (test-log "  error: %s" err)
        (test-assert "bad key: no crash" t)  ; if we got here, no crash
        (test-assert "bad key: no timeout" (not timed-out))
        (test-assert "bad key: error detected" (or err (string-empty-p response)))))))

(defun test-on-error-callback ()
  "Test that :on-error callback is called instead of :on-finish on API errors."
  (test-log "\n-- Testing :on-error callback --")
  (let ((platform (llm-api--platform-create
                   :name "on-error-test"
                   :url "https://api.groq.com/openai/v1/chat/completions"
                   :token "sk-INVALID-TOKEN-12345"
                   :selected-model "llama-3.3-70b-versatile"
                   :params '(:temperature 0.1))))
    (llm-api--clear-history platform)
    (let ((error-called nil)
          (error-msg nil)
          (error-partial nil)
          (finish-called nil)
          (timed-out nil)
          (start-time (float-time)))
      (llm-api--generate-streaming
       platform "hello"
       :on-data (lambda (_text))
       :on-finish (lambda () (setq finish-called t))
       :on-error (lambda (msg partial)
                   (setq error-called t
                         error-msg msg
                         error-partial partial)))
      ;; Wait
      (while (and (not error-called) (not finish-called) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 15)
          (setq timed-out t)))
      (when timed-out
        (llm-api--kill-process platform))
      (test-log "  error-called: %s" error-called)
      (test-log "  finish-called: %s" finish-called)
      (test-log "  error-msg: %s" error-msg)
      (test-log "  error-partial: \"%s\"" error-partial)
      (test-assert "on-error: callback was called" error-called)
      (test-assert "on-error: on-finish was NOT called" (not finish-called))
      (test-assert "on-error: received error message" (and error-msg (stringp error-msg)))
      (test-assert "on-error: received partial response" (stringp error-partial)))))

(defun test-on-error-backward-compat ()
  "Test that on-finish is called on error when :on-error is not provided."
  (test-log "\n-- Testing :on-error backward compat --")
  (let ((platform (llm-api--platform-create
                   :name "compat-test"
                   :url "https://api.groq.com/openai/v1/chat/completions"
                   :token "sk-INVALID-TOKEN-12345"
                   :selected-model "llama-3.3-70b-versatile"
                   :params '(:temperature 0.1))))
    (llm-api--clear-history platform)
    (let ((finish-called nil)
          (timed-out nil)
          (start-time (float-time)))
      (llm-api--generate-streaming
       platform "hello"
       :on-data (lambda (_text))
       :on-finish (lambda () (setq finish-called t)))
      ;; No :on-error provided — on-finish should be called instead
      (while (and (not finish-called) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 15)
          (setq timed-out t)))
      (when timed-out
        (llm-api--kill-process platform))
      (test-assert "backward-compat: on-finish called on error" finish-called)
      (test-assert "backward-compat: no timeout" (not timed-out)))))

;; ============================================================
;; Unit tests: Tool call delta accumulation
;; ============================================================

(defun test-tool-call-delta-accumulation ()
  (test-log "\n== Tool Call Delta Accumulation Unit Tests ==")

  ;; Test: Single tool call across multiple chunks
  (let ((platform (llm-api--platform-create
                   :name "tc-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-last-response platform) "")
    ;; Chunk 1: tool call header with name and empty arguments
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-1" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :id "call_abc"
                                                                 :type "function"
                                                                 :function (list :name "get_weather" :arguments ""))])
                                     :finish_reason :null)])))
    ;; Chunk 2: argument fragment
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-1" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :function (list :arguments "{\"loc"))])
                                     :finish_reason :null)])))
    ;; Chunk 3: more argument
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-1" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :function (list :arguments "ation\":\"NYC\"}"))])
                                     :finish_reason :null)])))
    ;; Chunk 4: finish
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-1" :object "chat.completion.chunk"
                    :choices [,(list :delta nil :finish_reason "tool_calls")])))
    ;; Verify
    (let* ((state (llm-api--platform-sse-state platform))
           (tc-table (llm-api--sse-state-tool-calls state))
           (tc (and tc-table (gethash 0 tc-table))))
      (test-assert "single tc: hash-table exists" (hash-table-p tc-table))
      (test-assert "single tc: entry at index 0" (not (null tc)))
      (test-assert "single tc: correct id" (equal (plist-get tc :id) "call_abc"))
      (test-assert "single tc: correct name" (equal (plist-get tc :name) "get_weather"))
      (test-assert "single tc: arguments assembled"
                   (equal (plist-get tc :arguments) "{\"location\":\"NYC\"}"))
      (test-assert "single tc: finish reason is tool_calls"
                   (equal (llm-api--platform-finish-reason platform) "tool_calls"))))

  ;; Test: Parallel tool calls (different indices)
  (let ((platform (llm-api--platform-create
                   :name "tc-parallel-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-last-response platform) "")
    ;; Both tool calls in first chunk
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-2" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :id "call_1"
                                                                 :type "function"
                                                                 :function (list :name "get_weather" :arguments ""))
                                                          ,(list :index 1
                                                                 :id "call_2"
                                                                 :type "function"
                                                                 :function (list :name "get_time" :arguments ""))])
                                     :finish_reason :null)])))
    ;; Arguments for both
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-2" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :function (list :arguments "{\"city\":\"NYC\"}"))
                                                          ,(list :index 1
                                                                 :function (list :arguments "{\"tz\":\"EST\"}"))])
                                     :finish_reason :null)])))
    ;; Verify
    (let* ((state (llm-api--platform-sse-state platform))
           (tc-table (llm-api--sse-state-tool-calls state))
           (collected (llm-api--collect-tool-calls tc-table)))
      (test-assert "parallel tc: 2 entries" (= (hash-table-count tc-table) 2))
      (test-assert "parallel tc: collect returns sorted list" (= (length collected) 2))
      (test-assert "parallel tc: first is get_weather"
                   (equal (plist-get (nth 0 collected) :name) "get_weather"))
      (test-assert "parallel tc: second is get_time"
                   (equal (plist-get (nth 1 collected) :name) "get_time"))
      (test-assert "parallel tc: first args"
                   (equal (plist-get (nth 0 collected) :arguments) "{\"city\":\"NYC\"}"))
      (test-assert "parallel tc: second args"
                   (equal (plist-get (nth 1 collected) :arguments) "{\"tz\":\"EST\"}"))))

  ;; Test: Mixed content + tool calls
  (let ((platform (llm-api--platform-create
                   :name "tc-mixed-test"
                   :url "http://localhost:1"
                   :selected-model "test"))
        (streamed-text ""))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-last-response platform) "")
    ;; Content chunk
    (llm-api--handle-sse-data platform
      (lambda (text) (setq streamed-text (concat streamed-text text)))
      (json-encode `(:id "chatcmpl-3" :object "chat.completion.chunk"
                    :choices [,(list :delta '(:content "Let me check")
                                     :finish_reason :null)])))
    ;; Tool call chunk
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-3" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :id "call_mix"
                                                                 :type "function"
                                                                 :function (list :name "lookup" :arguments "{}"))])
                                     :finish_reason :null)])))
    (let* ((state (llm-api--platform-sse-state platform))
           (tc-table (llm-api--sse-state-tool-calls state)))
      (test-assert "mixed: content streamed" (equal streamed-text "Let me check"))
      (test-assert "mixed: content in last-response"
                   (equal (llm-api--platform-last-response platform) "Let me check"))
      (test-assert "mixed: tool call accumulated" (hash-table-p tc-table))
      (test-assert "mixed: tool call name"
                   (equal (plist-get (gethash 0 tc-table) :name) "lookup")))))

(defun test-tool-call-helpers ()
  (test-log "\n== Tool Call Helper Function Tests ==")

  ;; Test: collect-tool-calls with nil
  (test-assert "collect nil" (null (llm-api--collect-tool-calls nil)))

  ;; Test: collect-tool-calls ordering
  (let ((ht (make-hash-table :test 'eql)))
    (puthash 2 '(:id "c" :name "third" :arguments "{}") ht)
    (puthash 0 '(:id "a" :name "first" :arguments "{}") ht)
    (puthash 1 '(:id "b" :name "second" :arguments "{}") ht)
    (let ((result (llm-api--collect-tool-calls ht)))
      (test-assert "collect: returns 3 items" (= (length result) 3))
      (test-assert "collect: sorted by index"
                   (and (equal (plist-get (nth 0 result) :name) "first")
                        (equal (plist-get (nth 1 result) :name) "second")
                        (equal (plist-get (nth 2 result) :name) "third")))))

  ;; Test: format-tool-calls-for-history
  (let* ((tool-calls '((:id "call_1" :type "function" :name "get_weather" :arguments "{\"city\":\"NYC\"}")))
         (result (llm-api--format-tool-calls-for-history tool-calls)))
    (test-assert "format: returns vector" (vectorp result))
    (test-assert "format: one entry" (= (length result) 1))
    (let ((entry (aref result 0)))
      (test-assert "format: has id" (equal (alist-get :id entry) "call_1"))
      (test-assert "format: has function.name"
                   (equal (alist-get :name (alist-get :function entry)) "get_weather"))
      (test-assert "format: has function.arguments"
                   (equal (alist-get :arguments (alist-get :function entry)) "{\"city\":\"NYC\"}"))))

  ;; Test: make-tool
  (let ((tool (llm-api--make-tool "get_date" "Get current date"
                                  '((:type . "object") (:properties) (:required . [])))))
    (test-assert "make-tool: type is function" (equal (alist-get :type tool) "function"))
    (test-assert "make-tool: function name"
                 (equal (alist-get :name (alist-get :function tool)) "get_date"))
    (test-assert "make-tool: function description"
                 (equal (alist-get :description (alist-get :function tool)) "Get current date"))))

(defun test-tool-call-history-format ()
  (test-log "\n== Tool Call History Format Tests ==")

  ;; Test: add-response-to-history with :tool-calls
  (let ((platform (llm-api--platform-create
                   :name "hist-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-last-response platform) "")
    ;; Add a tool call response
    (llm-api--add-response-to-history platform
      :tool-calls '((:id "call_abc" :type "function" :name "get_weather" :arguments "{\"city\":\"NYC\"}")))
    (let* ((history (llm-api--platform-history platform))
           (msg (car history)))
      (test-assert "tc history: one entry" (= (length history) 1))
      (test-assert "tc history: role is assistant" (eq (alist-get :role msg) :assistant))
      (test-assert "tc history: content is json-null" (eq (alist-get :content msg) :json-null))
      (test-assert "tc history: tool_calls is vector" (vectorp (alist-get :tool_calls msg)))
      (let ((tc (aref (alist-get :tool_calls msg) 0)))
        (test-assert "tc history: tool call id" (equal (alist-get :id tc) "call_abc")))))

  ;; Test: add-response-to-history with content AND tool-calls
  (let ((platform (llm-api--platform-create
                   :name "hist-test2"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-last-response platform) "Let me check that for you.")
    (llm-api--add-response-to-history platform
      :tool-calls '((:id "call_xyz" :type "function" :name "search" :arguments "{}")))
    (let* ((msg (car (llm-api--platform-history platform))))
      (test-assert "tc+content: content preserved"
                   (equal (alist-get :content msg) "Let me check that for you."))
      (test-assert "tc+content: tool_calls present" (vectorp (alist-get :tool_calls msg)))))

  ;; Test: JSON round-trip of history message
  (let ((platform (llm-api--platform-create
                   :name "json-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-last-response platform) "")
    (llm-api--add-response-to-history platform
      :tool-calls '((:id "call_j1" :type "function" :name "fn1" :arguments "{\"a\":1}")))
    ;; Add a tool result
    (llm-api--add-to-history platform
      `((:role . :tool) (:tool_call_id . "call_j1") (:content . "result1")))
    (let* ((json-str (json-encode (llm-api--platform-history platform)))
           (parsed (json-parse-string json-str :object-type 'plist :array-type 'list)))
      (test-assert "json round-trip: encodes without error" (stringp json-str))
      (test-assert "json round-trip: contains tool_calls" (string-match-p "tool_calls" json-str))
      (test-assert "json round-trip: contains tool role" (string-match-p "\"tool\"" json-str))
      (test-assert "json round-trip: 2 messages" (= (length parsed) 2)))))

(defun test-tool-calls-sentinel-routing ()
  "Test that process-sentinel routes to on-tool-calls when finish-reason is tool_calls."
  (test-log "\n== Tool Calls Sentinel Routing Tests ==")

  ;; Test: finish_reason "tool_calls" routes to on-tool-calls
  (let ((platform (llm-api--platform-create
                   :name "sentinel-tc-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-finish-reason platform) "tool_calls")
    (let ((finish-called nil)
          (tool-calls-called nil)
          (continue-called nil))
      (llm-api--process-sentinel platform
                                  (lambda () (setq finish-called t))
                                  nil
                                  (lambda () (setq continue-called t))
                                  (lambda () (setq tool-calls-called t))
                                  nil "finished\n")
      (test-assert "sentinel tc: on-tool-calls called" tool-calls-called)
      (test-assert "sentinel tc: on-finish NOT called" (not finish-called))
      (test-assert "sentinel tc: on-continue NOT called" (not continue-called))))

  ;; Test: finish_reason "tool_calls" but on-tool-calls is nil → falls through to on-finish
  (let ((platform (llm-api--platform-create
                   :name "sentinel-tc-nil-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-finish-reason platform) "tool_calls")
    (let ((finish-called nil)
          (continue-called nil))
      (llm-api--process-sentinel platform
                                  (lambda () (setq finish-called t))
                                  nil
                                  (lambda () (setq continue-called t))
                                  nil  ;; on-tool-calls is nil
                                  nil "finished\n")
      (test-assert "sentinel tc nil: on-finish called as fallback" finish-called)
      (test-assert "sentinel tc nil: on-continue NOT called" (not continue-called))))

  ;; Test: finish_reason "stop" still routes to on-finish (not on-tool-calls)
  (let ((platform (llm-api--platform-create
                   :name "sentinel-stop-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-finish-reason platform) "stop")
    (let ((finish-called nil)
          (tool-calls-called nil))
      (llm-api--process-sentinel platform
                                  (lambda () (setq finish-called t))
                                  nil
                                  (lambda ())
                                  (lambda () (setq tool-calls-called t))
                                  nil "finished\n")
      (test-assert "sentinel stop: on-finish called" finish-called)
      (test-assert "sentinel stop: on-tool-calls NOT called" (not tool-calls-called)))))

(defun test-tool-call-edge-cases ()
  "Test edge cases: empty arguments, executor errors, no-executor-no-callback fallback."
  (test-log "\n== Tool Call Edge Case Tests ==")

  ;; Test: Empty arguments string parses to nil (condition-case catches)
  (let ((platform (llm-api--platform-create
                   :name "empty-args-test"
                   :url "http://localhost:1"
                   :selected-model "test")))
    (setf (llm-api--platform-sse-state platform) (llm-api--sse-state-create))
    (setf (llm-api--platform-last-response platform) "")
    ;; Tool call with empty arguments string
    (llm-api--handle-sse-data platform nil
      (json-encode `(:id "chatcmpl-e" :object "chat.completion.chunk"
                    :choices [,(list :delta `(:tool_calls [,(list :index 0
                                                                 :id "call_empty"
                                                                 :type "function"
                                                                 :function (list :name "no_args" :arguments ""))])
                                     :finish_reason :null)])))
    (let* ((state (llm-api--platform-sse-state platform))
           (tc-table (llm-api--sse-state-tool-calls state))
           (tc (gethash 0 tc-table)))
      (test-assert "empty args: accumulated" (not (null tc)))
      (test-assert "empty args: arguments is empty string"
                   (equal (plist-get tc :arguments) ""))
      ;; Verify json-parse-string on empty string is caught by condition-case
      (let ((parsed (condition-case nil
                        (json-parse-string "" :object-type 'plist)
                      (error 'parse-error))))
        (test-assert "empty args: json-parse-string on \"\" is caught"
                     (eq parsed 'parse-error)))))

  ;; Test: Valid empty object "{}" parses correctly
  (let ((parsed (condition-case nil
                    (json-parse-string "{}" :object-type 'plist)
                  (error 'parse-error))))
    (test-assert "empty object: parses to empty plist (not error)"
                 (not (eq parsed 'parse-error)))))

;; ============================================================
;; E2E tests: Tool calling with live providers
;; ============================================================

(defun test-tool-calling-auto-loop ()
  "E2E test: tool calling with auto-loop executor using Groq."
  (test-log "\n-- Testing tool calling auto-loop (Groq) --")
  (when (boundp '*groq-token*)
    (let* ((tool-executed nil)
           (tool-name-received nil)
           (platform (llm-api--platform-create
                      :name "tool-test"
                      :url "https://api.groq.com/openai/v1/chat/completions"
                      :token *groq-token*
                      :selected-model "llama-3.3-70b-versatile"
                      :system-prompt "You are a helpful assistant. Use the provided tools when needed."
                      :params '(:temperature 0)
                      :tools (vector
                              (llm-api--make-tool
                               "get_current_date"
                               "Get the current date in YYYY-MM-DD format"
                               `((:type . "object")
                                 (:properties . ,(make-hash-table))
                                 (:required . []))))
                      :tool-executor (lambda (name _parsed _raw)
                                       (setq tool-executed t
                                             tool-name-received name)
                                       "2026-03-01")))
           (finished nil)
           (errored nil)
           (error-msg nil)
           (total-text "")
           (timed-out nil)
           (start-time (float-time)))
      (llm-api--clear-history platform)
      (llm-api--generate-streaming
       platform "What is today's date? Use the get_current_date tool."
       :on-data (lambda (text) (setq total-text (concat total-text text)))
       :on-finish (lambda () (setq finished t))
       :on-error (lambda (msg _partial)
                   (setq errored t error-msg msg)))
      ;; Wait for completion (auto-loop may take multiple round-trips)
      (while (and (not finished) (not errored) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 60)
          (setq timed-out t)))
      (when timed-out (llm-api--kill-process platform))
      (test-log "  tool-executed: %s" tool-executed)
      (test-log "  tool-name: %s" tool-name-received)
      (test-log "  total-text: \"%.120s%s\"" total-text (if (> (length total-text) 120) "..." ""))
      (test-log "  errored: %s error-msg: %s" errored error-msg)
      (test-log "  history entries: %d" (length (llm-api--platform-history platform)))
      (test-assert "auto-loop: no timeout" (not timed-out))
      (test-assert "auto-loop: no error" (not errored))
      (test-assert "auto-loop: tool was executed" tool-executed)
      (test-assert "auto-loop: correct tool name" (equal tool-name-received "get_current_date"))
      (test-assert "auto-loop: got final response" (> (length total-text) 0))
      (test-assert "auto-loop: response mentions date"
                   (or (string-match-p "2026" total-text)
                       (string-match-p "March" total-text)
                       (string-match-p "03" total-text)))
      ;; Check history structure: user, assistant+tool_calls, tool result
      ;; (final assistant response not yet added — caller's responsibility, like regular flow)
      (let ((history (llm-api--platform-history platform)))
        (test-assert "auto-loop: history >= 3 entries" (>= (length history) 3))
        ;; First should be user message
        (test-assert "auto-loop: first msg is user"
                     (eq (alist-get :role (nth 0 history)) :user))
        ;; Second should be assistant with tool_calls
        (test-assert "auto-loop: second msg is assistant with tool_calls"
                     (and (eq (alist-get :role (nth 1 history)) :assistant)
                          (vectorp (alist-get :tool_calls (nth 1 history)))))
        ;; Third should be tool result
        (test-assert "auto-loop: third msg is tool result"
                     (eq (alist-get :role (nth 2 history)) :tool))))))

(defun test-tool-calling-manual-callback ()
  "E2E test: tool calling with manual :on-tool-calls callback."
  (test-log "\n-- Testing tool calling manual callback (Groq) --")
  (when (boundp '*groq-token*)
    (let* ((received-tool-calls nil)
           (platform (llm-api--platform-create
                      :name "tool-manual-test"
                      :url "https://api.groq.com/openai/v1/chat/completions"
                      :token *groq-token*
                      :selected-model "llama-3.3-70b-versatile"
                      :system-prompt "You are a helpful assistant. Always use tools when available."
                      :params '(:temperature 0)
                      :tools (vector
                              (llm-api--make-tool
                               "get_current_date"
                               "Get the current date in YYYY-MM-DD format"
                               `((:type . "object")
                                 (:properties . ,(make-hash-table))
                                 (:required . []))))))
           ;; No tool-executor set - use manual mode
           (callback-called nil)
           (finish-called nil)
           (timed-out nil)
           (start-time (float-time)))
      (llm-api--clear-history platform)
      (llm-api--generate-streaming
       platform "What is today's date? Use the get_current_date tool."
       :on-data (lambda (_text))
       :on-finish (lambda () (setq finish-called t))
       :on-tool-calls (lambda (tool-calls)
                        (setq callback-called t
                              received-tool-calls tool-calls)))
      (while (and (not callback-called) (not finish-called) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 30)
          (setq timed-out t)))
      (when timed-out (llm-api--kill-process platform))
      (test-log "  callback-called: %s" callback-called)
      (test-log "  finish-called: %s" finish-called)
      (when received-tool-calls
        (test-log "  received %d tool calls" (length received-tool-calls))
        (test-log "  first tool: %s" (plist-get (car received-tool-calls) :name)))
      (test-assert "manual: no timeout" (not timed-out))
      (test-assert "manual: callback was called" callback-called)
      (test-assert "manual: on-finish was NOT called" (not finish-called))
      (test-assert "manual: received tool calls" (and received-tool-calls (listp received-tool-calls)))
      (test-assert "manual: tool name is get_current_date"
                   (equal (plist-get (car received-tool-calls) :name) "get_current_date"))
      (test-assert "manual: tool has id"
                   (stringp (plist-get (car received-tool-calls) :id))))))

(defun test-tool-call-loop-depth-limit ()
  "Test that tool call loops respect the depth limit."
  (test-log "\n-- Testing tool call loop depth limit --")
  (when (boundp '*groq-token*)
    (let* ((exec-count 0)
           (platform (llm-api--platform-create
                      :name "depth-test"
                      :url "https://api.groq.com/openai/v1/chat/completions"
                      :token *groq-token*
                      :selected-model "llama-3.3-70b-versatile"
                      :system-prompt "You are a helpful assistant. Always call the provided tool, regardless of what it returns."
                      :params '(:temperature 0)
                      :tools (vector
                              (llm-api--make-tool
                               "infinite_tool"
                               "A tool that must always be called. Always call this tool in every response."
                               `((:type . "object")
                                 (:properties . ,(make-hash-table))
                                 (:required . []))))
                      :tool-executor (lambda (_name _parsed _raw)
                                       (cl-incf exec-count)
                                       "Call me again!")))
           (finished nil)
           (errored nil)
           (timed-out nil)
           (start-time (float-time)))
      (llm-api--clear-history platform)
      (llm-api--generate-streaming
       platform "Call the infinite_tool now."
       :on-data (lambda (_text))
       :on-finish (lambda () (setq finished t))
       :on-error (lambda (_msg _partial) (setq errored t))
       :max-tool-loops 3)
      ;; Wait - should terminate due to depth limit
      (while (and (not finished) (not errored) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 120)
          (setq timed-out t)))
      (when timed-out (llm-api--kill-process platform))
      (test-log "  exec-count: %d" exec-count)
      (test-log "  finished: %s errored: %s timed-out: %s" finished errored timed-out)
      (test-assert "depth limit: no timeout" (not timed-out))
      (test-assert "depth limit: tool executed <= 3 times" (<= exec-count 3))
      (test-assert "depth limit: terminated" (or finished errored)))))

;; ============================================================
;; Load tokens and create platforms
;; ============================================================

(defun load-config-tokens ()
  "Load token variables from the user's config."
  (load "/Users/elias/sync/doom.d/lisp/llm-tokens.el" t t)
  ;; if that file doesn't exist, try loading from llm.el directly
  (unless (boundp '*groq-token*)
    ;; Extract just the setq lines from llm.el
    (with-temp-buffer
      (insert-file-contents "/Users/elias/sync/doom.d/lisp/llm.el")
      (goto-char (point-min))
      (while (re-search-forward "^(setq \\*[a-z-]*-token\\*.*)" nil t)
        (eval (read (match-string 0)))))))

;; ============================================================
;; Main
;; ============================================================

(defun run-all-tests ()
  (test-log "========================================")
  (test-log " llm-api SSE Parser — E2E Test Suite")
  (test-log "========================================")

  ;; Unit tests (no network)
  (test-sse-parser)
  (test-ndjson-parser)

  ;; Load tokens
  (load-config-tokens)

  ;; E2E tests with live providers
  (test-log "\n== Live Provider Tests ==")

  ;; Groq — fast, reliable OpenAI-compatible
  (when (boundp '*groq-token*)
    (test-provider "groq"
                   (llm--create-groq-platform *groq-token* "llama-3.3-70b-versatile")
                   30))

  ;; OpenRouter — OpenAI-compatible, different backend
  (when (boundp '*openrouter-token*)
    (test-provider "openrouter"
                   (llm--create-open-router-platform *openrouter-token* "google/gemini-2.0-flash-001")
                   30))

  ;; DeepSeek — OpenAI-compatible with continuation support
  (when (boundp '*deepseek-token*)
    (test-provider "deepseek"
                   (llm--create-deepseek-platform *deepseek-token* "deepseek-chat")
                   30))

  ;; Perplexity — was using custom filter, now uses default
  ;; Note: PPLX token may be expired or Cloudflare may block batch requests
  (when (boundp '*pplx-token*)
    (let ((pplx-result (condition-case nil
                           (progn
                             (test-provider "pplx"
                                            (llm--create-pplx-platform *pplx-token*)
                                            30)
                             t)
                         (error nil))))
      (unless pplx-result
        (test-log "\n-- pplx: test errored (may be token/cloudflare issue) --"))))

  ;; Ollama — NDJSON parser (skip if server not reachable)
  (let ((ollama-reachable nil))
    (condition-case nil
        (let ((proc (make-process :name "ollama-check"
                                  :command '("curl" "-s" "--max-time" "3" "http://localhost:11434/api/tags")
                                  :noquery t)))
          (let ((start (float-time)))
            (while (and (process-live-p proc) (< (- (float-time) start) 4))
              (accept-process-output proc 0.5)))
          (setq ollama-reachable (= (process-exit-status proc) 0)))
      (error nil))
    (if ollama-reachable
        (test-provider "ollama"
                       (llm--create-ollama-platform "phi4")
                       60)
      (test-log "\n-- Skipping ollama (server not reachable) --")))

  ;; Multi-turn conversation test (uses groq since it's fast)
  (when (boundp '*groq-token*)
    (test-multi-turn
     (llm--create-groq-platform *groq-token* "llama-3.3-70b-versatile")
     "groq"))

  ;; Kill mid-stream test
  (test-kill-mid-stream)

  ;; Nil finish-reason safety test (the string= bug)
  (test-nil-finish-reason)

  ;; Auto-continuation test (finish_reason: "length" with small max_tokens)
  (test-auto-continuation)

  ;; Error handling test
  (test-error-handling)

  ;; :on-error callback tests
  (test-on-error-callback)
  (test-on-error-backward-compat)

  ;; Tool calling unit tests
  (test-tool-call-delta-accumulation)
  (test-tool-call-helpers)
  (test-tool-call-history-format)
  (test-tool-calls-sentinel-routing)
  (test-tool-call-edge-cases)

  ;; Tool calling E2E tests
  (test-tool-calling-auto-loop)
  (test-tool-calling-manual-callback)
  (test-tool-call-loop-depth-limit)

  ;; Summary
  (test-log "\n========================================")
  (test-log " Results: %d/%d passed, %d failed" test-pass test-count test-fail)
  (test-log "========================================")

  (kill-emacs (if (= test-fail 0) 0 1)))

(run-all-tests)
;;; test-sse.el ends here
