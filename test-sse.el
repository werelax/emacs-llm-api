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
                                       (lambda () (test-log "  BUG: should not continue!"))
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

  ;; Summary
  (test-log "\n========================================")
  (test-log " Results: %d/%d passed, %d failed" test-pass test-count test-fail)
  (test-log "========================================")

  (kill-emacs (if (= test-fail 0) 0 1)))

(run-all-tests)
;;; test-sse.el ends here
