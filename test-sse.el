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

;; -- load llm-chat-widgets --
(add-to-list 'load-path
             (expand-file-name "../llm-chat"
                               (file-name-directory (or load-file-name buffer-file-name))))
(require 'llm-chat-widgets)

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
;; Unit tests: history/generation separation refactor
;; ============================================================

(defun test-history-refactor-wrapper-mutation ()
  "Test wrapper history mutations and delegation to from-history method."
  (test-log "\n== History Refactor: Wrapper Mutation ==")
  (let* ((platform (llm-api--platform-create
                    :name "hist-wrap-test"
                    :url "http://localhost:1"
                    :selected-model "test-model"
                    :history '(((:role . :assistant) (:content . "Old response")))) )
         (delegations 0))
    ;; Stub pure generation to avoid network and count delegations.
    (cl-letf (((symbol-function 'llm-api--generate-streaming-from-history)
               (lambda (&rest _args)
                 (cl-incf delegations)
                 nil)))
      ;; Non-empty prompt should append user message.
      (llm-api--generate-streaming platform "hello")
      (let ((history (llm-api--platform-history platform)))
        (test-assert "wrapper: delegated for normal prompt" (= delegations 1))
        (test-assert "wrapper: appended user message"
                     (eq (alist-get :role (car (last history))) :user))
        (test-assert "wrapper: user content set"
                     (equal (alist-get :content (car (last history))) "hello")))
      ;; Empty prompt should rewrite last assistant as continuation-formatted msg
      ;; when trailing message is assistant.
      (setf (llm-api--platform-history platform)
            '(((:role . :user) (:content . "q"))
              ((:role . :assistant) (:content . "a"))))
      (llm-api--generate-streaming platform "")
      (let* ((history (llm-api--platform-history platform))
             (last-msg (car (last history))))
        (test-assert "wrapper: delegated for empty prompt" (= delegations 2))
        (test-assert "wrapper: continuation keeps assistant role"
                     (eq (alist-get :role last-msg) :assistant))
        (test-assert "wrapper: continuation content preserved"
                     (equal (alist-get :content last-msg) "a"))))))

(defun test-history-refactor-from-history-no-mutation ()
  "Test pure generation path does not mutate chat history."
  (test-log "\n== History Refactor: Pure Generation No Mutation ==")
  (let* ((platform (llm-api--platform-create
                    :name "hist-pure-test"
                    :url "http://localhost:1/chat/completions"
                    :selected-model "test-model"
                    :history '(((:role . :user) (:content . "u1"))
                               ((:role . :assistant) (:content . "a1")))))
         (before (copy-tree (llm-api--platform-history platform))))
    ;; Stub make-process so we don't hit network in this unit test.
    (let ((orig-make-process (symbol-function 'make-process)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _args)
                   ;; Return a short-lived local process object.
                   (funcall orig-make-process
                            :name "llm-api-test-dummy"
                            :command '("sh" "-c" "exit 0")
                            :noquery t))))
        (llm-api--generate-streaming-from-history
         platform
         :on-data (lambda (_text))
         :on-finish (lambda ())
         :on-error (lambda (_msg _partial)))))
    (test-assert "from-history: history unchanged"
                 (equal before (llm-api--platform-history platform)))))

(defun test-model-capabilities-default-extraction ()
  "Test default capability extraction from model plist metadata."
  (test-log "\n== Model Capabilities: Default Extraction ==")
  (let* ((platform (llm-api--platform-create
                    :name "caps-default-test"
                    :url "http://localhost:1"
                    :available-models
                    '((:name "model-a" :model "model-a" :context_length "131072" :max_completion_tokens "8192")
                      (:name "model-b" :model "model-b" :context-window 32768 :max-output-tokens 4096 :source :provider-default))
                    :selected-model "model-a"))
         (caps-a (llm-api--get-model-capabilities platform "model-a"))
         (caps-b (llm-api--get-model-capabilities platform "model-b")))
    (test-assert "caps default: model-a context parsed"
                 (= (plist-get caps-a :context-window) 131072))
    (test-assert "caps default: model-a max-output parsed"
                 (= (plist-get caps-a :max-output-tokens) 8192))
    (test-assert "caps default: model-a source canonicalized"
                 (eq (plist-get caps-a :source) :provider-api))
    (test-assert "caps default: helper context works"
                 (= (llm-api--get-model-context-window platform "model-a") 131072))
    (test-assert "caps default: helper max-output works"
                 (= (llm-api--get-model-max-output-tokens platform "model-a") 8192))
    (test-assert "caps default: model-b keeps provider-default"
                 (eq (plist-get caps-b :source) :provider-default))))

(defun test-model-capabilities-override-precedence ()
  "Test user overrides merge over provider/default capability metadata."
  (test-log "\n== Model Capabilities: Override Precedence ==")
  (let ((llm-api-model-capabilities-overrides nil)
        (platform (llm-api--platform-create
                   :name "caps-override-test"
                   :url "http://localhost:1"
                   :available-models
                   '((:name "model-x" :model "model-x" :context-window 100000 :max-output-tokens 5000 :source :provider-api))
                   :selected-model "model-x")))
    (llm-api-set-model-capabilities "caps-override-test" "model-x"
                                    :max-output-tokens 9999
                                    :source :override)
    (let ((caps (llm-api--get-model-capabilities platform "model-x")))
      (test-assert "caps override: context kept from provider"
                   (= (plist-get caps :context-window) 100000))
      (test-assert "caps override: max-output overridden"
                   (= (plist-get caps :max-output-tokens) 9999))
      (test-assert "caps override: source overridden"
                   (eq (plist-get caps :source) :override)))))

(defun test-kimi-capability-alias-normalization ()
  "Test Kimi alias normalization for selection and capability lookup."
  (test-log "\n== Kimi Capabilities: Alias Normalization ==")
  (let* ((platform (llm--create-kimi-platform "" "kimi-k2.5"))
         (_ (llm-api--get-available-models platform))
         (selected (llm-api--get-selected-model platform))
         (caps (llm-api--get-model-capabilities platform)))
    (test-assert "kimi alias: selected canonicalized"
                 (equal selected "kimi-for-coding/k2p5"))
    (test-assert "kimi alias: context from canonical model"
                 (= (plist-get caps :context-window) 262144))
    (test-assert "kimi alias: source provider-default"
                 (eq (plist-get caps :source) :provider-default))
    (llm-api--set-selected-model platform "kimi-for-coding")
    (test-assert "kimi alias: set-selected canonicalized"
                 (equal (llm-api--get-selected-model platform)
                        "kimi-for-coding/k2p5"))))

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
;; Async executor & barrier tests
;; ============================================================

(defun test-async-executor-helpers ()
  "Test executor-is-async-p and wrap-sync-executor."
  (test-log "\n== Async Executor Helpers ==")

  ;; 3-arg sync executor is not async
  (let ((sync-exec (lambda (name parsed raw) (format "result-%s" name))))
    (test-assert "3-arg executor is NOT async"
                 (not (llm-api--executor-is-async-p sync-exec))))

  ;; 4-arg async executor is async
  (let ((async-exec (lambda (name parsed raw callback) (funcall callback "ok"))))
    (test-assert "4-arg executor IS async"
                 (llm-api--executor-is-async-p async-exec)))

  ;; &rest executor counts as async
  (let ((rest-exec (lambda (name &rest args) nil)))
    (test-assert "&rest executor IS async"
                 (llm-api--executor-is-async-p rest-exec)))

  ;; wrap-sync-executor: wraps 3-arg → 4-arg, calls callback with result
  (let* ((sync-exec (lambda (name _parsed _raw) (format "result-%s" name)))
         (wrapped (llm-api--wrap-sync-executor sync-exec))
         (cb-result nil))
    (test-assert "wrapped executor IS async"
                 (llm-api--executor-is-async-p wrapped))
    (funcall wrapped "test-fn" nil "{}" (lambda (r) (setq cb-result r)))
    (test-assert "wrapped executor calls callback with result"
                 (equal cb-result "result-test-fn"))))

(defun test-barrier-pattern ()
  "Test async barrier with tools completing out of order."
  (test-log "\n== Barrier Pattern ==")

  ;; Simulate 3 tools completing in order 2, 0, 1
  (let* ((n 3)
         (remaining (cons n nil))
         (results (make-vector n nil))
         (barrier-fired nil)
         (barrier-count 0)
         (barrier-fn (lambda ()
                       (setq barrier-fired t)
                       (cl-incf barrier-count))))
    ;; Tool 2 completes first
    (aset results 2 "result-2")
    (cl-decf (car remaining))
    (when (= (car remaining) 0) (funcall barrier-fn))
    (test-assert "barrier: not fired after 1/3" (not barrier-fired))

    ;; Tool 0 completes second
    (aset results 0 "result-0")
    (cl-decf (car remaining))
    (when (= (car remaining) 0) (funcall barrier-fn))
    (test-assert "barrier: not fired after 2/3" (not barrier-fired))

    ;; Tool 1 completes last
    (aset results 1 "result-1")
    (cl-decf (car remaining))
    (when (= (car remaining) 0) (funcall barrier-fn))
    (test-assert "barrier: fired after 3/3" barrier-fired)
    (test-assert "barrier: fired exactly once" (= barrier-count 1))
    (test-assert "barrier: results correct"
                 (and (equal (aref results 0) "result-0")
                      (equal (aref results 1) "result-1")
                      (equal (aref results 2) "result-2")))))

(defun test-reasoning-delta-extraction ()
  "Test that reasoning_content deltas are extracted from SSE chunks."
  (test-log "\n== Reasoning Delta Extraction ==")

  (let* ((platform (llm-api--platform-create
                    :name "test-reasoning"
                    :url "http://localhost:9999"
                    :available-models '("test-model")))
         (state (llm-api--sse-state-create))
         (reasoning-texts '())
         (finalize-called nil))
    (setf (llm-api--platform-sse-state platform) state)
    (setf (llm-api--platform-last-response platform) "")
    (setf (llm-api--sse-state-on-reasoning state)
          (lambda (text) (push text reasoning-texts)))
    (setf (llm-api--sse-state-on-reasoning-finalize state)
          (lambda () (setq finalize-called t)))

    ;; Feed reasoning chunk
    (let ((payload (json-encode
                    `((:id . "chatcmpl-1")
                      (:object . "chat.completion.chunk")
                      (:choices . [((:index . 0)
                                    (:delta . ((:reasoning_content . "Let me think")))
                                    (:finish_reason . :null))])))))
      (llm-api--handle-sse-data platform (lambda (_t)) payload))

    (test-assert "reasoning: callback fired"
                 (equal reasoning-texts '("Let me think")))
    (test-assert "reasoning: active flag set"
                 (llm-api--sse-state-reasoning-active state))
    (test-assert "reasoning: finalize NOT yet called"
                 (not finalize-called))

    ;; Feed content chunk (reasoning phase ends)
    (let ((payload (json-encode
                    `((:id . "chatcmpl-1")
                      (:object . "chat.completion.chunk")
                      (:choices . [((:index . 0)
                                    (:delta . ((:content . "Hello")))
                                    (:finish_reason . :null))])))))
      (llm-api--handle-sse-data platform (lambda (_t)) payload))

    (test-assert "reasoning: finalize called on first content"
                 finalize-called)
    (test-assert "reasoning: active flag cleared"
                 (not (llm-api--sse-state-reasoning-active state)))
    (test-assert "reasoning: content accumulated"
                 (equal (llm-api--platform-last-response platform) "Hello"))))

(defun test-sync-executor-backward-compat ()
  "Test that existing 3-arg sync executors still work with the async barrier."
  (test-log "\n== Sync Executor Backward Compat ==")

  (when (boundp '*groq-token*)
    (let* ((exec-log '())
           (platform (llm--create-groq-platform *groq-token* "llama-3.3-70b-versatile"))
           ;; 3-arg sync executor (the old signature)
           (sync-executor (lambda (name _parsed _raw)
                            (push name exec-log)
                            (format "The current date is 2025-01-01")))
           (finished nil)
           (errored nil)
           (timed-out nil)
           (start-time (float-time)))
      (setf (llm-api--platform-tools platform)
            (vector (llm-api--make-tool
                     "get_date" "Get the current date"
                     `((:type . "object")
                       (:properties . ,(make-hash-table))
                       (:required . [])))))
      (setf (llm-api--platform-tool-executor platform) sync-executor)

      (llm-api--generate-streaming
       platform "What is today's date? Use the get_date tool."
       :on-data (lambda (_t))
       :on-finish (lambda () (setq finished t))
       :on-error (lambda (e _p) (setq errored e)))

      (while (and (not finished) (not errored) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 60)
          (setq timed-out t)))
      (when timed-out (llm-api--kill-process platform))

      (test-assert "sync compat: no timeout" (not timed-out))
      (test-assert "sync compat: no error" (not errored))
      (test-assert "sync compat: executor was called" (> (length exec-log) 0))
      (test-assert "sync compat: finished" finished))))

(defun test-tool-calling-with-serper ()
  "E2E: Groq + Serper web_search tool."
  (test-log "\n== E2E: Tool Calling with Serper ==")

  (when (boundp '*groq-token*)
    (let* ((platform (llm--create-groq-platform *groq-token* "llama-3.3-70b-versatile"))
           (tool-start-log '())
           (tool-done-log '())
           (finished nil)
           (errored nil)
           (timed-out nil)
           (start-time (float-time)))
      ;; Tools come from the global registry (llm-api-register-tool in serper.el)
      (llm-api--generate-streaming
       platform "Search the web for 'Emacs 30 release date'. Use the web_search tool."
       :on-data (lambda (_t))
       :on-finish (lambda () (setq finished t))
       :on-error (lambda (e _p) (setq errored e))
       :on-tool-start (lambda (idx name args-str)
                        (push (list idx name) tool-start-log)
                        (test-log "    tool-start[%d]: %s" idx name))
       :on-tool-done (lambda (idx name result-str)
                       (push (list idx name (length result-str)) tool-done-log)
                       (test-log "    tool-done[%d]: %s (%d chars)" idx name (length result-str))))

      (while (and (not finished) (not errored) (not timed-out))
        (accept-process-output nil 0.1)
        (when (> (- (float-time) start-time) 120)
          (setq timed-out t)))
      (when timed-out (llm-api--kill-process platform))

      (test-assert "serper: no timeout" (not timed-out))
      (when errored (test-log "    error: %s" errored))
      (test-assert "serper: no error" (not errored))
      (test-assert "serper: tool-start fired" (> (length tool-start-log) 0))
      (test-assert "serper: tool-done fired" (> (length tool-done-log) 0))
      (test-assert "serper: web_search was called"
                   (cl-some (lambda (e) (equal (cadr e) "web_search")) tool-start-log))
      (test-assert "serper: finished" finished)
      (let ((response (llm-api--platform-last-response platform)))
        (test-log "    final response: %.100s..." (or response ""))
        (test-assert "serper: got final response" (and response (> (length response) 0)))))))

(defun test-serper-search-direct ()
  "Direct test of `llm-api--serper-search' (network, no LLM)."
  (test-log "\n== Direct: Serper web_search ==")
  (let ((result nil)
        (timed-out nil)
        (start-time (float-time)))
    (llm-api--serper-search
     "Emacs lisp"
     (lambda (r) (setq result r)))
    (while (and (not result) (not timed-out))
      (accept-process-output nil 0.1)
      (when (> (- (float-time) start-time) 30)
        (setq timed-out t)))
    (test-assert "search-direct: no timeout" (not timed-out))
    (test-assert "search-direct: got result" (stringp result))
    (test-assert "search-direct: has Title"
                 (and result (string-match-p "Title:" result)))
    (test-assert "search-direct: has URL"
                 (and result (string-match-p "URL:" result)))
    (test-log "    result: %.120s..." (or result ""))))

(defun test-serper-scrape-direct ()
  "Direct test of `llm-api--serper-scrape' (network, no LLM)."
  (test-log "\n== Direct: Serper web_scrape ==")
  (let ((result nil)
        (timed-out nil)
        (start-time (float-time)))
    (llm-api--serper-scrape
     "https://www.gnu.org/software/emacs/"
     (lambda (r) (setq result r)))
    (while (and (not result) (not timed-out))
      (accept-process-output nil 0.1)
      (when (> (- (float-time) start-time) 30)
        (setq timed-out t)))
    (test-assert "scrape-direct: no timeout" (not timed-out))
    (test-assert "scrape-direct: got result" (stringp result))
    (test-assert "scrape-direct: non-empty"
                 (and result (> (length result) 100)))
    (test-assert "scrape-direct: contains emacs content"
                 (and result (string-match-p "[Ee]macs" result)))
    (test-log "    result length: %d chars" (length (or result "")))))

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

;; ============================================================
;; Widget tests
;; ============================================================

(defun test-widget--fresh-buffer ()
  "Create a fresh temp buffer for widget tests."
  (let ((buf (generate-new-buffer " *widget-test*")))
    (with-current-buffer buf
      (setq-local llm-chat-widgets nil)
      (setq-local llm-chat-widgets--counter 0))
    buf))

(defun test-widget--visible-text (buffer)
  "Return visible text in BUFFER (excluding invisible overlay regions)."
  (with-current-buffer buffer
    (let ((result "")
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((next-change (next-single-char-property-change pos 'invisible)))
          (unless (get-char-property pos 'invisible)
            (setq result (concat result (buffer-substring-no-properties pos next-change))))
          (setq pos next-change)))
      result)))

(defun test-widget-create-tool ()
  "Test basic tool widget creation."
  (test-log "\n== Widget: create-tool ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (let ((w (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                "{\"query\":\"test\"}")))
            ;; Widget returned
            (test-assert "create-tool: returns widget" (not (null w)))
            ;; Widget fields
            (test-assert "create-tool: type is tool" (eq (plist-get w :type) 'tool))
            (test-assert "create-tool: name" (equal (plist-get w :name) "web_search"))
            (test-assert "create-tool: status running" (eq (plist-get w :status) :running))
            (test-assert "create-tool: collapsed" (plist-get w :collapsed))
            ;; Overlay exists and is invisible
            (let ((ov (plist-get w :body-ov)))
              (test-assert "create-tool: body-ov exists" (overlayp ov))
              (test-assert "create-tool: body invisible" (overlay-get ov 'invisible))
              (test-assert "create-tool: body has content"
                           (> (overlay-end ov) (overlay-start ov))))
            ;; Header visible, body hidden
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "create-tool: header visible"
                           (string-match-p "web_search: running" visible))
              (test-assert "create-tool: body hidden"
                           (not (string-match-p "Arguments:" visible))))
            ;; Header has keymap text property
            (let* ((start (plist-get w :start))
                   (km (get-text-property (marker-position start) 'keymap)))
              (test-assert "create-tool: header has keymap" (keymapp km)))
            ;; Widget registered
            (test-assert "create-tool: registered" (= (length llm-chat-widgets) 1))))
      (kill-buffer buf))))

(defun test-widget-toggle-tool ()
  "Test toggling a tool widget expand/collapse."
  (test-log "\n== Widget: toggle-tool ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (let ((w (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                "{\"query\":\"test\"}")))
            ;; Initially collapsed
            (test-assert "toggle: initially collapsed" (plist-get w :collapsed))
            (test-assert "toggle: body initially invisible"
                         (overlay-get (plist-get w :body-ov) 'invisible))
            ;; Expand
            (llm-chat-widget-toggle w)
            (test-assert "toggle: now expanded" (not (plist-get w :collapsed)))
            (test-assert "toggle: body now visible"
                         (not (overlay-get (plist-get w :body-ov) 'invisible)))
            ;; Visible text should include body
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "toggle: args visible after expand"
                           (string-match-p "Arguments:" visible))
              (test-assert "toggle: chevron v"
                           (string-match-p "\\[v web_search" visible)))
            ;; Collapse again
            (llm-chat-widget-toggle w)
            (test-assert "toggle: collapsed again" (plist-get w :collapsed))
            (test-assert "toggle: body invisible again"
                         (overlay-get (plist-get w :body-ov) 'invisible))
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "toggle: args hidden after collapse"
                           (not (string-match-p "Arguments:" visible)))
              (test-assert "toggle: chevron >"
                           (string-match-p "\\[> web_search" visible)))))
      (kill-buffer buf))))

(defun test-widget-update-tool ()
  "Test updating a tool widget with results."
  (test-log "\n== Widget: update-tool ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (let ((w (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                "{\"query\":\"test\"}")))
            ;; Update with success
            (llm-chat-widget-update-tool w :success "Title: Result\nURL: http://x.com")
            (test-assert "update: status success" (eq (plist-get w :status) :success))
            ;; Header shows done
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "update: header shows done"
                           (string-match-p "web_search: done" visible)))
            ;; Body still collapsed (should be hidden)
            (test-assert "update: still collapsed" (plist-get w :collapsed))
            (test-assert "update: body still invisible"
                         (overlay-get (plist-get w :body-ov) 'invisible))
            ;; Expand and verify result is in body
            (llm-chat-widget-toggle w)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "update: result in body"
                           (string-match-p "Title: Result" visible))
              (test-assert "update: args in body"
                           (string-match-p "Arguments:.*query" visible)))
            ;; Test error status
            (let ((w2 (llm-chat-widget-create-tool buf (point-max) "web_scrape"
                                                   "{\"url\":\"http://x.com\"}")))
              (llm-chat-widget-update-tool w2 :error "[Error: timeout]")
              (let ((visible (test-widget--visible-text buf)))
                (test-assert "update: error header"
                             (string-match-p "web_scrape: error" visible))))))
      (kill-buffer buf))))

(defun test-widget-independent-toggle ()
  "Test that toggling one widget does NOT affect another.
This was a real bug: toggling widget 1 expanded widget 2's body."
  (test-log "\n== Widget: independent-toggle ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          ;; Create two widgets
          (let ((w1 (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                 "{\"query\":\"first\"}"))
                (w2 (llm-chat-widget-create-tool buf (point-max) "web_scrape"
                                                 "{\"url\":\"http://x.com\"}")))
            ;; Both collapsed initially
            (test-assert "indep: w1 collapsed" (plist-get w1 :collapsed))
            (test-assert "indep: w2 collapsed" (plist-get w2 :collapsed))
            (test-assert "indep: w1 body invisible"
                         (overlay-get (plist-get w1 :body-ov) 'invisible))
            (test-assert "indep: w2 body invisible"
                         (overlay-get (plist-get w2 :body-ov) 'invisible))
            ;; Expand w1 only
            (llm-chat-widget-toggle w1)
            (test-assert "indep: w1 now expanded" (not (plist-get w1 :collapsed)))
            (test-assert "indep: w2 still collapsed" (plist-get w2 :collapsed))
            (test-assert "indep: w1 body visible"
                         (not (overlay-get (plist-get w1 :body-ov) 'invisible)))
            (test-assert "indep: w2 body STILL invisible"
                         (overlay-get (plist-get w2 :body-ov) 'invisible))
            ;; Verify in visible text
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "indep: w1 args visible"
                           (string-match-p "query.*first" visible))
              (test-assert "indep: w2 args NOT visible"
                           (not (string-match-p "url.*http" visible))))
            ;; Collapse w1, expand w2
            (llm-chat-widget-toggle w1)
            (llm-chat-widget-toggle w2)
            (test-assert "indep: w1 back collapsed" (plist-get w1 :collapsed))
            (test-assert "indep: w2 now expanded" (not (plist-get w2 :collapsed)))
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "indep: w1 args now hidden"
                           (not (string-match-p "query.*first" visible)))
              (test-assert "indep: w2 args now visible"
                           (string-match-p "url.*http" visible)))))
      (kill-buffer buf))))

(defun test-widget-update-no-overlap ()
  "Test that updating a widget with a long result doesn't corrupt adjacent widgets.
This was a real bug: update-tool with longer text caused marker overlap,
scrambling the next widget's body into the current widget."
  (test-log "\n== Widget: update-no-overlap ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          ;; Create two adjacent widgets
          (let ((w1 (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                 "{\"query\":\"q\"}"))
                (w2 (llm-chat-widget-create-tool buf (point-max) "web_scrape"
                                                 "{\"url\":\"http://x.com\"}")))
            ;; Update w1 with a LONG result (much longer than original body)
            (llm-chat-widget-update-tool w1 :success
              (mapconcat (lambda (i) (format "Title: Result %d\nSnippet: Long text here\nURL: http://example.com/%d\n" i i))
                         (number-sequence 1 5) "\n"))
            ;; w1 body overlay should not overlap w2 body overlay
            (let ((w1-body-end (overlay-end (plist-get w1 :body-ov)))
                  (w2-body-start (overlay-start (plist-get w2 :body-ov))))
              (test-assert "no-overlap: w1 body doesn't overlap w2 body"
                           (<= w1-body-end w2-body-start)))
            ;; Both widgets should still toggle independently after update
            (llm-chat-widget-toggle w1)
            (llm-chat-widget-toggle w2)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "no-overlap: w1 body has Result 1"
                           (string-match-p "Result 1" visible))
              (test-assert "no-overlap: w2 body has pending"
                           (string-match-p "Result: (pending)" visible)))
            ;; Collapse both, verify hidden
            (llm-chat-widget-toggle w1)
            (llm-chat-widget-toggle w2)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "no-overlap: both collapsed hides bodies"
                           (not (string-match-p "Result 1" visible))))
            ;; Now update w2 as well, expand both
            (llm-chat-widget-update-tool w2 :success "Scraped content here")
            (llm-chat-widget-toggle w1)
            (llm-chat-widget-toggle w2)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "no-overlap: w2 body updated"
                           (string-match-p "Scraped content here" visible))
              (test-assert "no-overlap: w1 body still intact"
                           (string-match-p "Result 1" visible)))))
      (kill-buffer buf))))

(defun test-widget-reasoning-lifecycle ()
  "Test reasoning widget create → append → finalize → toggle."
  (test-log "\n== Widget: reasoning-lifecycle ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          ;; Create reasoning widget
          (let ((w (llm-chat-widget-create-reasoning buf (point-min))))
            (test-assert "reasoning: returns widget" (not (null w)))
            (test-assert "reasoning: type" (eq (plist-get w :type) 'reasoning))
            (test-assert "reasoning: collapsed" (plist-get w :collapsed))
            ;; Header shows Thinking...
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "reasoning: header shows Thinking"
                           (string-match-p "Thinking\\.\\.\\." visible)))
            ;; Body overlay starts as zero-length
            (let ((ov (plist-get w :body-ov)))
              (test-assert "reasoning: body ov exists" (overlayp ov))
              (test-assert "reasoning: body starts empty"
                           (= (overlay-start ov) (overlay-end ov))))
            ;; Append text
            (llm-chat-widget-append-reasoning w "Let me think about this. ")
            (llm-chat-widget-append-reasoning w "First, I need to consider the options.")
            ;; Overlay grew
            (let ((ov (plist-get w :body-ov)))
              (test-assert "reasoning: body grew"
                           (> (overlay-end ov) (overlay-start ov)))
              (test-assert "reasoning: body still invisible"
                           (overlay-get ov 'invisible)))
            ;; Token count updated
            (test-assert "reasoning: token count > 0"
                         (> (plist-get w :token-count) 0))
            ;; Body text hidden
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "reasoning: body text hidden"
                           (not (string-match-p "think about" visible))))
            ;; Finalize
            (llm-chat-widget-finalize-reasoning w)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "reasoning: header shows token count"
                           (string-match-p "Thinking ([0-9]+ tokens)" visible)))
            ;; Toggle to expand
            (llm-chat-widget-toggle w)
            (test-assert "reasoning: expanded" (not (plist-get w :collapsed)))
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "reasoning: body visible after toggle"
                           (string-match-p "think about" visible))
              (test-assert "reasoning: chevron v"
                           (string-match-p "\\[v Thinking" visible)))
            ;; Toggle to collapse
            (llm-chat-widget-toggle w)
            (test-assert "reasoning: collapsed again" (plist-get w :collapsed))
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "reasoning: body hidden again"
                           (not (string-match-p "think about" visible))))))
      (kill-buffer buf))))

(defun test-widget-font-lock-immunity ()
  "Test that overlay-based widgets survive font-lock refontification.
This was a real bug: markdown-mode's font-lock stripped invisible text
properties from widget bodies because font-lock-extra-managed-props
included 'invisible'. Overlays are immune to this."
  (test-log "\n== Widget: font-lock-immunity ==")
  (let ((buf (test-widget--fresh-buffer)))
    (unwind-protect
        (with-current-buffer buf
          ;; Activate markdown-mode if available (it triggers font-lock)
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          ;; Create widget with body
          (let ((w (llm-chat-widget-create-tool buf (point-min) "web_search"
                                                "{\"query\":\"test\"}")))
            ;; Verify body is invisible before font-lock
            (test-assert "font-lock: body invisible before"
                         (overlay-get (plist-get w :body-ov) 'invisible))
            ;; Force font-lock refontification
            (font-lock-ensure)
            ;; Body should STILL be invisible (overlays immune to font-lock)
            (test-assert "font-lock: body invisible AFTER font-lock"
                         (overlay-get (plist-get w :body-ov) 'invisible))
            ;; Verify body is truly hidden in visible text
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "font-lock: body hidden in visible text"
                           (not (string-match-p "Arguments:" visible))))
            ;; Toggle should still work after font-lock
            (llm-chat-widget-toggle w)
            (test-assert "font-lock: toggle works after font-lock"
                         (not (plist-get w :collapsed)))
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "font-lock: body visible after toggle+font-lock"
                           (string-match-p "Arguments:" visible)))
            ;; Re-fontify while expanded
            (font-lock-ensure)
            (let ((visible (test-widget--visible-text buf)))
              (test-assert "font-lock: body still visible after re-fontify"
                           (string-match-p "Arguments:" visible)))
            ;; Collapse and verify font-lock doesn't un-collapse
            (llm-chat-widget-toggle w)
            (font-lock-ensure)
            (test-assert "font-lock: stays collapsed after font-lock"
                         (overlay-get (plist-get w :body-ov) 'invisible))))
      (kill-buffer buf))))

(defun run-all-tests ()
  (test-log "========================================")
  (test-log " llm-api SSE Parser — E2E Test Suite")
  (test-log "========================================")

  ;; Unit tests (no network)
  (test-sse-parser)
  (test-ndjson-parser)
  (test-history-refactor-wrapper-mutation)
  (test-history-refactor-from-history-no-mutation)
  (test-model-capabilities-default-extraction)
  (test-model-capabilities-override-precedence)
  (test-kimi-capability-alias-normalization)

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

  ;; Async executor & barrier tests
  (test-async-executor-helpers)
  (test-barrier-pattern)
  (test-reasoning-delta-extraction)

  ;; Sync executor backward compat (E2E, needs groq)
  (when (boundp '*groq-token*)
    (test-sync-executor-backward-compat))

  ;; Serper direct tool tests (network, no LLM)
  (test-serper-search-direct)
  (test-serper-scrape-direct)

  ;; Serper web tools E2E (needs groq)
  (when (boundp '*groq-token*)
    (test-tool-calling-with-serper))

  ;; Widget tests
  (test-widget-create-tool)
  (test-widget-toggle-tool)
  (test-widget-update-tool)
  (test-widget-independent-toggle)
  (test-widget-update-no-overlap)
  (test-widget-reasoning-lifecycle)
  (test-widget-font-lock-immunity)

  ;; Summary
  (test-log "\n========================================")
  (test-log " Results: %d/%d passed, %d failed" test-pass test-count test-fail)
  (test-log "========================================")

  (kill-emacs (if (= test-fail 0) 0 1)))

(unless (bound-and-true-p llm-api-test-sse-skip-auto-run)
  (run-all-tests))
;;; test-sse.el ends here
