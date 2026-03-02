;;; sse.el --- SSE and NDJSON stream parsers for llm-api -*- lexical-binding: t; -*-
;;
;; Provides robust, provider-agnostic parsers for Server-Sent Events (SSE)
;; and Newline-Delimited JSON (NDJSON) streams with proper partial-line
;; buffering and error detection.

;;; Code:

(require 'cl-lib)

(cl-defstruct (llm-api--sse-state
               (:constructor llm-api--sse-state-create)
               (:copier nil))
  "Per-generation state for SSE stream parsing."
  (buffer "")           ; leftover partial line from last filter call
  (error-buffer "")     ; accumulates non-SSE content for HTTP error detection
  (header-parsed nil)   ; whether we've seen at least one data: line
  (errorp nil)          ; error message string if an error was received
  (done nil)            ; set to t when [DONE] is received
  (tool-calls nil)      ; hash-table: integer index -> plist (:id :type :name :arguments)
  (on-reasoning nil)    ; callback for reasoning_content deltas
  (on-reasoning-finalize nil) ; callback when reasoning phase ends (first content delta)
  (reasoning-active nil)) ; t while reasoning deltas are streaming

(defun llm-api--sse-parse (state output callback)
  "Parse SSE stream data from OUTPUT using STATE for buffering.
STATE is an `llm-api--sse-state' struct (per-generation, not global).
CALLBACK is called with each complete SSE data payload string.
When `data: [DONE]' is received, CALLBACK is called with nil."
  (let* ((full (concat (llm-api--sse-state-buffer state) output))
         ;; Split on literal \n — we strip \r from each line after
         (parts (split-string full "\n"))
         ;; Last element is the trailing partial line (or "" if full ended with \n)
         (partial (car (last parts)))
         ;; All complete lines (everything except the trailing partial)
         (lines (butlast parts)))
    ;; Save partial line for next call
    (setf (llm-api--sse-state-buffer state) partial)
    ;; Process each complete line
    (dolist (raw-line lines)
      ;; Strip trailing \r (handles both \r\n and bare \n origins)
      (let ((line (if (and (> (length raw-line) 0)
                           (= (aref raw-line (1- (length raw-line))) ?\r))
                      (substring raw-line 0 -1)
                    raw-line)))
        (cond
         ;; Empty line: SSE event boundary / keep-alive
         ((string-empty-p line) nil)
         ;; data: line with space after colon (standard)
         ((string-prefix-p "data: " line)
          (setf (llm-api--sse-state-header-parsed state) t)
          (let ((payload (substring line 6)))
            (if (string= payload "[DONE]")
                (progn
                  (setf (llm-api--sse-state-done state) t)
                  (funcall callback nil))
              (funcall callback payload))))
         ;; data: line without space (edge case per SSE spec)
         ((string-prefix-p "data:" line)
          (setf (llm-api--sse-state-header-parsed state) t)
          (let ((payload (substring line 5)))
            (if (string= payload "[DONE]")
                (progn
                  (setf (llm-api--sse-state-done state) t)
                  (funcall callback nil))
              (funcall callback payload))))
         ;; SSE comment (starts with :)
         ((string-prefix-p ":" line) nil)
         ;; Other SSE fields we don't use
         ((or (string-prefix-p "event:" line)
              (string-prefix-p "id:" line)
              (string-prefix-p "retry:" line))
          nil)
         ;; Unrecognized content — likely an HTTP error body
         (t
          (when (not (llm-api--sse-state-header-parsed state))
            (setf (llm-api--sse-state-error-buffer state)
                  (concat (llm-api--sse-state-error-buffer state) line "\n")))))))))

(defun llm-api--ndjson-parse (state output callback)
  "Parse NDJSON stream data from OUTPUT using STATE for buffering.
STATE is an `llm-api--sse-state' struct (reused for buffering).
CALLBACK is called with each complete JSON line string.
Unlike SSE, there is no `data:' prefix — each line is raw JSON."
  (let* ((full (concat (llm-api--sse-state-buffer state) output))
         (parts (split-string full "\n"))
         (partial (car (last parts)))
         (lines (butlast parts)))
    ;; Save partial line for next call
    (setf (llm-api--sse-state-buffer state) partial)
    ;; Process each complete line
    (dolist (raw-line lines)
      (let ((line (if (and (> (length raw-line) 0)
                           (= (aref raw-line (1- (length raw-line))) ?\r))
                      (substring raw-line 0 -1)
                    raw-line)))
        (when (not (string-empty-p line))
          (setf (llm-api--sse-state-header-parsed state) t)
          (funcall callback line))))))

(defun llm-api--sse-flush (state)
  "Flush any remaining buffered content in STATE when the stream ends.
Moves leftover buffer content to error-buffer (if no SSE headers were seen)
so the sentinel can detect HTTP error responses that arrived without
a trailing newline."
  (let ((remaining (llm-api--sse-state-buffer state)))
    (when (and (not (string-empty-p remaining))
               (not (llm-api--sse-state-header-parsed state)))
      ;; Strip trailing \r
      (when (and (> (length remaining) 0)
                 (= (aref remaining (1- (length remaining))) ?\r))
        (setq remaining (substring remaining 0 -1)))
      (setf (llm-api--sse-state-error-buffer state)
            (concat (llm-api--sse-state-error-buffer state) remaining "\n")))
    (setf (llm-api--sse-state-buffer state) "")))

(defun llm-api--ndjson-flush (state callback)
  "Flush any remaining buffered content in STATE as a final NDJSON line.
Calls CALLBACK with the remaining buffer if non-empty."
  (let ((remaining (llm-api--sse-state-buffer state)))
    (when (not (string-empty-p remaining))
      ;; Strip trailing \r
      (when (and (> (length remaining) 0)
                 (= (aref remaining (1- (length remaining))) ?\r))
        (setq remaining (substring remaining 0 -1)))
      (when (not (string-empty-p remaining))
        (setf (llm-api--sse-state-header-parsed state) t)
        (funcall callback remaining)))
    (setf (llm-api--sse-state-buffer state) "")))

(provide 'llm-api-sse)
;;; sse.el ends here
