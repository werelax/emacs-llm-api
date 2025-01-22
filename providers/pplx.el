(require 'cl-lib)

(cl-defstruct (llm-api--pplx (:include llm-api--platform)
                             (:constructor llm-api--pplx-create)
                             (:copier nil)))

;; insert citations
(defun generate-citations-block (citations)
  (cl-loop for element in citations
           for index from 1
           collect (format "[%d]: %s" index element) into result
           finally (return (mapconcat 'identity result "\n"))))

(cl-defmethod llm-api--on-generation-finish-hook ((platform llm-api--pplx) on-data)
  ;; extract citattions
  (let* ((response (llm-api--platform-last-api-response platform))
         ;; last api response: (:id 3fbde125-0d71-403e-85ae-86b96f1fee8f :model sonar :created 1737506910 :usage (:prompt_tokens 113 :completion_tokens 57 :total_tokens 170) :citations (https://www.youtube.com/watch?v=eaIGRNQC1T4 https://simple.wikipedia.org/wiki/Game_Boy_Advance https://gamerant.com/kein-game-boy-advance-release-22-years-later/ https://www.nintendo.com/en-gb/Hardware/Nintendo-History/Game-Boy-Advance/Game-Boy-Advance-627139.html))
         (citations (plist-get response :citations)))
    (when citations
      (funcall on-data "\n\n")
      (funcall on-data (generate-citations-block citations)))))


(defun llm-api--response-filter-process-line (platform chunk on-data)
  ;; (message "llm-api--response-filter-process-line: '%s'" chunk)
  (when (and (listp chunk)
             ;; (string= "chat.completion" (plist-get chunk :object))
             (string-prefix-p "chat.completion" (plist-get chunk :object))
             )
    ;; save the last api response (for debug, reference and citations)
    (setf (llm-api--platform-last-api-response platform) chunk)
    (let ((choices (plist-get chunk :choices)))
      (when (and (listp choices)
                 (> (length choices) 0))
        (let* ((choice (car choices))
               (msg (plist-get choice :message))
               (delta (plist-get choice :delta))
               (_role (plist-get msg :role))
               (_content (plist-get msg :content))
               (finish-reason (plist-get choice :finish_reason))
               (content-delta (plist-get delta :content)))
          ;; store last response (full response on last filter call)
          (when (stringp content-delta)
            (cl-callf concat (llm-api--platform-last-response platform) content-delta))
          ;; stream the deltas
          (when (and (stringp content-delta)
                     (functionp on-data))
            (funcall on-data content-delta))
          ;; save the finish reason
          (when (not (eq finish-reason :null))
            (setf (llm-api--platform-finish-reason platform) finish-reason)))))))

(defvar *partial-line* "")

(cl-defmethod llm-api--response-filter ((platform llm-api--pplx) on-data _process output)
  ;; (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "?\n")))
    (dolist (line lines)
      (when (string-prefix-p  "data: " line)
        (setq *partial-line* "")
        (setq line (substring line (length "data: "))))
      ;; (message "llm-api--response-filter DATA-LINE: '%s'" line)
      (when (and (not (string-empty-p line))
                 (not (string= line "[DONE]")))
        (condition-case err
            (let ((chunk (json-parse-string (concat *partial-line* line) :object-type 'plist :array-type 'list)))
              ;; (message "CHUNK: %s" chunk)
              (llm-api--response-filter-process-line platform chunk on-data)
              (setq *partial-line* ""))
          (error
           ;; (message "line: %s" (concat *partial-line* line))
           ;; (message "ERR: %s" (error-message-string err))
           ;; probably the json line wasn't complete
           (setq *partial-line* line)))))))

(defun llm--create-pplx-platform (token)
  (llm-api--pplx-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("sonar" "sonar-pro")
   :selected-model "sonar-pro"
   :system-prompt "You are a helpful AI assistant.

Rules:
1. Provide only the final answer. It is important that you do not include any explanation on the steps below.
2. Do not show the intermediate steps information.

Steps:
1. Decide if the answer should be a brief sentence or a list of suggestions.
2. If it is a list of suggestions, first, write a brief and natural introduction based on the original query.
3. Followed by a list of suggestions, each suggestion should be split by two newlines."
   :params '(:temperature 0.7)))
