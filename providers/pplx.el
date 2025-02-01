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

(cl-defmethod llm-api--get-request-payload ((platform llm-api--pplx))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 120000)
    payload))

(cl-defmethod llm-api--response-filter ((platform llm-api--pplx) on-data _process output)
  (message "llm-api--response-filter: '%s'" output)
  (let ((lines (split-string output "\r?\n")))
    (dolist (line lines)
      (when (string-prefix-p "data: " line)
        (setq *partial-line* "")
        (setq line (substring line (length "data: "))))
      (when (and (not (string-empty-p line))
                 (not (string= line "[DONE]")))
        (condition-case err
            (let ((chunk (json-parse-string (concat *partial-line* line)
                                            :object-type 'plist
                                            :array-type 'list)))
              ;; Check if the response contains an error.
              (let ((err-data (plist-get chunk :error)))
                (if err-data
                    (if (and (listp err-data) (plist-get err-data :message))
                        (message "Error: %s" (plist-get err-data :message))
                      (message "Error: %s" err-data))
                  (llm-api--response-filter-process-line platform chunk on-data)))
              (setq *partial-line* ""))
          (error
           ;; Probably the JSON line wasn’t complete; save it for the next call.
           (setq *partial-line* line)))))))

(defun llm--create-pplx-platform (token)
  (llm-api--pplx-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("sonar" "sonar-pro" "sonar-reasoning")
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
