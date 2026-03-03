;;; pplx.el --- Perplexity provider for llm-api -*- lexical-binding: t; -*-

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
  ;; extract citations
  (let* ((response (llm-api--platform-last-api-response platform))
         (citations (plist-get response :citations)))
    (when citations
      (setf (plist-get response :citations) nil)
      (funcall on-data "\n\n")
      (funcall on-data (generate-citations-block citations)))))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--pplx))
  (let ((payload (cl-call-next-method platform)))
    (setf (plist-get payload :max_tokens) 120000)
    payload))

(cl-defmethod llm-api--invalidate-model-cache ((_platform llm-api--pplx))
  "Keep static model list for PPLX; no external model cache to invalidate."
  nil)

;; SSE parsing and JSON handling now inherited from the default implementation.
;; The default llm-api--response-filter handles partial-line buffering and error detection.
;; The default llm-api--handle-sse-data handles OpenAI-compatible JSON extraction.

(defun llm--create-pplx-platform (token)
  (llm-api--pplx-create
   :name "pplx"
   :url "https://api.perplexity.ai/chat/completions"
   :token token
   :available-models '("sonar" "sonar-pro" "sonar-reasoning" "sonar-reasoning-pro" "r1-1776" "sonar-deep-research")
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
