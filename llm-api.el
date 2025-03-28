;;; llm-api.el --- Adapter to use slm engines -*- lexical-binding: t; -*-
;;

;; Copyright (C) 2023 Elias Alonso
;;
;; Author: Elias Alonso <eliasgc@gmail.com>
;; Maintainer: Elias Alonso <eliasgc@gmail.com>
;; Created: noviembre 13, 2023
;; Modified: noviembre 13, 2023
;; Version: 0.0.1
;; Keywords: convenience games hypermedia languages processes tools
;; Homepage: https://fosforo.io
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'json)
(require 'spinner)
(require 'cl-lib)
(require 's)

(cl-defstruct (llm-api--platform
               (:constructor llm-api--platform-create)
               (:copier nil))
  name
  url
  token
  system-prompt
  (system-prompt-role :system)
  (history '())
  (available-models '())
  selected-model
  params
  process
  process-buffer-name
  process-buffer
  ;; state
  last-api-response
  last-response
  finish-reason)

(cl-defmethod initialize-instance :after ((platform llm-api--platform) &rest _)
  "Initialize PLATFORM."
  ;; sets selected-model with the first value of available-models if not set
  (unless (llm-api--platform-selected-model platform)
    (let ((models (llm-api--platform-available-models platform)))
      (when models
        (setf (llm-api--platform-selected-model platform) (car models))))))

(cl-defgeneric llm-api--get-available-models (platform)
  "Get available models for PLATFORM.")

(cl-defgeneric llm-api--set-selected-model (platform model)
  "Set selected MODEL for PLATFORM.")

(cl-defgeneric llm-api--get-selected-model (platform)
  "Get selected MODEL for PLATFORM.")

(cl-defgeneric llm-api--get-model-name (platform model)
  "Get the model name for PLATFORM.")

(cl-defgeneric llm-api--kill-process (platform)
  "Kill the process for PLATFORM.")

(cl-defgeneric llm-api--on-clear-history (platform)
  "Hook called when the history is cleared for PLATFORM.")

(cl-defgeneric llm-api--clear-history (platform)
  "Clear the history. Optionally specify the PLATFORM.")

(cl-defgeneric llm-api--get-history (platform)
  "Get the history. Optionally specify the PLATFORM.")

(cl-defgeneric llm-api--add-to-history (platform message)
  "Add MESSAGE to the history. Optionally specify the PLATFORM.")

(cl-defgeneric llm-api--remove-last-from-history (platform)
  "Remove the last message from history. Optionally specify the PLATFORM.")

(cl-defgeneric llm-api--get-request-headers (platform)
  "Get the request headers for PLATFORM.")

(cl-defgeneric llm-api--get-request-payload (platform)
  "Get the request payload for PLATFORM.")

(cl-defgeneric llm-api--get-curl-params (platform)
  "Get the curl command params for PLATFORM.")

(cl-defmethod llm-api--get-curl-url (platform)
  "Get the curl command url for PLATFORM.")

(cl-defgeneric llm-api--response-filter (platform process output)
  "Process OUTPUT of PROCESS for given PLATFORM.")

(cl-defgeneric llm-api--process-sentinel (platform on-finish continue process event)
  "Process sentinel function. ON-FINISH and CONITNUE to decide how to react.
Optionally specify the PROCESS and PLATFORM.")

(cl-defgeneric llm-api--on-generation-finish-hook (platform on-data)
  "Hook called after PLATFORM has finished generating a resonse.")

(cl-defgeneric llm-api--add-response-to-history (platform)
  "Add generated :assistant response to PLATFORM chat history.")

(cl-defgeneric llm-api--format-continuation-message (platform last-response)
  "Format LAST-RESPONSE as a continuation message for chat history.
Default implementation returns a simple assistant message format with
LAST-RESPONSE. PLATFORM may override this if they require special
continuation message formatting.")

(cl-defgeneric llm-api--generate-streaming (platform prompt &rest args)
  "Query PLATFORM for PROMPT. ARGS for extra control.")

;; load provider implementations

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "./providers/default.el" base-dir))
  ;; (load (expand-file-name "./providers/openchat-team.el" base-dir))
  ;; (load (expand-file-name "./providers/ollama-completion.el" base-dir))
  (load (expand-file-name "./providers/togetherai.el" base-dir))
  (load (expand-file-name "./providers/openai.el" base-dir))
  (load (expand-file-name "./providers/deepseek.el" base-dir))
  ;; (load (expand-file-name "./providers/awan-llm.el" base-dir))
  (load (expand-file-name "./providers/open-router.el" base-dir))
  (load (expand-file-name "./providers/ollama.el" base-dir))
  (load (expand-file-name "./providers/pplx.el" base-dir))
  (load (expand-file-name "./providers/groq.el" base-dir))
  (load (expand-file-name "./providers/featherless.el" base-dir))
  (load (expand-file-name "./providers/hyperbolic.el" base-dir))
  (load (expand-file-name "./providers/infermatic.el" base-dir)))

(provide 'llm-api)
;;; llm-api.el ends here
