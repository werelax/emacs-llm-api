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
  (history '())
  (available-models '())
  selected-model
  params
  process
  process-buffer-name
  process-buffer
  ;; state
  last-response)

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

(cl-defgeneric llm-api--get-request-headers (platform)
  "Get the request headers for PLATFORM.")

(cl-defgeneric llm-api--get-request-payload (platform)
  "Get the request payload for PLATFORM.")

(cl-defgeneric llm-api--get-curl-params (platform)
  "Get the curl command params for PLATFORM.")

(cl-defmethod llm-api--get-curl-url (platfrom)
  "Get the curl command url for PLATFORM.")

(cl-defgeneric llm-api--response-filter (platform process output)
  "Process OUTPUT of PROCESS for given PLATFORM.")

(cl-defgeneric llm-api--process-sentinel (platform process event)
  "Process sentinel function. Optionally specify the PROCESS and PLATFORM.")

(cl-defgeneric llm-api--on-generation-finish-hook (platform)
  "Hook called after PLATFORM has finished generating a resonse.")

(cl-defgeneric llm-api--add-generated-message-to-history (platform)
  "Add generated :assistant response to PLATFORM chat history.")

(cl-defgeneric llm-api--generate-streaming (platform prompt &rest args)
  "Query PLATFORM for PROMPT. ARGS for extra control.")

;; load provider implementations

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "./providers/default.el" base-dir))
  (load (expand-file-name "./providers/open-router.el" base-dir))
  (load (expand-file-name "./providers/openchat-team.el" base-dir))
  (load (expand-file-name "./providers/ollama.el" base-dir))
  (load (expand-file-name "./providers/ollama-completion.el" base-dir))
  (load (expand-file-name "./providers/pplx.el" base-dir))
  (load (expand-file-name "./providers/togetherai.el" base-dir)))

(provide 'llm-api)
;;; llm-api.el ends here
