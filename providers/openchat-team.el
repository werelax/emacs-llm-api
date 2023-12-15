;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct (llm-api--openchat-team (:include llm-api--platform)
                                      (:constructor llm-api--openchat-team-create)
                                      (:copier nil)))

(cl-defmethod llm-api--get-request-headers ((_platform llm-api--openchat-team))
  '("authority: openchat.team"
    "accept: */*"
    "accept-language: en-US,en;q=0.9"
    "content-type: application/json"
    "origin: https://openchat.team"
    "referer: https://openchat.team/"
    "sec-ch-ua: \"Google Chrome\";v=\"119\", \"Chromium\";v=\"119\", \"Not?A_Brand\";v=\"24\""
    "sec-ch-ua-mobile: ?0"
    "sec-ch-ua-platform: \"Linux\""
    "sec-fetch-dest: empty"
    "sec-fetch-mode: cors"
    "sec-fetch-site: same-origin"))

(cl-defmethod llm-api--get-request-payload ((platform llm-api--openchat-team))
  ;; (message "\n> history: %s\n\n" (llm-api--get-history platform))
  (let ((params (llm-api--platform-params platform)))
    `(:model (:id "openchat_v3.2_mistral" :maxLength 24576 :name "OpenChat Aura" :tokenLimit 8192)
      :messages ,(llm-api--get-history platform)
      :key: "",
      :prompt " "
      :temperature ,(plist-get params :temperature))))

(cl-defmethod llm-api--response-filter ((platform llm-api--openchat-team) on-data _process output)
  ;; accumulate total output
  (cl-callf concat (llm-api--openchat-team-last-response platform) output)
  ;; stream the output
  (funcall on-data output))

(cl-defmethod llm-api--add-generated-message-to-history ((platform llm-api--openchat-team))
  (let* ((last-msg-content (llm-api--openchat-team-last-response platform))
         (msg `((:role . :assistant) (:content . ,last-msg-content))))
    (llm-api--add-to-history platform msg)
    (setf (llm-api--openchat-team-last-response platform) nil)))

(defun llm--create-openchat-team-platform ()
  (llm-api--openchat-team-create
   :name "openchat.team"
   :url "https://openchat.team/api/chat"
   :available-models '("openchat_v3.2_mistral")
   :selected-model "openchat_v3.2_mistral"
   :params '(:temperature 0.2)))
