;;; typo-suggest.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Kadir Can Çetin

;; Author: Nic Ferrier <kadircancetin@gmail.com>
;; Keywords: typo, suggestion
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fixing typos with helm or company.

;;; Code:

;; code goes here

(require 'cl-lib)
(require 'hippie-exp)
(require 'json)
(require 'thingatpt)
(require 'helm)
(require 'company)


(defgroup typo-suggest nil
  "Fix the typos"
  :group 'matching
  :group 'helm)

(defcustom typo-suggest-suggestion-count 20
  "Number of suggestion for 'helm and company completion"
  :type 'integer
  :group 'typo-suggest)

(defcustom typo-suggest-timeout 1
  "Number of second to try maximum connection server time."
  :type 'integer
  :group 'typo-suggest)



(defun typo-suggest--fetch-results (query)
  "Fetching results from datamuse api and returns as a string"
  (with-current-buffer
      ;; TODO: show error or something to user if bigger than 'typo-suggest-timout sec
      (url-retrieve-synchronously
       (format "https://api.datamuse.com/sug?max=%s&s=%s"
               typo-suggest-suggestion-count query)
       nil t typo-suggest-timeout)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point)(point-min))(buffer-string)))

(defun typo-suggest--results (fetched_str)
  "Gets json but string datatype result of the datamuse api, returns list of strings suggestion."
  (mapcar 'cdr (mapcar 'car (json-read-from-string  (typo-suggest--fetch-results fetched_str)))))


;;;###autoload
(defun typo-suggest--helm-replace-word(x)
  "Replace the word under the cursor with x paramter."
  (interactive)
  (save-excursion
    (delete-region (beginning-of-thing 'word) (end-of-thing 'word))
    (insert x)))

(defun typo-suggest--do-helm(input)
  "Strating helm suggestion with input parameter."
  (helm :sources
        (helm-build-sync-source "Helm Word"
          :candidates (lambda (&optional _) (typo-suggest--results helm-input))
          :fuzzy-match nil
          :action '(("Insert" . insert)
                    ("Update Word" . typo-suggest--helm-replace-word)
                    ;; TODO: make configurable google translate integration.
                    ;;("Translate" . (lambda (x) (google-translate-translate "en" "tr" x)))
                    )
          :volatile t
          :must-match t
          ;; :nohighlight t
          :match-dynamic t)
        :buffer "*helm test*"
        :input input))


;;;###autoload
(defun typo-suggest-helm()
  "Get word suggestion from datamuse api with helm."
  (interactive)
  (typo-suggest--do-helm (thing-at-point 'word)))


;;;###autoload
(defun typo-suggest-company (command &optional arg &rest ignored)
  "Get word suggestion from datamuse api with company mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'typo-suggest-company))
    (prefix (company-grab-symbol))
    (candidates  (typo-suggest--results arg))
    (no-cache t)
    (require-match nil)
    (meta (format "Word search %s" arg))))



(provide 'typo-suggest)

;; typo-suggest.el ends here
