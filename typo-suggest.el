;;; typo-suggest.el --- Don't make typo with the help of helm and compny ;; -*- lexical-binding: t -*-

;; Copyright (C) 2020  Kadir Can Çetin

;; Author: Kadir Can Çetin <kadircancetin@gmail.com>
;; Keywords: convenience, wp
;; Package-Requires: ((emacs "24.2") (cl-lib "0") (json "0") (helm "3.0") (company "0.9.10"))
;; URL: https://github.com/kadircancetin/typo-suggest
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
(require 'json)
(require 'thingatpt)
(require 's)
(require 'dash)
(require 'helm)
(require 'company)


(defgroup typo-suggest nil
  "Fix the typos"
  :group 'matching
  :group 'helm)

(defcustom typo-suggest-suggestion-count 20
  "Number of suggestion for 'helm and company completion."
  :type 'integer
  :group 'typo-suggest)

(defcustom typo-suggest-timeout 1
  "Number of second to try maximum connection server time."
  :type 'integer
  :group 'typo-suggest)

(defcustom default-search-method 'datamuse
  ""
  :type 'symbol
  )



(defun typo-suggest--fetch-results (query)
  "Fetching results from datamuse api and return as a string.
Argument QUERY is string which will searched."
  (with-current-buffer
      ;; TODO: show error or something to user if bigger than 'typo-suggest-timeout sec
      (url-retrieve-synchronously
       (format "https://api.datamuse.com/sug?max=%s&s=%s"
               typo-suggest-suggestion-count query)
       nil t typo-suggest-timeout)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point)(point-min))(buffer-string)))

(defun typo-suggest--results (fetched_str)
  "Gets json str, return parsed elisp obj.
It returns list of strings suggestion.  Argument FETCHED_STR is
comes from `typo-suggest--fetch-result'."

  (mapcar 'cdr (mapcar 'car (json-read-from-string  (typo-suggest--fetch-results fetched_str)))))


(defun typo-suggest--helm-replace-word(x)
  "Replace the word under the cursor with X parameter."
  (interactive)
  (save-excursion
    (delete-region (beginning-of-thing 'word) (end-of-thing 'word))
    (insert x)))

(defun typo-suggest--do-helm(input)
  "Strating helm suggestion with INPUT parameter."
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

(defun typo-suggest--ispell-filter-fixes-line (terminal_output word)
  (car (-filter
        (lambda (line)
          (when (s-contains? (format "& %s" word) line)
            t))
        (split-string terminal_output "\n"))))

(defun typo-suggest--ispell-is-wrong (terminal_output)
  (s-contains? "#" (car (cdr (split-string terminal_output "\n")))))


(defun typo-suggest--parse-ispell-suggest(terminal_output word)
  (let* ((line-fixes (typo-suggest--ispell-filter-fixes-line terminal_output word))
         (is-wrong? (typo-suggest--ispell-is-wrong terminal_output)))

    (cond
     (line-fixes (-map 's-trim (split-string
                                (car (cdr (split-string line-fixes ":")))
                                ",")))
     (is-wrong? '("NOT FOUND"))
     (t (list word)))))

(defun typo-suggest--do-helm-ispell(input)
  "Strating helm suggestion with INPUT parameter."
  (helm :sources
        (helm-build-sync-source "Helm Word"
          :candidates
          (lambda (&optional _)
            (typo-suggest--parse-ispell-suggest
             (with-temp-buffer
               (call-process "bash" nil t nil "-c" (format "echo %s | ispell -a"  (or helm-input "nil")))
               (buffer-string))
             helm-input
             ))
          :volatile t
          :match-dynamic t
          :action '(("Insert" . insert)
                    ("Update Word" . typo-suggest--helm-replace-word)))
        :buffer "*helm test*"
        :input input))


;;;###autoload
(defun typo-suggest-helm()
  "Get word suggestion from datamuse api with helm."
  (interactive)
  (if (eq default-search-method 'ispell)
      (typo-suggest--do-helm-ispell (thing-at-point 'word))
    (typo-suggest--do-helm (thing-at-point 'word))))


;;;###autoload
(defun typo-suggest-company (command &optional arg &rest ignored)
  "Get word suggestion from datamuse api with company mode.
Argument COMMAND is used for company.
Optional argument ARG Is used from company to send which will search.
Optional argument IGNORED ignored arguments."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'typo-suggest-company))
    (prefix (company-grab-symbol))
    (candidates  (typo-suggest--results arg))
    (no-cache t)
    (require-match nil)
    (meta (format "Word search %s" arg))))



(provide 'typo-suggest)

;;; typo-suggest.el ends here
