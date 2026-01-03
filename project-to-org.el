;;; project-to-org.el --- Sync GitHub Projects to Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edd Wilder-James

;; Author: Edd Wilder-James <edd@example.com>
;; Version: 0.1.0
;; Keywords: tools, org, github
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This package provides a command to sync GitHub Projects to an Org-mode file.
;; It relies on an external Python script to perform the actual synchronization.

;;; Code:

(require 'org)

(defgroup project-to-org nil
  "Sync GitHub Projects to Org-mode."
  :group 'org
  :prefix "project-to-org-")

(defcustom project-to-org-python-command "uv run"
  "Command to run the Python script.
Defaults to 'uv run' which handles dependencies automatically."
  :type 'string
  :group 'project-to-org)

(defcustom project-to-org-script-path (expand-file-name "src/project_to_org/main.py" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Python script."
  :type 'file
  :group 'project-to-org)

(defun project-to-org--get-property (property)
  "Get the value of a file-level PROPERTY from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward (concat "^#\\+" property ":[ \t]*\\(.*\\)$") nil t)
        (match-string-no-properties 1)))))

;;;###autoload
(defun project-to-org-sync ()
  "Sync the current Org buffer with the configured GitHub Project.
Requires #+GITHUB_PROJECT_URL to be set in the file."
  (interactive)
  (let* ((project-url (project-to-org--get-property "GITHUB_PROJECT_URL"))
         (target-file (buffer-file-name))
         (target-buffer (current-buffer))
         (project-root (file-name-directory (or load-file-name buffer-file-name))))
    (unless project-url
      (user-error "No #+GITHUB_PROJECT_URL property found in this file"))
    
    (unless target-file
      (user-error "Buffer must be saved to a file before syncing"))
    
    (message "Syncing with GitHub Project...")
    
    (let ((output-buffer (generate-new-buffer "*project-to-org-output*"))
          (error-buffer (generate-new-buffer "*project-to-org-error*"))
          (default-directory project-root)
          (args (list project-to-org-script-path
                      "--project-url" project-url
                      "--org-file" target-file)))
      
      (set-process-sentinel
       (make-process
        :name "project-to-org"
        :buffer output-buffer
        :stderr error-buffer
        :command (append (split-string project-to-org-python-command) args))
       (lambda (proc event)
         (when (eq (process-status proc) 'exit)
           (let ((exit-code (process-exit-status proc)))
             (if (zerop exit-code)
                 (progn
                   (with-current-buffer target-buffer
                     (revert-buffer t t t)
                     (message "Sync complete!")))
               (with-current-buffer error-buffer
                 (message "Sync failed: %s" (buffer-string))))
             (kill-buffer (process-buffer proc))
             (kill-buffer error-buffer))))))))

(provide 'project-to-org)
;;; project-to-org.el ends here
