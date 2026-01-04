;;; project-to-org.el --- Sync GitHub Projects to Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Edd Wilder-James

;; Author: Edd Wilder-James <edd@example.com>
;; URL: https://github.com/ewilderj/project-to-org
;; Version: 0.1.0
;; Keywords: tools, org, github
;; Package-Requires: ((emacs "29.1"))

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
Defaults to \='uv run\=' which handles dependencies automatically."
  :type 'string
  :group 'project-to-org)

(defcustom project-to-org-script-path (expand-file-name "src/project_to_org/main.py" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Python script."
  :type 'file
  :group 'project-to-org)

(defcustom project-to-org-compact-urls t
  "Whether to display GitHub URLs compactly (e.g., owner/repo#123)."
  :type 'boolean
  :group 'project-to-org)

(defcustom project-to-org-inline-metadata t
  "Whether to display metadata badges inline on headings."
  :type 'boolean
  :group 'project-to-org)

(defcustom project-to-org-fold-properties t
  "Whether to auto-fold properties drawers."
  :type 'boolean
  :group 'project-to-org)

(defface project-to-org-compact-url
  '((t :inherit org-link :underline t))
  "Face for compact GitHub URL display."
  :group 'project-to-org)

(defface project-to-org-issue-badge
  '((t :inherit font-lock-constant-face :weight bold :underline t))
  "Face for issue number badges."
  :group 'project-to-org)

(defface project-to-org-assignee-badge
  '((t :inherit font-lock-variable-name-face))
  "Face for assignee badges."
  :group 'project-to-org)

(defface project-to-org-label-badge
  '((t :inherit font-lock-string-face :foreground "cyan"))
  "Face for label badges."
  :group 'project-to-org)

(defun project-to-org--parse-github-url (url)
  "Parse a GitHub URL and return (owner repo type number).
TYPE is either \='issue or \='pull-request.
Returns nil if URL is not a recognized GitHub issue/PR URL."
  (when (string-match "https://github\\.com/\\([^/]+\\)/\\([^/]+\\)/\\(issues\\|pull\\)/\\([0-9]+\\)" url)
    (list (match-string 1 url)
          (match-string 2 url)
          (if (string= (match-string 3 url) "issues") 'issue 'pull-request)
          (match-string 4 url))))

(defun project-to-org--compact-url-string (url)
  "Convert a GitHub URL to compact format (owner/repo#123)."
  (let ((parsed (project-to-org--parse-github-url url)))
    (when parsed
      (let ((owner (nth 0 parsed))
            (repo (nth 1 parsed))
            (number (nth 3 parsed)))
        (format "%s/%s#%s" owner repo number)))))

(defun project-to-org--buttonize-url (start end)
  "Make the URL between START and END into a clickable compact button."
  (let* ((url (buffer-substring-no-properties start end))
         (compact (project-to-org--compact-url-string url)))
    (when compact
      (let ((map (make-sparse-keymap)))
        (define-key map [mouse-1]
          (lambda () (interactive) (browse-url url)))
        (define-key map (kbd "RET")
          (lambda () (interactive) (browse-url url)))
        (define-key map [mouse-2]
          (lambda () (interactive) (kill-new url) (message "Copied: %s" url)))
        (define-key map (kbd "C-c C-c")
          (lambda () (interactive) (kill-new url) (message "Copied: %s" url)))

        (with-silent-modifications
          (put-text-property start end 'display compact)
          (put-text-property start end 'face 'project-to-org-compact-url)
          (put-text-property start end 'keymap map)
          (put-text-property start end 'mouse-face 'highlight)
          (put-text-property start end 'help-echo
                           (format "%s\nmouse-1: open in browser\nmouse-2: copy URL" url))
          (put-text-property start end 'project-to-org-url t))))))

(defun project-to-org--compact-urls-in-buffer ()
  "Apply compact URL display to all GitHub URLs in the current buffer."
  (when project-to-org-compact-urls
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ":URL: \\(https://github\\.com/[^ \n]+\\)" nil t)
        (project-to-org--buttonize-url (match-beginning 1) (match-end 1))))))

(defun project-to-org--get-entry-metadata ()
  "Extract metadata from the current entry's properties drawer.
Returns a plist with :issue-number, :assignees, :labels, and :url."
  (let ((issue-number (org-entry-get nil "ISSUE_NUMBER"))
        (assignees (org-entry-get nil "ASSIGNEES"))
        (labels (org-entry-get nil "LABELS"))
        (url (org-entry-get nil "URL")))
    (list :issue-number issue-number
          :assignees (when assignees (split-string assignees ", " t))
          :labels (when labels (split-string labels ", " t))
          :url url)))

(defun project-to-org--format-metadata-badges (metadata)
  "Format METADATA plist into a string with badges."
  (let ((parts '())
        (issue-num (plist-get metadata :issue-number))
        (assignees (plist-get metadata :assignees))
        (labels (plist-get metadata :labels))
        (url (plist-get metadata :url)))

    (when issue-num
      (let ((issue-text (concat "#" issue-num)))
        (if url
            (let ((map (make-sparse-keymap)))
              (define-key map [mouse-1]
                (lambda () (interactive) (browse-url url)))
              (define-key map (kbd "RET")
                (lambda () (interactive) (browse-url url)))
              (push (propertize issue-text
                               'face 'project-to-org-issue-badge
                               'keymap map
                               'mouse-face 'highlight
                               'help-echo (format "mouse-1: open %s" url))
                    parts))
          (push (propertize issue-text
                           'face 'project-to-org-issue-badge)
                parts))))

    (when assignees
      (push (propertize
             (concat "👤 " (mapconcat 'identity assignees ", "))
             'face 'project-to-org-assignee-badge)
            parts))
    
    (when labels
      (push (propertize
             (concat "🏷️ " (mapconcat 'identity labels ", "))
             'face 'project-to-org-label-badge)
            parts))

    (when parts
      (concat "  " (mapconcat 'identity (nreverse parts) " ")))))

(defun project-to-org--add-metadata-badge (limit)
  "Add metadata badge overlay to heading between point and LIMIT."
  (when (and project-to-org-inline-metadata
             (re-search-forward org-complex-heading-regexp limit t))
    (let* ((heading-start (match-beginning 0))
           ;; Group 4 is the headline text, but it can be nil
           ;; Fall back to end of line if no headline text
           (headline-end (or (match-end 4)
                             (save-excursion
                               (goto-char heading-start)
                               (line-end-position))))
           (metadata (save-excursion
                       (goto-char heading-start)
                       (project-to-org--get-entry-metadata)))
           (badge-text (project-to-org--format-metadata-badges metadata)))

      (when (and badge-text
                 headline-end
                 (> headline-end heading-start))
        ;; Create overlay on last character of headline
        ;; Use 'display property to show last char + badge
        (let* ((last-char-start (1- headline-end))
               (last-char (buffer-substring-no-properties last-char-start headline-end))
               (ov (make-overlay last-char-start headline-end nil t)))
          (overlay-put ov 'display (concat last-char badge-text))
          (overlay-put ov 'project-to-org-badge t))))
    t))

(defun project-to-org--add-all-metadata-badges ()
  "Add metadata badges to all headings in the buffer."
  (when project-to-org-inline-metadata
    (save-excursion
      (goto-char (point-min))
      (while (project-to-org--add-metadata-badge nil)))))

(defun project-to-org--remove-all-metadata-badges ()
  "Remove all metadata badge overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'project-to-org-badge t))

(defun project-to-org--fold-all-properties ()
  "Fold all properties drawers in the buffer."
  (when project-to-org-fold-properties
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:PROPERTIES:" nil t)
        (beginning-of-line)
        (org-fold-hide-drawer-toggle t)
        (forward-line 1)))))

(defconst project-to-org--github-color-map
  '(("GRAY"   . (:foreground "#d8dadd" :background "#57606a"))
    ("BLUE"   . (:foreground "#b6e3ff" :background "#0550ae"))
    ("GREEN"  . (:foreground "#aceebb" :background "#1a7f37"))
    ("YELLOW" . (:foreground "#f5e08a" :background "#7d5e08"))
    ("ORANGE" . (:foreground "#ffd8b5" :background "#bc4c00"))
    ("RED"    . (:foreground "#ffc1c0" :background "#cf222e"))
    ("PINK"   . (:foreground "#f9c9e2" :background "#bf3989"))
    ("PURPLE" . (:foreground "#e8d5f9" :background "#6639ba")))
  "Map GitHub project colors to Emacs face specs.
Each entry is (GITHUB-COLOR . (:foreground FG :background BG)).
Colors are swapped to account for Org TODO face rendering.")

(defvar-local project-to-org--status-color-overlays nil
  "List of overlays used for status color highlighting.")

(defun project-to-org--parse-status-colors (colors-str)
  "Parse COLORS-STR like \\='TODO=YELLOW STRT=PURPLE\\=' into an alist."
  (when colors-str
    (let ((pairs '()))
      (dolist (part (split-string colors-str " " t))
        (when (string-match "\\([^=]+\\)=\\([^=]+\\)" part)
          (push (cons (match-string 1 part) (match-string 2 part)) pairs)))
      (nreverse pairs))))

(defun project-to-org--remove-status-color-overlays ()
  "Remove all status color overlays from the buffer."
  (mapc #'delete-overlay project-to-org--status-color-overlays)
  (setq project-to-org--status-color-overlays nil))

(defun project-to-org--apply-status-color-overlays (status-colors)
  "Apply overlays for TODO keywords based on STATUS-COLORS alist."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-complex-heading-regexp nil t)
      ;; Group 2 is the TODO keyword - may be nil if no keyword
      (let ((todo-start (match-beginning 2))
            (todo-end (match-end 2)))
        (when (and todo-start todo-end)
          (let* ((todo-keyword (match-string 2))
                 (github-color (cdr (assoc todo-keyword status-colors)))
                 (color-spec (when github-color
                               (cdr (assoc github-color
                                           project-to-org--github-color-map)))))
            (when color-spec
              (let ((ov (make-overlay todo-start todo-end nil t))
                    (fg (plist-get color-spec :foreground))
                    (bg (plist-get color-spec :background)))
                ;; Use a complete face spec that overrides org-mode's TODO faces
                (overlay-put ov 'face `(:foreground ,fg :background ,bg :weight bold
                                                    :inherit nil))
                (overlay-put ov 'priority 1000)
                (overlay-put ov 'project-to-org-status-color t)
                (push ov project-to-org--status-color-overlays)))))))))

(defun project-to-org--setup-status-colors ()
  "Apply GitHub status colors to Org-mode TODO keywords using overlays."
  (project-to-org--remove-status-color-overlays)
  (when-let* ((colors-str (project-to-org--get-property "GITHUB_STATUS_COLORS"))
              (status-colors (project-to-org--parse-status-colors colors-str)))
    (project-to-org--apply-status-color-overlays status-colors)))

(defun project-to-org--setup-compact-urls ()
  "Set up compact URL display for the current buffer."
  (when (and (derived-mode-p 'org-mode)
             (project-to-org--get-property "GITHUB_PROJECT_URL"))
    (project-to-org--compact-urls-in-buffer)))

(defun project-to-org--setup-inline-metadata ()
  "Set up inline metadata display for the current buffer."
  (when (and (derived-mode-p 'org-mode)
             (project-to-org--get-property "GITHUB_PROJECT_URL"))
    (project-to-org--remove-all-metadata-badges)
    (project-to-org--add-all-metadata-badges)
    (project-to-org--fold-all-properties)))

;;;###autoload
(define-minor-mode project-to-org-mode
  "Minor mode for enhanced GitHub Project integration in Org-mode."
  :lighter " GH"
  :group 'project-to-org
  (if project-to-org-mode
      (progn
        (project-to-org--setup-status-colors)
        (project-to-org--setup-compact-urls)
        (project-to-org--setup-inline-metadata)
        (add-hook 'after-save-hook #'project-to-org--setup-compact-urls nil t)
        (add-hook 'after-save-hook #'project-to-org--setup-inline-metadata nil t)
        (add-hook 'after-save-hook #'project-to-org--setup-status-colors nil t))
    (remove-hook 'after-save-hook #'project-to-org--setup-compact-urls t)
    (remove-hook 'after-save-hook #'project-to-org--setup-inline-metadata t)
    (remove-hook 'after-save-hook #'project-to-org--setup-status-colors t)
    (project-to-org--remove-status-color-overlays)
    (project-to-org--remove-all-metadata-badges)))

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
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (let ((exit-code (process-exit-status proc)))
             (if (zerop exit-code)
                 (progn
                   (with-current-buffer target-buffer
                     (revert-buffer t t t)
                     ;; Re-parse Org in-buffer settings (#+TODO, etc.)
                     ;; This ensures org-todo-keywords-1 is updated
                     (org-set-regexps-and-options)
                     (font-lock-flush)
                     (when project-to-org-mode
                       (project-to-org--setup-status-colors)
                       (project-to-org--setup-compact-urls)
                       (project-to-org--setup-inline-metadata))
                     (message "Sync complete!")))
               (with-current-buffer error-buffer
                 (message "Sync failed: %s" (buffer-string))))
             (kill-buffer (process-buffer proc))
             (kill-buffer error-buffer))))))))

(provide 'project-to-org)
;;; project-to-org.el ends here
