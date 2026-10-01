;;; aven-core.el --- Shared primitives for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; Low-level state and helpers shared by every other `aven-*' module:
;; the CLI executable name, faces, and task-ref parsing.  This module
;; has no dependency on any other `aven-*' module.

;;; Code:

(require 'magit-section)

(defvar aven--executable "aven")

(defvar aven-status-buffer-name "*aven-status*")

(defconst aven--ref-pattern "[A-Z][A-Z0-9]*-[A-Z0-9]+"
  "Pattern matching a task ref such as APP-7KQ9, without anchors.")

(defgroup aven nil
  "Interface to the Aven task manager."
  :group 'tools)

(defface aven-ref-face '((t :foreground "gray50"))
  "Face for a task ref."
  :group 'aven)

(defface aven-project-face '((t :foreground "turquoise"))
  "Face for a task's project."
  :group 'aven)

(defface aven-label-face '((t :foreground "orange"))
  "Face for a task's labels."
  :group 'aven)

(defface aven-due-face '((t :foreground "gray50"))
  "Face for a task's due date."
  :group 'aven)

(defconst aven--org-link-pattern
  "\\[\\[\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]+\\)\\]\\)?\\]"
  "Pattern matching an Org bracket link, such as an org-roam
reference [[id:...][Title]]: group 1 is the target, group 2 the
optional description.")

(declare-function org-link-open-from-string "ol")
(defvar org-link-frame-setup)

(defun aven--follow-org-link (button)
  "Open the Org link that BUTTON stands for in another window, so the
Aven buffer stays visible alongside it."
  (require 'org)
  (let ((org-link-frame-setup (cons '(file . find-file-other-window)
                                    org-link-frame-setup)))
    (org-link-open-from-string (format "[[%s]]" (button-get button 'aven-link)))))

(defun aven--insert-text (text)
  "Insert TEXT, rendering its Org bracket links (e.g. org-roam
references) as buttons that show their description and follow
the link."
  (let ((start 0))
    (while (string-match aven--org-link-pattern text start)
      (let ((target (match-string 1 text))
            (label  (or (match-string 2 text) (match-string 1 text)))
            (end    (match-end 0)))
        (insert (substring text start (match-beginning 0)))
        (insert-text-button label
                            'action #'aven--follow-org-link
                            'aven-link target
                            'help-echo target
                            'follow-link t
                            'face 'link
                            ;; Font-lock would strip a plain `face'.
                            'font-lock-face 'link)
        (setq start end)))
    (insert (substring text start))))

(defun aven--ref-at-point ()
  "Task ref at point, or nil."
  (or (when-let* ((section (and (fboundp 'magit-current-section) (magit-current-section)))
                  (_ (eq (oref section type) 'aven-task)))
        (oref section value))
      (and (boundp 'aven-task--ref) aven-task--ref)
      (let ((sym (thing-at-point 'symbol t)))
        (when (and sym (string-match-p (concat "\\`" aven--ref-pattern "\\'") sym))
          sym))))

(defun aven/package-version ()
  "Display the package version"
  (interactive)
  (message "0.1.0"))

(defun aven--ref-on-line ()
  "Task ref at the start of the current line, as printed by `list'/`search'."
  (save-excursion
    (forward-line 0)
    (when (looking-at aven--ref-pattern)
      (match-string-no-properties 0))))

(defun aven--read-ref (prompt)
  "Read a task ref, defaulting to the one at point."
  (let ((default (aven--ref-at-point)))
    (read-string (if default (format "%s(%s) " prompt default) prompt)
                 nil nil default)))

(provide 'aven-core)
;;; aven-core.el ends here
