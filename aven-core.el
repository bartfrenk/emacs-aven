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
