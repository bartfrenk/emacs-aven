;;; aven-description.el --- Description editing for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; A magit-commit-message-style buffer for editing a task's
;; description, saved back via `aven text set' and guarded by the
;; SHA-256 read when the buffer was opened.

;;; Code:

(require 'aven-core)

;; Defined in aven-status.el / aven-task.el, both of which are loaded
;; before this module is used interactively; declared here only to
;; keep the byte-compiler quiet about the forward reference.
(declare-function aven-status-refresh "aven-status")
(declare-function aven-task-refresh "aven-task")

(defun aven--parse-sha256 (output)
  "First sha256=HASH field in OUTPUT, or nil."
  (when (string-match "sha256=\\([0-9a-f]+\\)" output)
    (match-string 1 output)))

(defvar-local aven-description--ref nil
  "Task ref this buffer's description belongs to.")

(defvar-local aven-description--field nil
  "Long text field this buffer edits, currently always \"description\".")

(defvar-local aven-description--sha256 nil
  "SHA-256 of the field's value as last read from or written to Aven.")

(defvar-local aven-description--origin nil
  "Buffer this description edit was started from, to return to on close.")

(defun aven-description--cleanup ()
  "Delete the scratch file backing an `aven-description-edit-mode' buffer."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (ignore-errors (delete-file buffer-file-name))))

(defun aven-description--after-save ()
  "Push this buffer's saved contents to Aven via `aven text set'."
  (let* ((ref aven-description--ref)
         (field aven-description--field)
         (file buffer-file-name)
         (sha aven-description--sha256)
         (result (with-temp-buffer
                   (let ((exit-code (call-process aven--executable nil t nil
                                                  "text" "set" ref field
                                                  "--file" file
                                                  "--if-sha256" sha)))
                     (cons exit-code (buffer-string))))))
    (if (zerop (car result))
        (progn
          (setq aven-description--sha256
                (or (aven--parse-sha256 (cdr result)) aven-description--sha256))
          (message "aven: saved %s for %s" field ref))
      (message "aven: %s" (string-trim (cdr result))))))

(defun aven--refresh-buffer (buffer)
  "Refresh BUFFER if it is an Aven status or task buffer, and return it."
  (with-current-buffer buffer
    (cond ((derived-mode-p 'aven-status-mode) (aven-status-refresh))
          ((derived-mode-p 'aven-task-mode) (aven-task-refresh) buffer)
          (t buffer))))

(defun aven-description--close ()
  "Kill this buffer immediately and return to the buffer this edit
was started from, refreshed and with point kept in place when
possible, or the Aven status buffer if that buffer no longer exists."
  (let ((origin aven-description--origin))
    (set-buffer-modified-p nil)
    (kill-buffer)
    (let* ((target (if (buffer-live-p origin)
                        (aven--refresh-buffer origin)
                      (aven-status-refresh)))
           ;; `switch-to-buffer' can reset point to a window's stale
           ;; `window-point' for TARGET if it was already displayed in
           ;; another window, so reapply the refreshed position after.
           (pos (with-current-buffer target (point))))
      (switch-to-buffer target)
      ;; Switching from the file-visiting scratch buffer can leave
      ;; `display-line-numbers-mode' turned on here even though it's
      ;; normally off in this buffer.
      (when (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1))
      (goto-char pos))))

(defun aven-description-finish ()
  "Push this buffer to Aven and close it, like `C-c C-c' in the magit
commit message buffer."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (aven-description--close))

(defun aven-description-cancel ()
  "Discard this buffer's changes and close it, like `C-c C-k' in the
magit commit message buffer."
  (interactive)
  (aven-description--close))

(defvar aven-description-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'aven-description-finish)
    (define-key map (kbd "C-c C-k") #'aven-description-cancel)
    map))

(define-minor-mode aven-description-edit-mode
  "Minor mode for a buffer editing an Aven long-text field, styled
after the magit commit message buffer. `C-c C-c'
(`aven-description-finish') pushes the buffer via `aven text set',
guarded by the SHA-256 read when the buffer was opened, and closes
the buffer immediately. `C-c C-k' (`aven-description-cancel')
discards any unsaved changes and closes the buffer immediately.
Either way, the buffer this edit was started from is left in view."
  :lighter " Aven-Edit"
  (if aven-description-edit-mode
      (progn
        (add-hook 'after-save-hook #'aven-description--after-save nil t)
        (add-hook 'kill-buffer-hook #'aven-description--cleanup nil t))
    (remove-hook 'after-save-hook #'aven-description--after-save t)
    (remove-hook 'kill-buffer-hook #'aven-description--cleanup t)))

(defun aven/edit-description (&optional ref)
  "Open a buffer to edit REF's description, saved back via `aven text set'.
REF defaults to the task at point; errors if there is none."
  (interactive)
  (let* ((ref (or ref (aven--ref-at-point)
                  (user-error "aven: no task at point")))
         (origin (current-buffer))
         (file (make-temp-file (format "aven-%s-description-" ref) nil ".md"))
         (result (with-temp-buffer
                   (let ((exit-code (call-process aven--executable nil t nil
                                                  "text" "get" ref "description"
                                                  "--output" file)))
                     (cons exit-code (buffer-string))))))
    (unless (zerop (car result))
      (delete-file file)
      (user-error "aven: %s" (string-trim (cdr result))))
    (let ((hash (aven--parse-sha256 (cdr result))))
      (find-file file)
      (cond ((fboundp 'gfm-mode) (gfm-mode))
            ((fboundp 'markdown-mode) (markdown-mode)))
      (setq-local aven-description--ref ref
                  aven-description--field "description"
                  aven-description--sha256 hash
                  aven-description--origin origin)
      (aven-description-edit-mode 1)
      (message "aven: editing description of %s (C-c C-c to push, C-c C-k to discard)" ref))))

(provide 'aven-description)
;;; aven-description.el ends here
