;;; aven-task.el --- Per-task buffer for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; The dedicated buffer showing one Aven task's properties and
;; description, built on `special-mode'.

;;; Code:

(require 'aven-core)
(require 'aven-data)
;; `evil-define-key' is a macro; the byte-compiler must see its real
;; definition at compile time or it silently compiles the calls below
;; into runtime calls to a nonexistent function `evil-define-key'.
;; This is compile-time only, so evil is still not a runtime dependency.
(eval-when-compile
  (require 'evil))

;; Defined in aven-transient.el, loaded before this module is used
;; interactively; declared here only to keep the byte-compiler quiet
;; about the forward reference from the evil keymap below.
(declare-function aven/edit-field "aven-transient")
(declare-function aven/note "aven-transient")
(declare-function aven/delete "aven-transient")
(declare-function aven/agent "aven-transient")
(declare-function aven/dispatch "aven-transient")

(defvar-local aven-task--ref nil
  "Ref of the task this buffer displays.")

(define-derived-mode aven-task-mode special-mode "Aven-Task"
  "Major mode for a buffer showing one Aven task's properties and description.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'aven-task-mode 'motion))

;; evil-snipe's local map takes precedence over the task buffer's own
;; evil bindings, so it would shadow s (agent).
(defvar evil-snipe-disabled-modes)
(with-eval-after-load 'evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'aven-task-mode))

(defun aven-task-refresh ()
  "Rebuild this Aven task buffer from the current state of its task,
keeping point on the same line and column when possible."
  (interactive)
  (let* ((ref aven-task--ref)
         (task (aven--task-json ref))
         (description (aven--task-description ref))
         (line (line-number-at-pos))
         (column (current-column)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (plist-get task :title) 'font-lock-face 'bold) "\n\n")
      (dolist (prop (aven--task-properties task))
        (insert (propertize (format "%s:" (car prop)) 'font-lock-face 'font-lock-comment-face)
                " " (cdr prop) "\n"))
      (insert "\n")
      (if (string-empty-p description)
          (insert (propertize "No description." 'font-lock-face 'shadow) "\n")
        (aven--insert-text description)
        (insert "\n")))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun aven--show-ref (ref)
  "Show REF in a dedicated Aven task buffer, with focus on its window."
  (let ((buf (get-buffer-create (format "*aven: %s*" ref))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aven-task-mode)
        (aven-task-mode)
        (setq-local aven-task--ref ref))
      (aven-task-refresh))
    (pop-to-buffer buf)))

(with-eval-after-load 'evil
  (evil-define-key 'motion aven-task-mode-map
    "g" #'aven-task-refresh
    "e" #'aven/edit-field
    "n" #'aven/note
    "d" #'aven/delete
    "s" #'aven/agent
    "?" #'aven/dispatch))

(provide 'aven-task)
;;; aven-task.el ends here
