;;; aven-process.el --- Running aven and the output buffer -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; Runs the `aven' CLI and displays its output in `*aven*', plus the
;; major mode for that output buffer.

;;; Code:

(require 'aven-core)
;; `evil-define-key' is a macro; the byte-compiler must see its real
;; definition at compile time or it silently compiles the calls below
;; into runtime calls to a nonexistent function `evil-define-key'.
;; This is compile-time only, so evil is still not a runtime dependency.
(eval-when-compile
  (require 'evil))

;; Defined in aven-status.el / aven-task.el, both of which are loaded
;; before this module is used interactively; declared here only to
;; keep the byte-compiler quiet about the forward reference.
(declare-function aven-status-refresh "aven-status")
(declare-function aven-task-refresh "aven-task")
(declare-function aven--show-ref "aven-task")

(defvar aven-output-font-lock-keywords
  `(("^\\$ aven .*$" . font-lock-comment-face)
    ("^description<<EOF$" . font-lock-preprocessor-face)
    ("^EOF$" . font-lock-preprocessor-face)
    ("^\\(Error:\\) error \\(\\S-+\\)" (1 'error) (2 'error))
    ("status=\\(todo\\)\\_>" 1 'success)
    ("status=\\(active\\)\\_>" 1 'warning)
    ("status=\\(done\\|canceled\\)\\_>" 1 'shadow)
    ("priority=\\(urgent\\)\\_>" 1 'error)
    ("priority=\\(high\\)\\_>" 1 'warning)
    ("priority=\\(low\\|none\\)\\_>" 1 'shadow)
    ("^\\s-*\\(ok\\)\\s-" 1 'success)
    ("^\\s-*\\(warn\\)\\s-" 1 'warning)
    ("^\\s-*\\(fail\\)\\s-" 1 'error)
    ("^\\s-*\\(\\.\\.\\)\\s-" 1 'shadow)
    ("^-+$" . font-lock-comment-face)
    ("^[A-Z][A-Za-z]+\\(?: [A-Za-z]+\\)*$" . font-lock-keyword-face)
    (,(concat "\\_<" aven--ref-pattern "\\_>") . font-lock-constant-face)
    ("\\_<\\([a-z][a-z_]*\\)=" 1 font-lock-variable-name-face)
    ("\"[^\"\n]*\"" . font-lock-string-face))
  "Font-lock keywords for `aven-output-mode'.")

(define-derived-mode aven-output-mode special-mode "Aven"
  "Major mode for displaying `aven' command output."
  (setq font-lock-defaults '(aven-output-font-lock-keywords)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'aven-output-mode 'motion))

(defun aven--call (buffer-name args &optional no-display)
  "Run aven with ARGS, a list of strings, and display the output in
BUFFER-NAME. When NO-DISPLAY is non-nil, the buffer is updated and
open Aven buffers are refreshed as usual, but the buffer is only
shown if the command failed; on success a summary goes to the echo
area instead."
  (let ((buf (get-buffer-create buffer-name))
        exit-code)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (aven-output-mode)
        (insert (format "$ aven %s\n\n"
                        (string-join (mapcar #'shell-quote-argument args) " ")))
        (setq exit-code (apply #'call-process aven--executable nil t nil args))
        (goto-char (point-min))))
    (when (get-buffer aven-status-buffer-name)
      (aven-status-refresh))
    (dolist (task-buf (buffer-list))
      (with-current-buffer task-buf
        (when (derived-mode-p 'aven-task-mode)
          (aven-task-refresh))))
    (if (and no-display (eql exit-code 0))
        (message "aven %s" (string-join args " "))
      (display-buffer buf))))

(defun aven--run (&rest args)
  "Run aven with ARGS and display the output in `*aven*'."
  (aven--call "*aven*" args))

(defun aven--run-quietly (&rest args)
  "Run aven with ARGS without popping up `*aven*' on success; open
Aven buffers are still refreshed, and the output buffer is shown if
the command fails."
  (aven--call "*aven*" args t))

(defun aven-output-visit-task ()
  "Show the task on the current line in a dedicated buffer."
  (interactive)
  (let ((ref (or (aven--ref-on-line) (aven--ref-at-point))))
    (unless ref
      (user-error "No task ref on this line"))
    (aven--show-ref ref)))

(with-eval-after-load 'evil
  (evil-define-key 'motion aven-output-mode-map
    (kbd "RET") #'aven-output-visit-task))

(provide 'aven-process)
;;; aven-process.el ends here
