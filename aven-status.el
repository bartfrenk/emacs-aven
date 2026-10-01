;;; aven-status.el --- Status buffer for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; The Aven status buffer: a magit-section listing of tasks grouped
;; by status, and the entry point `aven/status'.

;;; Code:

(require 'magit-section)
(require 'aven-core)
(require 'aven-data)
(require 'aven-agent)
;; `evil-define-key' is a macro; the byte-compiler must see its real
;; definition at compile time or it silently compiles the calls below
;; into runtime calls to a nonexistent function `evil-define-key'.
;; This is compile-time only, so evil is still not a runtime dependency.
(eval-when-compile
  (require 'evil))

;; Defined in aven-transient.el, loaded before this module is used
;; interactively; declared here only to keep the byte-compiler quiet
;; about the forward references from the evil keymap below.
(declare-function aven/context "aven-transient")
(declare-function aven/add "aven-transient")
(declare-function aven/edit-field "aven-transient")
(declare-function aven/note "aven-transient")
(declare-function aven/delete "aven-transient")
(declare-function aven/status-filter "aven-transient")
(declare-function aven/agent "aven-transient")
(declare-function aven/dispatch "aven-transient")

;; Defined in aven-task.el.
(declare-function aven--show-ref "aven-task")

(defvar aven-status-filter nil
  "Extra `aven list' arguments, such as \"--project=app\", that
narrow the tasks shown in the status buffer.")

(defun aven--priority-face (priority)
  "Face for PRIORITY, matching the colors of the Aven output buffer."
  (pcase priority
    ("urgent" 'error)
    ("high"   'warning)
    ("low"    'shadow)
    (_        'default)))

(defun aven--task-line-text (task &optional glyph)
  "Text of TASK's line: ref, priority, project, labels, GLYPH (marking
the state of its agent, if any), description, and due date. A
priority of none is left out."
  (let ((ref      (plist-get task :ref))
        (priority (plist-get task :priority))
        (project  (plist-get task :project))
        (labels   (plist-get task :labels))
        (due      (plist-get task :due_on))
        (title    (plist-get task :title)))
    (concat
     (propertize ref 'font-lock-face 'aven-ref-face)
     " "
     (unless (member priority '(nil "" "none"))
       (concat (propertize priority 'font-lock-face (aven--priority-face priority)) " "))
     (unless (string-empty-p project)
       (concat (propertize project 'font-lock-face 'aven-project-face) " "))
     (when labels
       (concat (propertize (string-join labels ",") 'font-lock-face 'aven-label-face) " "))
     (when glyph (concat glyph " "))
     title
     (unless (string-empty-p due)
       (concat " " (propertize (format "[%s]" due) 'font-lock-face 'aven-due-face))))))

(defun aven--insert-task-drawer (task)
  "Insert TASK's fields as properties, then its description, as the
body of its (folded) section."
  (insert "\n")
  (dolist (prop (aven--task-properties task))
    (insert "    " (propertize (format "%s:" (car prop)) 'font-lock-face 'font-lock-comment-face)
            " " (cdr prop) "\n"))
  (insert "\n")
  (let ((description (aven--task-description (plist-get task :ref))))
    (if (string-empty-p description)
        (insert "    " (propertize "No description." 'font-lock-face 'shadow) "\n")
      (dolist (line (split-string description "\n"))
        (insert "    ")
        (aven--insert-text line)
        (insert "\n"))))
  (insert "\n"))

(defun aven--insert-task-line (task &optional glyph)
  "Insert TASK as a folded section: the heading is its formatted line,
with GLYPH marking the state of its agent, and the body is its
properties drawer and description."
  (magit-insert-section (aven-task (plist-get task :ref) t)
    (magit-insert-heading (aven--task-line-text task glyph))
    (aven--insert-task-drawer task)))

(defun aven--insert-task-section (heading hide tasks &optional glyphs)
  "Insert a section titled HEADING listing TASKS, marking each task
with its glyph in GLYPHS, an alist of (REF . GLYPH).
When HIDE is non-nil, the section starts folded."
  (when tasks
    (magit-insert-section (aven-tasks heading hide)
      (magit-insert-heading (format "%s (%d)" heading (length tasks)))
      (dolist (task tasks)
        (aven--insert-task-line task (cdr (assoc (plist-get task :ref) glyphs))))
      (insert "\n"))))

(define-derived-mode aven-status-mode magit-section-mode "Aven-Status"
  "Major mode for the Aven status buffer.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'aven-status-mode 'motion))

;; evil-snipe's local map takes precedence over the status buffer's
;; own evil bindings, so it would shadow f (filter) and s (agent).
(defvar evil-snipe-disabled-modes)
(with-eval-after-load 'evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'aven-status-mode))

(defun aven-status-visit-task-or-toggle ()
  "Show the task at point in a dedicated buffer, or toggle the section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'aven-task))
        (aven--show-ref (oref section value))
      (when section (magit-section-toggle section)))))

(defun aven--section-for-ref (section ref)
  "The `aven-task' section for REF within SECTION's subtree, or nil."
  (if (and (eq (oref section type) 'aven-task) (equal (oref section value) ref))
      section
    (catch 'found
      (dolist (child (oref section children))
        (when-let* ((found (aven--section-for-ref child ref)))
          (throw 'found found))))))

(defun aven-status-refresh ()
  "Rebuild the Aven status buffer, keeping point on the same task
when possible."
  (interactive)
  (let* ((buf (get-buffer-create aven-status-buffer-name))
         (workspace (aven--current-workspace))
         (agents (aven--agent-worktrees))
         (glyphs (mapcar (lambda (agent)
                           (cons (plist-get (car agent) :ref)
                                 (aven--agent-glyph (cdr agent) (plist-get (car agent) :status))))
                         agents))
         ;; The status buffer doesn't list done and canceled tasks, but
         ;; ones whose worktree remains still need finishing.
         (leftovers (mapcar #'car
                            (seq-filter (lambda (agent)
                                          (and (aven--agent-closed-p (plist-get (car agent) :status))
                                               (cdr agent)
                                               (file-directory-p (cdr agent))))
                                        agents)))
         (groups (mapcar (lambda (group)
                           (cons (car group)
                                 (apply #'aven--list-json
                                        (concat "--status=" (cdr group))
                                        aven-status-filter)))
                         '(("Active"  . "active")
                           ("Todo"    . "todo")
                           ("Backlog" . "backlog")
                           ("Inbox"   . "inbox")))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aven-status-mode)
        (aven-status-mode))
      (let ((ref (aven--ref-at-point)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when workspace
            (insert (propertize (format "Workspace: %s" workspace) 'font-lock-face 'bold) "\n"))
          (when aven-status-filter
            (insert (propertize (format "Filter: %s" (string-join aven-status-filter " "))
                                'font-lock-face 'bold)
                    "\n"))
          (unless (eq (point-min) (point))
            (insert "\n"))
          (magit-insert-section (aven-status)
            (dolist (group groups)
              (aven--insert-task-section (car group) nil (cdr group) glyphs))
            (aven--insert-task-section "Closed, worktree remains" nil leftovers glyphs))
          (when (eq (point-min) (point-max))
            (insert (propertize "No tasks.\n" 'font-lock-face 'shadow)))
          (let ((magit-section-cache-visibility nil))
            (magit-section-show magit-root-section)))
        (let ((section (and ref (aven--section-for-ref magit-root-section ref))))
          (goto-char (if section (oref section start) (point-min))))))
    buf))

(defun aven/status ()
  "Open the Aven status buffer, the entry point for the Aven interface."
  (interactive)
  (switch-to-buffer (aven-status-refresh)))

(with-eval-after-load 'evil
  (evil-define-key 'motion aven-status-mode-map
    (kbd "RET") #'aven-status-visit-task-or-toggle
    ;; Logical-line motion overshoots the folded task bodies, which
    ;; are invisible text; visual-line motion steps over them cleanly.
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "g" #'aven-status-refresh
    "c" #'aven/context
    "a" #'aven/add
    "e" #'aven/edit-field
    "n" #'aven/note
    "d" #'aven/delete
    "f" #'aven/status-filter
    "s" #'aven/agent
    "?" #'aven/dispatch))

(provide 'aven-status)
;;; aven-status.el ends here
