;;; aven-transient.el --- Transient commands for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Package-Requires: ((emacs "27.1") (transient "0.4"))
;; Keywords: tools

;;; Commentary:

;; All transient prefixes (status-filter/context/add/agent/dispatch)
;; and the quick single-field edit commands.

;;; Code:

(require 'transient)
(require 'aven-core)
(require 'aven-data)
(require 'aven-process)
(require 'aven-description)
(require 'aven-status)
(require 'aven-agent)

(transient-define-prefix aven/status-filter ()
  "Filter the tasks shown in the Aven status buffer."
  :init-value (lambda (obj) (oset obj value aven-status-filter))
  :incompatible '(("--ready" "--blocked"))
  ["Filters"
   ("-r" "Ready only" "--ready")
   ("-b" "Blocked"    "--blocked")
   ("-e" "Epics"      "--epics")
   ("-u" "Upcoming"   "--upcoming")
   ("-d" "Overdue"    "--overdue")]
  ["Arguments"
   ("-p" "Project"  "--project="
    :reader (lambda (prompt initial history)
              (completing-read prompt (aven--project-keys) nil nil initial history)))
   ("-i" "Priority" "--priority=" :choices ("none" "low" "medium" "high" "urgent"))
   ("-l" "Label"    "--label="
    :reader (lambda (prompt initial history)
              (completing-read prompt (aven--label-names) nil nil initial history)))]
  ["Action"
   ("RET" "Apply" aven--status-filter-apply)
   ("c"   "Clear" aven--status-filter-clear)])

(defun aven--status-filter-apply (&optional args)
  (interactive (list (transient-args 'aven/status-filter)))
  (setq aven-status-filter args)
  (aven-status-refresh))

(defun aven--status-filter-clear ()
  (interactive)
  (setq aven-status-filter nil)
  (aven-status-refresh))

(defun aven/context ()
  "Show a context snapshot for the task at point, or a prompted one,
and select its window."
  (interactive)
  (aven--run "context" (or (aven--ref-at-point) (aven--read-ref "Context for: ")))
  (pop-to-buffer "*aven*"))

(transient-define-prefix aven/add ()
  "Create an Aven task."
  ["Arguments"
   ("-p" "Project"       "--project=")
   ("-s" "Status"        "--status=")
   ("-i" "Priority"      "--priority=")
   ("-l" "Label"         "--label=")
   ("-a" "Available at"  "--available-at=")
   ("-d" "Due"           "--due=")
   ("-e" "Epic"          "--epic")]
  ["Action"
   ("RET" "Create task" aven--add-task)])

(defun aven--add-task (&optional args)
  (interactive (list (transient-args 'aven/add)))
  (let ((title (read-string "Title: ")))
    (when (string-empty-p title)
      (user-error "aven: title required"))
    (apply #'aven--run "add" (append args (list title)))))

;;; Quick field edits

(defun aven--edit-field (flag-fn)
  "Run `aven edit' on the task at point, applying the flag returned
by calling FLAG-FN with its ref, without popping up the Aven output
buffer."
  (let ((ref (aven--ref-at-point)))
    (unless ref
      (user-error "aven: no task at point"))
    (aven--run-quietly "edit" ref (funcall flag-fn ref))))

(defun aven/edit-title ()
  "Set a task's title."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--title=" (read-string "Title: " (aven--task-field ref :title))))))

(defun aven/edit-status ()
  "Set a task's status."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--status="
             (completing-read "Status: "
                               '("inbox" "backlog" "todo" "active" "done" "canceled")
                               nil t (aven--task-field ref :status))))))

(defun aven/edit-priority ()
  "Set a task's priority."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--priority="
             (completing-read "Priority: "
                               '("none" "low" "medium" "high" "urgent")
                               nil t (aven--task-field ref :priority))))))

(defun aven/edit-project ()
  "Move a task to another project."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--project="
             (completing-read "Project: " (aven--project-keys) nil nil
                               (aven--task-field ref :project))))))

(defun aven/edit-label-add ()
  "Add a label to a task, creating it first if it doesn't exist yet."
  (interactive)
  (aven--edit-field
   (lambda (_ref)
     (let* ((existing (aven--label-names))
            (label (completing-read "Add label: " existing)))
       (unless (member label existing)
         (call-process aven--executable nil nil nil "label" "create" label))
       (concat "--label=" label)))))

(defun aven/edit-label-remove ()
  "Remove a label from a task."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (let ((labels (aven--task-field ref :labels)))
       (unless labels
         (user-error "aven: task has no labels to remove"))
       (concat "--remove-label="
               (completing-read "Remove label: " labels nil t))))))

(defun aven/edit-available-at ()
  "Set a task's availability date."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--available-at="
             (read-string "Available at: " (aven--task-field ref :available_at))))))

(defun aven/edit-available-at-clear ()
  "Clear a task's availability date."
  (interactive)
  (aven--edit-field (lambda (_ref) "--clear-available-at")))

(defun aven/edit-due ()
  "Set a task's due date."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (concat "--due=" (read-string "Due: " (aven--task-field ref :due_on))))))

(defun aven/edit-due-clear ()
  "Clear a task's due date."
  (interactive)
  (aven--edit-field (lambda (_ref) "--clear-due")))

(defun aven/edit-epic-toggle ()
  "Toggle whether a task is an epic."
  (interactive)
  (aven--edit-field
   (lambda (ref)
     (if (eq (plist-get (aven--task-json ref) :is_epic) t)
         "--epic=off"
       "--epic=on"))))

(transient-define-prefix aven/edit-field ()
  "Edit a single field of an Aven task."
  ["Edit field"
   ("t" "Title"            aven/edit-title)
   ("d" "Description"      aven/edit-description)
   ("s" "Status"           aven/edit-status)
   ("i" "Priority"         aven/edit-priority)
   ("p" "Project"          aven/edit-project)
   ("l" "Add label"        aven/edit-label-add)
   ("L" "Remove label"     aven/edit-label-remove)
   ("a" "Available at"     aven/edit-available-at)
   ("A" "Clear available"  aven/edit-available-at-clear)
   ("u" "Due"              aven/edit-due)
   ("U" "Clear due"        aven/edit-due-clear)
   ("e" "Toggle epic"      aven/edit-epic-toggle)])

(defun aven/note ()
  "Append a note to a task."
  (interactive)
  (let* ((ref (aven--read-ref "Note for: "))
         (text (read-string "Note: ")))
    (aven--run "note" ref text)))

(defun aven/delete ()
  "Delete the task at point after confirmation.
Aven only soft-deletes it: `aven restore' recovers it."
  (interactive)
  (let ((ref (aven--ref-at-point)))
    (unless ref
      (user-error "aven: no task at point"))
    (when (y-or-n-p (format "Delete %s \"%s\"? " ref (aven--task-field ref :title)))
      ;; The task's own buffer can no longer be refreshed once it is gone.
      (when-let* ((buf (get-buffer (format "*aven: %s*" ref))))
        (kill-buffer buf))
      (aven--run-quietly "delete" ref))))

;;; Agent

(declare-function magit-status "magit-status")

(defvar aven--agent-menu-state nil
  "`aven--agent-state' of the task the agent menu was opened for, plus
its ref under :ref.")

(defun aven/agent ()
  "Start, switch to or resume an agent on the task at point."
  (interactive)
  (let ((ref (or (aven--ref-at-point) (aven--read-ref "Agent for: "))))
    (setq aven--agent-menu-state (plist-put (aven--agent-state ref) :ref ref))
    (transient-setup 'aven--agent-menu)))

(defun aven--agent-menu-worktree ()
  (let ((worktree (plist-get aven--agent-menu-state :worktree)))
    (and worktree (file-directory-p worktree) worktree)))

(defun aven--agent-menu-heading ()
  (let ((task (plist-get aven--agent-menu-state :task)))
    (format "Agent: %s %s"
            (propertize (plist-get aven--agent-menu-state :ref) 'face 'aven-ref-face)
            (plist-get task :title))))

(defun aven--agent-menu-start-description ()
  (cond
   ((plist-get aven--agent-menu-state :shell) "Switch to agent")
   ((aven--agent-menu-worktree) "Resume agent")
   (t "Start agent")))

(transient-define-prefix aven--agent-menu ()
  "Start, switch to or resume an agent on an Aven task."
  [:description aven--agent-menu-heading
   ["Options"
    ("-b" "Base branch" "--base="
     :if-not aven--agent-menu-worktree)
    ("-c" "Agent config" "--config="
     :if-not (lambda () (plist-get aven--agent-menu-state :shell))
     :reader (lambda (prompt initial history)
               (completing-read prompt (aven--agent-config-names) nil t initial history)))]
   ["Actions"
    ("s" aven--agent-menu-start :description aven--agent-menu-start-description)
    ("e" "Start, editing the first message" aven--agent-menu-start-edit
     :if-not aven--agent-menu-worktree)
    ("d" "Worktree in dired" aven--agent-menu-dired :if aven--agent-menu-worktree)
    ("m" "Worktree in magit" aven--agent-menu-magit :if aven--agent-menu-worktree)]])

(defun aven--agent-menu-run (args edit)
  (aven-agent-start-or-switch (plist-get aven--agent-menu-state :ref)
                              :base (transient-arg-value "--base=" args)
                              :config (transient-arg-value "--config=" args)
                              :edit edit
                              :read-branch t))

(defun aven--agent-menu-start (&optional args)
  (interactive (list (transient-args 'aven--agent-menu)))
  (aven--agent-menu-run args nil))

(defun aven--agent-menu-start-edit (&optional args)
  (interactive (list (transient-args 'aven--agent-menu)))
  (aven--agent-menu-run args t))

(defun aven--agent-menu-dired ()
  (interactive)
  (dired (aven--agent-menu-worktree)))

(defun aven--agent-menu-magit ()
  (interactive)
  (require 'magit)
  (magit-status (aven--agent-menu-worktree)))

(defun aven/sync ()
  "Sync Aven with its remote server."
  (interactive)
  (aven--run "sync"))

(defun aven/doctor ()
  "Diagnose Aven startup, configuration, and workspace state."
  (interactive)
  (aven--run "doctor"))

(transient-define-prefix aven/dispatch ()
  "Transient interface to the Aven CLI."
  ["Aven"
   ["Query"
    ("c" "Context" aven/context)
    ("f" "Filter"  aven/status-filter
     :if (lambda () (derived-mode-p 'aven-status-mode)))]
   ["Task"
    ("a" "Add"         aven/add)
    ("e" "Edit field"  aven/edit-field)
    ("n" "Note"        aven/note)
    ("d" "Delete"      aven/delete)]
   ["Agent"
    ("s" "Agent"       aven/agent)]
   ["Workspace"
    ("g" "Sync"   aven/sync)
    ("y" "Doctor" aven/doctor)]])

(provide 'aven-transient)
;;; aven-transient.el ends here
