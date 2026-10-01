;;; aven-transient.el --- Transient commands for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Package-Requires: ((emacs "27.1") (transient "0.4"))
;; Keywords: tools

;;; Commentary:

;; All transient prefixes (list/search/show/context/add/dispatch)
;; and the quick single-field edit commands.

;;; Code:

(require 'transient)
(require 'aven-core)
(require 'aven-data)
(require 'aven-process)
(require 'aven-description)

(transient-define-prefix aven/list ()
  "List Aven tasks."
  ["Filters"
   ("-r" "Ready only"        "--ready")
   ("-o" "Open (nonterminal)" "--open")
   ("-b" "Blocked"           "--blocked")
   ("-e" "Epics"             "--epics")
   ("-u" "Upcoming"          "--upcoming")
   ("-d" "Overdue"           "--overdue")
   ("-a" "Include deleted"   "--all")]
  ["Arguments"
   ("-p" "Project"  "--project=")
   ("-s" "Status"   "--status=")
   ("-i" "Priority" "--priority=")
   ("-l" "Label"    "--label=")
   ("-n" "Limit"    "--limit=")]
  ["Action"
   ("RET" "List tasks" aven--list-tasks)])

(defun aven--list-tasks (&optional args)
  (interactive (list (transient-args 'aven/list)))
  (apply #'aven--run "list" args))

(transient-define-prefix aven/search ()
  "Search Aven tasks."
  ["Arguments"
   ("-p" "Project"        "--project=")
   ("-n" "Limit"          "--limit=")
   ("-a" "Include deleted" "--all")]
  ["Action"
   ("RET" "Search" aven--search-tasks)])

(defun aven--search-tasks (&optional args)
  (interactive (list (transient-args 'aven/search)))
  (let ((query (read-string "Search: ")))
    (apply #'aven--run "search" (append args (split-string query)))))

(transient-define-prefix aven/show ()
  "Show an Aven task."
  ["Arguments"
   ("-f" "Full detail" "--full")]
  ["Action"
   ("RET" "Show task" aven--show-task)])

(defun aven--show-task (&optional args)
  (interactive (list (transient-args 'aven/show)))
  (let ((ref (aven--read-ref "Show task: ")))
    (apply #'aven--run "show" (append args (list ref)))))

(defun aven/context ()
  "Show a context snapshot for a task."
  (interactive)
  (aven--run "context" (aven--read-ref "Context for: ")))

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
    ("L" "List"    aven/list)
    ("s" "Search"  aven/search)
    ("w" "Show"    aven/show)
    ("c" "Context" aven/context)]
   ["Task"
    ("a" "Add"         aven/add)
    ("e" "Edit field"  aven/edit-field)
    ("n" "Note"        aven/note)
    ("D" "Delete"      aven/delete)]
   ["Workspace"
    ("g" "Sync"   aven/sync)
    ("y" "Doctor" aven/doctor)]])

(provide 'aven-transient)
;;; aven-transient.el ends here
