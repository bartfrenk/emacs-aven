;;; aven.el --- Transient interface to the Aven CLI task manager -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Package-Requires: ((emacs "27.1") (transient "0.4") (magit-section "3.0"))
;; Keywords: tools

;;; Commentary:

;; Aven is a local-first task manager (the `aven' CLI).  This package
;; wraps it in a magit-like transient interface, plus a status buffer
;; and a per-task buffer, both built on `magit-section'.
;;
;; Evil bindings are set up automatically when `evil' is loaded, but
;; are not required: without evil, everything here is still reachable
;; interactively (M-x aven/status, aven/dispatch, ...).
;;
;; This package does not bind any global keys itself; callers should
;; bind `aven/status' or `aven/dispatch' to whatever key they like.

;;; Code:

(require 'transient)
(require 'magit-section)
;; `evil-define-key' is a macro; the byte-compiler must see its real
;; definition at compile time or it silently compiles the calls below
;; into runtime calls to a nonexistent function `evil-define-key'.
;; This is compile-time only, so evil is still not a runtime dependency.
(eval-when-compile
  (require 'evil))

(defvar aven--executable "aven")

(defvar aven-status-buffer-name "*aven-status*")

(defconst aven--ref-pattern "[A-Z][A-Z0-9]*-[A-Z0-9]+"
  "Pattern matching a task ref such as APP-7KQ9, without anchors.")

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

;;; Task data

(defun aven--task-json (ref)
  "Full JSON detail for REF, as a plist."
  (with-temp-buffer
    (let ((exit-code (call-process aven--executable nil t nil "show" ref "--json")))
      (if (zerop exit-code)
          (let ((json-array-type 'list)
                (json-object-type 'plist)
                (json-key-type 'keyword))
            (json-read-from-string (buffer-string)))
        (error "aven: %s" (string-trim (buffer-string)))))))

(defun aven--task-properties (task)
  "Alist of label/value pairs describing all of TASK's fields."
  (let ((priority  (plist-get task :priority))
        (labels    (plist-get task :labels))
        (due       (plist-get task :due_on))
        (available (plist-get task :available_at))
        (blocked   (plist-get task :blocked_by))
        (blocks    (plist-get task :blocks)))
    (delq nil
          (list (cons "ref" (propertize (plist-get task :ref)
                                         'font-lock-face 'aven-ref-face))
                (cons "status" (plist-get task :status))
                (unless (equal priority "none") (cons "priority" priority))
                (cons "project" (propertize (plist-get task :project)
                                             'font-lock-face 'aven-project-face))
                (when labels (cons "labels" (propertize (string-join labels ",")
                                                          'font-lock-face 'aven-label-face)))
                (unless (string-empty-p due) (cons "due" (propertize due
                                                                      'font-lock-face 'aven-due-face)))
                (unless (string-empty-p available) (cons "available" available))
                (when (and blocked (> blocked 0)) (cons "blocked by" (number-to-string blocked)))
                (when (and blocks (> blocks 0)) (cons "blocks" (number-to-string blocks)))
                (when (eq (plist-get task :is_epic) t) (cons "epic" "yes"))
                (when (eq (plist-get task :has_conflict) t) (cons "conflict" "yes"))
                (cons "created" (plist-get task :created_at))
                (cons "updated" (plist-get task :updated_at))
                (cons "id" (plist-get task :id))))))

(defun aven--task-description (ref)
  "Raw description text of REF, or the empty string on failure."
  (with-temp-buffer
    (if (zerop (call-process aven--executable nil t nil
                             "text" "get" ref "description" "--raw"))
        (string-trim (buffer-string))
      "")))

;;; Task buffer

(defvar-local aven-task--ref nil
  "Ref of the task this buffer displays.")

(define-derived-mode aven-task-mode special-mode "Aven-Task"
  "Major mode for a buffer showing one Aven task's properties and description.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'aven-task-mode 'motion))

(defun aven-task-refresh ()
  "Rebuild this Aven task buffer from the current state of its task."
  (interactive)
  (let* ((ref aven-task--ref)
         (task (aven--task-json ref))
         (description (aven--task-description ref)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (plist-get task :title) 'font-lock-face 'bold) "\n\n")
      (dolist (prop (aven--task-properties task))
        (insert (propertize (format "%s:" (car prop)) 'font-lock-face 'font-lock-comment-face)
                " " (cdr prop) "\n"))
      (insert "\n")
      (if (string-empty-p description)
          (insert (propertize "No description." 'font-lock-face 'shadow) "\n")
        (insert description "\n")))
    (goto-char (point-min))))

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
    "?" #'aven/dispatch))

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

(defun aven--call (buffer-name args)
  "Run aven with ARGS, a list of strings, and display the output in BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (aven-output-mode)
        (insert (format "$ aven %s\n\n"
                        (string-join (mapcar #'shell-quote-argument args) " ")))
        (apply #'call-process aven--executable nil t nil args)
        (goto-char (point-min))))
    (when (get-buffer aven-status-buffer-name)
      (aven-status-refresh))
    (dolist (task-buf (buffer-list))
      (with-current-buffer task-buf
        (when (derived-mode-p 'aven-task-mode)
          (aven-task-refresh))))
    (display-buffer buf)))

(defun aven--run (&rest args)
  "Run aven with ARGS and display the output in `*aven*'."
  (aven--call "*aven*" args))

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

(transient-define-prefix aven/edit ()
  "Edit an Aven task."
  ["Arguments"
   ("-s" "Status"           "--status=")
   ("-i" "Priority"         "--priority=")
   ("-p" "Project"          "--project=")
   ("-t" "Title"            "--title=")
   ("-l" "Add label"        "--label=")
   ("-L" "Remove label"     "--remove-label=")
   ("-a" "Available at"     "--available-at=")
   ("-A" "Clear available"  "--clear-available-at")
   ("-d" "Due"              "--due=")
   ("-D" "Clear due"        "--clear-due")
   ("-e" "Epic (on/off)"    "--epic=")]
  ["Action"
   ("RET" "Apply to task" aven--edit-task)])

(defun aven--edit-task (&optional args)
  (interactive (list (transient-args 'aven/edit)))
  (let ((ref (aven--read-ref "Edit task: ")))
    (apply #'aven--run "edit" (append args (list ref)))))

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

(defun aven-description--close ()
  "Kill this buffer immediately and bring the Aven status buffer into view."
  (set-buffer-modified-p nil)
  (kill-buffer)
  (switch-to-buffer (aven-status-refresh)))

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
Either way, the Aven status buffer is left in view."
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
                  aven-description--sha256 hash)
      (aven-description-edit-mode 1)
      (message "aven: editing description of %s (C-c C-c to push, C-c C-k to discard)" ref))))

;;; Quick field edits

(defun aven--edit-field (flag-fn)
  "Run `aven edit' on the task at point, applying the flag returned
by calling FLAG-FN with its ref."
  (let ((ref (aven--ref-at-point)))
    (unless ref
      (user-error "aven: no task at point"))
    (aven--run "edit" ref (funcall flag-fn ref))))

(defun aven--task-field (ref field)
  "REF's FIELD from `aven show', or nil if it is absent or empty."
  (let ((value (plist-get (aven--task-json ref) field)))
    (unless (or (null value) (and (stringp value) (string-empty-p value)))
      value)))

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
     (concat "--project=" (read-string "Project: " (aven--task-field ref :project))))))

(defun aven/edit-label-add ()
  "Add a label to a task."
  (interactive)
  (aven--edit-field
   (lambda (_ref) (concat "--label=" (read-string "Add label: ")))))

(defun aven/edit-label-remove ()
  "Remove a label from a task."
  (interactive)
  (aven--edit-field
   (lambda (_ref) (concat "--remove-label=" (read-string "Remove label: ")))))

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
    ("l" "List"    aven/list)
    ("s" "Search"  aven/search)
    ("w" "Show"    aven/show)
    ("c" "Context" aven/context)]
   ["Task"
    ("a" "Add"         aven/add)
    ("e" "Edit"        aven/edit)
    ("f" "Edit field"  aven/edit-field)
    ("d" "Description" aven/edit-description)
    ("n" "Note"        aven/note)]
   ["Workspace"
    ("g" "Sync"   aven/sync)
    ("y" "Doctor" aven/doctor)]])

;;; Status buffer

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

(defun aven--list-json (&rest args)
  "Run `aven list' with ARGS and return the parsed tasks."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process aven--executable nil t nil
                            "list" (append args (list "--json")))))
      (if (zerop exit-code)
          (let ((json-array-type 'list)
                (json-object-type 'plist)
                (json-key-type 'keyword))
            (json-read-from-string (buffer-string)))
        (error "aven: %s" (string-trim (buffer-string)))))))

(defun aven--task-line-text (task)
  "Text of TASK's line: ref, project, labels, description, and due date."
  (let ((ref     (plist-get task :ref))
        (project (plist-get task :project))
        (labels  (plist-get task :labels))
        (due     (plist-get task :due_on))
        (title   (plist-get task :title)))
    (concat
     (propertize ref 'font-lock-face 'aven-ref-face)
     " "
     (unless (string-empty-p project)
       (concat (propertize project 'font-lock-face 'aven-project-face) " "))
     (when labels
       (concat (propertize (string-join labels ",") 'font-lock-face 'aven-label-face) " "))
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
        (insert "    " line "\n"))))
  (insert "\n"))

(defun aven--insert-task-line (task)
  "Insert TASK as a folded section: the heading is its formatted line,
the body is its properties drawer and description."
  (magit-insert-section (aven-task (plist-get task :ref) t)
    (magit-insert-heading (aven--task-line-text task))
    (aven--insert-task-drawer task)))

(defun aven--insert-task-section (heading hide tasks)
  "Insert a section titled HEADING listing TASKS.
When HIDE is non-nil, the section starts folded."
  (when tasks
    (magit-insert-section (aven-tasks heading hide)
      (magit-insert-heading (format "%s (%d)" heading (length tasks)))
      (dolist (task tasks)
        (aven--insert-task-line task))
      (insert "\n"))))

(defun aven--current-workspace ()
  "Description of the active Aven workspace, or nil if it can't be found."
  (with-temp-buffer
    (when (zerop (call-process aven--executable nil t nil "doctor" "--json"))
      (let* ((json-array-type 'list)
             (json-object-type 'plist)
             (json-key-type 'keyword)
             (sections (plist-get (json-read-from-string (buffer-string)) :sections)))
        (catch 'found
          (dolist (section sections)
            (when (equal (plist-get section :code) "workspace")
              (dolist (row (plist-get section :rows))
                (when (equal (plist-get row :code) "workspace.active")
                  (throw 'found (plist-get row :value)))))))))))

(define-derived-mode aven-status-mode magit-section-mode "Aven-Status"
  "Major mode for the Aven status buffer.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'aven-status-mode 'motion))

(defun aven-status-visit-task-or-toggle ()
  "Show the task at point in a dedicated buffer, or toggle the section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'aven-task))
        (aven--show-ref (oref section value))
      (when section (magit-section-toggle section)))))

(defun aven-status-refresh ()
  "Rebuild the Aven status buffer."
  (interactive)
  (let* ((buf (get-buffer-create aven-status-buffer-name))
         (workspace (aven--current-workspace))
         (groups (list (cons "Active"  (aven--list-json "--status=active"))
                       (cons "Todo"    (aven--list-json "--status=todo"))
                       (cons "Backlog" (aven--list-json "--status=backlog"))
                       (cons "Inbox"   (aven--list-json "--status=inbox")))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aven-status-mode)
        (aven-status-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when workspace
          (insert (propertize (format "Workspace: %s" workspace) 'font-lock-face 'bold) "\n\n"))
        (magit-insert-section (aven-status)
          (dolist (group groups)
            (aven--insert-task-section (car group) nil (cdr group))))
        (when (eq (point-min) (point-max))
          (insert (propertize "No tasks.\n" 'font-lock-face 'shadow)))
        (let ((magit-section-cache-visibility nil))
          (magit-section-show magit-root-section)))
      (goto-char (point-min)))
    buf))

(defun aven/status ()
  "Open the Aven status buffer, the entry point for the Aven interface."
  (interactive)
  (switch-to-buffer (aven-status-refresh)))

(with-eval-after-load 'evil
  (evil-define-key 'motion aven-status-mode-map
    (kbd "RET") #'aven-status-visit-task-or-toggle
    "g" #'aven-status-refresh
    "l" #'aven/list
    "s" #'aven/search
    "w" #'aven/show
    "c" #'aven/context
    "a" #'aven/add
    "e" #'aven/edit-field
    "n" #'aven/note
    "?" #'aven/dispatch))

(provide 'aven)
;;; aven.el ends here
