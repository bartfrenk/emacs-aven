;;; aven-agent.el --- Work on Aven tasks with agent-shell -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; Starts an agent-shell (https://github.com/xenodium/agent-shell)
;; on an Aven task, in a git worktree of its own, and finds its way
;; back to it later.  The link between a task and its worktree lives
;; in the task's metadata:
;;
;;   agent-branch    the worktree's branch
;;   agent-worktree  the worktree's absolute path
;;   agent-base      the branch the worktree was created from
;;   agent-session   the agent-shell session id, to resume it
;;
;; agent-shell is only loaded once an agent is started.
;;
;; See docs/features/agent-shell.org for the design.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'aven-core)
(require 'aven-data)
(require 'aven-process)
(require 'aven-worktree)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell--new-shell "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--auto-preferred-config "agent-shell")
(declare-function agent-shell--resolve-config-designator "agent-shell")
(declare-function agent-shell--resolved-agent-configs "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function magit-log-other "magit-log")
(defvar agent-shell--state)

(defcustom aven-agent-branch-prefix "ag--"
  "Prefix of the default branch name for a task's worktree."
  :type 'string
  :group 'aven)

(defcustom aven-agent-prompt-function #'aven-agent-default-prompt
  "Function returning the opening message for an agent.
It is called with the keyword arguments :ref, :title, :branch, :base,
:context (the output of `aven context') and :resuming (non-nil when a
new session starts in an existing worktree)."
  :type 'function
  :group 'aven)

(cl-defun aven-agent-default-prompt (&key ref title branch base context resuming)
  "The default opening message for an agent working on REF."
  (concat
   (if resuming
       (format "Resume work on %s: %s. The notes below and the commits on \
this branch show what has been done so far.\n\n" ref title)
     (format "Work on %s: %s.\n\n" ref title))
   context "\n\n"
   (format "You are in a dedicated git worktree on branch `%s`, branched \
from `%s`. Commit your work on this branch.\n\n" branch base)
   "When the work is complete and committed: leave an `aven note` \
summarizing what changed and how you verified it, then stop. Do NOT mark \
the task done; it is marked done when the branch is merged. If you get \
blocked, leave a note explaining why and stop."))

;;; Task state

(defun aven--agent-state (ref)
  "What is known about REF's agent, as a plist: the task (:task), the
metadata linking it to a worktree (:branch, :worktree, :base and
:session), and the live agent-shell buffer in that worktree (:shell)."
  (let* ((full (aven--task-full-json ref))
         (metadata (plist-get full :metadata))
         (worktree (plist-get metadata :agent-worktree)))
    (list :task     (plist-get full :task)
          :branch   (plist-get metadata :agent-branch)
          :worktree worktree
          :base     (plist-get metadata :agent-base)
          :session  (plist-get metadata :agent-session)
          :shell    (and worktree (aven--agent-shell-buffer-for worktree)))))

(defun aven--agent-shell-buffer-for (directory)
  "The live agent-shell buffer whose working directory is DIRECTORY, or nil."
  (when (and (featurep 'agent-shell) (file-directory-p directory))
    (let ((directory (file-name-as-directory (file-truename directory))))
      (seq-find (lambda (buffer)
                  (equal directory
                         (file-name-as-directory
                          (file-truename (buffer-local-value 'default-directory buffer)))))
                (agent-shell-buffers)))))

(defun aven--agent-forget (ref state)
  "Remove the metadata linking REF to a worktree, as found in STATE.
Only keys REF has are removed: aven rejects removing a key that was
never assigned in the workspace."
  (when-let* ((args (delq nil
                          (mapcar (lambda (key)
                                    (when (plist-get state key)
                                      (format "--remove-metadata=agent-%s"
                                              (substring (symbol-name key) 1))))
                                  '(:branch :worktree :base :session)))))
    (apply #'aven--run-quietly "edit" ref args)))

;;; Display

(defface aven-agent-running-face '((t :inherit success))
  "Face for the glyph of a task whose agent is running."
  :group 'aven)

(defface aven-agent-parked-face '((t :inherit shadow))
  "Face for the glyph of a task whose worktree has no agent running."
  :group 'aven)

(defun aven--agent-worktrees ()
  "Alist of (REF . WORKTREE) for every task linked to a worktree."
  ;; aven rejects filtering on a key that no task in the workspace has
  ;; ever had, so check that one has first.
  (when (member "agent-worktree" (aven--metadata-keys))
    (mapcar (lambda (task)
              (let ((ref (plist-get task :ref)))
                (cons ref (plist-get (plist-get (aven--task-full-json ref) :metadata)
                                     :agent-worktree))))
            (aven--list-json "--has-metadata=agent-worktree"))))

(defun aven--agent-glyph (worktree)
  "Glyph for an agent in WORKTREE: ● when an agent-shell is running
there, ○ when none is, and nil when WORKTREE is nil or gone."
  (when (and worktree (file-directory-p worktree))
    (if (aven--agent-shell-buffer-for worktree)
        (propertize "●" 'font-lock-face 'aven-agent-running-face 'help-echo "Agent running")
      (propertize "○" 'font-lock-face 'aven-agent-parked-face 'help-echo "No agent running"))))

(defun aven--agent-branch-summary (worktree base)
  "How the branch checked out in WORKTREE compares with BASE, such as
\"3 commits ahead of main, clean\"."
  (let ((ahead (and base (aven--worktree-git worktree "rev-list" "--count"
                                             (concat base "..HEAD"))))
        (status (aven--worktree-git worktree "status" "--porcelain")))
    (string-join
     (delq nil
           (list (when (and ahead (zerop (car ahead)))
                   (let ((n (string-to-number (cdr ahead))))
                     (format "%d commit%s ahead of %s" n (if (= n 1) "" "s") base)))
                 (when (zerop (car status))
                   (if (string-empty-p (cdr status)) "clean" "uncommitted changes"))))
     ", ")))

(defun aven--agent-insert-button (label action)
  (insert-text-button label
                      'action (lambda (_button) (funcall action))
                      'follow-link t
                      'face 'link
                      ;; Font-lock would strip a plain `face'.
                      'font-lock-face 'link))

(defun aven--insert-agent-section (metadata)
  "Insert the Agent section of a task buffer, for a task with METADATA,
or nothing when the task has no worktree."
  (let ((branch (plist-get metadata :agent-branch))
        (worktree (plist-get metadata :agent-worktree))
        (base (plist-get metadata :agent-base))
        (label (lambda (text)
                 (insert (propertize (format "%s:" text) 'font-lock-face 'font-lock-comment-face)
                         " "))))
    (when worktree
      (insert "\n" (propertize "Agent" 'font-lock-face 'bold) "\n\n")
      (if (not (file-directory-p worktree))
          (progn (funcall label "worktree")
                 (insert (abbreviate-file-name worktree) " "
                         (propertize "(gone)" 'font-lock-face 'shadow) "\n"))
        (let ((shell (aven--agent-shell-buffer-for worktree)))
          (funcall label "branch")
          (aven--agent-insert-button
           branch (lambda ()
                    (require 'magit)
                    (let ((default-directory (file-name-as-directory worktree)))
                      (magit-log-other (list (if base (concat base ".." branch) branch))))))
          (let ((summary (aven--agent-branch-summary worktree base)))
            (unless (string-empty-p summary)
              (insert " (" summary ")")))
          (insert "\n")
          (funcall label "worktree")
          (aven--agent-insert-button (abbreviate-file-name worktree)
                                     (lambda () (dired worktree)))
          (insert "\n")
          (funcall label "agent")
          (insert (aven--agent-glyph worktree) " ")
          (if shell
              (aven--agent-insert-button (buffer-name shell)
                                         (lambda () (pop-to-buffer shell)))
            (insert (propertize "not running" 'font-lock-face 'shadow)))
          (insert "\n"))))))

;;; Project directory and branch

(defun aven--project-directory (project)
  "PROJECT's directory on disk: the one recorded with `aven project
path add', a choice when several are recorded, or a prompted-for one,
which is then recorded."
  (let* ((known (seq-filter #'file-directory-p (aven--project-paths project)))
         (add-new "Add a directory...")
         (choice (cond
                  ((null known) add-new)
                  ((null (cdr known)) (car known))
                  (t (completing-read (format "Directory of %s: " project)
                                      (append known (list add-new)) nil t)))))
    (if (not (equal choice add-new))
        choice
      (let ((dir (directory-file-name
                  (expand-file-name
                   (read-directory-name (format "Directory of %s: " project) nil nil t)))))
        (aven--run-quietly "project" "path" "add" project dir)
        dir))))

(defun aven--agent-default-branch (project-root title)
  "`aven-agent-branch-prefix' plus the first words of TITLE, with a
number appended when that branch or its worktree already exists."
  (let* ((words (split-string (downcase title) "[^a-z0-9]+" t))
         (name (concat aven-agent-branch-prefix
                       (string-join (seq-take words 4) "-")))
         (candidate name)
         (n 1))
    (while (or (aven--worktree-branch-exists-p project-root candidate)
               (file-exists-p (aven--worktree-path project-root candidate)))
      (setq n (1+ n)
            candidate (format "%s-%d" name n)))
    candidate))

(defun aven--agent-read-branch (project-root default)
  "Read a branch name for a new worktree, offering DEFAULT."
  (let ((branch (string-trim (read-string "Branch: " default))))
    (unless (aven--worktree-valid-branch-name-p project-root branch)
      (user-error "aven: `%s' is not a valid branch name" branch))
    branch))

;;; agent-shell

(defun aven--agent-config (designator)
  "The agent-shell config named by DESIGNATOR, a string or symbol, or
nil when DESIGNATOR is nil, letting agent-shell choose."
  (when designator
    (let ((designator (if (stringp designator) (intern designator) designator)))
      (or (agent-shell--resolve-config-designator designator)
          (user-error "aven: unknown agent-shell config `%s'" designator)))))

(defun aven--agent-config-names ()
  "Names of the configured agent-shell agents."
  (require 'agent-shell)
  (mapcar (lambda (config) (symbol-name (map-elt config :identifier)))
          (agent-shell--resolved-agent-configs)))

(defun aven--agent-record-session (ref shell-buffer known)
  "Record SHELL-BUFFER's session id on REF once its session exists,
unless it is KNOWN already."
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'init-finished
   :on-event (lambda (_event)
               (when-let* ((id (with-current-buffer shell-buffer
                                 (map-nested-elt agent-shell--state '(:session :id)))))
                 (unless (equal id known)
                   (aven--run-quietly "edit" ref (concat "--metadata=agent-session=" id)))))))

(cl-defun aven--agent-open-shell (ref worktree &key config prompt session edit)
  "Start an agent-shell for REF in WORKTREE and show it.  With SESSION,
resume that session; otherwise send PROMPT as the first message, or
with EDIT, leave it in the input to edit before sending."
  (let ((buffer
         (if session
             (let ((default-directory (file-name-as-directory worktree)))
               (agent-shell--start
                :config (or config
                            (agent-shell--auto-preferred-config)
                            (agent-shell-select-config :prompt "Resume with agent: ")
                            (user-error "aven: no agent-shell config available"))
                :session-id session
                :new-session t
                :no-focus t))
           (agent-shell--new-shell :location (file-name-as-directory worktree)
                                   :config config :no-display t))))
    (aven--agent-record-session ref buffer session)
    ;; Keep the glyphs in Aven buffers in step with the agent running.
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda () (run-at-time 0 nil #'aven--refresh-open-buffers))
                nil t))
    (aven--refresh-open-buffers)
    (cond
     (session (display-buffer buffer t))
     (edit (agent-shell--insert-to-shell-buffer :shell-buffer buffer :text prompt))
     (t (display-buffer buffer t)
        (agent-shell--insert-to-shell-buffer :shell-buffer buffer :text prompt
                                             :submit t :no-focus t)))
    buffer))

(defun aven--agent-prompt (ref task branch base resuming)
  (funcall aven-agent-prompt-function
           :ref ref
           :title (plist-get task :title)
           :branch branch
           :base base
           :context (with-temp-buffer
                      (if (zerop (call-process aven--executable nil t nil "context" ref))
                          (string-trim (buffer-string))
                        (error "aven: %s" (string-trim (buffer-string)))))
           :resuming resuming))

;;; Entry point

(cl-defun aven-agent-start-or-switch (ref &key base config edit read-branch)
  "Start, switch to or resume the agent working on REF.

Without a worktree for REF, create one and start an agent-shell in it
with the opening message from `aven-agent-prompt-function'.  With a
worktree and a live agent-shell, switch to that.  With a worktree but
no agent-shell, start one there, resuming the recorded session if any.

BASE overrides the branch a new worktree starts from.  CONFIG names
the agent-shell config to use.  EDIT leaves the opening message in the
shell's input instead of sending it.  READ-BRANCH asks for the branch
name of a new worktree instead of using the default."
  (require 'agent-shell)
  (let* ((state (aven--agent-state ref))
         (task (plist-get state :task))
         (worktree (plist-get state :worktree))
         (config (aven--agent-config config)))
    (when (eq (plist-get task :is_epic) t)
      (user-error "aven: %s is an epic; start an agent on a child task instead" ref))
    (when (and worktree (not (file-directory-p worktree)))
      (message "aven: worktree %s is gone; starting afresh" worktree)
      (aven--agent-forget ref state)
      (setq worktree nil))
    (cond
     ((and worktree (plist-get state :shell))
      (pop-to-buffer (plist-get state :shell)))
     (worktree
      (let ((session (plist-get state :session)))
        (aven--agent-open-shell
         ref worktree
         :config config
         :session session
         :edit edit
         :prompt (unless session
                   (aven--agent-prompt ref task (plist-get state :branch)
                                       (plist-get state :base) t)))))
     (t (aven--agent-start ref task :base base :config config :edit edit
                           :read-branch read-branch)))))

(cl-defun aven--agent-start (ref task &key base config edit read-branch)
  "Create a worktree for REF, whose details are TASK, and start an
agent-shell in it.  See `aven-agent-start-or-switch'."
  (let ((status (plist-get task :status))
        (blockers (plist-get task :blocked_by)))
    (when (and (member status '("done" "canceled"))
               (not (y-or-n-p (format "%s is %s.  Reopen it and start an agent? " ref status))))
      (user-error "aven: cancelled"))
    (when (and (numberp blockers) (> blockers 0)
               (not (y-or-n-p (format "%s is blocked by %d open task%s.  Start anyway? "
                                      ref blockers (if (= blockers 1) "" "s")))))
      (user-error "aven: cancelled"))
    (let* ((project (plist-get task :project))
           (project-root (if (or (null project) (string-empty-p project))
                             (user-error "aven: %s has no project" ref)
                           (aven--project-directory project)))
           (base (aven--worktree-base project-root base))
           (default (aven--agent-default-branch project-root (plist-get task :title)))
           (branch (if read-branch (aven--agent-read-branch project-root default) default))
           (worktree
            (aven--worktree-provision
             :project-root project-root
             :branch branch
             :base base
             :on-created
             (lambda (worktree)
               (apply #'aven--run-quietly "edit" ref
                      (append
                       (unless (equal status "active") (list "--status=active"))
                       (list (concat "--metadata=agent-branch=" branch)
                             (concat "--metadata=agent-worktree=" worktree)
                             (concat "--metadata=agent-base=" base))))))))
      (aven--agent-open-shell ref worktree
                              :config config
                              :edit edit
                              :prompt (aven--agent-prompt ref task branch base nil)))))

(provide 'aven-agent)
;;; aven-agent.el ends here
