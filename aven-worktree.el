;;; aven-worktree.el --- Provision a git worktree, honoring .workmux.yaml -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; Creates a git worktree entirely in Emacs, without the `workmux'
;; binary or tmux, while honoring the part of workmux's config
;; (`.workmux.yaml' and `~/.config/workmux/config.yaml') that governs
;; setting a worktree up: `base_branch' (including `auto'),
;; `main_branch', `worktree_dir', `worktree_prefix',
;; `files.copy'/`files.symlink' and the `post_create', `pre_merge' and
;; `pre_remove' hooks.  It also merges a worktree's branch back and
;; removes the worktree.  Worktrees created here look the same as ones
;; created by `workmux add', so `workmux list' and `workmux merge'
;; still work on them.
;;
;; This module knows nothing about Aven tasks; see aven-agent.el.

;;; Code:

(require 'cl-lib)
(require 'yaml)
(require 'aven-core)

(defvar aven--workmux-global-config-file
  (expand-file-name "workmux/config.yaml"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Where workmux keeps its global config.")

(defvar aven-worktree-output-buffer-name "*aven-worktree*")

(define-derived-mode aven-worktree-output-mode special-mode "Aven-Worktree"
  "Major mode for displaying worktree provisioning output.")

;;; Config

(cl-defstruct (aven--workmux-config)
  "The part of a merged workmux config that this module reads."
  main-branch base-branch worktree-dir worktree-prefix
  files-copy files-symlink post-create pre-merge pre-remove)

(defun aven--workmux-read-yaml-file (path)
  "The parsed alist for the YAML file at PATH, or nil if PATH is nil
or unreadable."
  (when (and path (file-readable-p path))
    (condition-case err
        (yaml-parse-string (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))
                           :object-type 'alist
                           :object-key-type 'string
                           :sequence-type 'list
                           :null-object nil
                           :false-object nil)
      (error (user-error "aven: failed to parse %s: %s" path (error-message-string err))))))

(defun aven--workmux-alist-get (alist key)
  (alist-get key alist nil nil #'equal))

(defun aven--workmux-merge-scalar (project global key)
  "KEY's value in PROJECT if it is set there, else in GLOBAL."
  (if (assoc key project)
      (aven--workmux-alist-get project key)
    (aven--workmux-alist-get global key)))

(defun aven--workmux-merge-list (project global key)
  "KEY's list value: PROJECT's list with each \"<global>\" entry
replaced by GLOBAL's list, or GLOBAL's list if PROJECT doesn't set KEY."
  (if (assoc key project)
      (let ((global-items (aven--workmux-alist-get global key)))
        (apply #'append
               (mapcar (lambda (item)
                         (if (equal item "<global>") global-items (list item)))
                       (aven--workmux-alist-get project key))))
    (aven--workmux-alist-get global key)))

(defun aven--workmux-config (project-root)
  "PROJECT-ROOT's `.workmux.yaml' merged over the global workmux config."
  (let* ((global (aven--workmux-read-yaml-file aven--workmux-global-config-file))
         (project (aven--workmux-read-yaml-file
                   (expand-file-name ".workmux.yaml" project-root)))
         (files-global (aven--workmux-alist-get global "files"))
         (files-project (aven--workmux-alist-get project "files")))
    (make-aven--workmux-config
     :main-branch     (aven--workmux-merge-scalar project global "main_branch")
     :base-branch     (aven--workmux-merge-scalar project global "base_branch")
     :worktree-dir    (aven--workmux-merge-scalar project global "worktree_dir")
     :worktree-prefix (or (aven--workmux-merge-scalar project global "worktree_prefix") "")
     :files-copy      (aven--workmux-merge-list files-project files-global "copy")
     :files-symlink   (aven--workmux-merge-list files-project files-global "symlink")
     :post-create     (aven--workmux-merge-list project global "post_create")
     :pre-merge       (aven--workmux-merge-list project global "pre_merge")
     :pre-remove      (aven--workmux-merge-list project global "pre_remove"))))

;;; Git

(defun aven--worktree-git (directory &rest args)
  "Run git ARGS from DIRECTORY, returning (EXIT-CODE . OUTPUT)."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "git" nil t nil args)))
        (cons exit-code (string-trim (buffer-string)))))))

(defun aven--worktree-repo-p (directory)
  "Non-nil when DIRECTORY is inside a git repository."
  (and (file-directory-p directory)
       (zerop (car (aven--worktree-git directory "rev-parse" "--is-inside-work-tree")))))

(defun aven--worktree-branch-exists-p (project-root branch)
  (zerop (car (aven--worktree-git project-root "show-ref" "--verify" "--quiet"
                                  (concat "refs/heads/" branch)))))

(defun aven--worktree-git-or-error (directory &rest args)
  "Run git ARGS from DIRECTORY, returning its output, or signal an
error with git's output when it fails."
  (let ((result (apply #'aven--worktree-git directory args)))
    (unless (zerop (car result))
      (error "aven: git %s failed: %s" (car args) (cdr result)))
    (cdr result)))

(defun aven--worktree-dirty-p (directory)
  "Non-nil when the checkout in DIRECTORY has uncommitted changes or
untracked files, other than agent-shell's own `.agent-shell'."
  (not (string-empty-p
        (aven--worktree-git-or-error directory "status" "--porcelain" "--"
                                     "." ":(exclude).agent-shell"))))

(defun aven--worktree-commits-ahead (directory base)
  "How many commits HEAD in DIRECTORY has that BASE doesn't."
  (string-to-number
   (aven--worktree-git-or-error directory "rev-list" "--count" (concat base "..HEAD"))))

(defun aven--worktree-main-checkout (worktree)
  "The main checkout of the repository WORKTREE belongs to."
  (let ((first (car (split-string
                     (aven--worktree-git-or-error worktree "worktree" "list" "--porcelain")
                     "\n"))))
    (string-remove-prefix "worktree " first)))

(defun aven--worktree-valid-branch-name-p (project-root branch)
  (zerop (car (aven--worktree-git project-root "check-ref-format" "--branch" branch))))

(defun aven--worktree-main-branch (project-root config)
  "PROJECT-ROOT's main branch: the configured `main_branch', else what
the local `origin/HEAD' points at, else `main' or `master'."
  (or (aven--workmux-config-main-branch config)
      (let ((result (aven--worktree-git project-root "symbolic-ref" "--short"
                                        "refs/remotes/origin/HEAD")))
        (and (zerop (car result))
             (string-remove-prefix "origin/" (cdr result))))
      (and (aven--worktree-branch-exists-p project-root "main") "main")
      (and (aven--worktree-branch-exists-p project-root "master") "master")
      (user-error "aven: could not determine the main branch of %s" project-root)))

(defun aven--worktree-base (project-root override)
  "The branch to create a worktree from: OVERRIDE if given, else the
configured `base_branch' (resolving `auto' to the main branch), else
PROJECT-ROOT's current branch."
  (let* ((config (aven--workmux-config project-root))
         (configured (aven--workmux-config-base-branch config)))
    (cond
     (override override)
     ((equal configured "auto") (aven--worktree-main-branch project-root config))
     (configured)
     (t (let ((result (aven--worktree-git project-root "branch" "--show-current")))
          (if (and (zerop (car result)) (not (string-empty-p (cdr result))))
              (cdr result)
            (user-error "aven: %s has no current branch to start from" project-root)))))))

(defun aven--worktree-path (project-root branch)
  "Where the worktree for BRANCH goes: under the configured
`worktree_dir' (supporting `~' and `{project}'), or workmux's default
`<project>__worktrees' next to PROJECT-ROOT, named after BRANCH with
`worktree_prefix' in front."
  (let* ((config (aven--workmux-config project-root))
         (project (file-name-nondirectory (directory-file-name project-root)))
         (configured (aven--workmux-config-worktree-dir config))
         (directory (if configured
                        (expand-file-name
                         (replace-regexp-in-string "{project}" project configured t t)
                         project-root)
                      (expand-file-name (concat project "__worktrees")
                                        (file-name-directory
                                         (directory-file-name project-root))))))
    (expand-file-name (concat (aven--workmux-config-worktree-prefix config)
                              (replace-regexp-in-string "/" "-" branch t t))
                      directory)))

;;; Provisioning

(defun aven--worktree-apply-file-ops (project-root worktree config)
  "Copy and symlink the files matched by CONFIG's `files.copy' and
`files.symlink' globs from PROJECT-ROOT into WORKTREE.  Globs that
match nothing are skipped."
  (let ((default-directory (file-name-as-directory project-root)))
    (dolist (op `((copy . ,(aven--workmux-config-files-copy config))
                  (symlink . ,(aven--workmux-config-files-symlink config))))
      (dolist (pattern (cdr op))
        (dolist (src (file-expand-wildcards pattern))
          (let ((dest (expand-file-name (file-relative-name src project-root) worktree)))
            (make-directory (file-name-directory dest) t)
            (pcase (car op)
              ('copy (if (file-directory-p src)
                         (copy-directory src dest nil t t)
                       (copy-file src dest t)))
              ('symlink (make-symbolic-link (expand-file-name src) dest t)))))))))

(defun aven--worktree-run-hooks (buffer kind project-root worktree commands
                                       &optional environment)
  "Run the shell COMMANDS of the KIND hook (such as \"post_create\")
in WORKTREE one by one, with workmux's hook environment variables and
ENVIRONMENT, a list of \"NAME=VALUE\" strings, set.  Output is
appended to BUFFER.  Signals an error at the first command that fails."
  (let ((default-directory (file-name-as-directory worktree))
        (process-environment
         (append environment
                 (list (concat "WM_HANDLE=" (file-name-nondirectory worktree))
                       (concat "WM_WORKTREE_PATH=" worktree)
                       (concat "WM_PROJECT_ROOT=" (directory-file-name project-root))
                       (concat "WM_CONFIG_DIR=" (directory-file-name project-root)))
                 process-environment)))
    (dolist (command commands)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "$ %s\n" command))))
      (let ((exit-code (call-process-shell-command command nil buffer t)))
        (unless (zerop exit-code)
          (error "aven: %s hook failed (exit %d): %s" kind exit-code command))))))

(defun aven--worktree-logged (header fn)
  "Call FN with the worktree output buffer, after replacing its
contents with HEADER.  When FN fails, the error is added to the buffer,
the buffer is shown, and the error is signalled again."
  (let ((buf (get-buffer-create aven-worktree-output-buffer-name)))
    (with-current-buffer buf
      (aven-worktree-output-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert header "\n\n")))
    (condition-case err
        (funcall fn buf)
      (error
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert "\n" (error-message-string err) "\n")))
       (display-buffer buf)
       (signal (car err) (cdr err))))))

(cl-defun aven--worktree-provision (&key project-root branch base on-created)
  "Create a git worktree for BRANCH in PROJECT-ROOT's repository and
set it up per PROJECT-ROOT's workmux config.  BRANCH is created from
BASE unless it already exists.  ON-CREATED, if non-nil, is called
with the worktree's path as soon as git has created it, before the
file operations and hooks run.  Returns the worktree's path."
  (let ((config (aven--workmux-config project-root))
        (worktree (aven--worktree-path project-root branch)))
    (aven--worktree-logged
     (format "Worktree %s\n  branch: %s\n  base:   %s" worktree branch base)
     (lambda (buf)
       (make-directory (file-name-directory worktree) t)
       (if (aven--worktree-branch-exists-p project-root branch)
           (aven--worktree-git-or-error project-root "worktree" "add" worktree branch)
         (aven--worktree-git-or-error project-root "worktree" "add" "-b" branch worktree base))
       (when on-created
         (funcall on-created worktree))
       (aven--worktree-apply-file-ops project-root worktree config)
       (aven--worktree-run-hooks buf "post_create" project-root worktree
                                 (aven--workmux-config-post-create config))))
    worktree))

(cl-defun aven--worktree-merge (&key worktree branch base rebase)
  "Merge BRANCH, checked out in WORKTREE, into BASE in the main checkout,
after running the `pre_merge' hooks.  By default this makes a merge
commit; with REBASE, BRANCH is rebased onto BASE and fast-forwarded.
Returns the short hash of BASE afterwards.

Nothing changes when the main checkout isn't on BASE or has
uncommitted changes, when a hook fails, or when there are conflicts."
  (let* ((main (aven--worktree-main-checkout worktree))
         (config (aven--workmux-config main)))
    (aven--worktree-logged
     (format "%s %s into %s" (if rebase "Rebase and fast-forward" "Merge") branch base)
     (lambda (buf)
       (let ((current (aven--worktree-git-or-error main "branch" "--show-current")))
         (unless (equal current base)
           (user-error "aven: %s is on %s, not %s; check out %s there first"
                       main current base base)))
       (unless (string-empty-p
                (aven--worktree-git-or-error main "status" "--porcelain" "--untracked-files=no"))
         (user-error "aven: %s has uncommitted changes; commit or stash them first" main))
       (aven--worktree-run-hooks buf "pre_merge" main worktree
                                 (aven--workmux-config-pre-merge config)
                                 (list (concat "WM_BRANCH_NAME=" branch)
                                       (concat "WM_TARGET_BRANCH=" base)))
       (if rebase
           (progn
             (unless (zerop (car (aven--worktree-git worktree "rebase" base)))
               (aven--worktree-git worktree "rebase" "--abort")
               (user-error "aven: rebasing %s onto %s hit conflicts; resolve them in %s"
                           branch base worktree))
             (aven--worktree-git-or-error main "merge" "--ff-only" branch))
         (unless (zerop (car (aven--worktree-git main "merge" "--no-ff" "--no-edit" branch)))
           (aven--worktree-git main "merge" "--abort")
           (user-error "aven: merging %s into %s hit conflicts; resolve them in %s"
                       branch base worktree)))
       (aven--worktree-git-or-error main "rev-parse" "--short" "HEAD")))))

(cl-defun aven--worktree-remove (&key worktree branch delete-branch force)
  "Remove WORKTREE after running the `pre_remove' hooks.  With
DELETE-BRANCH, also delete BRANCH; with FORCE, even if it isn't merged."
  (let* ((main (aven--worktree-main-checkout worktree))
         (config (aven--workmux-config main)))
    (aven--worktree-logged
     (format "Remove %s" worktree)
     (lambda (buf)
       (aven--worktree-run-hooks buf "pre_remove" main worktree
                                 (aven--workmux-config-pre-remove config))
       ;; The caller has checked for uncommitted work; what remains are
       ;; ignored files and agent-shell's own, which git would refuse.
       (aven--worktree-git-or-error main "worktree" "remove" "--force" worktree)
       (when delete-branch
         (aven--worktree-git-or-error main "branch" (if force "-D" "-d") branch))))))

(provide 'aven-worktree)
;;; aven-worktree.el ends here
