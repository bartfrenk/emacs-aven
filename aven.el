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
;;
;; The implementation is split across several `aven-*' files, loaded
;; below: aven-core (shared primitives), aven-data (CLI data access),
;; aven-process (running commands and the output buffer), aven-task
;; (the per-task buffer), aven-status (the status buffer),
;; aven-description (description editing), and aven-transient (all
;; transient commands).

;;; Code:

;; Loading this file directly (e.g. via `load-file') does not put its
;; own directory on `load-path', so the `require's below would fail
;; to find the submodules; add it explicitly first.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))

(require 'aven-core)
(require 'aven-data)
(require 'aven-process)
(require 'aven-task)
(require 'aven-status)
(require 'aven-description)
(require 'aven-transient)

(provide 'aven)
;;; aven.el ends here
