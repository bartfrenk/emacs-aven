;;; aven-data.el --- Data access for aven.el -*- lexical-binding: t; -*-

;; Author: Bart Frenk <bart.frenk@gmail.com>
;; Keywords: tools

;;; Commentary:

;; Functions that shell out to the `aven' CLI and return parsed data
;; (JSON or raw text), used by the task buffer, the status buffer, and
;; the quick field-edit commands.

;;; Code:

(require 'json)
(require 'aven-core)

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

(defun aven--task-full-json (ref)
  "Full JSON detail for REF including metadata and notes, as a plist
with the task itself under :task and its metadata under :metadata."
  (with-temp-buffer
    (let ((exit-code (call-process aven--executable nil t nil "show" ref "--full" "--json")))
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

(defun aven--task-field (ref field)
  "REF's FIELD from `aven show', or nil if it is absent or empty."
  (let ((value (plist-get (aven--task-json ref) field)))
    (unless (or (null value) (and (stringp value) (string-empty-p value)))
      value)))

(defun aven--project-keys ()
  "Keys of all existing projects in the active workspace."
  (with-temp-buffer
    (when (zerop (call-process aven--executable nil t nil "project" "list" "--json"))
      (let ((json-array-type 'list)
            (json-object-type 'plist)
            (json-key-type 'keyword))
        (mapcar (lambda (project) (plist-get project :key))
                (json-read-from-string (buffer-string)))))))

(defun aven--project-paths (project)
  "Directories associated with PROJECT through `aven project path add'."
  (with-temp-buffer
    (when (zerop (call-process aven--executable nil t nil
                               "project" "path" "list" project))
      (let (paths)
        (goto-char (point-min))
        (while (re-search-forward "path=\"\\([^\"]*\\)\"" nil t)
          (push (match-string 1) paths))
        (nreverse paths)))))

(defun aven--label-names ()
  "Names of all existing labels in the active workspace."
  (with-temp-buffer
    (when (zerop (call-process aven--executable nil t nil "label" "list" "--json"))
      (let ((json-array-type 'list))
        (json-read-from-string (buffer-string))))))

(provide 'aven-data)
;;; aven-data.el ends here
