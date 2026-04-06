;;; carriage-context-map.el --- Project map generation for context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; Project map (begin_map) generation via git ls-files, fd, or Emacs directory walk.
;; Extracted from carriage-context.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-context--dbg "carriage-context" (&rest args))
(declare-function carriage-project-root "carriage-utils" ())

(defcustom carriage-context-project-map-cache-ttl 2.0
  "TTL in seconds for cached Project Map (begin_map) generation."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-git-timeout-seconds 1.5
  "Timeout in seconds for Project Map's git-based file listing."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-fd-timeout-seconds 1.5
  "Timeout in seconds for Project Map's `fd` fallback file listing."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-emacs-walk-max-seconds 0.8
  "Soft time budget in seconds for the Emacs directory-walk fallback."
  :type 'number
  :group 'carriage-context)

(defcustom carriage-context-project-map-skip-dirs
  '(".git" ".hg" ".svn" "node_modules" ".venv" "venv" "__pycache__" "dist" "build" "target")
  "Directory base-names to skip in the Emacs directory-walk fallback."
  :type '(repeat string)
  :group 'carriage-context)

(defcustom carriage-context-project-map-max-bytes 200000
  "Maximum number of bytes allowed for the rendered Project Map block."
  :type 'integer
  :group 'carriage-context)

(defcustom carriage-context-project-map-max-paths 10000
  "Maximum number of repo-relative paths included in the Project Map."
  :type 'integer
  :group 'carriage-context)

(defvar carriage-context--project-map-cache (make-hash-table :test 'equal)
  "Cache mapping project root → Project Map record plist.")

(defvar-local carriage-context--project-map-warm-timer nil
  "Idle timer for warming Project Map cache.")

(defun carriage-context--project-map--cache-get (root)
  "Return cached Project Map record for ROOT, or nil."
  (when (and (hash-table-p carriage-context--project-map-cache)
             (stringp root) (not (string-empty-p root)))
    (gethash root carriage-context--project-map-cache)))

(defun carriage-context--project-map--cache-put (root rec)
  "Store Project Map REC for ROOT in cache."
  (when (and (hash-table-p carriage-context--project-map-cache)
             (stringp root) (not (string-empty-p root))
             (listp rec))
    (puthash root rec carriage-context--project-map-cache)))

(defun carriage-context--project-map--cache-fresh-p (rec ttl)
  "Return non-nil when REC cache entry is fresh per TTL seconds."
  (let* ((t0 (and (listp rec) (plist-get rec :time)))
         (now (float-time)))
    (and (numberp t0)
         (numberp ttl)
         (>= ttl 0.0)
         (< (- now t0) ttl))))

(defun carriage-context-project-map-invalidate (&optional root)
  "Invalidate Project Map cache for ROOT (or all roots when nil)."
  (if (null root)
      (when (hash-table-p carriage-context--project-map-cache)
        (clrhash carriage-context--project-map-cache))
    (when (and (hash-table-p carriage-context--project-map-cache)
               (stringp root))
      (remhash root carriage-context--project-map-cache))))

(defun carriage-context--project-map--node-make ()
  "Create a new project-map trie node."
  (list :dirs (make-hash-table :test 'equal) :files (make-hash-table :test 'equal)))

(defun carriage-context--project-map--node-valid-p (node)
  "Return non-nil when NODE is a valid project-map trie node."
  (and (listp node)
       (hash-table-p (plist-get node :dirs))
       (hash-table-p (plist-get node :files))))

(defun carriage-context--project-map--node-ensure (node)
  "Ensure NODE is a valid project-map trie node."
  (let ((n (if (listp node) node (carriage-context--project-map--node-make))))
    (unless (carriage-context--project-map--node-valid-p n)
      (setq n (carriage-context--project-map--node-make)))
    n))

(defun carriage-context--project-map--node-dirs (node)
  "Get :dirs hash-table from NODE."
  (plist-get (carriage-context--project-map--node-ensure node) :dirs))

(defun carriage-context--project-map--node-files (node)
  "Get :files hash-table from NODE."
  (plist-get (carriage-context--project-map--node-ensure node) :files))

(defun carriage-context--project-map--insert (root-node relpath)
  "Insert RELPATH into project map trie ROOT-NODE."
  (let* ((parts (split-string relpath "/" t))
         (node (carriage-context--project-map--node-ensure root-node))
         (n (1- (length parts))))
    (dotimes (i n)
      (let* ((part (nth i parts))
             (dirs (carriage-context--project-map--node-dirs node))
             (child (or (gethash part dirs) (carriage-context--project-map--node-make))))
        (puthash part (carriage-context--project-map--node-ensure child) dirs)
        (setq node (carriage-context--project-map--node-ensure child))))
    (let* ((filename (nth n parts))
           (files (carriage-context--project-map--node-files node)))
      (puthash filename t files))))

(defun carriage-context--project-map--sorted-keys (h)
  "Return sorted list of keys from hash-table H."
  (sort (hash-table-keys h) #'string<))

(defun carriage-context--project-map--render (node depth)
  "Render project map NODE as a tree string with given DEPTH."
  (let ((acc '())
        (dirs (carriage-context--project-map--node-dirs node))
        (files (carriage-context--project-map--node-files node))
        (dirnames (carriage-context--project-map--sorted-keys dirs))
        (filenames (carriage-context--project-map--sorted-keys files)))
    (dolist (f filenames)
      (push (format "%s%s" (make-string (* depth 2) ? ) f) acc))
    (dolist (d dirnames)
      (push (format "%s%s/" (make-string (* depth 2) ? ) d) acc)
      (let ((child (gethash d dirs)))
        (when child
          (setq acc (nconc (nreverse (carriage-context--project-map--render child (1+ depth))) acc)))))
    (nreverse acc)))

(defun carriage-context--project-map--call (default-dir program args timeout)
  "Call PROGRAM with ARGS in DEFAULT-DIR, return output string or nil on timeout/error."
  (let ((ok nil)
        (out nil))
    (condition-case err
        (progn
          (setq out
                (shell-command-to-string
                 (mapconcat #'shell-quote-argument
                            (cons program args) " ")))
          (setq ok t))
      (error
       (carriage-context--dbg "project-map: %s failed: %s" program (error-message-string err))))
    (when ok (string-trim out)))

(defun carriage-context--project-map--skip-dir-p (name)
  "Return non-nil when NAME should be skipped in directory walk."
  (member name (or (and (boundp 'carriage-context-project-map-skip-dirs)
                        carriage-context-project-map-skip-dirs)
                   '())))

(provide 'carriage-context-map)
;;; carriage-context-map.el ends here
