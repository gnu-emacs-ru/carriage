;;; carriage-context-file.el --- File reading and caching for context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Carriage contributors
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://gnu-emacs.ru/carriage

;;; Commentary:
;; File reading, normalization and caching for context collection.
;; Extracted from carriage-context.el for modularity.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function carriage-project-root "carriage-utils" ())

(defvar-local carriage-context--root-tru-cache nil
  "Memo table: root directory → truename.")

(defvar-local carriage-context--normalize-cache nil
  "Memo table: (root . path) → (ok . (rel . truename)).")

(defvar-local carriage-context--file-cache nil
  "Cache table: truename → (plist :mtime :size :time :ok :data).")

(defun carriage-context--inside-root-p (truename root)
  "Return non-nil if TRUENAME lies within ROOT.
Assumes TRUENAME is already a truename; avoids re-normalizing it.
Uses a small memo to avoid repeated (file-truename root) calls."
  (let* ((rt (or (gethash root carriage-context--root-tru-cache)
                 (let ((v (file-name-as-directory (file-truename root))))
                   (puthash root v carriage-context--root-tru-cache)
                   v)))
         (pt (file-name-as-directory truename)))
    (string-prefix-p rt pt)))

(defun carriage-context--normalize-path (path root)
  "Normalize PATH relative to ROOT; reject unsafe/TRAMP paths.
Paths outside ROOT are allowed (for context collection only); REL is the absolute truename.
Apply pipeline still enforces project-root boundaries (see `carriage-normalize-path' and spec/security-v2.org).
Return cons (ok . (rel . truename)) or (nil . reason-symbol). Uses memoization."
  (let* ((key (cons root path))
         (hit (and carriage-context--normalize-cache
                   (gethash key carriage-context--normalize-cache))))
    (if hit
        hit
      (let ((res
             (cond
              ((or (null path) (string-empty-p path))
               (cons nil 'empty))
              ((file-remote-p path)
               (cons nil 'remote))
              (t
               (let* ((abs (if (file-name-absolute-p path)
                               path
                             (expand-file-name path root)))
                      (true (ignore-errors (file-truename abs))))
                 (cond
                  ((null true) (cons nil 'unresolvable))
                  (t
                   (let* ((rel (if (carriage-context--inside-root-p true root)
                                   (file-relative-name true root)
                                 true)))
                     (cons t (cons rel true))))))))))
        (puthash key res carriage-context--normalize-cache)
        res))))

(defun carriage-context--read-file-safe (truename)
  "Read file contents from TRUENAME; return (ok . string-or-reason).
Uses a small cache with TTL and invalidation by file size/mtime."
  (let* ((attrs (ignore-errors (file-attributes truename)))
         (mtime (and attrs (nth 5 attrs)))
         (size  (and attrs (nth 7 attrs)))
         (now   (float-time))
         (ttl   (and (boundp 'carriage-context-file-cache-ttl) carriage-context-file-cache-ttl))
         (ce    (and carriage-context--file-cache (gethash truename carriage-context--file-cache)))
         (fresh (and ce
                     (equal (plist-get ce :mtime) mtime)
                     (equal (plist-get ce :size) size)
                     (or (null ttl)
                         (< (- now (or (plist-get ce :time) 0)) (or ttl 0))))))
    (if fresh
        (let ((ok (plist-get ce :ok))
              (data (plist-get ce :data)))
          (if ok (cons t data) (cons nil data)))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents truename)
            (let ((s (buffer-substring-no-properties (point-min) (point-max))))
              (let* ((ok (not (string-match-p "\0" s)))
                     (data (if ok s 'binary)))
                (puthash truename
                         (list :mtime mtime :size size :time (float-time)
                               :ok ok :data data)
                         carriage-context--file-cache)
                 (if ok (cons t s) (cons nil 'binary)))))
        (error
         (let ((reason (format "read-error:%s" (error-message-string err))))
           (puthash truename
                    (list :mtime mtime :size size :time (float-time)
                          :ok nil :data reason)
                    carriage-context--file-cache)
           (cons nil reason)))))))

(defun carriage-context--secret-path-p (rel tru)
  "Return non-nil when REL/TRU look like a secret-like path (denylist match)."
  (let ((rxs (and (boundp 'carriage-context-secret-path-regexps)
                  carriage-context-secret-path-regexps))
        (case-fold-search t)
        (r (or rel ""))
        (t0 (or tru "")))
    (and (listp rxs)
         (cl-some (lambda (rx)
                    (and (stringp rx) (not (string-empty-p rx))
                         (or (and (stringp r) (string-match-p rx r))
                             (and (stringp t0) (string-match-p rx t0)))))
                  rxs))))

(provide 'carriage-context-file)
;;; carriage-context-file.el ends here
