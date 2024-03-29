;;; twist.el --- Hot-reload packages with twist -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp
;; URL: https://github.com/emacs-twist/twist.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a companion package to twist.nix.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'filenotify)

(defvar comp-native-version-dir)

;; Declared as constants elsewhere
(defvar twist-current-manifest-file)
(defvar twist-running-emacs)

(defgroup twist nil
  "Nix-based package manager for Emacs."
  :group 'nix)

(defcustom twist-manifest-file (locate-user-emacs-file "twist-manifest.json")
  "Path to the state file tracking the state of the package set."
  :group 'twist
  :set (lambda (sym val)
         (set sym val)
         (when (bound-and-true-p twist-watch-mode)
           (twist--change-manifest-file val)))
  :type 'file)

(defvar twist-configuration-revision nil
  "Configuration revision of the current package set.")

(defvar twist-configuration-file-watch nil)

;;;; Updating from the primary manifest

;;;###autoload
(define-minor-mode twist-watch-mode
  "Global minor mode which supports notification of package updates."
  :global t
  (when twist-watch-mode
    (if (file-readable-p twist-manifest-file)
        (progn
          (twist--change-manifest-file)
          (when twist-configuration-file-watch
            (message "Started watching %s for package updates"
                     twist-manifest-file)))
      (message "Trying to turn on twist-watch-mode, but twist-manifest-file \
does not exist. Please check the variable and enable the mode again.")
      (setq twist-watch-mode nil))))

(defun twist--change-manifest-file (&optional file)
  (when twist-configuration-file-watch
    (file-notify-rm-watch twist-configuration-file-watch)
    (setq twist-configuration-file-watch nil))
  (when twist-watch-mode
    (setq twist-configuration-file-watch
          (file-notify-add-watch (or file twist-manifest-file)
                                 '(change) #'twist--handle-manifest-change))))

(defun twist--handle-manifest-change (_event)
  (when (twist--manifest-file-changed-p)
    (message (substitute-command-keys
              "twist: The package set has been changed. \
Run \\[twist-update] to start updating"
              'no-face))))

(defun twist--manifest-file-changed-p ()
  (and (file-readable-p twist-manifest-file)
       (not (equal (and twist-current-manifest-file
                        (file-exists-p twist-current-manifest-file)
                        (file-truename twist-current-manifest-file))
                   (file-truename twist-manifest-file)))
       (twist--json-differ-partially-p
        twist-manifest-file
        twist-current-manifest-file
        '("elispPackages"
          "infoPath"
          "nativeLoadPath"
          "executablePackages"))))

(defun twist--json-differ-partially-p (file1 file2 keys)
  (let ((obj1 (with-temp-buffer
                (insert-file-contents file1)
                (json-parse-buffer :object-type 'hash-table)))
        (obj2 (with-temp-buffer
                (insert-file-contents file2)
                (json-parse-buffer :object-type 'hash-table))))
    (catch 'package-set-differ
      (dolist (key keys)
        (unless (equal (gethash key obj1)
                       (gethash key obj2))
          (throw 'package-set-differ t))))))

;;;###autoload
(defun twist-update ()
  "Hot-reload packages from the new state."
  (interactive)
  (if (twist--manifest-file-changed-p)
      (progn
        (message "twist: Updating from file %s" twist-manifest-file)
        (unwind-protect
            (if (twist--update-from-file twist-manifest-file)
                (message "twist: Update complete.")
              (user-error "twist: Emacs has been upgraded. Please restart Emacs"))
          (garbage-collect)))
    (user-error "twist: No updates.")))

(defun twist--update-from-file (manifest-file)
  (let ((current-manifest (twist--read-manifest-file twist-current-manifest-file))
        (new-manifest (twist--read-manifest-file manifest-file)))
    ;; Return nil from this function if Emacs has been updated
    (when (equal (or (alist-get 'emacsPath current-manifest)
                     (error "Missing emacsPath from the current manifest"))
                 (or (alist-get 'emacsPath new-manifest)
                     (error "Missing emacsPath from the new manifest")))
      (twist--update-list 'exec-path
                          (alist-get 'executablePackages current-manifest)
                          (alist-get 'executablePackages new-manifest))
      (twist--maybe-swap-item 'native-comp-eln-load-path
                              (alist-get 'nativeLoadPath current-manifest)
                              (alist-get 'nativeLoadPath new-manifest))
      (let* ((old-packages (alist-get 'elispPackages current-manifest))
             (new-packages (alist-get 'elispPackages new-manifest))
             (unloaded-packages (seq-difference old-packages new-packages))
             reloaded-features)
        (require 'loadhist)
        (dolist (hist-ent load-history)
          (when-let* ((file (car-safe hist-ent))
                      (old-package (rassoc (twist--site-lisp-dir file)
                                           unloaded-packages)))
            (when (assq (car old-package) new-packages)
              (dolist (x (cdr hist-ent))
                (when (eq (car-safe x) 'provide)
                  (message "twist: Found a feature to reload: %s" (cdr x))
                  (push (cdr x) reloaded-features))))))
        (dolist (old-package unloaded-packages)
          (delete (cdr old-package) load-path)
          (message "twist: Removed an old package from load-path: %s"
                   (car old-package)))
        (dolist (new-package (seq-difference new-packages old-packages))
          (push (cdr new-package) load-path)
          (message "twist: Added a new package to load-path: %s"
                   (car new-package))
          (load (format "%s-autoloads" (car new-package))
                ;; autoloads may not exist
                'noerror))
        (when reloaded-features
          (message "twist: Reloading updated packages...")
          (dolist (file (mapcar #'symbol-name reloaded-features))
            ;; autoloads have been already loaded unconditionally
            (unless (string-suffix-p "-autoloads" file)
              (unless (load file 'noerror)
                (message "twist: Failed to load %s" file))))))
      (twist--maybe-swap-item 'Info-directory-list
                              (alist-get 'infoPath current-manifest)
                              (alist-get 'infoPath new-manifest)
                              'info)
      (setq twist-current-manifest-file (file-truename manifest-file)
            twist-configuration-revision (alist-get 'configurationRevision new-manifest))
      ;; Ensure non-nil is returned
      t)))

;;;; Partial loading from a secondary manifest

;;;###autoload
(defun twist-add-all-executable-packages (file)
  "Add the directories of all executable packages from FILE to `exec-path'."
  (interactive "fManifest file: ")
  (let ((manifest (twist--read-manifest-file file)))
    (twist--add-to-exec-path (seq-into (cdr (assq 'executablePackages manifest))
                                       'list))))

;;;###autoload
(defun twist-add-executable-packages (file)
  "Add the directories of some executable packages from FILE to `exec-path'."
  (interactive "fManifest file: ")
  (let* ((manifest (twist--read-manifest-file file))
         (dirs (completing-read-multiple "Add directories to exec-path: "
                                         (seq-into (cdr (assq 'executablePackages manifest))
                                                   'list)
                                         nil t)))
    (twist--add-to-exec-path (ensure-list dirs))))

(defun twist--add-to-exec-path (dirs)
  (let (added
        (global-exec-path (symbol-value 'exec-path))
        (global-path (getenv "PATH")))
    (dolist (dir dirs)
      (unless (member dir global-exec-path)
        (push dir (symbol-value 'exec-path))
        (push dir added)))
    (dolist (dir dirs)
      (unless (string-match-p (rx-to-string `(and (or bol ":") ,dir ":"))
                              global-path)
        (setq global-path (concat dir ":" global-path))))
    (setenv "PATH global-path")
    (if added
        (message "Added to exec-path %d directories (%s)"
                 (length added)
                 (mapcan (lambda (dir)
                           (thread-last
                             (directory-files dir)
                             (seq-filter #'file-executable-p)
                             (mapcar #'file-name-base)))
                         added))
      (message "No directory has been added to exec-path"))))

;;;; Utility functions

(defun twist--read-manifest-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :array-type 'array :object-type 'alist)))

(defun twist--site-lisp-dir (file)
  (when (string-match (rx bos "/nix/store/" (+ anything)
                          "/share/emacs/site-lisp/")
                      file)
    (match-string 0 file)))

(defun twist--update-list (symbol old new)
  (let ((removed (seq-difference old new #'equal))
        (added (seq-difference new old #'equal)))
    (dolist (x removed)
      (set-default symbol (delete x (default-value symbol)))
      (message "twist: Deleted from %s: %s" symbol x))
    (dolist (x added)
      (set-default symbol (cl-adjoin x (default-value symbol) :test #'equal))
      (message "twist: Added to %s: %s" symbol x))))

(defun twist--maybe-swap-item (symbol old new &optional feature)
  (unless (equal old new)
    (when feature
      (require feature))
    (let ((place (symbol-value symbol)))
      (when old
        (delete old place)
        (message "twist: Deleted from %s: %s" symbol old))
      (when new
        (cl-pushnew new place)
        (message "twist: Added to %s: %s" symbol new)))))

(provide 'twist)
;;; twist.el ends here
