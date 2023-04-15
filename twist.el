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
(defvar twist-current-state-file)
(defvar twist-running-emacs)

(defgroup twist nil
  "Nix-based package manager for Emacs."
  :group 'nix)

(defcustom twist-state-file (locate-user-emacs-file "twist.json")
  "Path to the state file tracking the state of the package set."
  :group 'twist
  :set (lambda (sym val)
         (set sym val)
         (when (bound-and-true-p twist-watch-mode)
           (twist--change-state-file val)))
  :type 'file)

(defvar twist-configuration-revision nil
  "Configuration revision of the current package set.")

(defvar twist-configuration-file-watch nil)

;;;###autoload
(define-minor-mode twist-watch-mode
  "Global minor mode which supports notification of package updates."
  :global t
  (when twist-watch-mode
    (twist--change-state-file)
    (when twist-configuration-file-watch
      (message "Started watching %s for package updates" twist-state-file))))

(defun twist--change-state-file (&optional file)
  (when twist-configuration-file-watch
    (file-notify-rm-watch twist-configuration-file-watch)
    (setq twist-configuration-file-watch nil))
  (when twist-watch-mode
    (setq twist-configuration-file-watch
          (file-notify-add-watch (or file twist-state-file)
                                 '(change) #'twist--handle-state-change))))

(defun twist--handle-state-change (_event)
  (when (twist--state-file-changed-p)
    (message (substitute-command-keys
              "twist: The package set has been changed. \
Run \\[twist-update] to start updating"
              'no-face))))

(defun twist--state-file-changed-p ()
  (and (file-readable-p twist-state-file)
       (not (equal (and twist-current-state-file
                        (file-exists-p twist-current-state-file)
                        (file-truename twist-current-state-file))
                   (file-truename  twist-state-file)))))

(defun twist-update ()
  "Hot-reload packages from the new state."
  (interactive)
  (if (twist--state-file-changed-p)
      (progn
        (message "Updating from file %s" twist-state-file)
        (twist--update-from-file twist-state-file)
        (garbage-collect)
        (message "twist: Update complete."))
    (user-error "No updates.")))

(defun twist--update-from-file (state-file)
  (let* ((current-state (twist--read-state-file twist-current-state-file))
         (new-state (twist--read-state-file state-file))
         (new-native-version (twist--get-native-version
                              (or (alist-get 'emacsPath new-state)
                                  (error "Missing 'emacsPath"))))
         (eln-compat-p (equal new-native-version comp-native-version-dir)))
    (twist--update-list 'exec-path
                        (alist-get 'executablePackages current-state)
                        (alist-get 'executablePackages new-state))
    (when eln-compat-p
      (twist--maybe-swap-item 'native-comp-eln-load-path
                              (alist-get 'nativeLoadPath current-state)
                              (alist-get 'nativeLoadPath new-state)))
    (let* ((old-packages (alist-get 'elispPackages current-state))
           (new-packages (alist-get 'elispPackages new-state))
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
        (load (format "%s-autoloads.el" new-package)
              ;; autoloads may not exist
              'noerror))
      (message "twist: Reloading updated packages...")
      (dolist (file (mapcar #'symbol-name reloaded-features))
        ;; autoloads have been already loaded unconditionally
        (unless (string-suffix-p "-autoloads" file)
          (load file))))
    (twist--maybe-swap-item 'Info-directory-list
                            (alist-get 'infoPath current-state)
                            (alist-get 'infoPath new-state)
                            'info)
    (setq twist-current-state-file (file-truename state-file)
          twist-configuration-revision (alist-get 'configurationRevision new-state))))

(defun twist--read-state-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :array-type 'array :object-type 'alist)))

;;;; Utility functions

(defun twist--site-lisp-dir (file)
  (when (string-match (rx bos "/nix/store/" (+ anything)
                          "/share/emacs/site-lisp/")
                      file)
    (match-string 0 file)))

(defun twist--update-list (symbol old new)
  (let ((removed (seq-difference old new #'equal))
        (added (seq-difference new old #'equal))
        (place (symbol-value symbol)))
    (dolist (x removed)
      (delete x place)
      (message "twist: Deleted from %s: %s" symbol x))
    (dolist (x added)
      (add-to-list symbol x)
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

(defun twist--get-native-version (dir)
  (thread-last
    (expand-file-name "share/emacs/native-lisp" dir)
    (directory-files)
    (cl-remove-if (lambda (name)
                    (string-prefix-p "." name)))
    (car)))

(provide 'twist)
;;; twist.el ends here
