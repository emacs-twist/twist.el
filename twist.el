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

(defvar comp-native-version-dir)

;; Declared as constants elsewhere
(defvar twist-current-digest-file)
(defvar twist-running-emacs)

(defcustom twist-emacs-systemd-service "emacs.service"
  "Name of the systemd service running Emacs."
  :type 'string)

(defvar twist-configuration-revision nil
  "Configuration revision of the current package set.")

(defvar twist-new-digest-file-and-revision nil)

;;;###autoload
(defun twist-reload-emacs-service ()
  "Manually trigger reloading the service for package updates."
  (interactive)
  ;; This needs to be run asynchronously, as otherwise it would cause
  ;; deadlock.
  (if (eq system-type 'gnu/linux)
      (twist--systemctl "reload")
    (user-error "Unsupported system thpe")))

;;;###autoload
(defun twist-restart-emacs-service ()
  "Restart the service unit running Emacs.

This command is simply provided as a convenience of the user, and
it is nothing specific to twist."
  (interactive)
  (if (eq system-type 'gnu/linux)
      (if (daemonp)
          ;; This is just not right implemented yet. I'll check this someday.
          (user-error "Cannot self-restart the Emacs daemon")
        (twist--systemctl "restart"))
    (user-error "Unsupported system thpe")))

(defun twist--systemctl (command &rest args)
  "Run a command on the emacs systemd service."
  (message "twist: %s: Signaling %s %s" twist-emacs-systemd-service command (or args ""))
  (apply #'start-process "Twist-Systemd" nil "systemctl" "--user" command
         twist-emacs-systemd-service
         args))

;;;###autoload
(defun twist-push-digest (file &optional revision)
  "Prepare for updating from a digest FILE."
  (unless (or (equal file twist-current-digest-file)
              (equal file (car twist-new-digest-file-and-revision)))
    (setq twist-new-digest-file-and-revision (list file revision))
    (message "twist: Received a new digest for updates.")))

(defun twist-update ()
  "Hot-reload packages from the digest."
  (interactive)
  (if (and twist-new-digest-file-and-revision
           (not (equal (car twist-new-digest-file-and-revision)
                       twist-current-digest-file)))
      (progn
        (message "Updating from digest %s%s"
                 (car twist-new-digest-file-and-revision)
                 (if-let (rev (cadr twist-new-digest-file-and-revision))
                     (format " (revision: %s)" rev)
                   ""))
        (twist--update-from-digest (car twist-new-digest-file-and-revision))
        (setq twist-new-digest-file-and-revision nil
              twist-configuration-revision (cadr twist-new-digest-file-and-revision))
        (garbage-collect)
        (message "twist: Update complete."))
    (user-error "No updates.")))

(defun twist--update-from-digest (digest-file)
  (let* ((current-digest (twist--read-digest-file twist-current-digest-file))
         (new-digest (twist--read-digest-file digest-file))
         (new-native-version (twist--get-native-version
                              (or (alist-get 'emacsPath new-digest)
                                  (error "Missing 'emacsPath"))))
         (eln-compat-p (equal new-native-version comp-native-version-dir)))
    (twist--update-list 'exec-path
                        (alist-get 'executablePackages current-digest)
                        (alist-get 'executablePackages new-digest))
    (when eln-compat-p
      (twist--maybe-swap-item 'native-comp-eln-load-path
                              (alist-get 'nativeLoadPath current-digest)
                              (alist-get 'nativeLoadPath new-digest)))
    (let* ((old-packages (alist-get 'elispPackages current-digest))
           (new-packages (alist-get 'elispPackages new-digest))
           (unloaded-packages (seq-difference old-packages new-packages))
           reloaded-features)
      (require 'loadhist)
      (dolist (hist-ent load-history)
        (when-let (old-package (rassoc (twist--site-lisp-dir (car hist-ent))
                                       unloaded-packages))
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
                            (alist-get 'infoPath current-digest)
                            (alist-get 'infoPath new-digest)
                            'info)
    (setq twist-current-digest-file digest-file)))

(defun twist--read-digest-file (file)
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
