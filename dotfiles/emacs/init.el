;;; -*- lexical-binding: t; -*-
;;; init.el
;;; Commentary:
;;; Code:

;; To create both core and config configuration directory.
(defvar my-emacs-core-dir (expand-file-name "core" user-emacs-directory))
(defvar my-emacs-config-dir (expand-file-name "config" user-emacs-directory))

;; Ensure both 'core' and 'config' directory exists.
(dolist (dir (list my-emacs-core-dir my-emacs-config-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))  ;; 't' is ensure creating the father directory

(add-to-list 'load-path my-emacs-core-dir)
(add-to-list 'load-path my-emacs-config-dir)


;; load core configuration
(require 'init-packages)      ;; packages manager
(require 'init-defaults)
(require 'init-encoding)
(require 'init-appearance)     ;; appearance configure
(require 'init-dashboard)
(use-package diminish)
;; load config configuration
;;(require 'config-projects)
(require 'config-completion)
(require 'config-org)
(require 'config-evil)
(require 'config-programming)
(require 'config-cc)
(require 'config-cuda)
(require 'config-ssh)
;; show startup time
(defun start/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'start/display-startup-time)
