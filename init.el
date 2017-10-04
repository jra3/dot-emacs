;;; package --- jallen's .emacs file
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;  he's so dreamy

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar my-start-time (current-time) "Time when Emacs was started")
(defvar config-load-path (file-name-directory (or load-file-name buffer-file-name)))
(defvar config-org-files '("config.org"))

(if (file-exists-p "~/.emacs.d/local.el")
    (load "~/.emacs.d/local.el"))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp")
           (default-directory my-lisp-dir))
      (add-to-list 'load-path my-lisp-dir)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'org)
(dolist (file config-org-files)
  (org-babel-load-file (concat config-load-path file)))

(require 'j-dired)
(require 'j-terminal-compat)
(require 'j-util)
(require 'j-globalkeys)
(require 'j-behavior)
(require 'j-org)

(require 'j-go)
(require 'j-cpp)
(require 'j-php)
(require 'j-python)
(require 'j-js)
(require 'j-html)
(require 'j-thrift)
(require 'j-markdown)

(require 'j-completion)
(require 'j-web)

(require 'flow)

(require 'j-helm)
(require 'helm-myles)


(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))

;;; init.el ends here
