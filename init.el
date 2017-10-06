;;; package --- jallen's .emacs file
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;  he's so dreamy

;;; Code:

(defvar my-start-time (current-time) "Time when Emacs was started")
(defvar config-load-path (file-name-directory (or load-file-name buffer-file-name)))
(defvar config-org-files '("config.org"))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Local customizations are loaded super-early to allow us to set
;; proxies so that we can fetch packages
(let ((local (concat config-load-path "local.el")))
  (if (file-exists-p local)
      (load local)))

;; The bulk of my config lives in these org files
(require 'org)
(dolist (file config-org-files)
  (org-babel-load-file (concat config-load-path file)))

;; TODO move dired to config.org
;; TODO move helm-myles to the extra dir
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp")
           (default-directory my-lisp-dir))
      (add-to-list 'load-path my-lisp-dir)
      (normal-top-level-add-subdirs-to-load-path)))
(require 'j-dired)
(require 'helm-myles)

(use-package load-dir
  :config (setq load-dirs (concat config-load-path "extra/")))

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))

;;; init.el ends here
