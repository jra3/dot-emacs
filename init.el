;;; package --- jallen's .emacs file
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;  he's so dreamy

;;; Code:

;(package-initialize) We'll do this in config.org

(defvar my-start-time (current-time) "Time when Emacs was started")

(defvar config-load-path (file-name-directory (or load-file-name buffer-file-name)))

(defvar config-org-files
  '("local-bootstrap.org" ;; local preamble, .gitignored
    "package-bootstrap.org"
    "local-before.org" ;; local init, .gitignored
    "config.org"
    "local-after.org" ;; local coda, .gitignored
    ))

(require 'package)
(package-initialize)
(require 'org)

(mapc
 (lambda (file)
   (let ((org-file (concat config-load-path file)))
     (if (file-exists-p org-file)
         (org-babel-load-file org-file))))
 config-org-files)

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))
(put 'narrow-to-region 'disabled nil)
