;;; package --- jallen's .emacs file
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;  he's so dreamy

;;; Code:

;(package-initialize) We'll do this in config.org

(defvar my-start-time (current-time) "Time when Emacs was started")

(defvar config-load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'package)
(setq package-archives nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-when-compile
  (require 'use-package)
					; install packages from package archive automatically
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package auto-package-update
	     :config
	     (setq auto-package-update-delete-old-versions t)
	     (setq auto-package-update-hide-results t)
	     (auto-package-update-at-time "03:00"))


(defvar config-org-files
  '("local-before.org" ;; local preamble, .gitignored
    "config.org"
    "local-after.org" ;; local coda, .gitignored
    ))

;; org, it's the best
(require 'org)

(mapc
 (lambda (file)
   (let ((org-file (concat config-load-path file)))
     (if (file-exists-p org-file)
         (org-babel-load-file org-file))))
 config-org-files)

(message "Start up time %.2fs" (float-time (time-subtract (current-time) my-start-time)))

;;; init.el ends here
