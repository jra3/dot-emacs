;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-case-fold-search nil)

;; mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

(setq scroll-step 1
      scroll-conservatively 10000)

;; Set +x on scripts stating with a shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; rectangular sections - C-<Enter>
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; make all backups in a single directory
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; =======================================================================

;; run garbage collect when idle for a long time
(run-with-idle-timer
 14400 t (lambda ()
          (shell-command "cd /home/jallen/www && git gc")))

;; mignight mode
(require 'midnight)
(midnight-delay-set 'midnight-delay 16200)

;; ---------------- Tweak Variables ---------------------
(setq make-backup-files t        ;; make backup files on save
      diff-switches "-u"         ;; unified diffs
      ring-bell-function 'ignore ;; no bell
      )

(provide 'j-behavior)
