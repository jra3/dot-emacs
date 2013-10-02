;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

(setq tags-case-fold-search nil)

;; spell checking in comments & strings
;;(add-hook ‘prog-mode-hook ‘flyspell-prog-mode)

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

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; make all backups in a single directory
(defvar user-temporary-file-directory "~/tmp/saves/"
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . user-temporary-file-directory))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq vc-make-backup-files t)

(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))

;; =======================================================================

;; mignight mode
(require 'midnight)
(midnight-delay-set 'midnight-delay 16200)

;; use unstupid regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; ---------------- Tweak Variables ---------------------
(setq make-backup-files t        ;; make backup files on save
      diff-switches "-u"         ;; unified diffs
      ring-bell-function 'ignore ;; no bell
      )

(provide 'j-behavior)
