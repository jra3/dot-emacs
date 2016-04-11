(require 'highlight-80+)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; (ido-mode t)
;; (ido-everywhere t)
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode t)
;; (require 'magit)
;; (setq magit-completing-read-function 'magit-ido-completing-read)
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode t)
;; (setq ido-vertical-show-count t)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; (require 'flx-ido)
;; (flx-ido-mode t)
;; (setq ido-enable-flex-matching t)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)

(setq tags-case-fold-search nil)

;; spell checking in comments & strings
;;(add-hook ‘prog-mode-hook ‘flyspell-prog-mode)

;; mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

(setq scroll-step 1
      scroll-conservatively 10000
      
      split-height-threshold 10000
      split-width-threshold 10000

      large-file-warning-threshold nil
      
      case-fold-search t
      transient-mark-mode t

      use-dialog-box nil
      use-file-dialog nil
      line-number-mode 1
      
      fill-column 78
      highlight-80+-columns 119
      
      indent-tabs-mode nil

      current-language-environment "utf-8"
      default-input-method "utf-8-prefix"
      c-basic-offset 2 ;; this is used by so much, don't put it in j-cpp
      global-auto-revert-mode t
      compilation-ask-about-save nil
      display-buffer-reuse-frames nil
      ediff-highlight-all-diffs nil
      
      flymake-no-changes-timeout 1

      alert-user-configuration (quote ((nil notifier nil)))
      )

(setq popwin:special-display-config
      (quote
       (("*Ibuffer*" :position top :noselect t :height 30)
        ("*Python Check*" :position top :noselect t :height 30)
        ("*magit-log*")
        ("*compilation*")
        ("*Python Doc*")
        ("*grep*")
        ("*Help*")
        ("*Completions*" :noselect t)
        ("*Occur*" :noselect t))))



;; Set +x on scripts stating with a shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; (add-hook 'before-save-hook 'whitespace-cleanup)

;; rectangular sections - C-<Enter>
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; show number of matches
(global-anzu-mode +1)

;; Awesome repo navigation
(projectile-global-mode)
(setq projectile-use-git-grep t)

(require 'yasnippet)
(define-key yas-minor-mode-map (kbd "<shift TAB>") 'yas-expand)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" "~/tmp/saves"))

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-other-window)

;; make all backups in a single directory
(defvar user-temporary-file-directory "~/tmp/saves/"
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)


(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

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
