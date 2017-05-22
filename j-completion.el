(require 'company)
(setq company-backends
      (quote
       (
        ;; company-hh-complete
	company-rtags
        company-nxml
        company-css
        company-xcode
        company-cmake
        company-capf
        company-files
        company-dabbrev
	)
       )
      )

(require 'rtags)

(defvar rtags-autostart-diagnostics)
(defvar rtags-completions-enabled)
(defvar company-backends)
(defvar c-mode-base-map)
(defvar rtags-use-helm)

(require 'helm-rtags)
(rtags-enable-standard-keybindings)

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)

;; company integration
(require 'company)
(push 'company-rtags company-backends)
(global-company-mode)
(delete 'company-backends 'company-clang)

;; ;; nice keyboard shortcuts
;; (define-key c-mode-base-map (kbd "<M-tab>")
;;   (function company-complete))
;; (define-key c-mode-base-map (kbd "M-.")
;;   (function rtags-find-symbol-at-point))
;; (define-key c-mode-base-map (kbd "M-,")
;;   (function rtags-find-references-at-point))

;; (define-key c-mode-base-map (kbd "<s-right>")
;;   (function rtags-location-stack-forward))
;; (define-key c-mode-base-map (kbd "<s-left>")
;;   (function rtags-location-stack-back))

(when (require 'helm nil :noerror)
  (setq rtags-use-helm t)
  )

;; ;; flycheck integration
;; (require 'flycheck)

;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;   "Flycheck integration."
;;   (interactive)
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)

;;   (setq rtags-enable-unsaved-reparsing nil)
;;   (setq-local flycheck-check-syntax-automatically t)
;;   (setq-local rtags-periodic-reparse-timeout 1)
;;   )

;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)


(setq rtags-path (expand-file-name "~/local/rtags-install/bin"))
(rtags-enable-standard-keybindings)

(setq rtags-rc-log-enabled t)
(setq rtags-completions-enabled t)
(setq rtags-use-helm t)

(require 'yasnippet)
;; (yas-installed-snippets-dir
;; (setq yas-snippet-dirs (quote ("~/.dot-emacs/snippets")) nil (yasnippet))

(provide 'j-completion)
