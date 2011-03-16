;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; (fringe-mode '(0 . right-only))
;; (define-fringe-bitmap 'bottom-right-angle [0] nil)
;; (define-fringe-bitmap 'right-bracket [0] nil)
;; (define-fringe-bitmap 'top-left-angle [0] nil)
;; (define-fringe-bitmap 'top-right-angle [0] nil)

;; hide n00b UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; diminish mode
(require 'diminish)
(defun load-diminish ()
  (diminish 'abbrev-mode "Abv")
  (diminish 'flymake-mode "Fly"))
(add-hook 'after-init-hook 'load-diminish)

;; me!
(make-face 'my-name-face)
(set-face-foreground 'my-name-face "#000000")
(set-face-background 'my-name-face "#ffff10")

(add-hook 'font-lock-mode-hook 'highlight-my-name)
(defun highlight-my-name () ""
  (font-lock-add-keywords nil '(("jallen" (0 'my-name-face t)))))

;; Screw that, I kill it on save anyway
(setq-default show-trailing-whitespace nil)

(defun jallen-font () (interactive) (set-default-font "DejaVu Sans Mono-10"))
(add-hook 'after-make-window-system-frame-hooks 'jallen-font)

(provide 'j-appearance)