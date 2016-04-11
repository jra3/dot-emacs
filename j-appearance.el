;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

(setq default-frame-alist
      (quote
       ((left-fringe . 1)
        (right-fringe . 1)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (font . "DejaVu Sans Mono-12"))))

;; highlight matcing parens when cursor is on one
(show-paren-mode t)

;; hide n00b UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; (require 'fringe-mode)
;; (setq fringe-mode (quote (1 . 1)) nil (fringe))

;; diminish mode
(require 'diminish)
(defun load-diminish ()
  (diminish 'abbrev-mode "Abv")
  (diminish 'flymake-mode "Fly"))
(add-hook 'after-init-hook 'load-diminish)

;; unique buffer names using path
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; me!
(make-face 'my-name-face)
(set-face-foreground 'my-name-face "#000000")
(set-face-background 'my-name-face "#ffff10")

(add-hook 'font-lock-mode-hook 'highlight-my-name)
(defun highlight-my-name () ""
       (font-lock-add-keywords nil '(("jallen" (0 'my-name-face t)))))

;; Screw that, I kill it on save anyway
(setq-default show-trailing-whitespace nil)

(defun jallen-font () (interactive) (set-frame-font "DejaVu Sans Mono-12"))
(defun jallen-font-no-contacts () (interactive) (set-frame-font "DejaVu Sans Mono-18"))
(defun jallen-blind () (interactive) (set-frame-font "DejaVu Sans Mono-30"))

(provide 'j-appearance)
