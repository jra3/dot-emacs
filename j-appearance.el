;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" system-name))

(setq default-frame-alist
      (quote
       ((left-fringe . 1)
        (right-fringe . 1)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (font . "Menlo-16"))))

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

(defun jallen-font () (interactive) (set-frame-font "Menlo-14"))
(defun jallen-font-no-contacts () (interactive) (set-frame-font "Menlo-18"))
(defun jallen-blind () (interactive) (set-frame-font "Menlo-30"))

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (anzu-mode . "")
    (helm-mode . " ⎈")
    (projectile-mode . " ⍴")
    (flymake-mode . " ⾶")    
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))
 
(provide 'j-appearance)
