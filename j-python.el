(require 'diminish)

(eval-after-load 'elpy
  '(progn
     (define-key elpy-mode-map (kbd "C-c C-f") 'projectile-find-file)  ;; unshadow projectile-find-file
     (define-key elpy-mode-map (kbd "<C-down>") 'python-nav-forward-block)
     (define-key elpy-mode-map (kbd "<C-up>") 'python-nav-backward-block)
     (define-key elpy-mode-map (kbd "<C-left>") nil)
     (define-key elpy-mode-map (kbd "<C-right>") nil)
))

(elpy-enable)
(setq elpy-rpc-backend "jedi")

;; Fixing a key binding bug in elpy
;;;;;;(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(provide 'j-python)
