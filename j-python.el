(require 'diminish)

(elpy-enable)
(elpy-clean-modeline)
(elpy-use-ipython)

(setq elpy-rpc-backend "jedi")
(setq python-check-command "flake8")

;; (define-key ac-completing-map (kbd "<up>") nil)
;; (define-key ac-completing-map (kbd "<down>") nil)
;; (define-key ac-completing-map (kbd "RET") nil)
;; (define-key ac-completing-map (kbd "<return>") nil)

;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(provide 'j-python)
