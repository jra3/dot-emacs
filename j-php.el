(require 'php-mode)
(require 'xhp-mode)
(require 'highlight-80+)

(add-hook 'php-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            (subword-mode 1)
            (define-key php-mode-map (kbd "M-g") 'tbgs)
            (local-set-key (kbd "RET") 'c-context-line-break)
            (local-set-key (kbd "C-c a") 'php-array-align)
            (local-set-key (kbd "C-M-T") 'pfff-infer-type-at-point)
            (c-set-offset 'case-label 2)
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)
            ))

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

(provide 'j-php)
