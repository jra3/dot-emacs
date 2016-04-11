(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(autoload 'css-mode "css-mode" nil t)

(require 'rainbow-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode t)))

(setq css-indent-level 2
      css-indent-offset 2)

(provide 'j-css)
