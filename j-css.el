
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(autoload 'css-mode "css-mode" nil t)

(require 'rainbow-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode t)))

(provide 'j-css)
