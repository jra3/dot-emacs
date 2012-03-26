(require 'rainbow-mode)
(require 'zencoding-mode)
(add-hook 'html-mode-hook (lambda () (zencoding-mode t)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode t)))

(provide 'j-html)