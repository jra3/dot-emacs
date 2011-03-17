(require 'cl)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'highlight-80+)
(add-hook 'js2-mode-hook
          (lambda ()
            (highlight-80+-mode t)))

(provide 'j-js)
