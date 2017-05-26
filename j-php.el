(require 'hack-mode)
(require 'xhp-mode)
(require 'highlight-80+)

(let
x    ((mode-hook (lambda ()
                  (c-set-style "hack-style")
                  (set (make-local-variable 'require-final-newline) t)
                  )))

  (add-hook 'xhp-mode-hook mode-hook)
  (add-hook 'hack-mode-hook mode-hook)

  )

(add-to-list 'auto-mode-alist '("\\.phpt?$" . hack-mode))
(provide 'j-php)
