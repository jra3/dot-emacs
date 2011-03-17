;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'highlight-80+)
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "M-g") 'fbgs)
            (setq compile-command "cd ~/fbcode; fbmake --color=no dbg -j 20")
            (highlight-80+-mode t)
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)))

(provide 'j-cpp)
