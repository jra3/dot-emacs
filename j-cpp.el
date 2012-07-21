;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "M-g") 'fbgs)
            (define-key c++-mode-map (kbd "M-S-o") 'ff-get-other-file)
            (setq compile-command "cd ~/fbcode; fbmake --fast --color=no dbg -j 20 -w")
            (highlight-80+-mode t)
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (local-set-key [f8] 'phplint-thisfile)
             ))

(provide 'j-cpp)
