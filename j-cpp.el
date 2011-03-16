
;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "M-g") 'fbgs)
            (setq compile-command "cd ~/fbcode; fbmake --color=no dbg -j 20")
            
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)))

;;(highlight-80+-mode t)
(provide 'j-cpp)
