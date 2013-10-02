;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)
(require 'google-c-style)

(add-hook 'c++-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ;; (c-set-offset 'arglist-intro '+)
            ;; (c-set-offset 'innamespace '-)
            (define-key c++-mode-map (kbd "C-c o") 'ff-get-other-file)
            (subword-mode 1)
            (setq require-final-newline t)
            (setq compilation-auto-jump-to-first-error t)
            ))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(provide 'j-cpp)
