;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)

(add-hook 'c++-mode-hook
          (lambda ()
            (setq compilation-auto-jump-to-first-error t)
            (define-key c++-mode-map (kbd "C-c o") 'ff-get-other-file)
            (highlight-80+-mode t)
            (subword-mode 1)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'innamespace '-)
            (setq require-final-newline t)))

(provide 'j-cpp)
