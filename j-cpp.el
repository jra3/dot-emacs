;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c++-mode-map (kbd "M-g") 'fbgs)
	    (define-key c++-mode-map (kbd "M-S-o") 'ff-get-other-file)
	    (highlight-80+-mode t)
	    (subword-mode 1)
	    (c-set-offset  'arglist-intro '+)
	    (setq require-final-newline t)))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (local-set-key [f8] 'phplint-thisfile)
	     ))

(provide 'j-cpp)
