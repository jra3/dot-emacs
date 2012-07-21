;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;; (defun my-tab-fix ()
;;   (local-set-key [tab] 'indent-or-expand))

;; (add-hook 'php-mode-hook 'my-tab-fix)
;; (add-hook 'c-mode-hook          'my-tab-fix)
;; (add-hook 'sh-mode-hook         'my-tab-fix)
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; more mode hooks, yada yada, etc ...

(provide 'j-completion)