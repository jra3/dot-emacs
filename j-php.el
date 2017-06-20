(require 'hack-mode)
(require 'xhp-mode)
(require 'highlight-80+)

(defun honk ()
  (interactive)
  (c-forward-syntactic-ws)
  )

(let
    ((mode-hook (lambda ()
                  (local-set-key (kbd "C-c t") 'hh-mural-open-dwim)
                  (local-set-key (kbd "M-.") 'hh-client-find-definition)

                  (local-set-key (kbd "M-w") 'honk)
                  (local-set-key (kbd "M-g") 'tbgs)
                  (local-set-key (kbd "M-S-g") 'tbgr)
                  (set (make-local-variable 'require-final-newline) t)

                  (whitespace-cleanup-mode)
                  )))

  (add-hook 'xhp-mode-hook mode-hook)
  (add-hook 'hack-mode-hook mode-hook)

  )

(add-to-list 'auto-mode-alist '("\\.phpt?$" . hack-mode))
(provide 'j-php)
