(require 'highlight-80+)

(require 'lsp-mode)
(require 'lsp-common)

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(lsp-flycheck-add-mode 'hack)

;; (require 'hack-mode)

(lsp-define-stdio-client 'hack-mode "hack" 'stdio
                         ;; find the project root by looking for the .hhconfig file
                         (lsp-make-traverser #'(lambda (dir)
                                                 (directory-files
                                                  dir
                                                  nil
                                                  "\\.hhconfig")))
                         "Hack Language Server"
                         '("hh" "lsp");; "--from=emacs")
                         )


(let
    ((mode-hook (lambda ()
                  ;; (local-set-key (kbd "C-c t") 'hh-mural-open-dwim)
                  ;; (local-set-key (kbd "M-.") 'hh-client-find-definition)

                  (local-set-key (kbd "M-g") 'tbgs)
                  (local-set-key (kbd "M-S-g") 'tbgr)
                  (set (make-local-variable 'require-final-newline) t)

                  ;; (lsp-mode t)
                  ;; (flycheck-mode)
                  (whitespace-cleanup-mode)
                  )))

  (add-hook 'hack-mode-hook mode-hook)

  )

(provide 'j-php)
