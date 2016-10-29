(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color) (background dark)) (:background "DimGrey" :box nil))))
 '(flymake-warnline ((((class color)) (:background "Gray20"))))
 '(isearch ((t (:background "blue" :foreground "white"))))
 '(lazy-highlight ((t (:background "yellow" :foreground "black")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "third_party" "third_party_dev" "node_modules" "interana_tailer/interana_tailer")))
 '(markdown-command "multimarkdown")
 '(org-agenda-restore-windows-after-quit t)
 '(org-stuck-projects
   (quote
    ("+project"
     ("TODO NEXT")
     ("action")
     "\\<IGNORE\\>")))
 '(package-selected-packages
   (quote
    (org-mime puppet-mode org-ac yaml-mode web-mode thrift tangotango-theme smex rainbow-mode rainbow-delimiters popwin org-pomodoro org-bullets nose markdown-mode magit json-mode js2-refactor js-comint helm-unicode helm-swoop helm-pydoc helm-projectile helm-orgcard helm-flymake helm-flx helm-describe-modes helm-descbinds helm-css-scss helm-company helm-commandlinefu helm-c-yasnippet google-c-style go-rename go-eldoc go-dlv go-autocomplete gitignore-mode gitconfig-mode flymake-yaml flymake-shell flymake-json flymake-google-cpplint flymake-go flymake-cursor flymake-css flymake flycheck exec-path-from-shell elpy diminish crontab-mode color-identifiers-mode coffee-mode anzu ac-js2 2048-game)))
 '(projectile-use-git-grep t)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
	   (locate-dominating-file buffer-file-name ".dir-locals.el"))))))
