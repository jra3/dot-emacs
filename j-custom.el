(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hack-field-name ((t (:inherit hack-default))))
 '(isearch ((t (:background "blue" :foreground "white"))))
 '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-report-syntactic-errors t)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag t)
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "third_party" "third_party_dev" "node_modules" "interana_tailer/interana_tailer")))
 '(elpy-rpc-python-command "python")
 '(holiday-bahai-holidays nil)
 '(holiday-islamic-holidays nil)
 '(holiday-oriental-holidays nil)
 '(indent-tabs-mode nil)
 '(markdown-command "multimarkdown")
 '(org-agenda-restore-windows-after-quit t)
 '(org-html-postamble-format
   (quote
    (("en" "<p class=\"author\">Author: %a (%e)</p>
powered by <p class=\"creator\">%c</p><p class=\"validation\">%v</p>"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-gnus org-habit org-id org-info org-protocol)))
 '(package-selected-packages
   (quote
    (jsx-mode wanderlust company-rtags helm-rtags ag helm-flycheck rjsx-mode org-mac-link org-mac-iCal org-mime puppet-mode org-ac yaml-mode web-mode thrift tangotango-theme smex rainbow-mode rainbow-delimiters popwin org-pomodoro org-bullets nose markdown-mode magit json-mode js2-refactor js-comint helm-unicode helm-swoop helm-pydoc helm-projectile helm-orgcard helm-flx helm-describe-modes helm-descbinds helm-css-scss helm-company helm-commandlinefu helm-c-yasnippet google-c-style go-rename go-eldoc go-dlv go-autocomplete gitignore-mode gitconfig-mode flymake-yaml flymake-shell flymake-json flymake-google-cpplint flymake-go flymake-cursor flymake-css flymake flycheck exec-path-from-shell elpy diminish crontab-mode color-identifiers-mode coffee-mode anzu ac-js2 2048-game)))
 '(projectile-use-git-grep t)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(send-mail-function (quote smtpmail-send-it)))
