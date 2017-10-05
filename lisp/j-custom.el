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
 '(c-default-style
   (quote
    ((java-mode . "java")
     (pike-mode . "pike")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-report-syntactic-errors t)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag t)
 '(company-bbdb-modes (quote (message-mode text-mode)))
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "third_party" "third_party_dev" "node_modules" "interana_tailer/interana_tailer")))
 '(elpy-rpc-python-command "python")
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc python-flake8)))
 '(holiday-bahai-holidays nil)
 '(holiday-islamic-holidays nil)
 '(holiday-oriental-holidays nil)
 '(indent-tabs-mode nil)
 '(lsp-enable-codeaction nil)
 '(lsp-enable-completion-at-point t)
 '(lsp-enable-eldoc t)
 '(lsp-enable-flycheck t)
 '(lsp-enable-indentation nil)
 '(lsp-enable-xref t)
 '(lsp-flow-binary
   "/data/users/jallen/www-hg/node_modules/.bin/flow-language-server")
 '(lsp-highlight-symbol-at-point t)
 '(lsp-response-timeout 3)
 '(markdown-command "multimarkdown")
 '(newsticker-url-list
   (quote
    (("Schneier on Security" "https://www.schneier.com/blog/atom.xml" nil nil nil)
     ("Xah Emacs Blog" "http://ergoemacs.org/emacs/blog.xml" nil nil nil)
     ("Sacha Chua" "http://sachachua.com/blog/feed/" nil nil nil))))
 '(org-agenda-restore-windows-after-quit t)
 '(org-html-postamble-format
   (quote
    (("en" "<p class=\"author\">Author: %a (%e)</p>
powered by <p class=\"creator\">%c</p><p class=\"validation\">%v</p>"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-gnus org-habit org-id org-info org-protocol)))
 '(org-protocol-project-alist
   (quote
    (("http://orgmode.org/worg/" :base-url "http://orgmode.org/worg/" :working-directory "/Users/jallen/org/" :online-suffix ".html" :working-suffix ".org"))))
 '(package-selected-packages
   (quote
    (avy minimap highlight-80+ uniquify use-package dired-details yaml-mode whitespace-cleanup-mode web-mode wanderlust thrift tangotango-theme smex rainbow-mode rainbow-delimiters powerline popwin org-pomodoro nose markdown-mode magit lsp-python json-mode js2-refactor js-comint htmlize helm-unicode helm-pydoc helm-projectile helm-orgcard helm-flycheck helm-flx helm-describe-modes helm-descbinds helm-css-scss helm-company helm-c-yasnippet google-c-style go-rename go-guru go-eldoc go-autocomplete gitignore-mode gitconfig-mode exec-path-from-shell elpy diminish crontab-mode company-flow color-identifiers-mode coffee-mode bbdb anzu ag)))
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
           (locate-dominating-file buffer-file-name ".dir-locals.el")))))
 '(send-mail-function (quote smtpmail-send-it))
 '(yas-snippet-dirs
   (quote
    ("/Users/jallen/.emacs.d/snippets" yas-installed-snippets-dir))))
