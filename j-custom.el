(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-user-configuration (quote ((nil notifier nil))))
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-mode 1)
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
                  (company-dabbrev-code company-gtags company-etags)
                  company-oddmuse company-files company-dabbrev)))
 '(compilation-ask-about-save nil)
 '(css-indent-level 2)
 '(css-indent-offset 2)
 '(current-language-environment "utf-8")
 '(default-frame-alist
    (quote
     ((left-fringe . 1)
      (right-fringe . 1)
      (menu-bar-lines . 0)
      (tool-bar-lines . 0)
      (font . "DejaVu Sans Mono-9"))))
 '(default-input-method "utf-8-prefix")
 '(default-major-mode (quote text-mode) t)
 '(display-buffer-reuse-frames nil)
 '(ediff-highlight-all-diffs nil)
 '(elpy-default-minor-modes
   (quote
    (eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)))
 '(elpy-mode-hook (quote (subword-mode hl-line-mode)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "third_party" "third_party_dev" "node_modules")))
 '(elpy-project-root-finder-functions (quote (elpy-project-find-projectile-root)))
 '(elpy-rgrep-ignored-directories (quote (".tox" "build" "dist" "third_party")))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "/opt/interana/third_party/bin/python2.7")
 '(elpy-test-nose-runner-command (quote ("nosetests" "-vs")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(espresso-indent-level 2)
 '(fill-column 78)
 '(flymake-no-changes-timeout 1)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(global-auto-revert-mode t)
 '(grep-command nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-80+-columns 119)
 '(indent-tabs-mode nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(large-file-warning-threshold nil)
 '(line-number-mode 1)
 '(markdown-open-command "mark")
 '(mode-require-final-newline t)
 '(org-agenda-files (quote ("~/org/notes.org")))
 '(org-bullets-bullet-list (quote ("◉" "○" "♥" "✈")))
 '(org-pomodoro-start-sound-p t)
 '(org-publish-use-timestamps-flag nil)
 '(org-startup-folded (quote content))
 '(popwin:special-display-config
   (quote
    (("*Ibuffer*" :position top :noselect t :height 30)
     ("*Python Check*" :position top :noselect t :height 30)
     ("*magit-log*")
     ("*compilation*")
     ("*Python Doc*")
     ("*grep*")
     ("*Help*")
     ("*Completions*" :noselect t)
     ("*Occur*" :noselect t))))
 '(projectile-use-git-grep t)
 '(py-basic-offset 4 t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(show-paren-mode t nil (paren))
 '(split-height-threshold 10000)
 '(split-width-threshold 10000)
 '(transient-mark-mode t)
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-pairing t)
 '(web-mode-enable-auto-quoting t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(yas-snippet-dirs (quote ("~/.dot-emacs/snippets")) nil (yasnippet)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color) (background dark)) (:background "DimGrey" :box nil))))
 '(flymake-warnline ((((class color)) (:background "Gray20"))))
 '(isearch ((t (:background "blue" :foreground "white"))))
 '(lazy-highlight ((t (:background "yellow" :foreground "black")))))
