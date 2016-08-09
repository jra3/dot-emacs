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
 '(org-agenda-files
   (quote
    ("~/org/gtd.org" "~/org/gtd.org_archive" "~/org/habits.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-span (quote day))
 '(projectile-use-git-grep t)
 '(safe-local-variable-values
   (quote
    ((eval setq default-directory
	   (locate-dominating-file buffer-file-name ".dir-locals.el"))))))
