(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg/")

(setq org-agenda-files (quote ("~/Dropbox/org/work.org")))
(setq org-mobile-inbox-for-pull "~/Dropbox/org/index.org")

(provide 'j-org)
