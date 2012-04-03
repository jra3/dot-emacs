(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg/")

(setq org-agenda-files (quote ("~/org/work.org")))
(setq org-mobile-inbox-for-pull "~/org/index.org")

(provide 'j-org)
