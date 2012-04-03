(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg/")

(setq org-agenda-files (quote ("~/org/work.org", "~/org/personal.org", "~/org/irc.org")))
(setq org-mobile-inbox-for-pull "~/org/captured.org")

(setq org-mobile-use-encryption t)
(setq org-mobile-encryption-password (getenv "MOBILE_ORG_PW"))

(provide 'j-org)
