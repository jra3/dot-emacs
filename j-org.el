(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/org")
(setq org-replace-disputed-keys t)

;; (setq org-agenda-files (quote ("~/org/work.org" "~/org/personal.org" "~/org/irc.org")))

(provide 'j-org)
