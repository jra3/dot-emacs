;; (require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; (setq org-agenda-files (quote ("~/org/work.org" "~/org/personal.org" "~/org/irc.org")))

(provide 'j-org)
