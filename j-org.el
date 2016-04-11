;; (require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-directory "~/org"

      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files (quote ("~/org/notes.org"))
      org-bullets-bullet-list (quote ("◉" "○" "♥" "✈"))

      org-pomodoro-start-sound-p t

      org-publish-use-timestamps-flag nil
      org-startup-folded (quote content))


;; (setq org-agenda-files (quote ("~/org/work.org" "~/org/personal.org" "~/org/irc.org")))

(provide 'j-org)
