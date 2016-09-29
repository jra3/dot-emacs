;; (require 'org-install)

(require 'org-ac)
(org-ac/config-default)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq
 org-directory "~/org"

 org-capture-templates '(; New inbox item to be processed
			 ("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
			  "* TODO %?\n  %i\n  %a")
			 ; Freeform journal entry
			 ("j" "Journal" entry (file+datetree "~/org/journal.org")
			  "* %?\nEntered on %U\n  %i\n  %a")
			 ; Breif TIL journal entry
			 ("l" "TIL" entry (file+datetree "~/org/journal.org" "TIL")
			  "* %(org-set-tags-to \"til\") %?\nLearned on %U\n  %i\n  %a"))

 org-default-notes-file (concat org-directory "/notes.org")
 org-agenda-files (list "~/org/gtd.org"
			"~/org/gtd.org_archive"
			"~/org/habits.org"
			"~/org/calendar.org")
 org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE")

 org-agenda-include-diary t
 org-agenda-include-all-todo t
 
 org-bullets-bullet-list (quote ("◉" "○" "♥" "✈"))
 
 org-pomodoro-start-sound-p t
 
 org-publish-use-timestamps-flag nil
 org-startup-folded (quote content))

 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(provide 'j-org)
