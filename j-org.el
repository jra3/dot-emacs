(require 'org-ac)
(org-ac/config-default)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

;; (require 'org-protocol)

;; An example of how custom sub-protocols work

;; (add-to-list 'org-protocol-protocol-alist
;;              '("Hello World" :protocol "hello-world"
;;                :function my-hello-world
;; 	       :kill-client t
;; 	       ))
;; ;; org-protocol:/hello-world:/

;; (defun my-hello-world (data)
;;   "Say hello to the world."
;;   (message data)
;;   (sit-for 3)
;;   nil)

(require 'org-habit)
(setq org-habit-preceding-days 14
      org-habit-following-days 1
      org-habit-show-habits-only-for-today t
      org-habit-graph-column 70
      org-habit-show-all-today t)

; I prefer return to activate a link
(setq org-return-follows-link t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-stuck-projects '("project" ("TODO NEXT") ("action") "\\<IGNORE\\>" ))
(setq org-tags-exclude-from-inheritance '("project"))

; http://www.brool.com/post/using-org-mode-with-gtd/
(setq org-agenda-custom-commands 
      '(
	("w" todo "WAITING" nil ("waiting.txt")) 
	("n" todo "NEXT" nil ("next.html"))
	("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))

	("D" "Daily Action List"
	 (
	  (agenda "" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up) )))
		      (org-deadline-warning-days 0)
		      ))))
	)
      )

(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)

(setq
 org-directory "~/org"

 org-capture-templates '(
					; New inbox item to be processed
			 ("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
			  "* TODO %?\n  %i\n  %a")
			 
					; Generic notebook entry
			 ("n" "notes" entry (file+datetree "~/org/notes.org")
			  "* %? %U\n")
			 
					; Create a new project entry
			 ("p" "project" entry (file+headline "~/org/gtd.org" "Projects")
			  "* %? :project:\n  %i\n")

					; New inbox item to be processed
			 ("a" "action" entry (file+headline "~/org/gtd.org" "Inbox")
			  "* TODO %? :action:\n  %i\n")

					; Freeform journal entry
			 ("j" "Journal" entry (file+datetree "~/org/journal.org")
			  "* %?\nEntered on %U\n  %i\n  %a")
			 
					; Breif TIL journal entry
			 ("l" "TIL" entry (file+datetree "~/org/journal.org" "TIL")
			  "* %?\nLearned on %U :til:\n  %i\n  %a")

					; 5-minute journal. Morning entry
			 ("m" "5min morning" entry (file+datetree "~/org/5-min-journal.org")
			  "* Morning\n  I am grateful for...\n  - %?\n  - \n  - \n\n  What will I do to make today great?\n  - \n  - \n  - \n\n  I am ...")
					; 5-minute journal. Evening entry
			 ("e" "5min evening" entry (file+datetree "~/org/5-min-journal.org")
			  "* Evening\n  3 amazing things that happened today...\n  - %?\n  - \n  - \n\n  How could I have made today even better?\n  - \n")
			 
			 )


 ;; Fontify src blocks
 org-src-fontify-natively t

 org-export-html-style-include-scripts nil
 org-export-html-style-include-default nil
 
 org-outline-path-complete-in-steps nil

 org-refile-use-outline-path t
 org-refile-targets '((nil :maxlevel . 1)
		      (org-agenda-files :maxlevel . 2))
 
 org-default-notes-file (concat org-directory "/notes.org")
 org-agenda-files (list "~/org/gtd.org"			
			"~/org/work.org"
			"~/org/personal.org"
			"~/org/habits.org")

  org-pomodoro-start-sound-p t
 org-publish-use-timestamps-flag nil
 org-startup-folded (quote content))


(setq
 org-hide-leading-stars t
 org-ellipsis " ↴"
 org-return-follows-link t

 ;; only show times on items in the agenda, if we have an item at a specified time
 ;; if we set it to true, then we see all the times every 2 hours.  Takes up too much space.
 org-agenda-use-time-grid nil
 
  ;; whenever I change state from TODO to DONE org will log that timestamp. Let's put that in a drawer
 org-log-into-drawer nil

 ;; make org-mode record the date when you finish a task
 org-log-done 'time
 ;;org-log-done 'nil

 ;; when you press S-down, org changes the timestamp under point
 org-edit-timestamp-down-means-later t

 ;; make the agenda start on today not wednesday
 org-agenda-start-on-weekday nil

 org-agenda-span 1
 ;; this tells the agenda to take up the whole window and hide all other buffers
 org-agenda-window-setup 'current-window
 ;; this tells org-mode to only quit selecting tags for things when you tell it that you are done with it
 org-fast-tag-selection-single-key nil
 org-html-validation-link nil
 org-export-kill-product-buffer-when-displayed t
 ;; are there more backends that I can use?
 org-export-backends '(ascii beamer html texinfo latex)

 ;; load in the org-modules
 ;;org-load-modules-maybe t

 ;; where to put the :action: or :work: tag after a heading.  80 colums over
 org-tags-column 80
 ;; don't ask me if I want to run an babel code block.  I know what I'm doing
 org-confirm-babel-evaluate nil
 ;; activate org speed commands
 org-use-speed-commands t)

;;a visual hint to let you know what line you are in in org-mode agenda
(add-hook 'org-agenda-finalize-hook (lambda () (hl-line-mode)))

(define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)

 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; ;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; ;; Show the clocked-in task - if any - in the header line
;; (defun sanityinc/show-org-clock-in-header-line ()
;;   (setq-default header-line-format '((" " org-mode-line-string " "))))

;; (defun sanityinc/hide-org-clock-from-header-line ()
;;   (setq-default header-line-format nil))

;; (add-hook 'org-clock-in-hook #'sanityinc/show-org-clock-in-header-line)
;; (add-hook 'org-clock-out-hook #'sanityinc/hide-org-clock-from-header-line)
;; (add-hook 'org-clock-cancel-hook #'sanityinc/hide-org-clock-from-header-line)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'j-org)
