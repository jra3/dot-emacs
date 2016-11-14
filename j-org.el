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
      org-habit-graph-column 66
      org-habit-show-all-today nil)

; I prefer return to activate a link
(setq org-return-follows-link t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-stuck-projects '("+project" ("TODO" "NEXT") () "\\<IGNORE\\>" ))
(setq org-tags-exclude-from-inheritance '("project"))

; http://www.brool.com/post/using-org-mode-with-gtd/
(setq org-agenda-custom-commands 
      '(
	;; ("w" todo "WAITING" nil ("waiting.txt")) 
	;; ("n" todo "NEXT" nil ("next.html"))

	("D" "Daily Action List"
	 (
	  (agenda "" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up) )))
		      (org-deadline-warning-days 0)
		      ))))
	
	("d" "Agenda + Next Actions"
	 (
	  (agenda "honker" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up) )))
		      (org-deadline-warning-days 0)
		      ))
	  (todo "NEXT")) nil ("next.html")
	  )

	("w" "Weekly Agenda + Next Actions"
	 (
	  (agenda "weekly" ((org-agenda-ndays 7)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up) )))
		      (org-deadline-warning-days 0)
		      ))
	  (todo "NEXT")) nil ("week.html")
	  )

	("W" "Weekly Review"
	 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
	  ;; type "l" in the agenda to review logged items 
	  (stuck "") ;; review stuck projects as designated by org-stuck-projects
	  (tags-todo "project") ;; review all projects
	  (todo "MAYBE") ;; review someday/maybe items
	  (todo "WAITING"))) ;; review waiting items 
	       
	)
      )

(defun org-id-finish (id)
  (org-id-goto id)
  (org-todo "DONE")
  )

(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)

(add-hook 'org-after-refile-insert-hook 'save-buffer)
(add-hook 'org-capture-after-finalize-hook 'save-buffer)

(setq
 org-directory "~/org"
 
 org-capture-templates '(

			 ("t" "Todo" entry ; New inbox item to be processed
			  (file+headline "~/org/gtd.org" "Inbox")
			  "* TODO %?\n  %i\n  %a")

			 ("p" "Project" entry ; Create a new project entry
			  (file+headline "~/org/gtd.org" "Projects")
			  "* %? :project:\n  %i\n")

			 ("a" "Action" entry ; New inbox item to be processed
			  (file+datetree "~/org/gtd.org" "Inbox")
			  "* TODO %? :action:\n  %i\n")

			 ("j" "Journal Entries")			 			 
			 ("jm" "Precious Memory" entry ; Freeform journal entry
			  (file+datetree "~/org/journal.org")
			  "* %? :memory:\n  %i\n  %a")
			 ("jl" "Today I Learned" entry ; Breif TIL journal entry
			  (file+datetree "~/org/journal.org" "TIL")
			  "* %?\nLearned on %U :til:\n  %i\n  %a")
			 ("j." "Journal" entry ; Freeform journal entry
			  (file+datetree "~/org/journal.org")
			  "* %?\nEntered on %U\n  %i\n  %a")

			 ("5" "5 Minute Journal")
			 ("5m" "Morning Entry" entry (file+datetree "~/org/5-min-journal.org")
			  "* Morning\n  I am grateful for...\n  - %?\n  - \n  - \n\n  What will I do to make today great?\n  - \n  - \n  - \n\n  I am ...")
					; 5-minute journal. Evening entry
			 ("5e" "Evening Entry" entry (file+datetree "~/org/5-min-journal.org")
			  "* Evening\n  3 amazing things that happened today...\n  - %?\n  - \n  - \n\n  How could I have made today even better?\n  - \n")

			 ("n" "notes" entry ; Generic notebook entry
			  (file+datetree "~/org/notes.org")
			  "* %? %U\n")

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
			"~/org/diary.org"
			"~/org/habits.org")

 org-pomodoro-start-sound-p t
 org-publish-use-timestamps-flag nil
 org-startup-folded (quote content))

;(add-hook 'org-capture-after-finalize-hook (lambda () (org-id-finish "D51323BB-B5A3-4D22-B232-3182ED2A371A")))

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
