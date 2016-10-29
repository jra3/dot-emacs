;; (require 'org-install)

(require 'org-ac)
(org-ac/config-default)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

(require 'org-protocol)
(require 'org-habit)
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
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
	("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
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

			 )

 org-outline-path-complete-in-steps nil

 org-refile-use-outline-path t
 org-refile-targets '((nil :maxlevel . 1)
		      (org-agenda-files :maxlevel . 2))
 
 org-default-notes-file (concat org-directory "/notes.org")
 org-agenda-files (list "~/org/gtd.org"			
			"~/org/work.org"
			"~/org/personal.org")


 org-agenda-include-all-todo t 
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
 org-log-into-drawer t

 ;; make org-mode record the date when you finish a task
 org-log-done 'time
 ;;org-log-done 'nil

 ;; when you press S-down, org changes the timestamp under point
 org-edit-timestamp-down-means-later t

 ;; make the agenda start on today not wednesday
 org-agenda-start-on-weekday nil

 ;; don't make the agenda only show saturday and Sunday if today is saturday. Make it show 7 days
 org-agenda-span 7
 ;; this tells the agenda to take up the whole window and hide all other buffers
 org-agenda-window-setup 'current-window
 ;; this tells org-mode to only quit selecting tags for things when you tell it that you are done with it
 org-fast-tag-selection-single-key nil
 org-html-validation-link nil
 org-export-kill-product-buffer-when-displayed t
 ;; are there more backends that I can use?
 org-export-backends '(ascii beamer html texinfo latex)
 ;;most of these modules let you store links to various stuff in org

 org-modules '(org-bbdb org-gnus org-info man org-habit org-mime org-clock org-crypt org-bullets org-id)

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
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook #'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook #'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook #'sanityinc/hide-org-clock-from-header-line)

;; (eval-after-load 'org-clock
;;   (define-key org-clock-mode-line-map [header-line mouse-2] #'org-clock-goto)
;;   (define-key org-clock-mode-line-map [header-line mouse-1] #'org-clock-menu))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice capture-finalize (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice capture-destroy (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame if it is the rememeber frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (org-capture)
  )

(provide 'j-org)
