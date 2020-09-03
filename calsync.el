;; $EMACS --batch -l ~/.emacs.d/calsync.el
(let ((ics-files (directory-files-recursively "~/Library/Calendars" "\.ics$"))
      (diary-target "/tmp/diary-sync")
      (diary-final "~/diary_cal"))
  (delete-file diary-target)
  (mapcar
   (lambda (ics-file)
     (icalendar-import-file ics-file diary-target))
   ics-files)
  (shell-command (format "grep -v '^ ' %s | sort | uniq > %s" diary-final diary-target)))
