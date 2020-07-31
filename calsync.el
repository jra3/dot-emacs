(let ((ics-files (directory-files-recursively "~/Library/Calendars" "\.ics$"))
      (diary-target "/tmp/diary-sync")
      (diary-final "~/diary.cal"))
  (delete-file diary-target)
  (mapcar
   (lambda (ics-file)
     (icalendar-import-file ics-file diary-target))
   ics-files)
  (rename-file diary-target diary-final t))
