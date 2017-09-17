(require 'subr-x)

(defface helm-myles-finish
    '((t (:foreground "Green")))
  "Face used in mode line when myles process is finish.")

(defun helm-myles-init ()
  "Initialize async myles process for `helm-source-myles'."
  (let* ((cmd (format "myles --client emacs --list --limit 100 %s"
                      (shell-quote-argument helm-pattern)))
         ;;(default-directory (string-trim (shell-command-to-string "hg root")))
         )

    (helm-log "Starting helm-myles process")
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))

    (prog1
        (start-process-shell-command
         "myles-process" helm-buffer
         cmd)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (unless (cdr helm-sources)
                      (insert "* Exit with code 1, no result found"))))
                 ((string= event "finished\n")
                  (with-helm-window
                    (setq mode-line-format
                          '(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format "[myles process finished - (%s results)]"
                                            (max (1- (count-lines
                                                      (point-min) (point-max)))
                                                 0))
                                    'face 'helm-myles-finish))))
                    (force-mode-line-update)))
                 (t
                  (helm-log "Error: myles %s"
                            (replace-regexp-in-string "\n" "" event))))))))
))

(defclass helm-myles-source (helm-source-async helm-type-file)
  ((candidates-process :initform 'helm-myles-init)
   (requires-pattern :initform 4)
   (history :initform 'helm-file-name-history)
   (candidate-number-limit :initform 300)))

(defvar helm-source-myles
  (helm-make-source "myles" 'helm-myles-source))

(defun helm-myles (arg)
  (interactive "P")
  (require 'helm-files)
  (let* ((default-directory (string-trim (shell-command-to-string "hg root"))))
    (helm :sources 'helm-source-myles
          :buffer "*helm myles*"
          :ff-transformer-show-only-basename nil
          :input nil
          :default (thing-at-point 'filename)
          :history 'helm-file-name-history)))


(global-set-key (kbd "C-M-l") 'helm-myles)

(provide 'helm-myles)
