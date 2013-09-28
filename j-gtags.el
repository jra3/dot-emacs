(autoload 'gtags-mode "gtags" "" t)

;; Move this to individual language configs
(add-hook 'c-mode-hook
   '(lambda ()
      (gtags-mode 1)
))






(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(add-hook 'after-save-hook #'gtags-update-hook)



;; (defun gtags-update-single(filename)  
;;   "Update Gtags database for changes in a single file"
;;   (interactive)
;;   (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

;; (defun gtags-update-current-file()
;;   (interactive)
;;   (defvar filename)
;;   (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
;;   (gtags-update-single filename)
;;   (message "Gtags updated for %s" filename))

;; (defun gtags-update-hook()
;;   "Update GTAGS file incrementally upon saving a file"
;;   (when gtags-mode
;;     (when (gtags-root-dir)
;;       (gtags-update-current-file))))
;; (add-hook 'after-save-hook 'gtags-update-hook)





(provide 'j-gtags)