(fset 'eq-align
      "\C-[xalign-regexp\C-m=\C-m")

;; buffer size manipulation
(fset 'grow-horiz   "\C-x}\C-x}\C-x}\C-x}\C-x}\C-x}\C-x}")
(fset 'shrink-horiz "\C-x{\C-x{\C-x{\C-x{\C-x{\C-x{\C-x{")
(fset 'grow-vert    "\C-x^\C-x^\C-x^\C-x^")

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
     '("\\[\\(ERROR\\|FAILURE\\|WARNING\\):\\(.*\\):\\([0-9]+\\)\\]" 2 3))

(defun edit-file-as-root ()
  (interactive)
  (let ((bfn buffer-file-name))
    (find-file (concatenate 'string "/sudo:root@localhost:" bfn))))

(defun windstick (&optional n)
  (interactive "P")
  (set-window-dedicated-p (selected-window) t))

(defun windrelease (&optional n)
  (interactive "P")
  (set-window-dedicated-p (selected-window) nil))

;; add color to shells
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

(setq custom-file (concat dot-emacs-dir "/j-custom.el"))
(load custom-file)

(defun jallen-sort-para ()
  "Sorts the paragraph in which the point is located"
  (interactive)
  (save-excursion
    (let (bpoint epoint)
      (backward-paragraph)
      (setq bpoint (point))
      (forward-paragraph)
      (setq epoint (point))
      (sort-lines nil bpoint epoint)
      )
    ))

;; refresh the list of unixnames as follows:
;; > db -u cdb.org -e 'SELECT unixname from unixname' > ~/.unixnames
;; > cat ~/.unixnames | xargs -n1 groups | grep engineers
;;   | cut -f1 -d\ > ~/.engineers
(defun jallen-insert-engineer-unixname (&rest ARGS)
  "Autocompletes engineer unixnames for insertion at point"
  (interactive)
  (insert
   (ido-completing-read "Reviewers: "
                        (jallen-read-lines "~/.engineers"))
   ))

(defun jallen-read-lines (fpath)
  "Return a list of lines of a file at at FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (split-string (buffer-string) "\n" t)))

(provide 'j-util)
