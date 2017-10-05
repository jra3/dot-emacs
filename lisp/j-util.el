(defun align-eq ()
  "Aligns multiple lines so that = lines up, as with variable assignments."
  (interactive)
  (align-regexp " = "))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
     '("\\[\\(ERROR\\|FAILURE\\|WARNING\\):\\(.*\\):\\([0-9]+\\)\\]" 2 3))

(defun edit-file-as-root ()
  (interactive)
  (let ((bfn buffer-file-name))
    (find-file (concatenate 'string "/sudo:root@localhost:" bfn))))

;; (defun windstick (&optional n)
;;   (interactive "P")
;;   (set-window-dedicated-p (selected-window) t))

;; (defun windrelease (&optional n)
;;   (interactive "P")
;;   (set-window-dedicated-p (selected-window) nil))

;; add color to shells
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

(setq custom-file "~/.emacs.d/lisp/j-custom.el")
(load custom-file)


(defun jallen-read-lines (fpath)
  "Return a list of lines of a file at at FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (split-string (buffer-string) "\n" t)))

(provide 'j-util)
